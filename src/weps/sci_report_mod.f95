!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sci_report_mod

   ! Routines and variables needed to calculate the soil conditioning index (SCI).

   type sci_summations
      real :: allbiomass
      real :: allerosion
      integer :: days
      real :: stir
      real :: energy
   end type sci_summations

   type(sci_summations), dimension(:), allocatable :: scisum 

  contains

    subroutine sci_report(isr, cellstate, soil )

!     + + + PURPOSE + + +
!     Calculate and write to file the SCI values for each subregion or
!     for subregion 0, calculate an area averaged value of SCI.

      use weps_cmdline_parms, only: soil_cond
      use soil_data_struct_defs, only: soil_def
      use file_io_mod, only: luosci
      use erosion_data_struct_defs, only: cellsurfacestate
      use manage_data_struct_defs, only: manFile
      use grid_mod, only: imax, jmax, ix, jy
      use sci_soil_texture_mod, only: get_sci_soil_multiplier

!     + + + ARGUMENT DECLARATIONS + + +
      integer :: isr                                                        ! subregion index
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
      type(soil_def), dimension(0:), intent(in) :: soil 

!     + + + LOCAL VARIABLES + + +
      integer idx, jdy
      integer sdx, nsubr
      real tarea, cellarea, texmult, avgtexmult
      real avgallbiomass, avgallerosion, avgallstir, avgallenergy
      real avgallwatererosion
      real, dimension(:), allocatable :: sarea
      real, dimension(:), allocatable :: sarea_ratio
      real, dimension(:), allocatable :: allbiomass_avg
      real, dimension(:), allocatable :: allerosion_avg
      real, dimension(:), allocatable :: stir_avg
      real, dimension(:), allocatable :: energy_avg
      integer :: sum_stat, alloc_stat
      real adjtotalRennerOM, sci_final
      real sci_om_factor, sci_er_factor, sci_fo_factor
      real totalRennerOM, rennerstir, totalRennerEros
      real totalRennerErosWater, totalRennerErosWind

      parameter (totalRennerOM = 0.35155) ! 3136.5 pounds/acre base value
      parameter (rennerstir = 101.0) ! base value
      parameter (totalRennerEros = 0.56939) ! make the sum
      parameter (totalRennerErosWater = 0.56939) ! 2.54 T/A/yr base value
      parameter (totalRennerErosWind = 0.0) ! base value

!     + + + LOCAL DEFINITIONS + + +
!     texclass - texture class index, see defn in usdatx.for
!     idx - index on x erosion grid direction
!     jdy - index on y erosion grid direction
!     sarea - area for each subregion
!     tarea - total area (all subregions)
!     cellarea - area of each cell (m^2)
!     allbiomass_avg - all biomass average over all days by subregion (kg/m^2)
!     allerosion_avg - all erosion average over all days by subregion (kg/m^2/yr)
!     stir_avg - average annual stir value by subregion
!     energy_avg - average annual energy value by subregion
!     totalRennerOM - base value of renner daily average total organic matter
!                     converted to kg/m^2/yr
!                    ( (1664.0 subsurf + 375.5 stand + 1097.0 flat) / 8921.791)
!     rennerstir - base value of STIR for renner rotation (Average annual total)
!     totalRennerEros - Sum of renner wind and water erosion
!     totalRennerErosWater - base value for renner erosion Water
!                       converted to kg/m^2/yr (2.54 * 0.2241702)
!     totalRennerErosWind - base value for renner erosion Wind (0)

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! NOTE: area weighting used below assumes uniform square grid
      !       if cell area by x,y index is available, change is simple
      ! initialize area totals
      tarea = 0.0
      cellarea = (ix*jy)
      avgtexmult = 0.0
      avgallbiomass = 0.0
      avgallerosion = 0.0
      avgallstir = 0.0
      avgallenergy = 0.0
      avgallwatererosion = 0.0

      if( isr .eq. 0 ) then
        nsubr = size(luosci) - 1
        sum_stat = 0
        allocate( sarea(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( sarea_ratio(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( allbiomass_avg(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( allerosion_avg(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( stir_avg(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( energy_avg(nsubr), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        if( sum_stat .gt. 0 ) then
           Write(*,*) 'ERROR: unable to allocate sci_report sum arrays'
        end if

        ! find subregion areas
        do sdx = 1, nsubr
          ! initialize subregion area totals
          sarea(sdx) = 0.0

          do idx = 1, imax-1
              do jdy = 1, jmax-1
                  if( cellstate(idx,jdy)%csr .eq. sdx ) then
                      sarea(sdx) = sarea(sdx) + cellarea
                  end if
               end do
          end do

          tarea = tarea + sarea(sdx)
        end do

        ! find subregion area ratios
        do sdx = 1, nsubr
          sarea_ratio(sdx) = sarea(sdx) / tarea
        end do

        do sdx = 1, nsubr

          allbiomass_avg(sdx) = scisum(sdx)%allbiomass / scisum(sdx)%days
          allerosion_avg(sdx) = scisum(sdx)%allerosion * 365.25 / scisum(sdx)%days 
          avgallbiomass = avgallbiomass + allbiomass_avg(sdx) * sarea_ratio(sdx)
          avgallerosion = avgallerosion + allerosion_avg(sdx) * sarea_ratio(sdx)
          avgallwatererosion =avgallwatererosion+soil(sdx)%WaterErosion*sarea_ratio(sdx)

          ! get soil texture multiplier
          texmult  = get_sci_soil_multiplier(sdx)
          avgtexmult = avgtexmult + texmult * sarea_ratio(sdx)

          ! field operation STIR averaging
          stir_avg(sdx) = scisum(sdx)%stir / manFile(sdx)%mperod
          avgallstir = avgallstir + stir_avg(sdx) * sarea_ratio(sdx)

          ! field operation energy averaging
          energy_avg(sdx) = scisum(sdx)%energy / manFile(sdx)%mperod
          avgallenergy = avgallenergy + energy_avg(sdx) * sarea_ratio(sdx)

        end do

        avgallerosion = -avgallerosion ! make erosion positive to match renner

      else
          avgallbiomass = scisum(isr)%allbiomass / scisum(isr)%days
          avgallerosion = -scisum(isr)%allerosion * 365.25 / scisum(isr)%days  ! make erosion positive to match renner
          avgallwatererosion = soil(isr)%WaterErosion

          ! get soil texture multiplier
          texmult  = get_sci_soil_multiplier(isr)
          avgtexmult = texmult

          ! field operation STIR averaging
          avgallstir = scisum(isr)%stir / manFile(isr)%mperod

          ! field operation energy averaging
          avgallenergy = scisum(isr)%energy / manFile(isr)%mperod

      end if

      adjtotalRennerOM = totalRennerOM * avgtexmult

      sci_om_factor = (avgallbiomass-adjtotalRennerOM) /adjtotalRennerOM
      sci_er_factor = (totalRennerEros - (avgallerosion + avgallwatererosion)) / totalRennerEros
      sci_fo_factor = (rennerstir - avgallstir) / rennerstir

      sci_final = 0.4 * sci_om_factor + 0.4 * sci_fo_factor + 0.2 * sci_er_factor

      ! write headers and values to soil-conditioning.out file
      write(luosci(isr), *) '#Soil_conditioning_index | diesel_energy_L/ha'
      write(luosci(isr), 1000) sci_final, avgallenergy
      write(luosci(isr), *) '#sci_om_factor | sci_er_factor | sci_fo_factor'
      write(luosci(isr), 1001) sci_om_factor, sci_er_factor, sci_fo_factor
      write(luosci(isr), *) '#totalRennerOM | totalRennerErosWater | totalRen&
     &nerErosWind | rennerstir'
      write(luosci(isr), 1002) totalRennerOM, totalRennerErosWater, totalRennerErosWind, rennerstir
      write(luosci(isr),*) '#avgbiomass | WindEros | WaterEros | avgallstir'
      write(luosci(isr), 1002) avgallbiomass, avgallerosion, avgallwatererosion, avgallstir
      write(luosci(isr), *) '#texturemult'
      write(luosci(isr), 1003) avgtexmult

 1000 format( f10.3,' | ', f10.3 )
 1001 format( 2(f10.4,' | '), f10.4 )
 1002 format( 3(f10.4,' | '), f10.4 )
 1003 format( f10.4 )

      return
    end subroutine sci_report

    subroutine sci_cum( isr, restot, cellstate )

      use weps_cmdline_parms, only: soil_cond
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_mod, only: imax, jmax

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(biototal), intent(in) :: restot
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     restot - structure containing residue totals

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      real total
      integer idx, jdy, ngdpt

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! initialize erosion totals
      total = 0.0
      ngdpt = 0
      do idx = 1, imax-1
          do jdy = 1, jmax-1
              if( isr .eq. cellstate(idx,jdy)%csr ) then
                  total = total + cellstate(idx,jdy)%egt
                  ngdpt = ngdpt + 1
              end if
           end do
      end do
      if( ngdpt .gt. 0 ) then
          total = total/ngdpt
      end if

      ! scisum(isr)%allbiomass = scisum(isr)%allbiomass + admtotto4(isr)
      scisum(isr)%allbiomass = scisum(isr)%allbiomass + restot%mftot    &
     &      + restot%msttot + restot%mbgtot + restot%mrttotto4

      scisum(isr)%allerosion = scisum(isr)%allerosion + total
      scisum(isr)%days = scisum(isr)%days + 1

      return
    end subroutine sci_cum

end module sci_report_mod
