!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine sci_report

!     + + + INCLUDE + + +
      include 'p1werm.inc'
      include 'command.inc'
      include 'file.inc'
      include 'm1subr.inc'
      include 's1dbh.inc'
      include 'erosion/e2grid.inc'
      include 'erosion/m2geo.inc'
      include 'main/sci_report_val.inc'
      include 'manage/man.inc'

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      integer isr, texclass
      integer idx, jdy, ngdpt(nsubr), tngdpt
      real sarea, tarea, cellarea, texmult, avgtexmult
      real avgallbiomass, avgallerosion, avgallstir, avgallenergy
      real avgallwatererosion
      real allbiomass_avg(nsubr), allerosion_avg(nsubr)
      real stir_avg(nsubr), energy_avg(nsubr)
      real adjtotalRennerOM, totalEros, sci_final
      real sci_om_factor, sci_er_factor, sci_fo_factor
      real totalRennerOM, rennerstir, totalRennerEros
      real totalRennerErosWater, totalRennerErosWind

      parameter (totalRennerOM = 0.35155) ! 3136.5 pounds/acre base value
      parameter (rennerstir = 101.0) ! base value
      parameter (totalRennerEros = 0.56939) ! make the sum
      parameter (totalRennerErosWater = 0.56939) ! 2.54 T/A/yr base value
      parameter (totalRennerErosWind = 0.0) ! base value

!     + + + LOCAL DEFINITIONS + + +
!     isr - subregion index
!     texclass - texture class index, see defn in usdatx.for
!     idx - index on x erosion grid direction
!     jdy - index on y erosion grid direction
!     ngdpt - number of grid points in each subregion
!     tngdpt - total number of grid points (all subregions)
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
      do isr = 1, nsubr

          allbiomass_avg(isr) = allbiomass_sum(isr) / days_sum(isr)
          allerosion_avg(isr) = allerosion_sum(isr)*365.25/days_sum(isr) 

          ! initialize subregion area totals
          sarea = 0.0

          do idx = 1, imax-1
              do jdy = 1, jmax-1
                  if( csr(idx,jdy) .eq. isr ) then
                      sarea = sarea + cellarea
                  end if
               end do
          end do

          tarea = tarea + sarea
          avgallbiomass = avgallbiomass + allbiomass_avg(isr) * sarea
          avgallerosion = avgallerosion + allerosion_avg(isr) * sarea
          avgallwatererosion =avgallwatererosion+WaterErosion(isr)*sarea

          ! find soil texture class from surface layer sand and clay content
          call usdatx( asfsan(1,isr), asfcla(1,isr), texclass )
          select case (texclass)
          case(1)  ! SAND
              texmult = 1.6
          case(2)  ! LOAMY SAND
              texmult = 1.6
          case(3)  ! SANDY LOAM
              texmult = 1.37
          case(4)  ! LOAM
              texmult = 1.37
          case(5)  ! SILT LOAM
              texmult = 1.37
          case(6)  ! SILT
              texmult = 1.37
          case(7)  ! S. CLAY LOAM
              texmult = 1.1
          case(8)  ! CLAY LOAM
              texmult = 1.1
          case(9)  ! SL. CLAY LOAM
              texmult = 1.1
          case(10) ! SANDY CLAY
              texmult = 1.0
          case(11) ! SILTY CLAY
              texmult = 1.0
          case(12) ! CLAY
              texmult = 1.0
          case default
              texmult = 1.4
          end select
          avgtexmult = avgtexmult + texmult * sarea

          ! field operation STIR averaging
          stir_avg(isr) = stir_sum(isr) / mperod(isr)
          avgallstir = avgallstir + stir_avg(isr) * sarea

          ! field operation energy averaging
          energy_avg(isr) = energy_sum(isr) / mperod(isr)
          avgallenergy = avgallenergy + energy_avg(isr) * sarea

      end do

      avgallbiomass = avgallbiomass / tarea
      avgallerosion = -avgallerosion / tarea ! make erosion positive to match renner
      avgallwatererosion = avgallwatererosion / tarea
      avgtexmult = avgtexmult / tarea
      avgallstir = avgallstir / tarea
      avgallenergy = avgallenergy / tarea

      adjtotalRennerOM = totalRennerOM * avgtexmult

      sci_om_factor = (avgallbiomass-adjtotalRennerOM) /adjtotalRennerOM
      sci_er_factor = (totalRennerEros                                  &
     &              - (avgallerosion + avgallwatererosion))             &
     &              / totalRennerEros
      sci_fo_factor = (rennerstir - avgallstir) / rennerstir

      sci_final = 0.4 * sci_om_factor                                   &
     &          + 0.4 * sci_fo_factor                                   &
     &          + 0.2 * sci_er_factor

      ! write headers and values to soil-conditioning.out file
      write(luosci, *) '#Soil_conditioning_index | diesel_energy_L/ha'
      write(luosci, 1000) sci_final, avgallenergy
      write(luosci, *) '#sci_om_factor | sci_er_factor | sci_fo_factor'
      write(luosci, 1001) sci_om_factor, sci_er_factor, sci_fo_factor
      write(luosci, *) '#totalRennerOM | totalRennerErosWater | totalRen&
     &nerErosWind | rennerstir'
      write(luosci, 1002) totalRennerOM, totalRennerErosWater,          &
     &                    totalRennerErosWind, rennerstir
      write(luosci,*) '#avgbiomass | WindEros | WaterEros | avgallstir'
      write(luosci, 1002) avgallbiomass, avgallerosion,                 &
     &                    avgallwatererosion, avgallstir
      write(luosci, *) '#texturemult'
      write(luosci, 1003) avgtexmult

 1000 format( f10.3,' | ', f10.3 )
 1001 format( 2(f10.4,' | '), f10.4 )
 1002 format( 3(f10.4,' | '), f10.4 )
 1003 format( f10.4 )

      return
      end
