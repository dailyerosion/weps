!$Author$
!$Date$
!$Revision$
!$HeadURL$

module weps_output_mod

  contains

    subroutine bpools( isr, plant, restot, biotot, decompfac )

      ! print out many of the biomass pool components (used for debugging purposes)
      ! These files use the following columnar format.  Some are filled with zeros
      ! to make it easier to select specific columns for comparisons between the
      ! crop and individual biomass pools (not all pools have the same variables)

      use weps_main_mod, only: old_run_file, rootp, am0ifl
      use datetime_mod, only: get_psim_doy, get_psim_year, get_psim_daysim, get_psim_juld
      use biomaterial, only: plant_pointer, residue_pointer, biototal, decomp_factors
      use file_io_mod, only: luocrp1, luobio1, makenamnum, makedir, fopenk
      use decomp_data_struct_defs, only: am0dfl
      use climate_input_mod, only: cli_day
      use input_run_xml_mod, only: nsubr

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(decomp_factors), intent(in) :: decompfac

!     + + + LOCAL VARIABLES + + +
      integer :: pjuld  ! present julian day
      integer doy, cy
      real total
      integer :: ipool   ! index for pool number
      integer :: jpool   ! index for moving pools
      type(plant_pointer), pointer :: thisPlant       ! pointer used to interate plant pointer chain
      type(residue_pointer), pointer :: thisResidue   ! pointer used to interate residue pointer chain
      character*30 :: dec_text ! decomposition detail age pool output file name text string
      character*30 :: subr_text ! subregion output directory text string
      integer, dimension(3) :: resday
      real, dimension(3) :: mst
      real, dimension(3) :: mf
      real, dimension(3) :: mbg
      real, dimension(3) :: mrt
      real, dimension(3) :: fscv
      real, dimension(3) :: ffcv
      real, dimension(3) :: ftcv
      real, dimension(3) :: rsai
      real, dimension(3) :: rlai
      real, dimension(3) :: dstm
      real, dimension(3) :: zht

!     + + + END OF SPECIFICATIONS + + +

      pjuld = get_psim_juld(isr)

      if( .not. am0ifl(isr) ) then
        cy = get_psim_year(isr)
        doy = get_psim_doy(isr)
      end if

      if ((am0dfl(isr) .eq. 1).or.(am0dfl(isr).eq.3)) then

        ! day, month, year
        ! flat crop cover, standing crop cover, total crop cover
        ! crop cover fract, crop SAI, crop LAI
        ! total crop biomass, 0.0, standing crop mass
        ! (no "flat crop biomass")
        ! crop root mass, 0.0, crop yield mass
        ! (no "buried crop biomass")
        ! qty crop stems per area, crop height, crop root depth, repr stem dia

        ! Dead Crop Biomass Pool
        ! write file header if still initializing
        if (am0ifl(isr) .eqv. .true.) then
          write(luocrp1(isr),*) '#daysim doy yy Tmin Tmax Tavg', &
              ' Tfacabove', &
              ' Water Wfacstand Wfacflat Ddaystand Ddayflat Mstand1', &
              ' Mstand2 Mstand3 MstandAll Mflat1 Mflat2 Mflat3', &
              ' MflatAll MaboveAll Mburied1 Mburied2 Mburied3', &
              ' MburiedAll Mroot1 Mroot2 Mroot3 MrootAll Cstand1', &
              ' Cstand2 Cstand3 CstandAll Cflat1 Cflat2 Cflat3', &
              ' CflatAll Cstand+flat1 Cstand+flat2 Cstand+flat3', &
              ' Cstand+flatAll SAI1 SAI2 SAI3 SAIAll LAI1 LAI2 LAI3', &
              ' LAIAll Biodrag #stem1 #stem2 #stem3 #stemAll Hstem1', &
              ' Hstem2 Hstem3 HstemAll Mrt4all'

          write(luobio1(isr),*) '#daysim doy cy', &
          ' biotot%ffcvtot biotot%fscvtot biotot%ftcvtot', &
          ' 0.0 biotot%rsaitot biotot%rlaitot', &
          ' biotot%mtot biotot%mftot biotot%msttot', &
          ' biotot%mrttot biotot%mbgtot', &
          ' biotot%dstmtot biotot%zht_ave 0.0 0.0'
        else

          total = restot%msttot + restot%mftot   !sum of standing and flat residue mass, all pools

          ipool = 0
          do ipool = 1, 3
            resday(ipool) = huge(resday)
            mst(ipool) = 0.0
            mf(ipool) = 0.0
            mbg(ipool) = 0.0
            mrt(ipool) = 0.0
            fscv(ipool) = 0.0
            ffcv(ipool) = 0.0
            ftcv(ipool) = 0.0
            rsai(ipool) = 0.0
            rlai(ipool) = 0.0
            dstm(ipool) = 0.0
            zht(ipool) = 0.0
          end do
          ! point to youngest plant (may be living or weed residue)
          thisPlant => plant
          ! interate to get first three resiude pools
          do while ( associated(thisPlant) )
            thisResidue => thisPlant%residue
            ! interate over all residue
            do while (associated(thisResidue))
            
              do ipool = 1, 3
                if( thisResidue%resday .lt. resday(ipool) ) then
                  ! Younger than residue in this pool
                  do jpool = 3, ipool+1, -1
                    ! move pools down one index
                    resday(jpool) = resday(jpool-1)
                    mst(jpool) = mst(jpool-1)
                    mf(jpool) = mf(jpool-1)
                    mbg(jpool) = mbg(jpool-1)
                    mrt(jpool) = mrt(jpool-1)
                    fscv(jpool) = fscv(jpool-1)
                    ffcv(jpool) = ffcv(jpool-1)
                    ftcv(jpool) = ftcv(jpool-1)
                    rsai(jpool) = rsai(jpool-1)
                    rlai(jpool) = rlai(jpool-1)
                    dstm(jpool) = dstm(jpool-1)
                    zht(jpool) = zht(jpool-1)
                  end do
                  ! insert younger residue in this pool
                  resday(ipool) = thisResidue%resday
                  mst(ipool) = thisResidue%deriv%mst
                  mf(ipool) = thisResidue%deriv%mf
                  mbg(ipool) = thisResidue%deriv%mbg
                  mrt(ipool) = thisResidue%deriv%mrt
                  fscv(ipool) = thisResidue%deriv%fscv
                  ffcv(ipool) = thisResidue%deriv%ffcv
                  ftcv(ipool) = thisResidue%deriv%ftcv
                  rsai(ipool) = thisResidue%deriv%rsai
                  rlai(ipool) = thisResidue%deriv%rlai
                  dstm(ipool) = thisResidue%dstm
                  zht(ipool) = thisResidue%zht
                  exit
                end if
              end do

              ! set to next residue
              thisResidue => thisResidue%olderResidue
            end do
            ! point to next older plant
            thisPlant => thisPlant%olderPlant
          end do

          ! insert double blank lines to demarcate years
          if( doy .eq. 1 ) then
              write (luocrp1(isr),'(a)')
              write (luocrp1(isr),'(a)')
          end if

          ! NOTE: tf=temperature factor, wf=water factor, dd=decomposition day
          write(luocrp1(isr),2222) get_psim_daysim(isr), doy, cy, & ! simulation day, day of year, year
          cli_day(pjuld)%tdmn, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdav, decompfac%itcs, & ! tmin, tmax, tavg, tf  
          decompfac%aqua, decompfac%iwcs, decompfac%iwcf, & ! precip, wf standing, wf flat
          decompfac%idds, decompfac%iddf, &                 ! dd standing, dd flat
          mst(1), mst(2), mst(3), restot%msttot, & ! mass, standing
          mf(1), mf(2), mf(3), restot%mftot, &     ! mass, flat
          total, &                                 ! sum of standing and flat residue mass, all pools
          mbg(1), mbg(2), mbg(3), restot%mbgtot, & ! mass, below ground
          mrt(1), mrt(2), mrt(3), restot%mrttot, & ! mass, roots
          fscv(1), fscv(2), fscv(3), restot%fscvtot, & ! cover provided by standing residue (fraction)
          ffcv(1), ffcv(2), ffcv(3), restot%ffcvtot, & ! cover provided by flat residue (fraction)
          ftcv(1), ftcv(2), ftcv(3), restot%ftcvtot, & ! cover provided by standing+flat residue (fraction)
          rsai(1), rsai(2), rsai(3), restot%rsaitot, & ! stem area index 
          rlai(1), rlai(2), rlai(3), restot%rlaitot, & ! leaf area index
          restot%rcdtot, &                             ! biodrag
          dstm(1), dstm(2), dstm(3), restot%dstmtot, & ! stems (no/m2) 
          zht(1), zht(2), zht(3), restot%zht_ave, & ! stem height for each residue pool
          restot%mrttotto4                          ! root mass to 4 inches

   
2222     format (' ',i6,' ',i3,' ',i4,' ', 3f7.1, f7.3, f7.2, 4f7.3, 17(1x,f8.4), 21f7.4, 4(1x,f7.2), 4(1x,f7.3), f8.4)

          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass
          ! qty residue stems per area, "ave" residue height, 0.0, 0.0
          ! (no "ave" root depth or stem dia computed across residue pools)

2345     format (i6,i4,i5,3f10.5,13f10.3)

          ! All Residue Pools Combined
          write(luobio1(isr),2345) get_psim_daysim(isr), doy, cy, &
          biotot%ffcvtot, biotot%fscvtot, biotot%ftcvtot, &
          0.0, biotot%rsaitot, biotot%rlaitot, &
          biotot%mtot, biotot%mftot, biotot%msttot, &
          biotot%mrttot, biotot%mbgtot, &
          biotot%dstmtot, biotot%zht_ave, 0.0, 0.0
        endif
      endif

      if ((am0dfl(isr) .eq. 2).or.(am0dfl(isr).eq.3)) then
          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass, 0.0
          ! (no residue yield mass)
          ! qty residue stems per area, residue height, 0.0, rep stem dia
          ! (no root depth for residue pools)

        ! point to youngest living plant
        thisPlant => plant
        ! interate to get first three resiude pools
        do while ( associated(thisPlant) )
          thisResidue => thisPlant%residue
          ! interate over all residue
          do while (associated(thisResidue))

            if( thisResidue%bout%luo .lt. 0 ) then
              ! output unit not yet opened, open it
              if( old_run_file .and. (nsubr .eq. 1) ) then
                ! create the name
                subr_text = ''
              else
                ! create subregion name
                subr_text = makenamnum( 'subregion', isr, nsubr, '/' )
              end if

              ! create subdirectory for detailed decomp ouput (does nothing if exists)
              call makedir(trim(rootp)//trim(subr_text)//'decomp/' )
              ! create the name (6 digits for plant, 6 digits for residue)
              dec_text = makenamnum('decomp/dec', thisPlant%bout%num, 654321, '_', thisResidue%bout%num, 654321, '.btmp')

              ! assign logical unit number of opening file to array
              call fopenk (thisResidue%bout%luo, trim(rootp) // trim(subr_text) // trim(dec_text), 'unknown')
              ! write header to this new file
              write(thisResidue%bout%luo,*) '#daysim resday resyear doy yy pool#', &
                ' cumddysta cumddyflat cumddybg10 flatcov standcov', &
                ' totalcov covfact silhoutte leafarea totalmass', &
                ' flatmass standmass bgrootmass bgshootmass stemnumb', &
                ' height repstemdia stemstandm leafstandm storstandm', &
                ' stemflatm leafflatm storflatm rstorflatm rfiberflatm',&
                ' stembgm leafbgm storbgm rstorgbm rfibergbm name'
              write(thisResidue%bout%luo,'(a)')
              write(thisResidue%bout%luo,'(a)')
            end if

2355        format (i6,1x,i5,1x,i4,1x,i3,1x,i4,1x,i2,30(1x,f10.5),1x,a30)

            ! Residue Pool
            write(thisResidue%bout%luo,2355) get_psim_daysim(isr), &
                thisResidue%resday, thisResidue%resyear, doy, cy, thisResidue%bout%num, &
                thisResidue%cumdds, thisResidue%cumddf, thisResidue%cumddg(10), &
                thisResidue%deriv%ffcv, thisResidue%deriv%fscv, thisResidue%deriv%ftcv, &
                thisPlant%database%covfact, thisResidue%deriv%rsai, thisResidue%deriv%rlai, &
                thisResidue%deriv%m, thisResidue%deriv%mf, thisResidue%deriv%mst, &
                thisResidue%deriv%mrt, thisResidue%deriv%mbg, &
                thisResidue%dstm, thisResidue%zht, thisResidue%xstmrep, &
                thisResidue%standstem, thisResidue%standleaf, &
                thisResidue%standstore, thisResidue%flatstem, &
                thisResidue%flatleaf, thisResidue%flatstore, &
                thisResidue%flatrootstore, thisResidue%flatrootfiber, &
                thisResidue%deriv%mbgstem, thisResidue%deriv%mbgleaf, &
                thisResidue%deriv%mbgstore, thisResidue%deriv%mbgrootstore, &
                thisResidue%deriv%mbgrootfiber, &
                thisPlant%bname

            ! set to next older residue
            thisResidue => thisResidue%olderResidue
          end do
          ! point to next older plant
          thisPlant => thisPlant%olderPlant
        end do

      endif

    end subroutine bpools

    subroutine plotdata(isr, noerod, manFile, subrsurf, cellstate)

      use weps_main_mod, only: am0ifl
      use datetime_mod, only: get_simdate, get_simdate_doy, get_simdate_daysim, get_simdate_jday
      use file_io_mod, only: luoplt
      use erosion_data_struct_defs, only: threshold
      use erosion_data_struct_defs, only: cellsurfacestate
      use erosion_data_struct_defs, only: awadir, awudmx
      use erosion_data_struct_defs, only: am0efl
      use erosion_data_struct_defs, only: subregionsurfacestate
      use grid_mod, only: imax, jmax
      use hydro_data_struct_defs, only: am0hfl, hydro_state
      use soil_data_struct_defs, only: am0sfl
      use manage_data_struct_defs, only: man_file_struct
      use crop_data_struct_defs, only: am0cfl
      use decomp_data_struct_defs, only: am0dfl
      use climate_input_mod, only: cli_day
      use wind_mod, only: biodrag

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr
      type(threshold), intent(in) :: noerod
      type(man_file_struct), intent(in) :: manFile
      type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
 
!       Edit History
!       04-Mar-99       wjr     created

      integer :: doy   ! day of year
      integer :: day   ! day of month
      integer :: month ! month of year
      integer :: year  ! year of simulation

      integer ngdpt  !number of grid cells within field
      integer idx, jdy   !local loop vars

      real :: total
      real :: suspen
      real :: pmten
      real :: pm2_5
      
      real :: res_rlaitot   ! total of all leaf area index (all residue pools)
      real :: res_rsaitot   ! total of all stem area index (all residue pools)
      real :: res_height    ! sai index averaged crop height (all residue pools)
      real :: res_biodrag   ! total biodrag all residue pools
      real :: crop_rlaitot  ! total of all leaf area index (all crop pools)
      real :: crop_rsaitot  ! total of all stem area index (all crop pools)
      real :: crop_height   ! sai index averaged crop height (all crop pools)
      real :: crop_biodrag  ! total biodrag all crop pools

!     + + + OUTPUT FORMATS + + +
!     format for header of plot file
!2050 format (1x,'#daysim','|','doy','|','day','|','mon','|',' yr ',    &
!    &     '|',' tot_loss ',                                            &
!    &     '|','  suspen  ','|','  pm10 ','|',' max_wind ',             &
!    &     '|',' dir_wind ','|','  precip  ','|',' Surf_H2O',           &
!    &     '|','  ridge_ht ','|',' ridge_or ','|','  r_rough ',         &
!    &     '|','   gmd    ','|',' ag_stab  ','|',' cr_fract ',          &
!    &     '|','loose_mass','|','loose_frac','|',' bulk_den',           &
!    &     '|','   fl_cov%','|','  st_cov% ','|',' crop_lai ',          &
!    &     '|',' crop_sai ','|','crop_st_mass','|','can_cov ')

 2040 format (1x,'#daysim','|','doy','|','day','|','mon','|',' yr ')
 2041 format ('|',' tot_loss ','|','  suspen  ','|','  pm10    ','|','  pm2_5   ')
 2042 format ('|',' max_wind ','|',' dir_wind ','|','  precip  ','|','snow_depth','|',' Surf_H2O ')
 2043 format ('|',' cr_fract ','|',' cr_thick ','|','cr_ms_los ','|','cr_fr_los ')
 2044 format ('|',' ridg_ht  ','|',' ridg_wid ','|',' ridg_sp  ','|',' ridg_or  ','|','ridg_sp_wn')
 2045 format ('|',' dike_ht  ','|',' dike_sp  ','|',' r_rough  ')
 2046 format ('|','  gmd_p   ','|','  gsd_p   ','|','  mnot    ','|','   minf   ')
 2047 format ('|',' ag_stab  ','|',' bulk_den ')
 2048 format ('|',' fl_cov%  ','|',' bio_hght ','|',' bio_sai  ','|',' bio_lai  ')
 2049 format ('|',' crop_hght','|',' crop_sai ','|',' crop_lai ','|',' crop_drag')
 2050 format ('|',' res_av_ht','|',' res_sai  ','|',' res_lai  ','|',' res_drag ')
 2051 format ('|',' ag_cf_abr','|',' cr_cf_abr')
 2052 format ('|',' pm10_abr ','|',' pm10_emt ','|',' pm10_brk ')
 2053 format ('|',' pm2_5_abr','|',' pm2_5_emt','|',' pm2_5_brk')
 2054 format ('|',' sf1ic    ','|',' sf10ic   ','|',' sf84ic   ','|',' sf200ic  ')
 2055 format ('|',' sf1      ','|',' sf10     ','|',' sf84     ','|',' sf200    ')
 2056 format ('|','eros','|','snow', &
              '|','wus_anemom','|','wus_random','|','wus_ridge ', &
              '|','wus_biodrg','|',' ne_wus   ','|','t_ne_bare ', &
              '|','t_flat_cov','|','t_surf_wet','|',' t_ag_den ', &
              '|',' t_wust   ')
 2057 format ('|','rwus_anemo','|','rwus_rando','|','rwus_ridge','|','rwus_biodr')
 2058 format ('|','r_ne_bare ','|','r_flat_cov','|','r_surf_wet','|',' r_ag_den ')
 2059 format ('|',' ne_sf84  ','|',' ne_rock  ','|',' ne_wzzo  ','|',' ne_sfcv  ')

!     + + + END SPECIFICATIONS + + +

      ! Don't print plotdata "plot.out" file unless a debug flag is set
      if((am0hfl(isr).gt.0).or.(am0sfl(isr).gt.0).or.(manFile%am0tfl.gt.0) &
     &  .or.(am0cfl(isr).gt.0).or.(am0dfl(isr).gt.0).or.(am0efl.gt.0)) then

        ! write file header if still initializing
        if (am0ifl(isr) .eqv. .true.) then
           write (luoplt(isr), 2040, ADVANCE="NO")
           write (luoplt(isr), 2041, ADVANCE="NO")
           write (luoplt(isr), 2042, ADVANCE="NO")
           write (luoplt(isr), 2043, ADVANCE="NO")
           write (luoplt(isr), 2044, ADVANCE="NO")
           write (luoplt(isr), 2045, ADVANCE="NO")
           write (luoplt(isr), 2046, ADVANCE="NO")
           write (luoplt(isr), 2047, ADVANCE="NO")
           write (luoplt(isr), 2048, ADVANCE="NO")
           write (luoplt(isr), 2049, ADVANCE="NO")
           write (luoplt(isr), 2050, ADVANCE="NO")
           write (luoplt(isr), 2051, ADVANCE="NO")
           write (luoplt(isr), 2052, ADVANCE="NO")
           write (luoplt(isr), 2053, ADVANCE="NO")
           write (luoplt(isr), 2054, ADVANCE="NO")
           write (luoplt(isr), 2055, ADVANCE="NO")
           write (luoplt(isr), 2056, ADVANCE="NO")
           write (luoplt(isr), 2057, ADVANCE="NO")
           write (luoplt(isr), 2058, ADVANCE="NO")
           write (luoplt(isr), 2059, ADVANCE="YES")
           return
        endif

        ! initialize erosion totals
        total = 0.0
        suspen = 0.0
        pmten = 0.0
        pm2_5 = 0.0

        ngdpt = 0 ! (imax-1) * (jmax-1)  !Number of grid cells
        do idx = 1, imax-1
           do jdy = 1, jmax-1
              if( (isr .eq. 0) .or. (isr .eq. cellstate(idx,jdy)%csr) ) then
                 total = total + cellstate(idx,jdy)%egt
                 !salt = salt + (cellstate(idx,jdy)%egtcs
                 suspen = suspen + cellstate(idx,jdy)%egtss
                 pmten = pmten + cellstate(idx,jdy)%egt10
                 pm2_5 = pm2_5 + cellstate(idx,jdy)%egt2_5
                 ngdpt = ngdpt + 1
              end if
           end do
        end do
        if( ngdpt .gt. 0 ) then
           total = total/ngdpt
           suspen = suspen/ngdpt
           pmten = pmten/ngdpt
           pm2_5 = pm2_5/ngdpt
        !else no points totals will still be 0.0
        end if

        doy = get_simdate_doy()
        call get_simdate( day, month, year)

        ! insert double blank lines to demarcate years
        if( doy .eq. 1 ) then
            write (luoplt(isr),'(a)')
            write (luoplt(isr),'(a)')
        end if

        ! sum leaf /stem areas accross crop and residue pools
        res_rlaitot = 0.0
        res_rsaitot = 0.0
        res_height = 0.0
        res_biodrag = 0.0
        crop_rlaitot = 0.0
        crop_rsaitot = 0.0
        crop_height = 0.0
        crop_biodrag = 0.0
        do idx = 1, subrsurf%npools
            if( subrsurf%brcdInput(idx)%residue ) then
                res_rlaitot = res_rlaitot + subrsurf%brcdInput(idx)%rlai
                res_rsaitot = res_rsaitot + subrsurf%brcdInput(idx)%rsai
            else
                crop_rlaitot = crop_rlaitot + subrsurf%brcdInput(idx)%rlai
                crop_rsaitot = crop_rsaitot + subrsurf%brcdInput(idx)%rsai
            end if
        end do
        do idx = 1, subrsurf%npools
            if( subrsurf%brcdInput(idx)%residue ) then
                if( res_rsaitot .gt. 0.0 ) then
                    res_height = res_height + subrsurf%brcdInput(idx)%rsai * subrsurf%brcdInput(idx)%rsai / res_rsaitot
                end if
                res_biodrag = res_biodrag + biodrag( 0.0, 0.0, subrsurf%brcdInput(idx)%rlai, subrsurf%brcdInput(idx)%rsai, &
                              subrsurf%brcdInput(idx)%rg, subrsurf%brcdInput(idx)%xrow, &
                              subrsurf%brcdInput(idx)%zht, subrsurf%aszrgh )
            else
                crop_height = crop_height + subrsurf%brcdInput(idx)%rsai * subrsurf%brcdInput(idx)%rsai / crop_rsaitot
                crop_biodrag = crop_biodrag + biodrag( 0.0, 0.0, subrsurf%brcdInput(idx)%rlai, subrsurf%brcdInput(idx)%rsai, &
                               subrsurf%brcdInput(idx)%rg, subrsurf%brcdInput(idx)%xrow, &
                               subrsurf%brcdInput(idx)%zht, subrsurf%aszrgh )
            end if
        end do

        write (luoplt(isr), 2080, ADVANCE="NO")  &
             get_simdate_daysim(), doy, &
             day, month, year, &
             total, suspen, pmten, pm2_5, &
             awudmx, awadir, cli_day(get_simdate_jday())%zdpt, subrsurf%ahzsnd, subrsurf%ahrwc0(subrsurf%nswet/2), &
             subrsurf%asfcr, subrsurf%aszcr, subrsurf%asmlos, subrsurf%asflos, &
             subrsurf%aszrgh, subrsurf%asxrgw, subrsurf%asxrgs, subrsurf%asargo, subrsurf%sxprg, &
             subrsurf%asxdkh, subrsurf%asxdks, subrsurf%aslrr, &
             subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, &
             subrsurf%bsl(1)%aseags, subrsurf%bsl(1)%asdblk

        write (luoplt(isr), 2084, ADVANCE="NO") &
             subrsurf%abffcv, subrsurf%abzht, subrsurf%abrsai, subrsurf%abrlai

        write (luoplt(isr), 2084, ADVANCE="NO") &
             crop_height, crop_rsaitot, crop_rlaitot, crop_biodrag

        write (luoplt(isr), 2084, ADVANCE="NO") &
             res_height, res_rsaitot, res_rlaitot, res_biodrag

        write (luoplt(isr), 2082, ADVANCE="NO") &
             subrsurf%acanag, subrsurf%acancr

        write (luoplt(isr), 2083, ADVANCE="NO") &
             subrsurf%asf10an, subrsurf%asf10en, subrsurf%asf10bk
     
        write (luoplt(isr), 2083, ADVANCE="NO") &
             subrsurf%asf2_5an, subrsurf%asf2_5en, subrsurf%asf2_5bk
     
        write (luoplt(isr), 2084, ADVANCE="NO") &
             subrsurf%sf1ic, subrsurf%sf10ic, subrsurf%sf84ic, subrsurf%sf200ic

        write (luoplt(isr), 2084, ADVANCE="NO") &
             subrsurf%sfd1, subrsurf%sfd10, subrsurf%sfd84, subrsurf%sfd200

        ! additional friction velocity and threshold outputs
        write (luoplt(isr), 2085, ADVANCE="NO") &
             noerod%erosion, noerod%snowdepth, &
             noerod%wus_anemom, noerod%wus_random, noerod%wus_ridge, &
             noerod%wus_biodrag, noerod%wus, noerod%bare, &
             noerod%flat_cov, noerod%surf_wet, noerod%ag_den, &
             noerod%wust

        ! guard against underflow, division fails
        if( noerod%wus .gt. tiny(noerod%wus) ) then
          ! ratios of friction velocity outputs
          write (luoplt(isr), 2084, ADVANCE="NO") &
             min(9999.9, noerod%wus_anemom/noerod%wus), min(9999.9, noerod%wus_random/noerod%wus), &
             min(9999.9,noerod%wus_ridge/noerod%wus), min(9999.9, noerod%wus_biodrag/noerod%wus)
        else
          ! zero denominator, write zero values
          write (luoplt(isr), 2084, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        if( noerod%wust .gt. tiny(noerod%wust) ) then
          ! ratios of friction velocity threshold outputs
          write (luoplt(isr), 2084, ADVANCE="NO") &
             noerod%bare/noerod%wust, noerod%flat_cov/noerod%wust, &
             noerod%surf_wet/noerod%wust, noerod%ag_den/noerod%wust
        else
          ! zero denominator, write zero values
          write (luoplt(isr), 2084, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        ! soil related threshold values
        write (luoplt(isr), 2084, ADVANCE="YES") noerod%sfd84, noerod%asvroc, &
          noerod%wzzo, noerod%sfcv

 2080   format (' ',i6,' ',i3,' ',i2,' ',i2,' ',i4,' ', &
                  31(f10.3,' '))

 2082   format ( 2(f10.4,' ') )
 2083   format ( 3(f10.4,' ') )
 2084   format ( 4(f10.4,' ') )
 2085   format ( 2('  ',i1,'  '),10(f10.4,' ') )

      endif

    end subroutine plotdata

    subroutine openfils()
! ***************************************************************** wjr
! Contains init code from main

!       Edit History
!       10-Mar-99       wjr     created

      use weps_cmdline_parms, only: calc_confidence, calibrate_crops, run_erosion, soil_cond, wepp_hydro
      use file_io_mod, only: luogui1, luomandate, luoharvest_si, luoharvest_en, luohydrobal, luoseason
      use file_io_mod, only: luoharvest_calib, luoharvest_calib_parm, luobarr, luo_erod, luoplt, luosci, luostir
      use file_io_mod, only: luohydro, luohlayers, luowater, luosurfwat, luoweather, luotempsoil
      use file_io_mod, only: luocrp1, luobio1, luod_above, luod_below, luocrop, luoshoot, luoinpt
      use file_io_mod, only: luosoilsurf, luosoillay, luomanage, luoasd, luowc
      use file_io_mod, only: luoci,  luohdb, luosdb, luotdb, luocdb, luoddb
      use file_io_mod, only: luowepphdrive, luowepperod, luoweppplot, luoweppsum
      use file_io_mod, only: makenamnum, makedir, fopenk
      use erosion_data_struct_defs, only: am0efl
      use hydro_data_struct_defs, only: am0hfl, am0hdb
      use soil_data_struct_defs, only: am0sfl, am0sdb
      use manage_data_struct_defs, only: manFile
      use crop_data_struct_defs, only: am0cfl, am0cdb
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use weps_main_mod, only: old_run_file, rootp
      use crop_mod, only: cpout
      use input_run_xml_mod, only: nsubr, nbr

!     + + +   LOCAL VARIABLES + + +
      integer idx, alloc_stat, sum_stat
      character*30, dimension(:), allocatable :: subr_text ! subregion subdirectory text string
      logical :: flag_set
      integer :: tflmax
      integer :: tdbmax

      ! allocate the subregion name, number combination text for subregions
      allocate( subr_text(nsubr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate subr_text array'
      end if

      ! create subregion directory names
      do idx = 1, nsubr
         if( old_run_file .and. (nsubr .eq. 1) ) then
            ! create the name
            subr_text(idx) = ''
         else
            ! create the name
            subr_text(idx) = makenamnum( 'subregion', idx, nsubr, '/' )
            ! create the subdirectory
            call makedir(trim(rootp)//trim(subr_text(idx)) )
         end if
      end do

!     these files are opened at all times

      sum_stat = 0
      allocate( luogui1(0:nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luomandate(0:nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoharvest_si(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoharvest_en(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luohydrobal(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( luoseason(nsubr), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate luomandate, luoharvest_, luohydrobal, luoseason arrays'
      end if
      if( .not. old_run_file .or. (nsubr .gt. 1) ) then
         call fopenk (luogui1(0), trim(rootp) // 'gui1_data.out', 'unknown')
         call fopenk (luomandate(0), trim(rootp) // 'mandate.out', 'unknown')
      end if
      do idx = 1, nsubr
         call fopenk (luogui1(idx), trim(rootp) // trim(subr_text(idx)) // 'gui1_data.out', 'unknown')
         call fopenk (luomandate(idx), trim(rootp) // trim(subr_text(idx)) // 'mandate.out', 'unknown')
         call fopenk (luoharvest_si(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_si.out', 'unknown')
         call fopenk (luoharvest_en(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_en.out', 'unknown')
         call fopenk (luohydrobal(idx), trim(rootp) // trim(subr_text(idx)) // 'hydrobal.out', 'unknown')
         call fopenk (luoseason(idx), trim(rootp) // trim(subr_text(idx)) // 'season.out', 'unknown')
      end do

      if (calibrate_crops .gt. 0) then
         sum_stat = 0
         allocate( luoharvest_calib(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoharvest_calib_parm(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoharvest_calib, luoharvest_calib_parm arrays'
         end if
         do idx = 1, nsubr
            ! calibration harvest output file
            call fopenk (luoharvest_calib(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_calib.out', 'unknown')
            ! calibration harvest output file for GUI
            call fopenk (luoharvest_calib_parm(idx), trim(rootp) // trim(subr_text(idx)) // 'harvest_calib_parm.out', 'unknown')
         end do
      endif

      ! open erosion output files
      if (am0efl.gt.0) then
         allocate( luobarr(nbr), stat=alloc_stat )
         ! open barrier output files
         do idx = 1, nbr
            call fopenk (luobarr(idx), rootp(1:len_trim(rootp)) // makenamnum( 'barrier', idx, nbr, '.out' ), 'unknown')
         end do
      endif

      if (btest(am0efl,0)) then
       call fopenk (luo_erod, rootp(1:len_trim(rootp)) // 'daily_erod.out', 'unknown')
      endif

!     open plot data file
      tflmax = 0
      do idx = 1, nsubr
         tflmax = max(tflmax, manFile(idx)%am0tfl)
      end do
      if(     (maxval(am0hfl).gt.0) .or. (maxval(am0sfl).gt.0) .or. (tflmax.gt.0) &
         .or. (maxval(am0cfl).gt.0) .or. (maxval(am0dfl).gt.0) .or. (am0efl.gt.0)) then
         allocate( luoplt(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoplt array'
         end if
         do idx = 1, nsubr
            if(     (am0hfl(idx).gt.0) .or. (am0sfl(idx).gt.0) .or. (manFile(idx)%am0tfl.gt.0) &
               .or. (am0cfl(idx).gt.0) .or. (am0dfl(idx).gt.0) .or. (am0efl.gt.0)) then
               call fopenk (luoplt(idx), trim(rootp) // trim(subr_text(idx)) // 'plot.out', 'unknown')
            end if
         end do
      endif

!     open output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
         sum_stat = 0
         allocate( luosci(0:nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luostir(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosci, luostir arrays'
         end if
         if( .not. old_run_file .or. (nsubr .gt. 1) ) then
            call fopenk (luosci(0), trim(rootp) // 'sci_energy.out', 'unknown')
         end if
         do idx = 1, nsubr
            call fopenk (luosci(idx), trim(rootp) // trim(subr_text(idx)) // 'sci_energy.out', 'unknown')
            call fopenk (luostir(idx), trim(rootp) // trim(subr_text(idx)) // 'stir_energy.out', 'unknown')
         end do
      end if

!     open detailed output files for hydro
      flag_set = .false.
      do idx = 1, nsubr
         if ((am0hfl(idx) .eq. 1) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 7)) then
            flag_set = .true.
         end if
      end do
      if( flag_set ) then
         sum_stat = 0
         allocate( luohydro(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luohlayers(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luohydro, luohlayers arrays'
         end if
         do idx = 1, nsubr
            if ((am0hfl(idx) .eq. 1) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 7)) then
               call fopenk (luohydro(idx), trim(rootp) // trim(subr_text(idx)) // 'hydro.out', 'unknown')
               call fopenk (luohlayers(idx), trim(rootp) // trim(subr_text(idx)) // 'hlayers.out', 'unknown')
            end if
         end do
      endif

      flag_set = .false.
      do idx = 1, nsubr
         if ((am0hfl(idx) .eq. 2) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 7)) then
            flag_set = .true.
         end if
      end do
      if( flag_set ) then
         allocate( luowater(nsubr), stat=alloc_stat )
         allocate( luosurfwat(nsubr), stat=alloc_stat )
         allocate( luoweather(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luowater array'
         end if
         do idx = 1, nsubr
            if ((am0hfl(idx) .eq. 2) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 7)) then
               call fopenk (luowater(idx), trim(rootp) // trim(subr_text(idx)) // 'water.out', 'unknown')
               call fopenk (luosurfwat(idx), trim(rootp) // trim(subr_text(idx)) // 'surfwat.out', 'unknown')
               call fopenk (luoweather(idx), trim(rootp) // trim(subr_text(idx)) // 'weather.out', 'unknown')
            end if
         end do
      end if

      flag_set = .false.
      do idx = 1, nsubr
         if ((am0hfl(idx) .eq. 4) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 7)) then
            flag_set = .true.
         end if
      end do
      if( flag_set ) then
         allocate( luotempsoil(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luotempsoil array'
         end if
         do idx = 1, nsubr
            if ((am0hfl(idx) .eq. 4) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 7)) then
               call fopenk (luotempsoil(idx), trim(rootp) // trim(subr_text(idx)) // 'temp.out', 'unknown')
            end if
         end do
      end if

! open files for outputing the crop and decomp biomass variables - LEW
      flag_set = .false.
      do idx = 1, nsubr
         if ((am0dfl(idx) .eq. 1).or.(am0dfl(idx).eq.3)) then
            flag_set = .true.
         end if
      end do
      if( flag_set ) then
         sum_stat = 0
         allocate( luocrp1(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luobio1(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luod_above(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luod_above, luocrp1 or luobio1 array'
         end if

         do idx = 1, nsubr
            if ((am0dfl(idx) .eq. 1).or.(am0dfl(idx).eq.3)) then
               call fopenk (luocrp1(idx), trim(rootp) // trim(subr_text(idx)) // 'decomp.out', 'unknown')
               call fopenk (luobio1(idx), trim(rootp) // trim(subr_text(idx)) // 'bio1.btmp', 'unknown')
               call fopenk (luod_above(idx), trim(rootp) // trim(subr_text(idx)) // 'dabove.out', 'unknown')
            end if
         end do
      endif

      flag_set = .false.
      do idx = 1, nsubr
         if ((am0dfl(idx) .eq. 2).or.(am0dfl(idx).eq.3)) then
            flag_set = .true.
         end if
      end do
      if( flag_set ) then
         ! create dbelow.out unit number array for subregions
         allocate( luod_below(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            write(*,*) 'ERROR: unable to allocate luod_below array'
         end if
         do idx = 1, nsubr
            if ((am0dfl(idx) .eq. 2).or.(am0dfl(idx).eq.3)) then
               ! open dbelow.out in each subregion
               call fopenk (luod_below(idx), trim(rootp) // trim(subr_text(idx)) // 'dbelow.out', 'unknown')
            end if
         end do
      endif

      if( maxval(am0cfl) .gt. 0) then
         sum_stat = 0
         allocate( luocrop(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoshoot(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoinpt(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luocrop, luoshoot, luoinpt arrays'
         end if

         do idx = 1, nsubr
            if (am0cfl(idx) .gt. 0) then
               ! daily crop output of most state variables 
               call fopenk (luocrop(idx), trim(rootp) // trim(subr_text(idx)) // 'crop.out', 'unknown')
               call fopenk (luoshoot(idx), trim(rootp) // trim(subr_text(idx)) // 'shoot.out', 'unknown')
               ! echo crop input data - AR
               call fopenk (luoinpt(idx), trim(rootp) // trim(subr_text(idx)) // 'inpt.out', 'unknown')
            end if
         end do
      endif

        ! print headings for crop output files
        ! season.out, crop.out, shoot.out, inpt.out
      do idx = 1, nsubr
        call cpout(idx)
      end do

      if( maxval(am0sfl) .eq. 1 ) then
         ! soil detail output files
         sum_stat = 0
         allocate( luosoilsurf(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luosoillay(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosoilsurf, luosoillay arrays'
         end if
         do idx = 1, nsubr
            if( am0sfl(idx) .eq. 1 ) then
               ! soil surface
               call fopenk(luosoilsurf(idx), trim(rootp) // trim(subr_text(idx)) // 'soilsurf.out', 'unknown')
               ! soil layers
               call fopenk(luosoillay(idx), trim(rootp) // trim(subr_text(idx)) // 'soillay.out', 'unknown')
            end if
         end do
      endif

      if (tflmax .ge. 1) then
         allocate( luomanage(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luomanage array'
         end if
        allocate( luoasd(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoasd array'
         end if
        allocate( luowc(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luowc array'
         end if
         do idx = 1, nsubr
            if (BTEST(manFile(idx)%am0tfl,0)) then
               call fopenk (luomanage(idx), trim(rootp) // trim(subr_text(idx)) // 'manage.out', 'unknown')
            end if
            if (BTEST(manFile(idx)%am0tfl,0)) then
               call fopenk (luoasd(idx), trim(rootp) // trim(subr_text(idx)) // 'asd.out', 'unknown')
            end if
            if (BTEST(manFile(idx)%am0tfl,1)) then
               call fopenk (luowc(idx), trim(rootp) // trim(subr_text(idx)) // 'wc.out', 'unknown')
            end if
         end do
      end if

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         call fopenk(luoci, rootp(1:len_trim(rootp)) // 'ci.out', 'unknown')
      endif

         ! create arrays for subregion debug output files
      if (maxval(am0hdb) .eq. 1) then
         allocate( luohdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luohdb array'
         end if
         do idx = 1, nsubr
            if (am0hdb(idx) .eq. 1) then
               call fopenk (luohdb(idx), trim(rootp) // trim(subr_text(idx)) // 'hdbug.out', 'unknown')
            end if
         end do
      end if

      if (maxval(am0sdb) .eq. 1) then
         allocate( luosdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luosdb array'
         end if
         do idx = 1, nsubr
            if (am0sdb(idx) .eq. 1) then
               call fopenk (luosdb(idx), trim(rootp) // trim(subr_text(idx)) // 'sdbug.out', 'unknown')
            end if
         end do
      end if

      tdbmax = 0
      do idx = 1, nsubr
         tdbmax = max(tdbmax, manFile(idx)%am0tdb)
      end do
      if (tdbmax .eq. 1) then
         allocate( luotdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luotdb array'
         end if
         do idx = 1, nsubr
            if (manFile(idx)%am0tdb .eq. 1) then
               call fopenk (luotdb(idx), trim(rootp) // trim(subr_text(idx)) // 'tdbug.out', 'unknown')
            end if
         end do
      end if

      if (maxval(am0cdb) .eq. 1) then
         allocate( luocdb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luocdb array'
         end if
         do idx = 1, nsubr
            if (am0cdb(idx) .eq. 1) then
               call fopenk (luocdb(idx), trim(rootp) // trim(subr_text(idx)) // 'cdbug.out', 'unknown')
            end if
         end do
      end if

      if (maxval(am0ddb) .eq. 1) then
         allocate( luoddb(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luoddb array'
         end if
         do idx = 1, nsubr
            if (am0ddb(idx) .eq. 1) then
               call fopenk (luoddb(idx), trim(rootp) // trim(subr_text(idx)) // 'ddbug.out', 'unknown')
            end if
         end do
      end if

!   WEPP Related files

       if (wepp_hydro .gt. 1) then
         allocate( luowepphdrive(nsubr), stat=alloc_stat )
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luowepphdrive array'
         end if
         do idx = 1, nsubr
            call fopenk (luowepphdrive(idx), trim(rootp) // trim(subr_text(idx)) // 'wepp_runoff.out', 'unknown')
            write(luowepphdrive(idx),*) ' WEPP Flow Routing Output'
            write(luowepphdrive(idx),*) ' # day   mon  yr     precip  runoff    peakro  effdrn    effint   effdrr/rainfall excess'
            write(luowepphdrive(idx),*) '                      (mm)   (mm)     (mm/hr)  (min)    (mm/hr)     (min)'
         end do
       endif

       if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
         sum_stat = 0
         allocate( luowepperod(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoweppplot(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         allocate( luoweppsum(nsubr), stat=alloc_stat )
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate luowepperod, luoweppplot, luoweppsum arrays'
         end if
         do idx = 1, nsubr
            call fopenk(luowepperod(idx), trim(rootp) // trim(subr_text(idx)) // 'wepp_eroevents.out','unknown')
            write(luowepperod(idx),*) 'WEPP Erosion Events Output'
            write(luowepperod(idx),*) &
            'day mo  year    Precp  Runoff  IR-det Av-det Mx-det  Point  Av-dep Max-dep  Point Sed.Del    ER'
            write(luowepperod(idx),*) &
            '--- --  ----     (mm)    (mm)  kg/m^2 kg/m^2 kg/m^2    (m)  kg/m^2  kg/m^2    (m)  (kg/m)  ----'

            call fopenk(luoweppplot(idx), trim(rootp) // trim(subr_text(idx)) // 'wepp_eroplot.out','unknown')
     
            call fopenk(luoweppsum(idx), trim(rootp) // trim(subr_text(idx)) // 'wepp_summary.out','unknown')
            write(luoweppsum(idx),*) 'WEPS/WEPP Common Model'
            write(luoweppsum(idx),*) 'March 3, 2009  (2009.3)'
            write(luoweppsum(idx),*) '---------------------------------------'
         end do
       endif

      ! free memory from local subregion text strings
      deallocate( subr_text, stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to deallocate subr_text array'
      end if

    end subroutine openfils

    subroutine closefils()

      use weps_cmdline_parms, only: calc_confidence, calibrate_crops, run_erosion, soil_cond, wepp_hydro
      use file_io_mod, only: luicli, luiwin
      use file_io_mod, only: luogui1, luomandate, luoharvest_si, luoharvest_en, luohydrobal, luoseason
      use file_io_mod, only: luoharvest_calib, luoharvest_calib_parm, luobarr, luo_erod, luoplt, luosci
      use file_io_mod, only: luohydro, luohlayers, luowater, luosurfwat, luoweather, luotempsoil
      use file_io_mod, only: luocrp1, luobio1, luod_above, luod_below, luocrop, luoshoot, luoinpt
      use file_io_mod, only: luosoilsurf, luosoillay, luomanage, luoasd, luowc
      use file_io_mod, only: luoci,  luohdb, luosdb, luotdb, luocdb, luoddb
      use file_io_mod, only: luowepphdrive, luowepperod, luoweppplot, luoweppsum
      use erosion_data_struct_defs, only: am0efl
      use hydro_data_struct_defs, only: am0hfl, am0hdb
      use soil_data_struct_defs, only: am0sfl, am0sdb
      use manage_data_struct_defs, only: manFile
      use crop_data_struct_defs, only: am0cfl, am0cdb
      use decomp_data_struct_defs, only: am0dfl, am0ddb
      use input_run_mod, only: old_run_file
      use input_run_xml_mod, only: nsubr, nbr

      ! local variables
      integer idx

      ! files opened in inprun.for
      close(luicli)
      close(luiwin)
      do idx = 1, nsubr
         if (am0hdb(idx) .eq. 1) close(luohdb(idx))
         if (am0sdb(idx) .eq. 1) close(luosdb(idx))
         if (manFile(idx)%am0tdb .eq. 1) close(luotdb(idx))
         if (am0cdb(idx) .eq. 1) close(luocdb(idx))
         if (am0ddb(idx) .eq. 1) close(luoddb(idx))
      end do

      ! these files are opened at all times
      if( .not. old_run_file .or. (nsubr .gt. 1) ) then
         close(luogui1(0))
         close(luomandate(0))
      end if
      do idx = 1, nsubr
         close(luogui1(idx))
         close(luomandate(idx))
         close(luoharvest_si(idx))
         close(luoharvest_en(idx))
         close(luohydrobal(idx))
         close(luoseason(idx))
      end do

      if (calibrate_crops .gt. 0) then
         do idx = 1, nsubr
            ! calibration harvest output file
            close(luoharvest_calib(idx))
            ! calibration harvest output file for GUI
            close(luoharvest_calib_parm(idx))
         end do
      endif

      ! barrier output file
      if (am0efl.gt.0) then
        do idx = 1, nbr
          close(luobarr(idx))
        end do
      endif

      if (btest(am0efl,0)) then
        close(luo_erod)
      endif

!     plot data file
      do idx = 1, nsubr
         if(    (am0hfl(idx).gt.0) .or. (am0sfl(idx).gt.0) .or. (manFile(idx)%am0tfl.gt.0) &
           .or. (am0cfl(idx).gt.0) .or. (am0dfl(idx).gt.0) .or. (am0efl.gt.0)      ) then
           close(luoplt(idx))
         endif
      end do

      ! output file for soil conditioning index
      if( soil_cond .gt. 0 ) then
         do idx = 1, nsubr
            close(luosci(idx))
            ! close(luostir(idx))
         end do
      end if

      ! detailed output files for hydro
      do idx = 1, nsubr
         if ((am0hfl(idx) .eq. 1) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 7)) then
            close(luohydro(idx))
            close(luohlayers(idx))
         endif
         if ((am0hfl(idx) .eq. 2) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 3) .or. (am0hfl(idx) .eq. 7)) then
            close(luowater(idx))
            close(luosurfwat(idx))
            close(luoweather(idx))
         end if
         if ((am0hfl(idx) .eq. 4) .or. (am0hfl(idx) .eq. 5) .or. (am0hfl(idx) .eq. 6) .or. (am0hfl(idx) .eq. 7)) then
            close(luotempsoil(idx))
         end if
      end do

      ! detailed output files for management (& asd)
      do idx = 1, nsubr
         if (BTEST(manFile(idx)%am0tfl,0)) close(luomanage(idx)) ! manage.out
         if (BTEST(manFile(idx)%am0tfl,0)) close(luoasd(idx))    ! asd.out - LEW
         if (BTEST(manFile(idx)%am0tfl,0)) close(luowc(idx))     ! wc.out - LEW
      end do

      ! files for outputing the crop and decomp biomass variables - LEW
      do idx = 1, nsubr
         if ((am0dfl(idx) .eq. 1).or.(am0dfl(idx).eq.3)) then
            close(luocrp1(idx))
            close(luobio1(idx))
            close(luod_above(idx))
         endif
         if ((am0dfl(idx) .eq. 2).or.(am0dfl(idx).eq.3)) then
           ! files to match number of biomass pools

           close(luod_below(idx))
         endif

         if (am0cfl(idx) .gt. 0) then
            ! daily crop output of most state variables 
            close(luocrop(idx))
            close(luoshoot(idx))
            ! echo crop input data - AR
            close(luoinpt(idx))
         endif

         if ((am0sfl(idx) .eq. 1)) then
            ! soil detail output files
            ! soil surface
            close(luosoilsurf(idx))
            ! soil layers
            close(luosoillay(idx))
         endif
      end do

      if ((calc_confidence .gt. 0)) then
         ! Confidence Interval output file
         close(luoci)
      endif

      do idx = 1, nsubr
         if (wepp_hydro .gt. 1) then
            close (luowepphdrive(idx))
         endif
  
         if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
            close (luowepperod(idx))
            close (luoweppplot(idx))
            close (luoweppsum(idx))
         endif
      end do

    end subroutine closefils

    subroutine dbgdmp(day, soil, croptot, biotot, hstate, h1et)
! ****************************************************************** wjr
!     The dumps variables that have gone out of range

!       EDIT HISTORY
!       01-Mar-99       wjr     original coding

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal, ncanlay
      use erosion_data_struct_defs, only: awdair, awadir, awhrmx, awudmx, awudmn, awudav, subday, ntstep
      use climate_input_mod, only: cli_today, cli_tyav, amzele
      use solar_mod, only: amalat, amalon
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state
      use erosion_data_struct_defs, only: subregionsurfacestate

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: day
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(hydro_state), intent(in) :: hstate
      type(hydro_derived_et), intent(in) :: h1et

      integer  idx

      real     tstmin
      parameter (tstmin=1e-10)
      
      real     tstmax
      parameter (tstmax=1e10)

      write(*,*) 's1surf'

      if (soil%aszcr.lt.0.0.or.soil%aszcr.gt.23.0) &
        write(*,*) 'day ',day,' aszcr ', soil%aszcr

      if (soil%asfcr.lt.0.0.or.soil%asfcr.gt.1.0) &
        write(*,*) 'day ',day,' asfcr ', soil%asfcr

      if (soil%asmlos.lt.0.0.or.soil%asmlos.gt.2.0) &
        write(*,*) 'day ',day,' asmlos ', soil%asmlos

      if (soil%asflos.lt.0.0.or.soil%asflos.gt.1.0) &
        write(*,*) 'day ',day,' asflos ', soil%asflos

      if (soil%asdcr.lt.0.6.or.soil%asdcr.gt.2.0) &
        write(*,*) 'day ',day,' asdcr ', soil%asdcr

      if (soil%asecr.lt.0.1.or.soil%asecr.gt.7.0) &
        write(*,*) 'day ',day,' asecr ', soil%asecr

      if (soil%asfald.lt.0.05.or.soil%asfald.gt.0.25) &
        write(*,*) 'day ',day,' asfald ', soil%asfald

      if (soil%asfalw.lt.0.05.or.soil%asfalw.gt.0.2) &
        write(*,*) 'day ',day,' asfalw ', soil%asfalw

      write(*,*) 's1sgeo'

      if (soil%aszrgh.lt.0.0.or.soil%aszrgh.gt.500.0) &
     &  write(*,*) 'day ',day,' aszrgh ', soil%aszrgh

      if (soil%asxrgw.lt.10.0.or.soil%asxrgw.gt.4000.0) &
     &  write(*,*) 'day ',day,' asxrgw ', soil%asxrgw

      if (soil%asxrgs.lt.10.0.or.soil%asxrgs.gt.2000.0)                   &
     &  write(*,*) 'day ',day,' asxrgs ', soil%asxrgs

      if (soil%asargo.lt.0.0.or.soil%asargo.gt.179.0)                     &
     &  write(*,*) 'day ',day,' asargo ', soil%asargo

      if (soil%asxdks.lt.0.0.or.soil%asxdks.gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdks ', soil%asxdks

      if (soil%asxdkh.lt.0.0.or.soil%asxdkh.gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdkh ', soil%asxdkh

      if (soil%aslrr.lt.1.0.or.soil%aslrr.gt.30.0) &
     &  write(*,*) 'day ',day,' aslrr ', soil%aslrr

      write(*,*) 'w1wind'

      if (awadir.lt.0.0.or.awadir.gt.360.0)                             &
     &  write(*,*) 'day ',day,' awadir ', awadir

      if (awhrmx.lt.1.0.or.awhrmx.gt.24.0)                              &
     &  write(*,*) 'day ',day,' awhrmx ', awhrmx

      if (awudmx.lt.0.0.or.awudmx.gt.50.0)                              &
     &  write(*,*) 'day ',day,' awudmx ', awudmx

      if (awudmn.lt.0.0.or.awudmn.gt.25.0)                              &
     &  write(*,*) 'day ',day,' awudmn ', awudmn

      if (awudav.lt.0.0.or.awudav.gt.35.0)                              &
     &  write(*,*) 'day ',day,' awudav ', awudav

      do idx=1,size(subday)
        if( subday(idx)%awu .lt. 0.0 .or. subday(idx)%awu .gt. 35.0 )   &
     &    write(*,*) 'day ',day,' awu(',idx,') ',  subday(idx)%awu
      end do

      if (awdair.lt.0.0.or.awdair.gt.tstmax)                            &
     &  write(*,*) 'day ',day,' awdair ', awdair

      write(*,*) 'b1geom'

      if (biotot%rsaitot .lt. 0.0 .or. biotot%rsaitot .gt. 1.0)  &
     &  write(*,*) 'day ',day,' biotot%rsaitot ', biotot%rsaitot

      if (biotot%rlaitot .lt. 0.0 .or. biotot%rlaitot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%rlaitot ', biotot%rlaitot

      do idx=1,ncanlay
        if (biotot%rsaz(idx) .lt. 0.0 .or. biotot%rsaz(idx) .gt. tstmax) &
          write(*,*) 'day ',day,' biotot%rsaz(',idx,') ', biotot%rsaz(idx)

        if (biotot%rlaz(idx) .lt. 0.0 .or. biotot%rlaz(idx) .gt. tstmax) &
          write(*,*) 'day ',day,' biotot%rlaz(',idx,') ', biotot%rlaz(idx)
      end do

      if (biotot%ffcvtot .lt. 0.0 .or. biotot%ffcvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%ffcvtot ', biotot%ffcvtot

      if (biotot%fscvtot .lt. 0.0 .or. biotot%fscvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%fscvtot ', biotot%fscvtot

      if (biotot%ftcvtot .lt. 0.0 .or. biotot%ftcvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%ftcvtot ', biotot%ftcvtot

      write(*,*) 'w1clig'

      if (cli_today%tdav.lt.-20.0.or.cli_today%tdav.gt.50.0)     &
     &  write(*,*) 'day ',day,' cli_today%tdav ', cli_today%tdav

      if (cli_tyav.lt.0.0.or.cli_tyav.gt.30.0)      &
     &  write(*,*) 'day ',day,' cli_tyav ', cli_tyav

      if (cli_today%tdmx.lt.0.0.or.cli_today%tdmx.gt.50.0) &
     &  write(*,*) 'day ',day,' cli_today%tdmx ', cli_today%tdmx

      if (cli_today%tdmn.lt.-20.0.or.cli_today%tdmn.gt.40.0) &
     &  write(*,*) 'day ',day,' cli_today%tdmn ', cli_today%tdmn

      if (cli_today%tdpt.lt.0.0.or.cli_today%tdpt.gt.40.0) &
     &  write(*,*) 'day ',day,' cli_today%tdpt ', cli_today%tdpt

      if (cli_today%zdpt.lt.0.0.or.cli_today%zdpt.gt.1000.0) &
     &  write(*,*) 'day ',day,' cli_today%zdpt ', cli_today%zdpt

      if (cli_today%eirr.lt.0.0.or.cli_today%eirr.gt.tstmax) &
     &  write(*,*) 'day ',day,' cli_today%eirr ', cli_today%eirr

      write(*,*) 's1layd'

      do idx=1,soil%nslay
        if (soil%asdsblk(idx).lt.tstmin.or.soil%asdsblk(idx).gt.tstmax) &
          write(*,*) 'day ',day,' asdsblk(',idx,') ', soil%asdsblk(idx)

        if (soil%aszlyd(idx).lt.tstmin.or.soil%aszlyd(idx).gt.tstmax) &
          write(*,*) 'day ',day,' aszlyd(',idx,') ', soil%aszlyd(idx)
      end do

      write(*,*) 's1layr'

      if (soil%nslay.lt.1.or.soil%nslay.gt.10) &
     &  write(*,*) 'day ',day,' nslay ', soil%nslay

      if (soil%aszlyt(1).lt.10.0.or.soil%aszlyt(1).gt.10.0) &
     &  write(*,*) 'day ',day,' aszlyt(1) ', soil%aszlyt(1)

      if (soil%nslay.gt.1.and. (soil%aszlyt(2).lt.40.0.or.soil%aszlyt(2).gt.40.0)) &
        write(*,*) 'day ',day,' aszlyt(2) ', soil%aszlyt(2)

      if (soil%nslay.gt.2.and. (soil%aszlyt(3).lt.50.0.or.soil%aszlyt(3).gt.100.0)) &
     &  write(*,*) 'day ',day,' aszlyt(3) ', soil%aszlyt(3)

      if (soil%nslay.gt.3.and. (soil%aszlyt(4).lt.50.0.or.soil%aszlyt(4).gt.100.0)) &
     &  write(*,*) 'day ',day,' aszlyt(4) ', soil%aszlyt(4)

      do idx=5,soil%nslay
        if (soil%nslay.ge.idx.and. (soil%aszlyt(idx).lt.1.0.or.soil%aszlyt(idx).gt.1000.0)) &
          write(*,*) 'day ',day,' aszlyt(',idx,') ', soil%aszlyt(idx)
      end do

      write(*,*) 's1phys'

      do idx=1, soil%nslay
        if (soil%asdblk(idx).lt.0.50.or.soil%asdblk(idx).gt.2.5) &
          write(*,*) 'day ',day,' asdblk(',idx,') ', soil%asdblk(idx)
      end do

      write(*,*) 's1dbh'

      do idx=1,soil%nslay
        if (soil%asfsan(idx).lt.0.0.or.soil%asfsan(idx).gt.1.0) &
          write(*,*) 'day ',day,' asfsan(',idx,') ', soil%asfsan(idx)

        if (soil%asfsil(idx).lt.0.0.or.soil%asfsil(idx).gt.1.0) &
          write(*,*) 'day ',day,' asfsil(',idx,') ', soil%asfsil(idx)

        if (soil%asfcla(idx).lt.0.0.or.soil%asfcla(idx).gt.1.0) &
          write(*,*) 'day ',day,' asfcla(',idx,') ', soil%asfcla(idx)

        if (soil%asvroc(idx).lt.0.0.or.soil%asvroc(idx).gt.1.0) &
          write(*,*) 'day ',day,' asvroc(',idx,') ', soil%asvroc(idx)
      end do

      write(*,*) 's1agg'

      do idx=1, soil%nslay
      if (soil%asdagd(idx).lt.0.6.or.soil%asdagd(idx).gt.2.5)             &
     &  write(*,*) 'day ',day,' asdagd(',idx,') ', soil%asdagd(idx)

      if (soil%aseags(idx).lt.0.1.or.soil%aseags(idx).gt.7.0)             &
     &  write(*,*) 'day ',day,' aseags(',idx,') ', soil%aseags(idx)

      if (soil%aslagm(idx).lt.0.03.or.soil%aslagm(idx).gt.30.0)           &
     &  write(*,*) 'day ',day,' aslagm(',idx,') ', soil%aslagm(idx)

      if (soil%aslagn(idx).lt.0.001.or.soil%aslagn(idx).gt.5.0)           &
     &  write(*,*) 'day ',day,' aslagn(',idx,') ', soil%aslagn(idx)

      if (soil%aslagx(idx).lt.1.0.or.soil%aslagx(idx).gt.1000.0)          &
     &  write(*,*) 'day ',day,' aslagx(',idx,') ', soil%aslagx(idx)

      if (soil%as0ags(idx).lt.1.0.or.soil%as0ags(idx).gt.20.0)            &
     &  write(*,*) 'day ',day,' as0ags(',idx,') ', soil%as0ags(idx)
      end do

      write(*,*) 's1dbc'
      
      do idx=1, soil%nslay
      if (soil%as0ph(idx).lt.0.0.or.soil%as0ph(idx).gt.14.0)              &
     &  write(*,*) 'day ',day,' as0ph(',idx,') ', soil%as0ph(idx)

      if (soil%asfcce(idx).lt.0.0.or.soil%asfcce(idx).gt.100.0)           &
     &  write(*,*) 'day ',day,' asfcce(',idx,') ', soil%asfcce(idx)

      if (soil%asfcec(idx).lt.0.0.or.soil%asfcec(idx).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfcec(',idx,') ', soil%asfcec(idx)

      if (soil%asfom(idx).lt.0.0.or.soil%asfom(idx).gt.tstmax)            &
     &  write(*,*) 'day ',day,' asfom(',idx,') ', soil%asfom(idx)
      end do

      write(*,*) 'm1sim'

      if (ntstep.lt.1.or.ntstep.gt.96)                                  &
     &  write(*,*) 'day ',day,' ntstep ', ntstep

      if (amalat.lt.15.0.or.amalat.gt.75.0)                             &
     &  write(*,*) 'day ',day,' amalat ', amalat

      if (amalon.lt.70.0.or.amalon.gt.170.0)                            &
     &  write(*,*) 'day ',day,' amalon ', amalon

      if (amzele.lt.0.0.or.amzele.gt.2500.0)                            &
     &  write(*,*) 'day ',day,' amzele ', amzele

      write(*,*) 'm1subr'

      if (soil%amrslp.lt.0.0.or.soil%amrslp.gt.1.0)                       &
     &  write(*,*) 'day ',day,' amrslp ', soil%amrslp

      write(*,*) 'h1temp'

      do idx=1,soil%nslay
      if (soil%tsav(idx).lt.-20.0.or.soil%tsav(idx).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsav(',idx,') ', soil%tsav(idx)

      if (soil%tsmx(idx).lt.-20.0.or.soil%tsmx(idx).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsmx(',idx,') ', soil%tsmx(idx)

      if (soil%tsmn(idx).lt.-20.0.or.soil%tsmn(idx).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsmn(',idx,') ', soil%tsmn(idx)
      end do

      write(*,*) 'h1hydro'

      do idx=1, soil%nslay
      if (soil%ahrwc(idx).lt.0.011.or.soil%ahrwc(idx).gt.0.379)           &
     &  write(*,*) 'day ',day,' ahrwc(',idx,') ', soil%ahrwc(idx)

      if (soil%aheaep(idx).lt.-17.91.or.soil%aheaep(idx).gt.0.0)          &
     &  write(*,*) 'day ',day,' aheaep(',idx,') ', soil%aheaep(idx)

      if (soil%ahrsk(idx).lt.0.0.or.soil%ahrsk(idx).gt.0.001)             &
     &  write(*,*) 'day ',day,' ahrsk(',idx,') ', soil%ahrsk(idx)

      if (soil%ah0cb(idx).lt.0.917.or.soil%ah0cb(idx).gt.27.927)          &
     &  write(*,*) 'day ',day,' ah0cb(',idx,') ', soil%ah0cb(idx)
      end do

      if (hstate%zsno.lt.0.0.or.hstate%zsno.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ahzsno ', hstate%zsno

      if (h1et%zirr.lt.0.0.or.h1et%zirr.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zirr ', h1et%zirr

      if (h1et%zper.lt.0.0.or.h1et%zper.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zper ', h1et%zper

      if (h1et%zrun.lt.0.0.or.h1et%zrun.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zrun ', h1et%zrun

      if (hstate%zsmt.lt.0.0.or.hstate%zsmt.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ahzsmt ', hstate%zsmt

      do idx=1, soil%nslay
      if (soil%ahrwcw(idx).lt.0.005.or.soil%ahrwcw(idx).gt.0.242)         &
     &  write(*,*) 'day ',day,' ahrwcw(',idx,') ', soil%ahrwcw(idx)

      if (soil%ahrwcf(idx).lt.0.012.or.soil%ahrwcf(idx).gt.0.335)         &
     &  write(*,*) 'day ',day,' ahrwcf(',idx,') ', soil%ahrwcf(idx)

      if (soil%ahrwcs(idx).lt.0.208.or.soil%ahrwcs(idx).gt.0.440)         &
     &  write(*,*) 'day ',day,' ahrwcs(',idx,') ', soil%ahrwcs(idx)

      if (soil%ahrwca(idx).lt.0.0.or.soil%ahrwca(idx).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ahrwca(',idx,') ', soil%ahrwca(idx)
      end do

      write(*,*) 'c1gen'

      if (croptot%rsaitot.lt.0.0.or.croptot%rsaitot.gt.tstmax) &
          write(*,*) 'day ',day,' croptot%rsaitot ', croptot%rsaitot

      if (croptot%rlaitot.lt.0.0.or.croptot%rlaitot.gt.tstmax) &
          write(*,*) 'day ',day,' croptot%rlaitot ', croptot%rlaitot

      do idx=1,ncanlay
      if (croptot%rsaz(idx).lt.0.0.or.croptot%rsaz(idx).gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rsaz(',idx,') ', croptot%rsaz(idx)

      if (croptot%rlaz(idx).lt.0.0.or.croptot%rlaz(idx).gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rlaz(',idx,') ', croptot%rlaz(idx)
      end do

      if (croptot%ffcvtot.lt.0.0.or.croptot%ffcvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%ffcvtot ', croptot%ffcvtot

      if (croptot%fscvtot.lt.0.0.or.croptot%fscvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%fscvtot ', croptot%fscvtot

      if (croptot%ftcvtot.lt.0.0.or.croptot%ftcvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%ftcvtot ', croptot%ftcvtot

      write(*,*) 'c1glob'

      if (croptot%zht_ave.lt.0.0.or.croptot%zht_ave.gt.3.0) &
         write(*,*) 'day ',day,' croptot%zht_ave ', croptot%zht_ave

      if (croptot%mtot.lt.0.0.or.croptot%mtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%mtot ', croptot%mtot

      if (croptot%msttot.lt.0.0.or.croptot%msttot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%msttot ', croptot%msttot

      if (croptot%mrttot.lt.0.0.or.croptot%mrttot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%mrttot ', croptot%mrttot

      do idx = 1, soil%nslay
        if (croptot%mrtz(idx).lt.0.0.or.croptot%mrtz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%mrtz ', croptot%mrtz(idx)
      end do

      if (croptot%rsaitot.lt.0.0.or.croptot%rsaitot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rsaitot ', croptot%rsaitot

      if (croptot%rlaitot.lt.0.0.or.croptot%rlaitot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rlaitot ', croptot%rlaitot

      do idx = 1, ncanlay 
        if (croptot%rsaz(idx).lt.0.0.or.croptot%rsaz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%rsaz ', croptot%rsaz(idx)
        if (croptot%rlaz(idx).lt.0.0.or.croptot%rlaz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%rlaz ', croptot%rlaz(idx)
      end do

      if (croptot%ffcvtot.lt.0.0.or.croptot%ffcvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%ffcvtot ', croptot%ffcvtot

      if (croptot%fscvtot.lt.0.0.or.croptot%fscvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%fscvtot ', croptot%fscvtot

      if (croptot%ftcvtot.lt.0.0.or.croptot%ftcvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%ftcvtot ', croptot%ftcvtot

      if (croptot%dstmtot.lt.0.0.or.croptot%dstmtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%dstmtot ', croptot%dstmtot

      write(*,*) 'end dbgdmp'
      
    end subroutine dbgdmp

end module weps_output_mod
