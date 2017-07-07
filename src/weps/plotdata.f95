!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine plotdata(sr, soil, crop, restot, croptot, biotot, noerod, manFile, cellstate)

      use weps_main_mod, only: daysim, report_loop
      use datetime_mod, only: get_simdate, get_simdate_doy
      use file_io_mod, only: luoplt
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: threshold
      use erosion_data_struct_defs, only: cellsurfacestate
      use erosion_data_struct_defs, only: awadir, awudmx
      use erosion_data_struct_defs, only: am0efl
      use grid_mod, only: imax, jmax
      use hydro_data_struct_defs, only: am0hfl
      use soil_data_struct_defs, only: am0sfl
      use manage_data_struct_defs, only: man_file_struct, lastoper
      use crop_data_struct_defs, only: am0cfl
      use decomp_data_struct_defs, only: am0dfl
      use climate_input_mod, only: cli_today

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(threshold), intent(in) :: noerod
      type(man_file_struct), intent(in) :: manFile
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
 
!       Edit History
!       04-Mar-99       wjr     created

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'h1db1.inc'
      include 'h1hydro.inc'
      include 'm1subr.inc'
      include 'main/plot.inc'

      integer day, month, year, doy

      integer ngdpt  !number of grid cells within field
      integer idx, jdy   !local loop vars

!     + + + OUTPUT FORMATS + + +
!     format for header of plot file
 2050 format (1x,'#daysim','|','doy','|','day','|','mon','|',' yr ',    &
     &     '|',' tot_loss ',                                            &
     &     '|','  suspen  ','|','  pm10 ','|',' max_wind ',             &
     &     '|',' dir_wind ','|','  precip  ','|',' Surf_H2O',           &
     &     '|','  ridge_ht ','|',' ridge_or ','|','  r_rough ',         &
     &     '|','   gmd    ','|',' ag_stab  ','|',' cr_fract ',          &
     &     '|','loose_mass','|','loose_frac','|',' bulk_den',           &
     &     '|','   fl_cov%','|','  st_cov% ','|',' crop_lai ',          &
     &     '|',' crop_sai ','|','crop_st_mass','|','can_cov ')

!     header of plot file (daily crop values derived from mass, column headers)
 2051 format ('|',' crop_ht ','|','crp_rep_stm_dia',                    &
     &        '|','crop_drag','|','crp_soil_cov')
!     header of plot file (daily decomp values derived from mass, column headers)
 2052 format ('|','res_av_ht','|',' res_sai ','|',' res_lai ',          &
     &        '|',' res_drag ','|','res_can_cov','|','res_soil_cov')
!     header of plot file (friction velocity and threshold values)
 2053 format ('|','eros','|','snow',                                    &
     &        '|','wus_anemom','|','wus_random','|','wus_ridge',        &
     &        '|','wus_biodrag','|',' ne_wus ','|','t_ne_bare',         &
     &        '|',' t_flat_cov','|','t_surf_wet','|','t_ag_den ',       &
     &        '|',' t_wust    ')
!     header of plot file (friction velocity ratios)
 2054 format ('|','rwus_anemom','|','rwus_random',                      &
     &        '|','rwus_ridge','|','rwus_biodrag')
!     header of plot file (velocity threshold ratios)
 2055 format ('|','r_ne_bare','|',' r_flat_cov',                        &
     &        '|','r_surf_wet','|','r_ag_den ')
!     header of plot file (velocity threshold ratios)
 2056 format ('|','ne_sf84','|',' ne_rock',                             &
     &        '|','ne_wzzo','|','ne_sfcv ')
!     operation name(s) at end of line
 2057 format ('|',' operation    ','|','    crop  ')

!     + + + END SPECIFICATIONS + + +

      ! Don't print plotdata "plot.out" file unless a debug flag is set
      if((am0hfl(sr).gt.0).or.(am0sfl(sr).gt.0).or.(manFile%am0tfl.gt.0) &
     &  .or.(am0cfl(sr).gt.0).or.(am0dfl(sr).gt.0).or.(am0efl.gt.0)) then

        ! write file header if still initializing
        if (am0ifl .eqv. .true.) then
           write (luoplt(sr), 2050, ADVANCE="NO")
           write (luoplt(sr), 2051, ADVANCE="NO")
           write (luoplt(sr), 2052, ADVANCE="NO")
           write (luoplt(sr), 2053, ADVANCE="NO")
           write (luoplt(sr), 2054, ADVANCE="NO")
           write (luoplt(sr), 2055, ADVANCE="NO")
           write (luoplt(sr), 2056, ADVANCE="NO")
           write (luoplt(sr), 2057, ADVANCE="YES")
           return
        endif

        ! initialize erosion totals
        total = 0.0
        suspen = 0.0
        pmten = 0.0

        if( report_loop ) then
           ngdpt = 0     ! (imax-1) * (jmax-1)  !Number of grid cells
           do idx = 1, imax-1
              do jdy = 1, jmax-1
                 if( (sr .eq. 0) .or. (sr .eq. cellstate(idx,jdy)%csr) ) then
                    total = total + cellstate(idx,jdy)%egt
                    !salt = salt + (cellstate(idx,jdy)%egt - cellstate(idx,jdy)%egtss
                    suspen = suspen + cellstate(idx,jdy)%egtss
                    pmten = pmten + cellstate(idx,jdy)%egt10
                    ngdpt = ngdpt + 1
                 end if
              end do
           end do
           if( ngdpt .gt. 0 ) then
              total = total/ngdpt
              suspen = suspen/ngdpt
              pmten = pmten/ngdpt
           !else no points totals will still be 0.0
           end if
        end if

        call get_simdate(day,month,year)
        doy = get_simdate_doy()

        ! make operation name available for this day
        if ((lastoper(sr)%day .eq. day) .and. (lastoper(sr)%mon .eq. month) .and. &
     &      (lastoper(sr)%yr .eq. amnryr(sr))) then
           operat = lastoper(sr)%name
           crname = crop%bname
        else
           operat = '                                               '
           crname = '                                               '
        end if

        ! insert double blank lines to demarcate years
        if( doy .eq. 1 ) then
            write (luoplt(sr),*)
            write (luoplt(sr),*)
        end if

        write (luoplt(sr), 2080, ADVANCE="NO")  &
     &                    daysim, doy,                                  &
     &                    day, month, year,                             &
     &                    total, suspen, pmten,                         &
     &                    awudmx, awadir, cli_today%zdpt, ahrwc0(12, sr), &
     &                    soil%aszrgh, soil%asargo, soil%aslrr, &
     &                    soil%aslagm(1), soil%aseags(1), soil%asfcr, &
     &                    soil%asmlos, soil%asflos, soil%asdblk(1), &
     &                    biotot%ffcvtot, biotot%fscvtot,               &
     &                    croptot%rlaitot, croptot%rsaitot,             &
     &                    croptot%msttot, croptot%ftcancov

        write (luoplt(sr), 2081, ADVANCE="NO")                              &
     &   croptot%zht_ave, croptot%xstmrep, croptot%rcdtot, croptot%ftcvtot

        write (luoplt(sr), 2082, ADVANCE="NO")                              &
     &       restot%zht_ave, restot%rsaitot, restot%rlaitot,            &
     &       restot%rcdtot, restot%ftcancov, restot%ftcvtot

        ! additional friction velocity and threshold outputs
        write (luoplt(sr), 2085, ADVANCE="NO")                              &
     &       noerod%erosion, noerod%snowdepth,                          &
     &       noerod%wus_anemom, noerod%wus_random, noerod%wus_ridge,    &
     &       noerod%wus_biodrag, noerod%wus, noerod%bare,               &
     &       noerod%flat_cov, noerod%surf_wet, noerod%ag_den,           &
     &       noerod%wust

        ! guard against underflow, division fails
        if( noerod%wus .gt. tiny(noerod%wus) ) then
          ! ratios of friction velocity outputs
          write (luoplt(sr), 2086, ADVANCE="NO")                            &
     &       noerod%wus_anemom/noerod%wus, noerod%wus_random/noerod%wus,&
     &       noerod%wus_ridge/noerod%wus, noerod%wus_biodrag/noerod%wus
        else
          ! zero denominator, write zero values
          write (luoplt(sr), 2086, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        if( noerod%wust .gt. tiny(noerod%wust) ) then
          ! ratios of friction velocity threshold outputs
          write (luoplt(sr), 2086, ADVANCE="NO")                            &
     &       noerod%bare/noerod%wust, noerod%flat_cov/noerod%wust,      &
     &       noerod%surf_wet/noerod%wust, noerod%ag_den/noerod%wust
        else
          ! zero denominator, write zero values
          write (luoplt(sr), 2086, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        ! soil related threshold values
        write (luoplt(sr), 2086, ADVANCE="NO") noerod%sfd84, noerod%asvroc, &
     &    noerod%wzzo, noerod%sfcv


        write (luoplt(sr), 2090, ADVANCE="NO") operat
        write (luoplt(sr), 2091, ADVANCE="YES") crname

 2080   format (' ',i6,' ',i3,' ',i2,' ',i2,' ',i4,' ',                 &
     &            3(f10.3,' '),                                         &
     &            4(f10.3,' '),                                         &
     &            3(f10.3,' '),                                         &
     &            3(f10.3,' '),                                         &
     &            3(f10.3,' '),                                         &
     &            2(f10.3,' '),                                         &
     &            2(f10.3,' '),                                         &
     &            2(f10.3,' ') )

 2081   format ( 4(f10.4,' ') )
 2082   format ( 6(f10.4,' ') )
 2085   format ( 2(i1,' '),10(f10.4,' ') )
 2086   format ( 4(f10.4,' ') )
 2090   format ( a35,' ' )
 2091   format ( a35,' ' )

      endif

      return
      end

