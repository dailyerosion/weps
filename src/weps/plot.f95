!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine plotdata(sr, restot, croptot, biotot, noerod, cellstate)

      use weps_interface_defs
      use file_io_mod, only: luoplt
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: threshold
      use erosion_data_struct_defs, only: cellsurfacestate

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: sr
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(threshold), intent(in) :: noerod
      type(cellsurfacestate), dimension(0:,0:), intent(out) :: cellstate     ! initialized grid cell state values
 
!       Edit History
!       04-Mar-99       wjr     created

      include 'p1werm.inc'
! ***      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'c1glob.inc'
      include 'h1db1.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'
      include 's1agg.inc'
      include 's1surf.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'h1hydro.inc'
      include 'm1subr.inc'
      include 'erosion/m2geo.inc'
      include 'manage/oper.inc'
      include 'main/main.inc'
      include 'main/plot.inc'
      include 'c1info.inc'

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
      if((am0hfl.gt.0).or.(am0sfl.gt.0).or.(am0tfl.gt.0)                &
     &  .or.(am0cfl.gt.0).or.(am0dfl.gt.0).or.(am0efl.gt.0)) then

        ! write file header if still initializing
        if (am0ifl .eqv. .true.) then
           write (luoplt, 2050, ADVANCE="NO")
           write (luoplt, 2051, ADVANCE="NO")
           write (luoplt, 2052, ADVANCE="NO")
           write (luoplt, 2053, ADVANCE="NO")
           write (luoplt, 2054, ADVANCE="NO")
           write (luoplt, 2055, ADVANCE="NO")
           write (luoplt, 2056, ADVANCE="NO")
           write (luoplt, 2057, ADVANCE="YES")
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

        call caldatw(day,month,year)
        doy = dayear (day, month, year)

        ! make operation name available for this day
        if ((lopday .eq. day) .and. (lopmon .eq. month) .and.           &
     &      (lopyr .eq. amnryr(sr))) then
           operat = opname
           crname = ac0nam(sr)
        else
           operat = '                                               '
           crname = '                                               '
        end if

        ! insert double blank lines to demarcate years
        if( doy .eq. 1 ) then
            write (luoplt,*)
            write (luoplt,*)
        end if

        write (luoplt, 2080, ADVANCE="NO")                              &
     &                    daysim, doy,                                  &
     &                    day, month, year,                             &
     &                    total, suspen, pmten,                         &
     &                    awudmx, awadir, awzdpt, ahrwc0(12, sr),       &
     &                    aszrgh(sr), asargo(sr), aslrr(sr),            &
     &                    aslagm(1,sr), aseags(1,sr), asfcr(sr),        &
     &                    asmlos(sr), asflos(sr), asdblk(1,sr),         &
     &                    biotot%ffcvtot, biotot%fscvtot,               &
     &                    croptot%rlaitot, croptot%rsaitot,             &
     &                    croptot%msttot, croptot%ftcancov

        write (luoplt, 2081, ADVANCE="NO")                              &
     &   croptot%zht_ave, acxstmrep(sr), croptot%rcdtot, croptot%ftcvtot

        write (luoplt, 2082, ADVANCE="NO")                              &
     &       restot%zht_ave, restot%rsaitot, restot%rlaitot,            &
     &       restot%rcdtot, restot%ftcancov, restot%ftcvtot

        ! additional friction velocity and threshold outputs
        write (luoplt, 2085, ADVANCE="NO")                              &
     &       noerod%erosion, noerod%snowdepth,                          &
     &       noerod%wus_anemom, noerod%wus_random, noerod%wus_ridge,    &
     &       noerod%wus_biodrag, noerod%wus, noerod%bare,               &
     &       noerod%flat_cov, noerod%surf_wet, noerod%ag_den,           &
     &       noerod%wust

        if( noerod%wus .gt. 0.0 ) then
          ! ratios of friction velocity outputs
          write (luoplt, 2086, ADVANCE="NO")                            &
     &       noerod%wus_anemom/noerod%wus, noerod%wus_random/noerod%wus,&
     &       noerod%wus_ridge/noerod%wus, noerod%wus_biodrag/noerod%wus
        else
          ! zero denominator, write zero values
          write (luoplt, 2086, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        if( noerod%wust .gt. 0.0 ) then
          ! ratios of friction velocity threshold outputs
          write (luoplt, 2086, ADVANCE="NO")                            &
     &       noerod%bare/noerod%wust, noerod%flat_cov/noerod%wust,      &
     &       noerod%surf_wet/noerod%wust, noerod%ag_den/noerod%wust
        else
          ! zero denominator, write zero values
          write (luoplt, 2086, ADVANCE="NO") 0.0, 0.0, 0.0, 0.0
        end if

        ! soil related threshold values
        write (luoplt, 2086, ADVANCE="NO") noerod%sfd84, noerod%asvroc, &
     &    noerod%wzzo, noerod%sfcv


        write (luoplt, 2090, ADVANCE="NO") operat
        write (luoplt, 2091, ADVANCE="YES") crname

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

