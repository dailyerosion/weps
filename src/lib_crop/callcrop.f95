!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine callcrop(daysim, sr, soil, crop, residue, restot, croptot, h1et)
! ***************************************************************** wjr
! Wrapper to call crop

      use weps_interface_defs, ignore_me=>callcrop
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use timer_mod, only: timer, TIMCROP, TIMSTART, TIMSTOP
      use crop_data_struct_defs, only: am0cdb
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + +   ARGUMENT DECLARATIONS + + +
      integer daysim
      integer sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue  ! structure containing full residue pool description
      type(biototal), intent(in) :: restot
      type(biototal), intent(inout) :: croptot
      type(hydro_derived_et), intent(in) :: h1et

! Includes
      include 'p1werm.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'm1flag.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'crop/prevstate.inc'
      include 'crop/prevderiv.inc'
      include 'crop/gcrop.inc'

! Local Variables
      integer lay

!     + + + END OF SPECIFICATIONS + + +

      call timer(TIMCROP,TIMSTART)

! Note that crop "may" really require (admbgz + admrtz) in place of admbgz
! because crop wants to know the amount of biomass in each soil layer
! for nutrient cycling.  However, since the nutrient cycling is supposed
! to be disabled, we won't worry about it right now.  LEW - 04/23/99

      ! check for a valid growing crop
      if(      (crop%database%shoot .le. 0.0) &
          .or. (acdpop(sr) .le. 0.0) &
          .or. (ac0idc(sr) .le. 0) ) then
          ! this is not a valid growing crop
          crop%growth%am0cgf = .false.
      end if

!     only continue if crop is growing
      if( crop%growth%am0cgf ) then

         if (am0cdb(sr).eq.1) call cdbug(sr, soil, crop, restot, h1et)

         call cropgrow(sr, soil%nslay, soil%aszlyd, &
     &   crop%database%ck, crop%database%grf, crop%database%ehu0, crop%database%zmxc, &
     &   crop%bname,ac0idc(sr), acxrow(sr), &
     &   crop%database%tdtm, crop%database%zmrt, crop%database%tmin, crop%database%topt, &
     &   crop%database%fd1(1), crop%database%fd2(1), crop%database%fd1(2), crop%database%fd2(2), &
     &   crop%database%bceff, &
     &   crop%database%alf, crop%database%blf, crop%database%clf, &
     &   crop%database%dlf, crop%database%arp, crop%database%brp, crop%database%crp, &
     &   crop%database%drp, crop%database%aht, crop%database%bht, &
     &   crop%database%sla, crop%database%hue, crop%database%tverndel, &
     &   ahtsmx(1,sr), ahtsmn(1,sr),                                    &
     &   ahfwsf(sr),                                                    &
     &   crop%growth%am0cif, &
     &   acthudf(sr), crop%database%baf, &
     &   crop%geometry%hyfg, crop%database%thum, acdpop(sr), crop%database%dmaxshoot, &
     &   crop%database%storeinit, crop%database%fshoot, &
     &   crop%database%growdepth, crop%database%fleafstem, crop%database%shoot, &
     &   crop%database%diammax, crop%database%ssa, crop%database%ssb, &
     &   crop%database%fleaf2stor, crop%database%fstem2stor, crop%database%fstor2stor, &
     &   crop%database%yld_coef, crop%database%resid_int, crop%database%xstm, &
     &   crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &   crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &   crop%growth%mshoot, crop%growth%mtotshoot, crop%mass%stemz, &
     &   crop%mass%rootstorez, crop%mass%rootfiberz, &
     &   crop%geometry%zht, crop%geometry%zshoot, crop%geometry%dstm, crop%geometry%zrtd, &
     &   crop%growth%dayap, crop%growth%dayam, &
     &   crop%growth%thucum, crop%growth%trthucum, &
     &   crop%geometry%grainf, crop%growth%zgrowpt, crop%growth%fliveleaf, &
     &   crop%growth%leafareatrend, crop%growth%stemmasstrend, crop%growth%twarmdays, &
     &   crop%growth%tchillucum, crop%growth%thardnx, crop%growth%thu_shoot_beg, &
     &   crop%growth%thu_shoot_end, crop%geometry%xstmrep, &
     &   prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr),      &
     &   prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),         &
     &   prevmshoot(sr), prevbgstemz(1,sr),                             &
     &   prevrootstorez(1,sr), prevrootfiberz(1,sr),                    &
     &   prevht(sr), prevzshoot(sr), prevstm(sr), prevrtd(sr),          &
     &   prevdayap(sr), prevhucum(sr), prevrthucum(sr),                 &
     &   prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),           &
     &   prevdayspring(sr), daysim, crop%growth%dayspring, crop%database%zloc_regrow, &
     &   agmstandstem(sr), agmstandleaf(sr), agmstandstore(sr),         &
     &   agmflatstem(sr), agmflatleaf(sr), agmflatstore(sr),            &
     &   agmbgstemz(1,sr),                                              &
     &   agzht(sr), agdstm(sr), agxstmrep(sr), aggrainf(sr) )

         if (am0cdb(sr).eq.1) call cdbug(sr, soil, crop, restot, h1et)
      end if

      ! check for abandoned stems in crop regrowth
      if( ( agmstandstem(sr) + agmstandleaf(sr) + agmstandstore(sr)     &
     &     + agmflatstem(sr) + agmflatleaf(sr) + agmflatstore(sr) )     &
     &    .gt. 0.0 ) then
          ! zero out residue pools which crop is not transferring
          agmflatrootstore(sr) = 0.0
          agmflatrootfiber(sr) = 0.0
          do lay = 1, soil%nslay
              agmbgleafz(lay,sr) = 0.0
              agmbgstorez(lay,sr) = 0.0
              agmbgrootstorez(lay,sr) = 0.0
              agmbgrootfiberz(lay,sr) = 0.0
          end do
          call trans(                                                   &
     &      agmstandstem(sr), agmstandleaf(sr), agmstandstore(sr),      &
     &      agmflatstem(sr), agmflatleaf(sr), agmflatstore(sr),         &
     &      agmflatrootstore(sr), agmflatrootfiber(sr),                 &
     &      agmbgstemz(1,sr), agmbgleafz(1,sr), agmbgstorez(1,sr),      &
     &      agmbgrootstorez(1,sr), agmbgrootfiberz(1,sr),               &
     &      agzht(sr), agdstm(sr), agxstmrep(sr), aggrainf(sr),         &
     &      crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck, &
     &      crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg, &
     &      crop%database%resevapa, crop%database%resevapb, &
     &      soil%nslay, residue)
      end if

      ! update all derived globals for crop global variables
      call cropupdate(                                                  &
            soil%aszrgh, soil%aszlyd, &
     &      ac0rg(sr), acxrow(sr), &
     &      soil%nslay, crop%database%ssa, crop%database%ssb, &
     &      acdpop(sr), &
     &      ahztranspdepth(sr), ahzfurcut(sr),                          &
     &      ahztransprtmin(sr), ahztransprtmax(sr), crop, croptot  )

      ! set prevday derived variable for later reference in end_season
      prevcancov(sr) = crop%deriv%fcancov

      call timer(TIMCROP,TIMSTOP)

      end
