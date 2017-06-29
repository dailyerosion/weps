!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   dogroup (sr, soil, manFile)

!     + + + PURPOSE + + +
!     Dogroup reads in any coefficients associated with the group of
!     processes. 

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs, ignore_me=>dogroup
      use manage_data_struct_defs, only: lastoper, man_file_struct
      use soil_data_struct_defs, only: soil_def
      use manage_data_struct_mod, only: getManVal

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'manage/man.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(man_file_struct), intent(in) :: manFile

!     + + + ARGUMENT DEFINITIONS + + +
!     iunit - management file handle 
!     sr - the subregion number

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     cropno - number that identifies the crop to be sown
!     tdepth - tillage depth (mm)
!     ti - tillage intensity (fraction)

!     fracarea - fraction of area affected by process

!     + + + LOCAL VARIABLES + + +
!      character*256   line
!      character*1    grdumy

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTION DECLARATONS + + +
!      integer tillay

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

!      line = mtbl(mcur(sr))
!      read(line, 1001, err=901) grdumy, lastoper(sr)%grcode,            &
!     &                                  lastoper(sr)%grname
! 1001 format(a1,1x,i2,1x,a)
      lastoper(sr)%grcode = manFile%grp%grpType
      lastoper(sr)%grname = manFile%grp%grpName
!***  print*, 'SR',sr,' Processing process:', lastoper(sr)%grcode,' ',lastoper(sr)%grname

      select case (lastoper(sr)%grcode)

      case (1)
!-----START tillage process (process code 01)
!     get process parameters
!     get additional line of data
!        mcur(sr) = mcur(sr) + 1
!        line = mtbl(mcur(sr))
!     read tillage depth, intensity and area
!        read(line(2:len_trim(line)), *, err=901)                        &
!     &   tdepth, ti, fracarea, tstddepth, tmindepth, tmaxdepth
        call getManVal(manFile%grp, 'gtdepth', tdepth)
        call getManVal(manFile%grp, 'gtilint', ti)
        call getManVal(manFile%grp, 'gtilArea', fracarea)
        call getManVal(manFile%grp, 'gtstddepth', tstddepth)
        call getManVal(manFile%grp, 'gtmindepth', tmindepth)
        call getManVal(manFile%grp, 'gtmaxdepth', tmaxdepth)
! ***        if (getr(iunit,sr,fracarea,fracarea,fracarea,1,'r').gt.0) then
! ***          print*,'ERROR in doproc ',prcode
! ***          stop
! ***        endif
!     pre-process stuff
        tlayer = tillay(tdepth, soil%aszlyt, soil%nslay)
!     do process (usually just processes or other operations)
!     post-process stuff
!-----END tillage process (process code 01)

      case (2)
!-----START biomass manipulation process (process code 02)
!     get process parameters
!        rtn = getr(iunit, sr, bioflg,bioflg,bioflg,1,'i')
!     expected last parameter for process, thus mlpos==0
!     get additional line of data
!        mcur(sr) = mcur(sr) + 1
!        line = mtbl(mcur(sr))
!     read biomass area affected
!        read(line(2:len_trim(line)), *, err=901) fracarea
        call getManVal(manFile%grp, 'gbioarea', fracarea)
! ***        if (getr(iunit,sr,fracarea,fracarea,fracarea,1,'r').gt.0) then
! ***          print*,'ERROR in doproc ',prcode
! ***          stop
! ***        endif
!     pre-process stuff
!     do process (should include a tillage operation)
!     post-process stuff
!-----END biomass manipulation process (process code 02)

      case (3)
!-----START grow process (process code 03)
!     get process parameters
!     expected last parameter for process, thus mlpos==0
!     get additional line of data
!        mcur(sr) = mcur(sr) + 1
!        line = mtbl(mcur(sr))
!       read crop name
!        cropname = line(2:71)   !at present, line ends with < symbol at 72
        call getManVal(manFile%grp, 'gcropname', cropname)
!        read(line(2:len_trim(line)),*, err=901) cropname
! ***        if (getr(iunit, sr, cropname,cropname,cropname,1,'c').gt.0) then
! ***          print*,'ERROR in doproc ',prcode
! ***          stop
! ***        endif
!     pre-process stuff
!     do process (should include a tillage operation)
!     post-process stuff
!-----END grow process (process code 03)

      case (4)
!-----START ammend process (process code 04)
! *** 04   continue
!     get process parameters
!       expected last parameter for process, thus mlpos==0
!     get additional line of data
!        mcur(sr) = mcur(sr) + 1
!        line = mtbl(mcur(sr))
!       read amendment name
!        amdname = line(2:71)   !at present, line ends with < symbol at 72
        call getManVal(manFile%grp, 'gamdname', amdname)
!        read(line(2:len_trim(line)),*, err=901) amdname
! ***        if (getr(iunit, sr, amdname,amdname,amdname,1,'c').gt.0) then
! ***          print*,'ERROR in doproc ',prcode
! ***          stop
! ***        endif
!     pre-process stuff
!     do process (could include a tillage operation)
!     post-process stuff
!-----END ammend process (process code 04)
!      case default
!        goto 902
      end select
      return

! Error stops
      
!  901 write(0, 9901) line
! 9901 format('Error in procedure line ', a)
!      call exit (1)
!  902 write(0, 9902) line
! 9902 format('Bad procedure type ', a)
!      call exit (1)
      end
