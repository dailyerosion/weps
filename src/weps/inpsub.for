!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine inpsub (isr)
! ***************************************************************** wjr
! reads initial field conditions (IFC) file for all subregions
!
!  NOTE:  Obsolete routine.  Likely to be removed at some later date.
!         This routine is only used for "unlabeled" soil IFC files
!         (pre-version 1.0) - LEW
!
!     Edit History
!     06-Feb-99   wjr   created

      use weps_interface_defs, ignore_me=>inpsub
      use file_io_mod, only: fopenk
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + Arguments + + +
      integer isr

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*256
      integer       linnum, typidx
      integer       max_typidx  !Maximum number of lines to read in
      real          temp   ! temporary variable, throw away value
      integer       lui1   ! input file unit number

!     + + + Initializations + + +
      linnum = 0
      typidx = 0

!     Old ifc file format contains 52 lines
!     New ifc file format contains 54 lines
!     To reduce the amount of code being changed,
!     "max_typidx" is set equal to 54 for both cases.
!     If (ifc_format .eq. 1) then the two additional lines
!     are skipped for "old" ifc formatted files.
!     Mon Sep 24 14:48:13 CDT 2001 - LEW

      max_typidx = 54   ! new ifc format (additional parms)

!     open simulation run file
      call fopenk (lui1, sinfil(isr), 'old')

!     read subregion information
  100   if (typidx.eq. max_typidx) go to 190  !do subregion initializations
        linnum = linnum + 1
        read (lui1,'(a)',err=81) line
!
! skip comment lines
        if (line(1:1) .eq. '#') go to 100
!
!!use case statement to appropriately assign values
        typidx = typidx + 1
        if (report_debug >= 3) write (6,*) linnum, typidx, ":", line
        select case (typidx)
        case (1)
!     read initial field conditions file
          am0sid(isr) = line(1:160)
        case (2)
          read(line,*,err=82) am0tax(isr)
        case (3)
          read(line,*,err=82) nslay(isr)
        case (4)
          read(line,*,err=82) (aszlyt(lay,isr), lay=1,nslay(isr))
!     read soil physical properties
        case (5)
          read(line,*,err=82) (asfsan(lay,isr), lay=1,nslay(isr))
        case (6)
          read(line,*,err=82) (asfsil(lay,isr), lay=1,nslay(isr))
        case (7)
          read(line,*,err=82) (asfcla(lay,isr), lay=1,nslay(isr))
        case (8)
          read(line,*,err=82) (asvroc(lay,isr), lay=1,nslay(isr))
        case (9)
          read(line,*,err=82) (asfcs(lay,isr), lay=1,nslay(isr))
        case (10)
          read(line,*,err=82) (asfms(lay,isr), lay=1,nslay(isr))
        case (11)
          read(line,*,err=82) (asffs(lay,isr), lay=1,nslay(isr))
        case (12)
          read(line,*,err=82) (asfvfs(lay,isr), lay=1,nslay(isr))
        case (13)
          read(line,*,err=82) (asfwdc(lay,isr), lay=1,nslay(isr))
        case (14)
          read(line,*,err=82) (asdblk(lay,isr), lay=1,nslay(isr))
          do lay=1,nslay(isr)                    ! initialize "dry" bd, even though it isn't used with these IFC files
             asfcle(lay,isr) = 0.0 
          end do
! *** debugging write
!           write(*,*) ' inpsub: ', asdblk(1:7,isr)
! *** eodw
        case (15)
          read(line,*,err=82) (asdwblk(lay,isr), lay=1,nslay(isr))
        case (16)
!     aggregate properties
          read(line,*,err=82) (aslagm(lay,isr), lay=1,nslay(isr))
        case (17)
          read(line,*,err=82) (as0ags(lay,isr), lay=1,nslay(isr))
        case (18)
          read(line,*,err=82) (aslagx(lay,isr), lay=1,nslay(isr))
        case (19)
          read(line,*,err=82) (aslagn(lay,isr), lay=1,nslay(isr))
        case (20)
          read(line,*,err=82) (asdagd(lay,isr), lay=1,nslay(isr))
        case (21)
          read(line,*,err=82) (aseags(lay,isr), lay=1,nslay(isr))
        case (22)
!     read crust properties
          read(line,*,err=82) aszcr(isr)
        case (23)
          read(line,*,err=82) asdcr(isr)
        case (24)
          read(line,*,err=82) asecr(isr)
        case (25)
          read(line,*,err=82) asfcr(isr)
        case (26)
!     read surface properties
          read(line,*,err=82) asmlos(isr)
!      write(*,*) ' inpsub: asmlos(isr) ', asmlos(isr)
        case (27)
          read(line,*,err=82) asflos(isr)
        case (28)
          read(line,*,err=82) aslrr(isr)
          aslrro(isr) = aslrr(isr)
        case (29)
          read(line,*,err=82) asargo(isr)
        case (30)
          read(line,*,err=82) aszrgh(isr)
        case (31)
          read(line,*,err=82) asxrgs(isr)
        case (32)
          read(line,*,err=82) asxrgw(isr)

        ! this is where dike height and spacing should be read in.
        ! they are not, but need to be initialized.
        ! case (??)
          asxdks(isr) = 0.0
          asxdkh(isr) = 0.0

        case (33)
!     read soil hydrologic properties
          read(line,*,err=82) (ahrwc(lay,isr), lay=1,nslay(isr))
        case (34)
          read(line,*,err=82) (ahrwcs(lay,isr), lay=1,nslay(isr))
        case (35)
          read(line,*,err=82) (ahrwcf(lay,isr), lay=1,nslay(isr))
        case (36)
          read(line,*,err=82) (ahrwcw(lay,isr), lay=1,nslay(isr))
        case (37)
          read(line,*,err=82) (ahrwc1(lay,isr), lay=1,nslay(isr))
        case (38)
          read(line,*,err=82) (ah0cb(lay,isr), lay=1,nslay(isr))
        case (39)
          read(line,*,err=82) (aheaep(lay,isr), lay=1,nslay(isr))
        case (40)
          read(line,*,err=82) (ahrsk(lay,isr), lay=1,nslay(isr))
        case (41)
          read(line,*,err=82) ah0cnp(isr)
        case (42)
          read(line,*,err=82) ah0cng(isr)
        case (43)
          read(line,*,err=82) asfald(isr)

!         Code added to "skip" extra parameter not available in "old" ifc files
          ! set default outflow height to zero (minimum depression storage)
          ahzoutflow(isr) = 0.0
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
             if( amrslp(isr) .lt. -1.5 ) then
                ! weps.run specifies a level basin with no runoff
                amrslp(isr) = 0.0
                ! set outflow height of 1/2 meter (minimum depression storage)
                ahzoutflow(isr) = 0.5
             else if( amrslp(isr) .lt. 0.0 ) then
                ! no value entered by user (from weps.run)
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          endif

        case (44)    !This line only gets read if new ifc format is specified
          ! check value read in from weps.run
          if( amrslp(isr) .lt. -1.5 ) then
             ! weps.run specifies a level basin with no runoff
             amrslp(isr) = 0.0
             ! set outflow height of 1/2 meter (minimum depression storage)
             ahzoutflow(isr) = 0.5
          else if( amrslp(isr) .lt. 0.0 ) then
             ! no value entered by user (from weps.run)
             read(line,*,err=82) amrslp(isr)
             ! check subregion slope value for validity
             if( amrslp(isr) .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                amrslp(isr) = 0.01
             end if
          else
             ! value from weps.run being used, throw away soil value
             read(line,*,err=82) temp
          end if

        case (45)
!         read soil chemical properties
          read(line,*,err=82) (asfom(lay,isr), lay=1,nslay(isr))

        case (46)
          read(line,*,err=82) (as0ph(lay,isr), lay=1,nslay(isr))
        case (47)
          read(line,*,err=82) (asfcce(lay,isr), lay=1,nslay(isr))
!       read other soil chemical properties needed by the CROP
        case (48)
          read(line,*,err=82) (asfcec(lay,isr), lay=1,nslay(isr))
        case (49)
          read(line,*,err=82) (asfsmb(lay,isr), lay=1,nslay(isr))
        case (50)
          read(line,*,err=82) (as0ec(lay,isr), lay=1,nslay(isr))
        case (51)
          read(line,*,err=82) (asrsar(lay,isr), lay=1,nslay(isr))
        case (52)
          read(line,*,err=82) (asftan(lay,isr), lay=1,nslay(isr))
        case (53)
          read(line,*,err=82) (asftap(lay,isr), lay=1,nslay(isr))

!         Code added to "skip" extra parameter not available in "old" ifc files
          if (ifc_format .eq. 1) then !old ifc format (skip next typidx line)
             typidx = typidx + 1
          endif

        case (54)    !This line only gets read if new ifc format is specified
          read(line,*,err=82) (asfcle(lay,isr), lay=1,nslay(isr))

        end select
        goto 100

      ! reading of subregion IFC elements complete
  190 continue 

      !initialize new variables not in either of these  "old" ifc file formats 
         bedrock_depth = 99990.0
         restrict_depth = 99990.0
        do lay = 1, nslay(isr)
            asfvcs(lay,isr) = 0.0
            ahfredsat(lay,isr) = 0.0
            asdwsrat(lay, isr) = -1.0
        end do


      ! set layer thickness of the soils as is appropriate for the simulation
      call spllay(isr)

      ! calculate wet albedo from dry
      asfalw(isr) = asfald(isr)/((1.33**2.)*(1-asfald(isr))+asfald(isr))

      ! texture based calculation of settled bulk density and particle density
      call proptext(nslay(isr),asfcla(1,isr),asfsan(1,isr),asfom(1,isr),&
     &              asdsblk(1,isr), asdsblk(1,isr), asdprocblk(1,isr),  &
     &              asdwblk(1,isr), asdwsrat(1, isr), asdpart(1,isr) )

      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,nslay(isr)

!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        else if( wc_type.eq.1 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
        else if( wc_type.eq.2 ) then
!           Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
        else if( wc_type.eq.3 ) then
!!          Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
!           density and convert to (grav). Using Saxton Method
            call propsaxt(asfsan(lay,isr), asfcla(lay,isr),             &
     &                    ahrwcs(lay,isr),                              &
     &                    ahrwcf(lay,isr), ahrwcw(lay,isr) )
!!          use volumetric saturation to calculate bulk density
            asdwblk(lay,isr) = (1.0-ahrwcs(lay,isr)) * asdpart(lay,isr)
!           Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        end if

!       set soil to field capacity not wilting point
        ahrwc(lay,isr) = ahrwcf(lay,isr)

!       make sure settled bd is greater than or equal to wet bulk density
        if( asdsblk(lay,isr).lt.asdwblk(lay,isr) ) then
            asdsblk(lay,isr) = asdwblk(lay,isr)
        endif

!       set initial condition to wet bulk density, not dry
        asdblk(lay,isr) = asdwblk(lay,isr)
!       set previous day bulk density
        asdblk0(lay,isr) = asdblk(lay,isr)

!       set saturation based on definition
        ahrwcs(lay,isr) = 1.0/asdblk(lay,isr)-1.0/asdpart(lay,isr)
        if(ahrwcs(lay,isr).lt.ahrwcf(lay,isr)) then
!            ahrwcf(lay,isr) = ahrwcs(lay,isr)
            write(*,*) 'Layer, Field Capacity > Saturation',            &
     &                 lay, ahrwcf(lay,isr), ahrwcs(lay,isr)
        endif

!      output for soil file screening
!        write(*,1000) sinfil,lay,aszlyt(lay,isr),
!     &        asfsan(lay,isr),asfcla(lay,isr),asfom(lay,isr),
!     &        asdwblk(lay,isr),asdblk(lay,isr),ahrwcs(lay,isr),
!     &        ahrwcf(lay,isr),ahrwcw(lay,isr),
!     &        ahrwcf(lay,isr)-ahrwcw(lay,isr),
!     &        1.0 - asdwblk(lay,isr)/asdpart(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr),
!     &        ahrwcf(lay,isr)*asdwblk(lay,isr)-
!     &        ahrwcw(lay,isr)*asdwblk(lay,isr)

      end do

      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        nslay(isr), aszlyd(1,isr), asdblk(1,isr), asdpart(1,isr), &
     &        asfcla(1,isr), asfsan(1,isr), asfom(1,isr), asfcec(1,isr),&
     &        ahrwcs(1,isr), ahrwcf(1,isr), ahrwcw(1,isr),ahrwcr(1,isr),&
     &        ahrwca(1,isr), ah0cb(1,isr), aheaep(1,isr), ahrsk(1,isr), &
     &        ahfredsat(1,isr) )

          do lay=1,nslay(isr)
              ! set soil to field capacity not wilting point
              ahrwc(lay,isr) = ahrwcf(lay,isr)
          end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( nslay(isr), asdblk(1,isr), asdpart(1,isr), &
     &                     ahrwcf(1,isr), ahrwcw(1,isr),                &
     &                     asfcla(1,isr), asfom(1,isr),                 &
     &                     ah0cb(1,isr), aheaep(1,isr) )
      end if

!       some soil characteristic values for crop nutrient effects 
!       were originally planned and then dropped and are not included in 
!       layer splitting. A Debug full debug compile complains
!       that these values are not initialized when they are mixed as
!       part of  management process. they are initialized here to avoid
!       removing them from mix and invert
        do lay = 1, nslay(isr)
            ascmg(lay,isr) = 0.0
            ascna(lay,isr) = 0.0
            asfesp(lay,isr) = 0.0
            asfnoh(lay,isr) = 0.0
            asfpoh(lay,isr) = 0.0
            asfpsp(lay,isr) = 0.0
        end do

      close (lui1)

      return
   81 write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,' ',a)
      call exit(1)

   82 write(*,9002) trim(sinfil(isr)), linnum, typidx, trim(line)
9002  format (' inpsub error - original format IFC file ',a,            &
     &' on line #',i4,'(',i2,')',' ',a)
      call exit(1)

      stop

      end

