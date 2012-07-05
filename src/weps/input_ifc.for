!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine input_ifc
! ***************************************************************** wjr
! reads initial field conditions (IFC) file (Version: 1.0)
!
!     Edit History
!     Fri Oct  8 16:54:30 CDT 2004 - LEW
!     based upon "inpsub.for" routine
!
          
!     + + + MODULES + + +
      use stir_soil_texture, only : update_stir_soil_multiplier
      
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
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
      include 'file.inc'
      include 'command.inc'          !declarations for commandline args

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*512
      integer       isr

!     + + + FUNCTION DECLARATIONS + + +
      real   plant_wat_g
!
      integer linnum
      data linnum /1/

      isr = 1           ! can only handle a soil IFC file for a single subregion (#1)

      call fopenk (lui1, sinfil, 'old') ! open IFC file

!     Check to see if this is a "versioned" IFC file
      read (lui1,'(a)',err=901) line
      if (line(1:12) .eq. 'Version: 1.0') then
          call inp_ifc_v1  ! For version 1.0 IFC file format only
      else if (line(1:12) .eq. 'Version: 1.1') then
          call inp_ifc_v1_1  ! For version 1.1 IFC file format only
      else  ! Assuming obsolete unversioned IFC file formats only
          close (lui1)
          call inpsub  ! For obsolete IFC file formats only
          return
      end if

!! removed code reading IFC file data - moved to inp_ifc.for
!! which now handles both version 1.0 and version 1.1 IFC file formats

      ! initialize new variables not read in from ifc file 
      do lay = 1, nslay(isr)
          ahfredsat(lay,isr) = 0.0
      end do

      ! Set layer thickness of the soils as is appropriate for the simulation
      call spllay_ifc()

      ! Wet Albedo (calculate from dry albedo)
      asfalw(isr) = asfald(isr)/((1.33**2.)*(1-asfald(isr))+asfald(isr))

      ! Settled Bulk Density and Particle Density (texture based calculation)
      call proptext( nslay(isr), asfcla(1,isr), asfsan(1,isr),          &
     &               asfom(1,isr), asdsblk(1,isr), asdpart(1,isr) )

      do lay=1,nslay(isr)
      ! make sure settled bd is greater than or equal to wet bulk density
        if( asdsblk(lay,isr).lt.asdwblk(lay,isr) ) then
            write(*,*) 'WARNING: settled bd (',asdsblk(lay,isr),        &  ! NOTE:  Changed to "WARNING" so message
     &                 ') < wet bd (',asdwblk(lay,isr),'), sbd = wbd' !wouldn't display in GUI popup Warning dialog box
            asdsblk(lay,isr) = asdwblk(lay,isr)
        endif
      end do



      ! calculate (or recalculate) additional values from soil basic properties
      do lay=1,nslay(isr)
!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then                ! This is OK, this is the way values are now read in
            continue                           ! Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)

        else if( wc_type.eq.1 ) then           ! Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            ! Print out warning/error message about invalid commandline argument for this soil IFC file version
            write(*,*) 'Warning: -S1 (wc_type=1) is invalid commandline &
     &argument for this soil file, ignoring flag'

        else if( wc_type.eq.2 ) then           ! Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
            ! Print out warning/error message about invalid commandline argument for this soil IFC file version
            write(*,*) 'Warning: -S2 (wc_type=2) is invalid commandline &
     &argument for this soil file, ignoring flag'

        else if( wc_type.eq.3 ) then           ! Default method.  It resets many input values
            ! Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
            ! density and convert to (grav). Using Saxton Method
            call propsaxt(asfsan(lay,isr), asfcla(lay,isr),             &
     &                    ahrwcs(lay,isr),                              &
     &                    ahrwcf(lay,isr), ahrwcw(lay,isr) )
            ! use volumetric saturation to calculate bulk density
            asdwblk(lay,isr) = (1.0-ahrwcs(lay,isr)) * asdpart(lay,isr)
            ! Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        end if
      end do

      do lay=1,nslay(isr)
!       set saturation based on definition
!       ahrwcs(lay,isr) = 1.0/asdblk(lay,isr)-1.0/asdpart(lay,isr)   ! Is this based on gravimetric values?

        if(ahrwcs(lay,isr).lt.ahrwcf(lay,isr)) then
            write(*,*) 'WARNING: Layer, Field Capacity > Saturation',   &  ! NOTE:  Changed to "WARNING" so message
     &                 lay, ahrwcf(lay,isr), ahrwcs(lay,isr)          !wouldn't display in GUI popup Warning dialog box
!           ahrwcf(lay,isr) = ahrwcs(lay,isr)
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


!         do lay=1,nslay(isr)
!             ! set soil to field capacity not wilting point
!             ahrwc(lay,isr) = ahrwcf(lay,isr)
!         end do
      else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( nslay(isr), asdblk(1,isr), asdpart(1,isr), &
     &                     ahrwcf(1,isr), ahrwcw(1,isr),                &
     &                     asfcla(1,isr), asfom(1,isr),                 &
     &                     ah0cb(1,isr), aheaep(1,isr) )
      end if


!!     used with output for soil file screening
! 1000     format(a50,1x,i2,f7.0,20f7.4)
!          stop

!         write out the soil water capacity plant available by depth
          write(*,*) 'inpsub:total 500mm',                              &
     &    plant_wat_g( 0.0, 500.0, ahrwcf(1,isr), ahrwcw(1,isr),        &
     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) ),      &
     &    plant_wat_g( 500.0, 1000.0, ahrwcf(1,isr), ahrwcw(1,isr),     &
     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) ),      &
     &    plant_wat_g( 1000.0, 1500.0, ahrwcf(1,isr), ahrwcw(1,isr),    &
     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) )

!       some soil characteristic values for crop nutirent effects 
!       were originally planned and then dropped and are not included in 
!       layer splitting. A Debug full debug compile complains
!       that these values are not initialized when they are mixed as
!       part of  management process. they are initialized here to avoid
!       removing them from mix and invert
        do lay = 1, nslay(isr)
            ! I've removed them from the mix and invert functions.  However, they might still be
            ! used in (hopefully) dead crop code.
            ascmg(lay,isr) = 0.0
            ascna(lay,isr) = 0.0
            asfesp(lay,isr) = 0.0
            asfnoh(lay,isr) = 0.0
            asfpoh(lay,isr) = 0.0
            asfpsp(lay,isr) = 0.0
            ! obsolete variables removed from "versioned" ifc files
              ! Can't not initialize them here because crop apparently is still using them somewhere
            asfsmb(lay,isr) = 0.0
            asrsar(lay,isr) = 0.0
            asftan(lay,isr) = 0.0
            asftap(lay,isr) = 0.0
  
        end do

      ! Check if override of rock fragments are specified (only single subregion for now)
      write(6,*) 'SoilRockFragments(',isr,') = ', SoilRockFragments(isr)
      if (SoilRockFragments(isr) .ge. 0.0) then
        do lay = 1, nslay(isr)
          asvroc(lay,isr) = SoilRockFragments(isr)
          write (*,*) 'asvroc(',lay,',',isr,')', asvroc(lay,isr)
        end do
      end if
      
      !Update the stir soil texture multiplier.  This is called only once after the soil 
      !is read so layer mixing does not affect the texture multiplier.  Only the top layer used.   
      !only handles a single subregion for now
      call update_stir_soil_multiplier(isr,asfsan(1,1),asfcla(1,1))

      close (lui1)
      return

 901  write(*,9001) trim(sinfil), linnum, trim(line)
9001  format(' Error in IFC file ',a,' on line #',i4,' ',a)
      call exit(1)

      stop
      end

