!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine input_ifc(isr)
! ***************************************************************** wjr
! reads initial field conditions (IFC) file (Version: 1.0)

!     Edit History
!     Fri Oct  8 16:54:30 CDT 2004 - LEW
!     based upon "inpsub.for" routine
          
!     + + + MODULES + + +
      use weps_interface_defs, ignore_me=>input_ifc
      use sci_soil_texture_mod, only : update_sci_soil_multiplier
      use stir_soil_texture_mod, only : update_stir_soil_multiplier
      use file_io_mod, only: fopenk      

!     + + + ARGUMENTS + + +
      integer, intent(in) :: isr

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
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

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      integer       lay
      character     line*512
      integer       lui1

!     + + + LOCAL DEFINITIONS + + +
!     lay - soil layer counter
!     line - hold input line text
!     isr - subregion counter
!     lui1 - input file unit number

      integer linnum
      data linnum /1/

      ! temporary initialization until proper structures put this in better place
      ! these are accumulated in update_period_update_vars but are not set until
      ! erosion is called at least once (sbinit, sbpm10)
      acanag = 0
      acancr = 0

      call fopenk (lui1, sinfil(isr), 'old') ! open IFC file
       
!     Check to see if this is a "versioned" IFC file
      read (lui1,'(a)',err=901) line
      if (line(1:12) .eq. 'Version: 1.0') then
         call inp_ifc_v1(isr, lui1)  ! For version 1.0 IFC file format only
      else if (line(1:12) .eq. 'Version: 1.1') then
         call inp_ifc_v1_1(isr, lui1)  ! For version 1.1 IFC file format only
      else  ! Assuming obsolete unversioned IFC file formats only
         close (lui1)    
         call inpsub(isr)  ! For obsolete IFC file formats only
         return            ! initialization is already done in inpsub
      end if
                   
      close (lui1)    
     
!! removed code reading IFC file data - moved to inp_ifc.for
!! which now handles both version 1.0 and version 1.1 IFC file formats

      ! initialize new variables not read in from ifc file 
      do lay = 1, nslay(isr)
          ahfredsat(lay,isr) = 0.0
          asdwsrat(lay, isr) = -1.0
      end do

      ! Set layer thickness of the soils as is appropriate for the simulation
      call spllay_ifc(isr)

      ! Wet Albedo (calculate from dry albedo)
      asfalw(isr) = asfald(isr)/((1.33**2.)*(1-asfald(isr))+asfald(isr))

      ! Settled Bulk Density, Reference Bulk Density, and Particle Density (texture based calculation)
      call proptext(nslay(isr),asfcla(1,isr),asfsan(1,isr),asfom(1,isr),&
     &              asdblk(1,isr), asdsblk(1,isr), asdprocblk(1,isr),   &
     &              asdwblk(1,isr), asdwsrat(1, isr), asdpart(1,isr) )

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

      ! Check if override of rock fragments are specified
      if (SoilRockFragments(isr) .ge. 0.0) then
        do lay = 1, nslay(isr)
          asvroc(lay,isr) = SoilRockFragments(isr)
        end do
      end if
      
      !Update the stir soil texture multiplier.  This is called only once after the soil 
      !is read so layer mixing does not affect the texture multiplier.  Only the top layer used.   
      call update_sci_soil_multiplier(isr,asfsan(1,isr),asfcla(1,isr))
      call update_stir_soil_multiplier(isr,asfsan(1,isr),asfcla(1,isr))

      return

 901  write(*,9001) trim(sinfil(isr)), linnum, trim(line)
9001  format(' Error in IFC file ',a,' on line #',i4,' ',a)
      call exit(1)

      stop
      end

