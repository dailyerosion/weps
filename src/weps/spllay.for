!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine spllay
! ***************************************************************** wjr
! Converts NASIS layered IFC files into 10,40,50,... IFC files
!
!     Edit History
!     07-Feb-01   wjr   created
!
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
!      integer      lay
!      character    line*256
      integer       isr
      real          totthk, curdep, tgtthk
      integer       otnlay(mnsz)
      real          newthk(mnsz)
      integer       oldcur, newcur, tmpcur
      integer       dolcur
      integer       ldx

!     set up target layers (use as max for now)
      real          targetthk(mnsz)
      real          targetdep(mnsz)
      real          mfac     ! multipier for progressive layer thickness
      real          tinfser  ! total inflation series for interlayer adjustment

      ! create temporary layer thickness and depth and value arrays
      real          tempthk(mnsz)
      real          tempdep(mnsz)
      integer       tempstat(mnsz) ! 0 = target layer, 1 = fixed input layer
      real          tempval(mnsz)
      integer       tempnslay

      targetthk(1) = 10
      targetthk(2) = 40
      targetthk(3) = 50
      targetthk(4) = 50
      targetthk(5) = 50
      targetthk(6) = 75
      targetthk(7) = 75
      do ldx=8,mnsz
          targetthk(ldx) = 100
      end do

      ! set multiplier factor
      mfac = 1.0 + layer_infla/100.0
      do isr = 1,nsubr

        ! alternative layering
        targetthk(1) = layer_scale
        targetdep(1) = targetthk(1)
        do ldx=2,mnsz
          targetthk(ldx) = targetthk(ldx-1) * mfac
          targetdep(ldx) = targetdep(ldx-1) + targetthk(ldx)
        end do

        ! based on depth to impermeable, bedrock layer, increase depth 
        ! of soil. With a unit gradient at the bottom boundary, no water
        ! will move up from the lower boundary.
        aszlyt(nslay(isr), isr) = aszlyt(nslay(isr), isr) + 3000.0

        ! compute out depth to bottom of soil layer
        aszlyd(1, isr) = aszlyt(1, isr)
        do ldx = 2, nslay(isr)
          aszlyd(ldx, isr) = aszlyd(ldx-1, isr) + aszlyt(ldx, isr)
        end do

        ! set temporary layer thicknesses, matching input layer boundaries
        ! checking termination layer to get same total soil thickness
        ! set number of layers
        oldcur = 1
        newcur = 1
        tmpcur = 1
        do while ( (newcur .le. mnsz) .and. (oldcur .le. nslay(isr)) )
          if( targetdep(newcur) .le. aszlyd(oldcur, isr) ) then
            ! totally within layer
            tempthk(tmpcur) = targetthk(newcur)
            tempdep(tmpcur) = targetdep(newcur)
            tempstat(tmpcur) = 0   ! target layer boundary
            tmpcur = tmpcur + 1
            newcur = newcur + 1
          else if( tempdep(tmpcur-1) .le. aszlyd(oldcur, isr) ) then
            ! crossed layer boundary set at layer boundary
            tempthk(tmpcur) = aszlyd(oldcur,isr) - tempdep(tmpcur-1) 
            tempdep(tmpcur) = aszlyd(oldcur,isr)
            ! adjust target thickness to match new layer division
            targetthk(newcur) = targetdep(newcur) - aszlyd(oldcur,isr)
            tempstat(tmpcur) = 1   ! input layer boundary
            ! increment counters
            tmpcur = tmpcur + 1
            oldcur = oldcur + 1
          end if 
        end do
        tempnslay = tmpcur-1

        ! even out layer spacing of last surface layers
        ! search for first original layer boundary
        newcur = 1
        do while( (newcur.lt.tempnslay) .and. (tempstat(newcur).eq.0) )
          newcur = newcur + 1
        end do
        ! surface layers only, average last two layers
        totthk = tempthk(newcur-1) + tempthk(newcur)
        tgtthk = totthk / 2.0
        ! redo layers
        tempthk(newcur-1) = tgtthk
        if( newcur .eq. 2 ) then
          ! only two surface layers, keep indexes in bounds
          tempdep(newcur-1) = tempthk(newcur-1)
        else
          tempdep(newcur-1) = tempdep(newcur-2) + tempthk(newcur-1)
        end if
        ! get the last layer of the interval exact
        tempthk(newcur) = tempdep(newcur) - tempdep(newcur-1)

        ! even out layer spacing between fixed layers
        oldcur = 0
        newcur = 0
        do tmpcur = 1, tempnslay
          if( tempstat(tmpcur) .eq. 1 ) then
            ! fixed layer found
            oldcur = newcur
            newcur = tmpcur
          end if
          if( oldcur .gt. 0 ) then
            ! below surface layers
            dolcur = (newcur-oldcur)
            ! add up series used to set layer adjustment series
            tinfser = 1.0
            do ldx = 1, dolcur-1
              tinfser = tinfser + mfac**ldx
            end do
            totthk = tempdep(newcur) - tempdep(oldcur)
            tgtthk = totthk / tinfser
            do ldx = oldcur+1, newcur-1
              ! redo layers in this interval
              tempthk(ldx) = tgtthk * mfac ** (ldx - oldcur - 1)
              tempdep(ldx) = tempdep(ldx-1) + tempthk(ldx)
            end do
            ! get the last layer of the interval exact
            tempthk(ldx) = tempdep(newcur) - tempdep(newcur-1)
            ! set so that adjustment not done until next permanent layer
            oldcur = 0
          end if
        end do

      ! debug write of layering created
!      do ldx=1,nslay(isr)
!          write(*,*) 'Old_Layer: ', ldx, aszlyt(ldx,isr),aszlyd(ldx,isr)
!      end do
!      do ldx=1,tempnslay
!          write(*,*) 'New_Layer: ',ldx, tempthk(ldx), tempdep(ldx),     &
!     &                             tempstat(ldx)
!      end do
!      ldx = 1
!      do ldx=1,mnsz
!          write(*,*) 'Tar_Layer: ', ldx, targetthk(ldx), targetdep(ldx)
!      end do

        ! average soil properties and put back into property arrays
        ! save old layer values of property before placing new values
        ! into enlarged array. All layers are averaged, allowing for
        ! new layers to be either smaller of larger tha original
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfsan(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfsil(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfcla(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asvroc(1,isr),    &
     &                     tempnslay, tempdep )

        ! New variable added that isn't read in "old" IFC file formats
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfvcs(1,isr),    &
     &                     tempnslay, tempdep )

        call move_ave_val( nslay(isr), aszlyd(1,isr), asfcs(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfms(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asffs(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfvfs(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfwdc(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asdblk(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfcle(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asdwblk(1,isr),   &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), aslagm(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), as0ags(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), aslagx(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), aslagn(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asdagd(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), aseags(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrwc(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrwcs(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrwcf(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrwcw(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrwc1(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ah0cb(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), aheaep(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahrsk(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfom(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), as0ph(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfcce(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfcec(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asfsmb(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), as0ec(1,isr),     &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asrsar(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asftan(1,isr),    &
     &                     tempnslay, tempdep )
        call move_ave_val( nslay(isr), aszlyd(1,isr), asftap(1,isr),    &
     &                     tempnslay, tempdep )
        ! This value only available in new IFC format
        if (ifc_format .gt. 1) then
            call move_ave_val( nslay(isr), aszlyd(1,isr), asfcle(1,isr),&
     &                         tempnslay, tempdep )
        end if

        ! New variable added that isn't read in any IFC file formats
        ! but is calculated with the -w4 cmdline option wc_type == 4
        ! before the layers are split
        call move_ave_val( nslay(isr), aszlyd(1,isr), ahfredsat(1,isr), &
     &                     tempnslay, tempdep )

        ! set new number of soil layers into original variable
        nslay(isr) = tempnslay
        ! put new thickness into array
        do ldx = 1, tempnslay
          aszlyt(ldx, isr) = tempthk(ldx)
        end do
        ! recalculate  depth to bottom of soil layer
        call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )

      end do

      return

      do isr = 1,nsubr

      oldcur = 0
      newcur = 0
      totthk = 0
      tgtthk = 0
   10 continue
        oldcur = oldcur + 1
        totthk = totthk + aszlyt(oldcur,isr)
        newcur = newcur + 1
        tgtthk = tgtthk + targetthk(newcur)
        otnlay(newcur) = oldcur
   20   if( tgtthk .lt. totthk ) then
          newcur = newcur + 1
          tgtthk = tgtthk + targetthk(newcur)
          otnlay(newcur) = oldcur
          go to 20
        end if
      if (oldcur .lt. nslay(isr)) goto 10
      nslay(isr) = newcur
      do ldx = 1, nslay(isr)
        aszlyt(ldx, isr) = targetthk(ldx)
      end do

      go to 1000

! end of hot fix

!     + + + FUNCTION DECLARATIONS + + +
!
!     convert layers by subregion
!
!      do isr = 1,nsubr

        write(*,*) 'ready to spllay:'

        totthk = 0;
        do ldx = 1, nslay(isr)
           totthk = totthk + aszlyt(ldx, isr)
        enddo
!
!       stop if soil sample is less than 40mm thick
!
        if (totthk .lt. 40.0) then
            write(0,9002) 
9002        format('Total soil thickness too thin for WEPS')
            call exit (1)
        endif

        oldcur = 1
        newcur = 1
        dolcur = 1
        if (aszlyt(dolcur,isr) .ge. 150) then
           newthk(1) = 10
           newthk(2) = 40
           newthk(3) = 50
           otnlay(1) = oldcur
           otnlay(2) = oldcur
           otnlay(3) = oldcur
           aszlyt(1, isr) = aszlyt(1, isr) - 100
           newcur = 4
           oldcur = 1
        elseif (aszlyt(dolcur,isr) .ge. 120) then
           newthk(1) = aszlyt(1, isr) * 1 / 15
           newthk(2) = aszlyt(1, isr) * 4 / 15
           newthk(3) = aszlyt(1, isr) * 5 / 15
           newthk(4) = aszlyt(1, isr) * 5 / 15
           otnlay(1) = oldcur
           otnlay(2) = oldcur
           otnlay(3) = oldcur
           otnlay(4) = oldcur
           newcur = 5
           oldcur = 2
        elseif (aszlyt(dolcur,isr) .ge. 70) then
           newthk(1) = aszlyt(1, isr) * 1 / 10
           newthk(2) = aszlyt(1, isr) * 4 / 10
           newthk(3) = aszlyt(1, isr) * 5 / 10
           otnlay(1) = oldcur
           otnlay(2) = oldcur
           otnlay(3) = oldcur
           newcur = 4
           oldcur = 2
        elseif(aszlyt(dolcur,isr) .ge. 20) then
           newthk(1) = aszlyt(1, isr) * 1 / 5
           newthk(2) = aszlyt(1, isr) * 4 / 5
           otnlay(1) = oldcur
           otnlay(2) = oldcur
           newcur = 3
           oldcur = 2
        else
           newthk(1) = aszlyt(dolcur, isr) * 1 / 5
           otnlay(1) = oldcur
           newcur = 2
           oldcur = 2
           if (newthk(1).lt.4) newthk(1) = 4
        endif
 
        dolcur = oldcur
        if (newcur .eq. 2) then
          if (aszlyt(dolcur,isr) .ge. newthk(1) * 14) then
             newthk(2) = newthk(1) * 4
             newthk(3) = newthk(1) * 5
             otnlay(2) = oldcur
             otnlay(3) = oldcur
             aszlyt(dolcur,isr) = aszlyt(dolcur,isr) - newthk(2) -      &
     &       newthk(3)
             newcur = 4
             oldcur = 2
          elseif(aszlyt(dolcur,isr) .ge. newthk(1) * 11) then
             newthk(2) = newthk(1) * 4 / 14
             newthk(3) = newthk(1) * 5 / 14
             newthk(4) = newthk(1) * 5 / 14
             otnlay(2) = oldcur
             otnlay(3) = oldcur
             otnlay(4) = oldcur
             newcur = 5
             oldcur = 3
          elseif(aszlyt(dolcur,isr) .ge. newthk(1) * 6) then
             newthk(2) = newthk(1) * 4 / 9
             newthk(3) = newthk(1) * 5 / 9
             otnlay(2) = oldcur
             otnlay(3) = oldcur
             newcur = 4
             oldcur = 3
          else
             newthk(2) = aszlyt(dolcur,isr)
             otnlay(2) = oldcur
             newcur = 3
             oldcur = 3
          endif
        endif

        dolcur = oldcur
        if(newcur.eq.3) then
          if (aszlyt(dolcur,isr) .ge. newthk(2)*1.25 + 50) then
            newthk(3) = newthk(2) * 1.25
            aszlyt(dolcur,isr) = aszlyt(dolcur,isr) - 50
            otnlay(3) = oldcur
            newcur = 4
            oldcur = 3
          elseif (aszlyt(dolcur,isr) .ge. newthk(2)*1.25) then
            newthk(3) = newthk(2) * .5
            newthk(4) = newthk(2) * .5
            otnlay(3) = oldcur
            otnlay(4) = oldcur
            newcur = 5
            oldcur = 3
          else
            newthk(3) = aszlyt(dolcur,isr)
            if (newthk(3) .lt. 10) newthk(3) = 10
            otnlay(3) = oldcur
            newcur = 4
            oldcur = 3
          endif
        endif

        curdep = 0
        do ldx = 1, newcur-1
          curdep = curdep + newthk(ldx)
        enddo

!
! layers in top 200 mm are targeted to 50 mm thickness
!
        do while (curdep .lt. 200)
          if (aszlyt(oldcur, isr) .gt. 100) then
            newthk(newcur) = 50
            aszlyt(oldcur, isr) = aszlyt(oldcur, isr) - 50
            otnlay(newcur) = oldcur
            newcur = newcur + 1
            curdep = curdep + 50
          else if (aszlyt(oldcur, isr) .gt. 50) then
            newthk(newcur) = aszlyt(oldcur, isr) / 2.0
            newthk(newcur+1) = newthk(newcur)
            otnlay(newcur) = oldcur
            otnlay(newcur+1) = oldcur
            newcur = newcur + 2
            oldcur = oldcur + 1
            if (oldcur .gt. nslay(isr)) then
              exit
            endif
          else
            newthk(newcur) = aszlyt(oldcur, isr)
            curdep = curdep + newthk(newcur)
            otnlay(newcur) = oldcur
            newcur = newcur + 1
            oldcur = oldcur + 1
            if (oldcur .gt. nslay(isr)) then
              exit
            endif
          endif
        enddo
!
! no more input layers left
!
        if (oldcur .gt. nslay(isr)) goto 120
!
! layers in top 200-400 mm are targeted to 50-100 mm thickness
!   depending on depth
!
        do while (curdep .lt. 400)
          tgtthk = 50 + ((curdep - 200) / 200) * 50
          if (aszlyt(oldcur, isr) .gt. tgtthk * 2) then
            newthk(newcur) = tgtthk
            aszlyt(oldcur, isr) = aszlyt(oldcur, isr) - 50
            otnlay(newcur) = oldcur
            newcur = newcur + 1
            curdep = curdep + 50
          else if (aszlyt(oldcur, isr) .gt. tgtthk) then
            newthk(newcur) = aszlyt(oldcur, isr) / 2.0
            newthk(newcur+1) = newthk(newcur)
            otnlay(newcur) = oldcur
            otnlay(newcur+1) = oldcur
            newcur = newcur + 2
            oldcur = oldcur + 1
            if (oldcur .gt. nslay(isr)) then
              exit
            endif
          else
            newthk(newcur) = aszlyt(oldcur, isr)
            curdep = curdep + newthk(newcur)
            otnlay(newcur) = oldcur
            newcur = newcur + 1
            oldcur = oldcur + 1
            if (oldcur .gt. nslay(isr)) then
              exit
            endif
          endif
        enddo
!
!  no more input layers left
!
        if (oldcur .gt. nslay(isr)) goto 120
!
! layers lower than 400 mm are targeted at 100 mm
!
        do while (oldcur .le. nslay(isr))
          if (aszlyt(oldcur, isr) .gt. 200) then
             newthk(newcur) = 100
             aszlyt(oldcur, isr) = aszlyt(oldcur, isr) - 100
             otnlay(newcur) = oldcur
             newcur = newcur + 1
          else
             newthk(newcur) = aszlyt(oldcur, isr) / 2
             newthk(newcur+1) = aszlyt(oldcur, isr) / 2
             otnlay(newcur) = oldcur
             otnlay(newcur+1) = oldcur
             newcur = newcur + 2
             oldcur = oldcur + 1
          endif
        enddo
!
! update number of layers
!
        nslay(isr) = newcur -1
!
! transfer new layer thicknesses to global array
!
        do ldx = 1, nslay(isr)
          aszlyt(ldx, isr) = newthk(ldx)
        enddo
!
!      update soil properties for new layers
!
!     note!, this is done backward assuming that more layers are created
!     than exist in the original soil layering and the old value
!     will be used before it is overwritten
 1000 continue
      do ldx = nslay(isr), 2, -1
        asfsan(ldx, isr) = asfsan(otnlay(ldx), isr)
        asfsil(ldx, isr) = asfsil(otnlay(ldx), isr)
        asfcla(ldx, isr) = asfcla(otnlay(ldx), isr)
        asvroc(ldx, isr) = asvroc(otnlay(ldx), isr)

        ! New variable added that isn't read in "old" IFC file formats
        asfvcs(ldx, isr)  = asfvcs(otnlay(ldx), isr)

        asfcs(ldx, isr)  = asfcs(otnlay(ldx), isr)
        asfms(ldx, isr)  = asfms(otnlay(ldx), isr)
        asffs(ldx, isr)  = asffs(otnlay(ldx), isr)
        asfvfs(ldx, isr) = asfvfs(otnlay(ldx), isr)
        asfwdc(ldx, isr) = asfwdc(otnlay(ldx), isr)
        asdblk(ldx, isr) = asdblk(otnlay(ldx), isr)
        asfcle(ldx, isr) = asfcle(otnlay(ldx), isr)   ! added so that "mix" and "invert" have them
        asdblk0(ldx, isr) = asdblk0(otnlay(ldx), isr)
        asdwblk(ldx, isr)= asdwblk(otnlay(ldx), isr)
        asdsblk(ldx, isr)= asdsblk(otnlay(ldx), isr)
        asdpart(ldx, isr)= asdpart(otnlay(ldx), isr)
        aslagm(ldx, isr) = aslagm(otnlay(ldx), isr)
        as0ags(ldx, isr) = as0ags(otnlay(ldx), isr)
        aslagx(ldx, isr) = aslagx(otnlay(ldx), isr)
        aslagn(ldx, isr) = aslagn(otnlay(ldx), isr)
        asdagd(ldx, isr) = asdagd(otnlay(ldx), isr)
        aseags(ldx, isr) = aseags(otnlay(ldx), isr)
        ahrwc(ldx, isr)  = ahrwc(otnlay(ldx), isr)
        ahrwcs(ldx, isr) = ahrwcs(otnlay(ldx), isr)
        ahrwcf(ldx, isr) = ahrwcf(otnlay(ldx), isr)
        ahrwcw(ldx, isr) = ahrwcw(otnlay(ldx), isr)
        ahrwc1(ldx, isr) = ahrwc1(otnlay(ldx), isr)
        ah0cb(ldx, isr)  = ah0cb(otnlay(ldx), isr)
        aheaep(ldx, isr) = aheaep(otnlay(ldx), isr)
        ahrsk(ldx, isr)  = ahrsk(otnlay(ldx), isr)
        asfom(ldx, isr)  = asfom(otnlay(ldx), isr)
        as0ph(ldx, isr)  = as0ph(otnlay(ldx), isr)
        asfcce(ldx, isr) = asfcce(otnlay(ldx), isr)
        asfcec(ldx, isr) = asfcec(otnlay(ldx), isr)
        asfsmb(ldx, isr) = asfsmb(otnlay(ldx), isr)
        as0ec(ldx, isr)  = as0ec(otnlay(ldx), isr)
        asrsar(ldx, isr) = asrsar(otnlay(ldx), isr)
        asftan(ldx, isr) = asftan(otnlay(ldx), isr)
        asftap(ldx, isr) = asftap(otnlay(ldx), isr)
        asfcle(ldx, isr) = asfcle(otnlay(ldx), isr)
      end do
 120  continue
      end do

      return
      end
