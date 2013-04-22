!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sb1out
!**********************************************************************
      subroutine sb1out( jj, nn, hr, ws, wdir, o_unit, subrsurf, cellstate )

!     + + + PURPOSE + + +
!     To print to file tst.out some key variables used in erosion
!     use wind dir of 270 for most to see output along wind direction

      use weps_interface_defs
      use datetime_mod, only: get_systime_string
      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, awzypt, anemht, wzoflg, ntstep
      use grid_mod, only: awa, kbr, imax, jmax, amasim, amxsim

!     + + + ARGUEMENT DECLARATIONS + + +
      real ws, wdir, hr
      integer  jj, nn, o_unit
      type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + ARGUMENT DEFINITIONS + + +
!     o_unit= Unit number for output file

!     + + + LOCAL VARIABLES + + +
      !integer m, n, k
      integer initflag, ipd, npd
      save    initflag, ipd, npd
      integer yr, mo, da
      real    hhrr, tims
      save    yr, mo, da, hhrr, tims
      integer i,j

!     + + + END SPECIFICATIONS + + +

!     output headings?
      if (initflag .eq. 0) then

        ipd = 0
        npd = nn * ntstep

        tims = 3600*24/ntstep     !seconds in each emission period
        call caldatw (da, mo, yr) !Set day, month and year
        hhrr = 0 - tims/3600        !Pre-set hhrr so we get end of period times

        write (o_unit,*)
        write (o_unit,*) 'OUT PUT from sb1out'
        write (o_unit,*)

        ! Print date of Run
        write(o_unit,"(1x,'Date of run: ',a21)") get_systime_string()
        write(o_unit,*)

        write (unit=o_unit,fmt="(a,f5.2,a2,a,i1)")                      &
     &   ' anemht = ', anemht, 'm', '    wzoflg = ', wzoflg
        write (unit=o_unit,fmt="(a,f6.2,a4)")                           &
     &   ' wind direction = ', wdir, 'deg'
        write (unit=o_unit,fmt="(a,f6.2,a4)")                           &
     &   ' wind direction relative to field orientation = ', awa, 'deg'
      write (o_unit,*)
        write (unit=o_unit,fmt="(a,i1)") ' wind quadrant = ', kbr
        write (o_unit,*)
        write (o_unit,*) 'orientation and dimensions of sim region'
        write (o_unit,*) 'amasim(deg)  amxsim - (x1,y1) (x2,y2)'
        write(o_unit,fmt="(1x,5f8.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write (o_unit,*)

       write (o_unit,*) "Surface properties"
      write (o_unit,fmt="(a,f8.2,a)")                                   &
     &  "Ridge spacing parallel to wind direction",                     &
     &   subrsurf%sxprg, " (mm)"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &  "Crop row spacing", subrsurf%acxrow, " (mm)"
      write (o_unit,fmt="(a,i2,a)")                                     &
     &  "Crop seeding location relative to ridge", subrsurf%ac0rg,      &
     &  " (0 - furrow, 1 - ridge)"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &  "Composite weighted average biomass height",                    &
     &   subrsurf%abzht, " (m)"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &  "Biomass leaf area index", subrsurf%abrlai, " (m^2/m^2)"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &  "Biomass stem area index", subrsurf%abrsai, " (m^2/m^2)"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &  "Biomass flat cover", subrsurf%abffcv, " (m^2/m^2)"

      write (o_unit,fmt="(a,f8.2,a)")                                   &
     &       "Average yearly total precipitation ", awzypt, " (mm)"
      write (o_unit,*)



        write(o_unit,fmt="(1x,a)") "<field dimensions>"
       write(o_unit,fmt="(1x,5f10.2)")amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
        write(o_unit,fmt="(1x,a)") "</field dimensions>"

        write (o_unit,*)
        initflag = 1    !     turn off heading output
      endif

      ipd = ipd + 1
      if (hhrr .ge. 24) then
         hhrr = tims/3600
         call caldatw (da, mo, yr)
      else
         hhrr = hhrr + tims/3600
      endif

      call caldatw (da, mo, yr)
!      write (o_unit, fmt="(a, 3(i3), f6.2, 4(i4)f7.2)")                     &
!     & ' day mon yr hhrr upd_pd jj nn npd ',da,mo,yr,hr,ipd,jj,nn,npd,hhrr
      write (o_unit, fmt="(a, i5, 2(i3), f7.3, 4(i4))")                 &
     & ' yr mon day hr upd_pd jj nn(subpd) npd (sbqout 1)',             &
     &   yr,mo, da, hr,ipd,   jj,nn,       npd
      write (o_unit,*)
      write (o_unit, fmt="(a, f5.2, 2(f7.2))")                          &
     &     ' pd wind speed, dir and dir rel to field ', ws, wdir, awa
      write (o_unit,*)

      write (o_unit,*) "Surface layer properties"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Surface course fragments",                                &
     &       subrsurf%bsl(1)%asvroc, " (m^3/m^3)"
      write (o_unit,fmt="(a,a,f5.2,a)") "Initial soil ",                &
     & "mass fraction in surface layer < 0.10 mm ",                     &
     & subrsurf%sf10ic, " (kg/kg)"
      write (o_unit,fmt="(a,a,f5.2,a)") "Initial soil ",                &
     & "mass fraction in surface layer < 0.84 mm ",                     &
     & subrsurf%sf84ic, " (kg/kg)"

      write (o_unit,*) "PM10 emission properties"
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Soil fraction PM10 in abraded suspension ",               &
     &       subrsurf%asf10an
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Soil fraction PM10 in emitted suspension ",               &
     &       subrsurf%asf10en
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Soil fraction PM10 in saltation breakage suspension ",    &
     &       subrsurf%asf10bk
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Coefficient of abrasion of aggregates ", subrsurf%acanag
      write (o_unit,fmt="(a,f5.2,a)")                                   &
     &       "Coefficient of abrasion of crust ", subrsurf%acancr

!Grid cell data

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Friction Velocity', 'friction velocity', '(m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wus, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Threshold Surface Friction Velocity',                           &
     & 'threshold friction velocity', '(m/s)'
       do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wust, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Transport Threshold Surface Friction Velocity',                 &
     & 'transport threshold friction velocity', '(m/s)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%wusp, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

!Grid Cell Surface properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Random Roughness', 'random roughness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%slrr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Oriented Roughness', 'ridge height', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szrgh, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Surface Rock', 'surface volume rock fraction', '(m^3/m^3)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%svroc, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size<0.01', 'mass fraction < 0.01 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf1, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size<0.1', 'mass fraction < 0.1 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf10, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size<0.84', 'mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size<2.0', 'mass fraction < 2.0 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf200, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Soil Agg. Size for u* to be the thresh. friction velocity',     &
     &'"effective" mass fraction < 0.84 mm size', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sf84mn, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Mobile soil removable from aggregated surface',                 &
     & 'mass removable', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smaglos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Change in mobile soil on aggregated surface',                   &
     & 'net mass change', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%dmlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")

! Crust properties
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Consolidated crust thickness', 'crust thickness', '(mm)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%szcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Fraction of Surface covered with Crust','crust cover','(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sfcr, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Fraction of Crusted Surface covered with Loose Erodible Soil ', &
     & 'loose erodible material', '(fract.)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%sflos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write(o_unit,fmt="(' <grid data> |',i5,2(i3),f7.3,3('|',a))")     &
     &  yr, mo, da, hr,                                                 &
     & 'Mass of Loose Erodible Soil on Crusted Surface',                &
     & 'loose erodible material', '(kg/m^2)'
      do j = jmax-1, 1, -1
        write (o_unit, fmt="(500f12.4)") (cellstate(i,j)%smlos, i = 1, imax-1)
      end do
      write(o_unit,fmt="(' </grid data>')")
      write (o_unit,*)

!      write (o_unit,20)  anemht,wzoflg,kbr, jj, ws
!     set output increment
!      m = (imax - 1)/8
!      m = max0(m,1)
!      n = (jmax-1)/2
!      n = max(n,1)
!
!      write (o_unit,*)  'sb1out output'
!      write (o_unit,*) 'for prior wind speed'
!      write (o_unit,21) (egt(k,n),k=1,(imax-1),m)
!      write (o_unit,22) (egtss(k,n),k=1,(imax-1),m)
!      write (o_unit,23??) ((egtss(k,n)/(egt(k,n)+0.0001)),k=1,(imax-1),m)
!      write (o_unit,*)

!      write (o_unit,18)  (k , k=1,(imax-1),m), n
      
!      write (o_unit,13)  (cellstate(k,n)%sf1,k=1,(imax-1),m)
!      write (o_unit,23)  (cellstate(k,n)%sf10,k=1,(imax-1),m)
!      write (o_unit,24)  (cellstate(k,n)%sf84,k=1,(imax-1),m)
!      write (o_unit,35)  (cellstate(k,n)%sf200,k=1,(imax-1),m)
!      write (o_unit,12)  (cellstate(k,n)%svroc,k=1,(imax-1),m)! edit ljh 1-22-05
!      write (o_unit,36)  (cellstate(k,n)%dmlos,k=1,(imax-1),m)
!      write (o_unit,37)  (cellstate(k,n)%smaglos,k=1,(imax-1),m)
!      write (o_unit,43)  (cellstate(k,n)%smaglosmx,k=1,(imax-1),m)
!      write (o_unit,39)  (cellstate(k,n)%sf84mn,k=1,(imax-1),m)
!      write (o_unit,40)   subrsurf%sf84ic, subrsurf(icsr)%sf10ic, asvroc(1,1) !edit ljh 1-22-05
!      write (o_unit,42)   acanag(1), acancr(1), awzypt
!      write (o_unit,10)   asf10an(1), asf10en(1), asf10bk(1)
!      write (o_unit,25)  (cellstate(k,n)%szcr,k=1,(imax-1),m)
!      write (o_unit,26)  (cellstate(k,n)%sfcr,k=1,(imax-1),m)
!      write (o_unit,27)  (cellstate(k,n)%smlos,k=1,(imax-1),m)
!      write (o_unit,28)  (cellstate(k,n)%sflos,k=1,(imax-1),m)

!      write (o_unit,29)  (cellstate(k,n)%szrgh,k=1,(imax-1),m)
!      write (o_unit,30)  (cellstate(k,n)%slrr,k=1,(imax-1),m)
!      write (o_unit,38)   subrsurf%sxprg,  subrsurf%abzht,              &
!     &                    subrsurf%abrlai,                              &
!     &                    subrsurf%abrsai, subrsurf%abffcv
!      write (o_unit,41)   subrsurf%acxrow, subrsurf%ac0rg
!      write (o_unit,31)  subrsurf%ahrwcw(1), subrsurf%ahrwc0(12)
!      write (o_unit,32)  (cellstate(k,n)%wus,k=1,(imax-1),m)
!      write (o_unit,33)  (cellstate(k,n)%wusp,k=1,(imax-1),m)
!      write (o_unit,34)  (cellstate(k,n)%wust,k=1,(imax-1),m)
!      write (o_unit,44)  (cellstate(k,n)%wusto,k=1,(imax-1),m)
!      write (o_unit,*)

!     output formats
!   10 format (1x, 'sf10an =',f6.3,'  sf10en =',f6.3,'  sf10bk =',f6.3) 
!   15 format (1x, ' (m)          (m/s)    ')
!   18 format (1x, 'i..n,j', 3i6, 17i7)
!   20 format (1x, 'anemht wzoflg  kbr jj ws',                           &
!     &  f6.0, 3i6, f6.2)

!   21 format (1x, 'egt=', 20f6.2)
!   22 format (1x, 'egtss=', 20f6.2)

!   13 format (1x, 'sf1= ', 20f7.4)
!   23 format (1x, 'sf10= ', 20f7.3)
!   24 format (1x, 'sf84= ', 20f7.3)
!   12 format (1x, 'svroc=', 20f7.3)   !edit ljh 1-22-05
!   35 format (1x, 'sf200=', 20f7.3)
!   36 format (1x, 'dmlos=', 20f7.3)
!   37 format (1x, 'smaglos=',20f7.3)
!   43 format (1x, 'smaglosmx=',20f7.3)
!   39 format (1x, 'sf84mn=',20f7.3)
!   40 format (1x, 'sf84ic =',f4.2,'  sf10ic =',f4.2,'   asvroc=',f4.2)
!   42 format (1x, 'canag =', f6.3,'   cancr = 'f6.3,'  awzypt=',f6.0)
!   41 format (1x, 'acxrow=', f6.2, '  ac0rg=', i3)

!   25 format (1x, 'szcr= ', 20f7.2)
!   26 format (1x, 'sfcr= ', 20f7.3)
!   27 format (1x, 'smlos=', 20f7.3)
!   28 format (1x, 'sflos=', 20f7.3)

!   29 format (1x, 'szrgh=', 20f7.2)
!   30 format (1x, 'slrr= ', 20f7.2)
!   38 format (1x, 'sxprg=', f6.0, '  abzht=', f6.2, '  abrlai=', f4.2,  &
!     &             '  abrsai=', f5.3, '  abffcv=',f4.3)
!   31 format (1x, 'ahrwcw=',f4.2,'  ahrwc0(12)=', f6.2)
!   32 format (1x, 'wus= ', 20f7.3)
!   33 format (1x, 'wusp=', 20f7.3)
!   34 format (1x, 'wust=', 20f7.3)
!   44 format (1x, 'wusto=', 20f5.3)

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

