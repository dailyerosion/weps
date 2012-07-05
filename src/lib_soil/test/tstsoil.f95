!$Author$
!$Date$
!$Revision$
!$HeadURL$

!Standalone program to test the soil submodel
!This program calls the subroutine 'soil'
!In WEPS the subroutine 'callsoil' calls the subroutine 'soil'
!This program was adapted from 'callsoil.for'

!Cleaned up some of the code so it will compile and run
!Checked in some example input files (soil.in and daily.in)
!Added some documentation but still not working right - jt 5/11/09

      integer thick(99), top(99), mid(99), bot(99)
      real stability(10)

! Arguments
      integer daysim
      integer isr
      integer ldx
      integer i
      integer j
      real ahzwid2
      character*99 input_in
      character*99 daily_in
      character*99 rough_out
      character*99 crust_out
      character*99 aggsta_out
      character*99 aggsize_out
      character*99 bulkdens_out

! Includes
      include 'p1werm.inc'
      include 'b1glob.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1agg.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'
      include 's1surf.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'w1clig.inc'
      include 'timer.inc'
      include 'precision.inc' !declaration for portable math range checking

      ! initialize command line arguments
      call cmdline

      isr = 1

      max_real = huge(1.0) * 0.999150
      max_arg_exp = log(max_real)

!Input files
      write(*,*) "Enter (Unit=2, input_in) soil input file name"
! This is the input file for soil properties
      read(*,*) input_in
      write(*,*) "Enter (Unit=1, daily_in) daily hydrology and weather file name"
! This is the input file for hydrology and temp
      read(*,*) daily_in

!Output files
      write(*,*) "Roughness output file: roughness.out "
      ! read(*,*) rough_out
      rough_out = "roughness.out"
      write(*,*) "Crust output file: crust.out"
      ! read(*,*) crust_out
      crust_out = "crust.out"
      write(*,*) "Aggr stability output file: aggstab.out"
      ! read(*,*) aggstability_out
      aggsta_out = "aggstab.out"
      write(*,*) "Aggr sise distr output file: aggsdistr.out"
      ! read(*,*) aggsizedistribution_out
      aggsize_out = "aggsdistr.out"
      write(*,*) "Bulk density output file: bulkdens.out"
      ! read(*,*) bulkdens_out
      bulkdens_out = "bulkdens.out"
      !   open input files

      open(1,daily_in)
      open(2,input_in)

      !   open output files
      open(13,rough_out)
      open(14,crust_out)
      open(15,aggsta_out)
      open(16,aggsize_out)
      open(17,bulkdens_out)

      !read one time variables (from Unit 2)
!LAYER INFO
      call fi(nslay(isr)) !number of soil layers
      !write(*,*) "number of soil layers", nslay(isr)
      do i = 1, nslay(isr)
        call fr(aszlyt(i,isr)) !layer i thickness, mm
        !write(*,*) "layer i thickness, mm ", i, aszlyt(i,isr)
      end do
!ORGANIC MATTER, SAND, SILT, CLAY, ROCK
      call fr(asfom(1,isr))   !fraction of organic matter
      !write(*,*) "fraction of organic matter", asfom(1,isr)
      call fr(asfsan(1,isr))  !fraction of sand
      call fr(asfsil(1,isr))  !fraction of silt
      call fr(asfcla(1,isr))  !fraction of clay
      call fr(asvroc(1,isr))  !fraction of rock (by volume)
!WATER
      call fr(ahrwca(1,isr))  !max. soil available water content on mass basis, kg water/kg soil
      call fr(ahrwcw(1,isr))  !wilting point = 15 bar-grav. soil water content, kg/kg
      call fr(ahrwcs(1,isr))  !saturated soil water content, kg/kg
      call fr(ahlocirr(isr))  !location of irrigation applied: positive = above soil, negative = buried
!RIDGE AND RANDOM ROUGHNESS
      call fr(aslrr(isr))     !random roughness height, mm
      call fr(aszrgh(isr))    !ridge height, mm
      call fr (asxrgs(isr))   !ridge spacing, mm
!CRUST
      call fr(asfcr(isr))     !fraction of soil crust cover, m2/m2
      call fr(aszcr(isr))     !crust thickness, mm
      call fr(asflos(isr))    !fraction of crust covered with loose material
      call fr(asmlos(isr))    !mass of loose material on crust
      call fr(asecr(isr))     !crust stability
      call fr(asdcr(isr))     !crust density
!AGGREGATE STABILITY
      call fr(aseags(0,isr))  !aggregate stability, ln(J/kg)
!AGGREGATE SIZE DISTRIBUTION
      call fr(aslagm(0,isr))  !aggregate geometric mean diameter, mm
      call fr(aslagn(0,isr))  !minimum aggregate diameter, mm
!DENSITY
      call fr(asdsblk(1,isr)) !consolidated (settled) soil bulk density, Mg/m3
      call fr(asdblk(0,isr))  !actual bulk density, Mg/m3
!BIOMASS
      call fr(abffcv(isr))    !fraction flat biomass cover
      call fr(abfscv(isr))    !fraction standing biomass cover
!OTHER
      call fr(asfcce(1,isr))  !fraction soil calcium carbonate equivalent
      call fr(asfcec(1,isr))  !soil cation exchange capacity, cmol/kg
      !write(*,*) "soil cation exchange capacity, cmol/kg", asfcec(1,isr)

      call soil_layer_dimensions(nslay(isr),aszlyt,thick,top,mid,bot)

      !Assign the value of layer 1 (or 'layer' 0) to all other layers
      do i = 1, nslay(isr)
        ahrwca(i,isr) = ahrwca(1,isr) !soil avaiable water content on mass basis, kg water/kg soil
        ahrwcw(i,isr) = ahrwcw(1,isr) !wilting point = 15 bar-grav. soil water content, kg/kg
        ahrwcs(i,isr) = ahrwcs(1,isr) !saturated soil water content, kg/kg
        asdsblk(i,isr)= asdsblk(1,isr)!consolidated (settled) soil bulk density by layer, Mg/m3
        asdblk(i,isr) = asdblk(0,isr) !actual bulk density, Mg/m3
        aseags(i,isr) = aseags(0,isr) !aggregate stability, ln(J/kg).
        asfcce(i,isr) = asfcce(1,isr) !fraction soil calcium carbonate equivalent
        asfcec(i,isr) = asfcec(1,isr) !soil cation exchange capacity, cmol/kg
        asfcla(i,isr) = asfcla(1,isr) !fraction of clay
        asfom(i,isr) = asfom(1,isr)   !fraction of organic matter.
        asfsan(i,isr) = asfsan(1,isr) !fraction of sand
        asfsil(i,isr) = asfsil(1,isr) !fraction of silt
        aslagm(i,isr) = aslagm(0,isr) !aggregate geometric mean diameter, mm.
        aslagn(i,isr) = aslagn(0,isr) !minimum aggregate diameter, mm.
        asvroc(i,isr) = asvroc(1,isr) !fraction of rock (soil volume)
      end do

      write(15,*) '                                            ',       &
     &  'Aggregate Stability, ln(J/kg)'
      write(16,*) '                                            ',       &
     &  'Geometric Mean Diameter, mm'
      write(17,*) '                                            ',       &
     &  '    Bulk Density, g/cm3'

      do j=13,17 !write header to 5 different output files
        write(j,10,ADVANCE='NO')
10      format('      pr irr mlt inf depth swc  Tsmn  Tsmx')
        if (j .eq. 13) then !specific info for roughness output
          write(j,*) '   rh      rr'
        elseif (j .eq. 14) then !specific info for crust output
          write(j,*) '  fract  thick  loose  loose   stab   dens ',     &
     &  '   rh     rr'
        else
          write(j,*) '             by layer'
        endif

        write(j,11,ADVANCE='NO')
11      format(' day  mm  mm  mm  mm  mm  kg/kg   C     C ')
        if (j .eq. 13) then !specific info for roughness output
          write(j,*) '   mm      mm'
        else if (j .eq. 14) then !specific info for crust output
          write(j,*) '  m2/m2    mm   m2/m2  kg/m2 ln(J/kg) Mg/m2',     &
     &  '   mm     mm'
        else
          !write layer number
          write(j,09) (i, i=1,nslay(isr))
09        format(20i7)
        endif
      enddo

      !skip first 14 lines of daily input data file
        character*44 dailyline
      do i=1,14
        read(1,*) dailyline
        !write(*,*) dailyline
      end do

      daysim = 0
      do while (2 > 1) !only leave this loop when eof.
      daysim = daysim + 1
      !read daily variables
      read(1,*,end=200)ahrwc(1,isr),                                    &
     & awzdpt,ahzirr(isr),ahzsmt(isr), ahzinf(isr), ahzwid(isr),        &
     & awtdav,ahtsmn(1,isr),ahtsmx(1,isr)

      ! set daily maximum value to daily value
      ahrwcdmx(1,isr) = ahrwc(1,isr)

      ahzwid2 = ahzwid(isr) !save in an other variable for later writing

      !Assign the value of layer 1 to all other layers
      do j = 2, nslay(isr)
        ahrwc(j,isr) = ahrwc(1,isr)
        ahrwcdmx(j,isr) = ahrwcdmx(1,isr)
        ahtsmn(j,isr) = ahtsmn(1,isr)
        ahtsmx(j,isr) = ahtsmx(1,isr)
      end do

      call timer(TIMSOIL,TIMSTART)
!
!            if (am0sdb .eq. 1) call sdbug(isr, nslay(isr))
            call soil (daysim, ahlocirr(isr), ahzirr(isr), ahzsmt(isr), &
     &                 ahtsmx(1,isr), ahtsmn(1,isr),                    &
     &                 ahrwc(1,isr), ahrwcdmx(1,isr), ahrwca(1,isr),    &
     &                 ahrwcw(1,isr), ahrwcs(1,isr),                    &
     &                 aszlyt(1,isr), nslay(isr),                       &
     &                 asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),     &
     &                 asfom(1,isr), asvroc(1,isr),                     &
     &                 asxrgs(isr), aszrgh(isr), aszrho(isr),           &
     &                 aslrr(isr), aslrro(isr),                         &
     &                 aszcr(isr), asfcr(isr), asecr(isr),              &
     &                 asdcr(isr), asmlos(isr), asflos(isr),            &
     &                 asdsblk(1,isr), asdwblk(1,isr),                  &
     &                 asdblk(0,isr), asdagd(0,isr),                    &
     &                 aslagm(0,isr), aslagn(0,isr),                    &
     &                 as0ags(0,isr), aslagx(0,isr), aseags(0,isr),     &
     &                 aseagm(1,isr), aseagmn(1,isr), aseagmx(1,isr),   &
     &                 ask4d(1,isr), aslmin(1,isr), aslmax(1,isr),      &
     &                 abffcv(isr), abfscv(isr),                        &
     &                 asfcce(1,isr), asfcec(1,isr),                    &
     &                 ahzinf(isr), ahzwid(isr), awzdpt, awtdav         &
     &                )
!            if (am0sdb .eq. 1) call sdbug(isr, nslay(isr))

! Depth to layer bottoms need to be updated after SOIL and MANAGE
            aszlyd(1, isr) = aszlyt(1, isr)
            do  ldx = 2, nslay(isr)
               aszlyd(ldx,isr) = aszlyt(ldx,isr) + aszlyd(ldx-1, isr)
            end do

      call timer(TIMSOIL,TIMSTOP)

! write ridge height and random roughness
      write(13,12) daysim,                                              &
     & int(awzdpt),int(ahzirr(isr)),int(ahzsmt(isr)),                   &
     & int(ahzinf(isr)),int(ahzwid2),                                   &
     & ahrwc(1,isr),ahtsmn(1,isr),ahtsmx(1,isr),                        &
     & aszrgh(isr), aslrr(isr)
! write crust
      write(14,12) daysim,                                              &
     & int(awzdpt),int(ahzirr(isr)),int(ahzsmt(isr)),                   &
     & int(ahzinf(isr)),int(ahzwid2),                                   &
     & ahrwc(1,isr),ahtsmn(1,isr),ahtsmx(1,isr),                        &
     & asfcr(isr),aszcr(isr),asflos(isr),asmlos(isr),                   &
     & asecr(isr),asdcr(isr),aszrgh(isr), aslrr(isr)
! write dry aggregate stability
      write(15,12) daysim,                                              &
     & int(awzdpt),int(ahzirr(isr)),int(ahzsmt(isr)),                   &
     & int(ahzinf(isr)),int(ahzwid2),                                   &
     & ahrwc(1,isr),ahtsmn(1,isr),ahtsmx(1,isr),                        &
     & (aseags(i,isr), i=1,nslay(isr))
! write aggregate size distribution
      write(16,12) daysim,                                              &
     & int(awzdpt),int(ahzirr(isr)),int(ahzsmt(isr)),                   &
     & int(ahzinf(isr)),int(ahzwid2),                                   &
     & ahrwc(1,isr),ahtsmn(1,isr),ahtsmx(1,isr),                        &
     & (aslagm(i,isr), i=1,nslay(isr))
! write bulk density
      write(17,12) daysim,                                              &
     & int(awzdpt),int(ahzirr(isr)),int(ahzsmt(isr)),                   &
     & int(ahzinf(isr)),int(ahzwid2),                                   &
     & ahrwc(1,isr),ahtsmn(1,isr),ahtsmx(1,isr),                        &
     & (asdblk(i,isr), i=1,nslay(isr))

12    format(i4, 5i4, f6.2, 2f6.1, 20f7.2)

      end do

200   continue
! end of daily processing

! write soil layer dimensions for first day
      call write_soil_layer_dimensions(nslay(isr),thick, top, mid, bot, &
     &                                             'first')
      !calculate soil layer info for last day
      call soil_layer_dimensions(nslay(isr), aszlyt,thick,top,mid, bot)
      !write soil layer dimensions for last day
      call write_soil_layer_dimensions(nslay(isr),thick, top, mid, bot, &
     &                                             'last ')

      do j=13,17
        write(j,*)
        write(j,410) ahrwcw(1,isr)
410     format(' water content at wilting point (kg/kg) ', f6.2)
        write(j,420) ahrwcw(1,isr) + ahrwca(1,isr)
420     format(' water content at field capacity (kg/kg)', f6.2)
        write(j,430) ahrwcs(1,isr)
430     format(' water content at saturation (kg/kg)    ', f6.2)
      enddo

      do j=13,17
        write(j,*)
      enddo

      write(14,220) asfcla(1,isr)
220   format(' clay fraction:', f5.2)
      write(14,230) asfsan(1,isr)
230   format(' sand fraction:', f5.2)
      write(14,240) asfom(1,isr)
240   format(' organic matter fraction:', f5.2)
      write(14,250) asfcce(1,isr)
250   format(' fraction calcium carbonate equivalent:', f5.2)

      write(15,*) 'Aggregate Stability, ln(J/kg): '
      write(15,20) aseagmn(1,isr)
20    format(' minimum        ', f6.2)
      do i=1,9  !calculate aggregate stability at 10, 20, ... 90 % above minimum agg. sta.
        stability(i) = aseagmn(1,isr) + i * 0.1 * (aseagmx(1,isr)       &
     &- aseagmn(1,isr))
        write(15,22) i*10, stability(i)
      enddo
22    format(i3, ' % above min.', f6.2)
      write(15,40) aseagmx(1,isr)
40    format(' maximum        ', f6.2)

      write(16,320) aslmin(1,isr)
320   format(' minimum geometric mean diameter:', f6.2, ' mm')
      write(16,330) aslmax(1,isr)
330   format(' maximum geometric mean diameter:', f6.2, ' mm')
      write(16,335) asfsan(1,isr)
335   format(' sand fraction:', f5.2)
      write(16,350) asfsil(1,isr)
350   format(' silt fraction:', f5.2)
      write(16,340) asfcla(1,isr)
340   format(' clay fraction:', f5.2)
      write(16,360) asfom(1,isr)
360   format(' organic matter fraction:', f5.2)
      write(16,370) asfcce(1,isr)
370   format(' fraction calcium carbonate equivalent:', f5.2)

      write(17,150) asdsblk(1,isr)
150   format(' settled bulk density:', f6.2)

      end


      subroutine fr(dummy) !fetch real from input file
! lines in the input file that start with # will be skipped
      real dummy
      character*44 line
1     read(2,*) line
      !write(*,*) "fr line***", line
      if (line(1:1) .ne. '#') goto 2
      goto 1
2     read(line,*) dummy
      !write(*,*) "fr dummy***", dummy
      return
      end

      subroutine fi(dummy) !fetch integer from input file
! lines in the input file that start with # will be skipped
      integer dummy
      character*44 line
1     read(2,*) line
      !write(*,*) "fi line***", line
      if (line(1:1) .ne. '#') goto 2
      goto 1
2     read(line,*) dummy
      !write(*,*) "fi dummy***", dummy
      return
      end

! calculate top, midpoint, and bottom of soil layers
      subroutine soil_layer_dimensions(n,thickness,thick, top, mid, bot)
      integer i, j, n, thick(99), top(99), mid(99), bot(99)
      real thickness(99,1)
      do i = 1, n !do for all n soil layers
        thick(i) = thickness(i,1) !layer thickness of layer i, mm
        bot(i) = 0
        do j = 1, i
          bot(i) = bot(i) + thick(j) !bottom of layer i below surface,mm
        end do
        top(i) = bot(i) - thick(i) !top of layer i below surface, mm
        mid(i) = bot(i) - 0.5*thick(i) !midpoint of layer i below surface, mm
      end do
      return
      end

! write soil layer dimensions to 3 different output files
      subroutine write_soil_layer_dimensions(n,thick,top,mid, bot, text)
      integer i, j, n, thick(99), top(99), mid(99), bot(99)
      character*5 text
      do j=15,17
        write(j,*)
        write(j,2,ADVANCE='NO')
        write(j,4,ADVANCE='NO') text
        write(j,*) 'day:'
        write(j,13,ADVANCE='NO')
        write(j,09)(i, i=1,n)
        write(j,14,ADVANCE='NO')
        write(j,09)(thick(i), i=1,n)
        write(j,15,ADVANCE='NO')
        write(j,9) (top(i), i=1,n)
        write(j,16,ADVANCE='NO')
        write(j,9) (mid(i), i=1,n)
        write(j,17,ADVANCE='NO')
        write(j,9) (bot(i), i=1,n)
      enddo
2     format('On the ')
4     format(a6)
9     format(20i7)
13    format('    layer number                          ')
14    format('    thickness (mm)                        ')
15    format('    top (mm)                              ')
16    format('    midpoint (mm)                         ')
17    format('    bottom (mm)                           ')
      return
      end
