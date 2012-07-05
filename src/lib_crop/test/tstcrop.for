!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

c     file: 'tstcrop.for'

c     program   crop submodel of WERM

c     + + + PURPOSE + + +
c     This is the test program of crop of the Wind Erosion Research Model.
c     Its purpose is to control the sequence of events during a
c     simulation run.

c     author: John Tatarko
c     version: 92.10

c     + + + KEY WORDS + + +
c     wind, erosion, hydrology, tillage, soil, crop, decomposition
c     management

c#include <f77_floatingpoint.h>

c     external continue_hdl
c     external common_handler


c     + + + GLOBAL COMMON BLOCKS + + +
*$noereference
      include 'p1werm.inc'
      include 'p1unconv.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1db3.inc'
      include 'c1db4.inc'
      include 'c1db5.inc'
      include 'c1glob.inc'
      include 'd1glob.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'w1pavg.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

c     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
*$reference

c     + + + LOCAL VARIABLES + + +
      character header*80

      logical cflag, wflag, wcflag

      integer cd, cm, cy, ccd, ccm, ccy, cwd, cwm, cwy,
     i        diff, div,
     i        i, isr,
     i        l
c      for crop test routines
      integer pdate,pd,pm,py,hdate,hd,hm,hy

      real dur,
     r     grad,
     r     tp,
     r     wdir, wvel,
     r     xmav

c         used for debugging under unix
c     integer ieeer
c     integer ieee_handler
c      character out*80


c     + + + LOCAL DEFINITIONS + + +

c   am0*fl    - These are switches for production of submodel
c               output, where the asterisk represents the first
c               letter of the submodel name.
c   am0ifl    - This variable is an initialization flag which is
c               set to .false. after the first simulation day.
c   ccd       - The current day of the CLIGEN file.
c   ccm       - The current month of the CLIGEN file.
c   ccy       - The current year of the CLIGEN file.
c   cflag     - This logical variable controls output of warning
c               messages for mismatch of CLIGEN and simulation dates.
c   cd        - The current day of simulation month.
c   cm        - The current month of simulation year.
c   cy        - The current year of simulation run.
c   clifil    - This variable holds the CLIGEN input file name.
c   cwd       - The current day of the WINDGEN file.
c   cwm       - The current month of the WINDGEN file.
c   cwy       - The current year of the WINDGEN file.
c   daysim    - This variable holds the total current days of simulation.
c   diff      - This variable holds the number of simulation days.
c   div
c   grad      - Global radiation (ly/day) as read in from CLIGEN.
c   header    - Dummy variable to read in character values which
c               are not used.
c   i         - This variable is a counter for simulation loops.
c   id,im,iy  - The initial day, month, and year of simulation.
c   ijday     - This variable contains the initial julian day of
c               the simulation run.
c   isr       - This variable holds the subregion index.
c   l         - This variable is an index on soil layers.
c   ld,lm,ly  - The last day, month, and year of simulation.
c   ljday     - This variable contains the last julian day of
c               the simulation run.
c   nsubr     - This variable holds the total number of subregions.
c   subflg    - This logical variable is used to read header information
c               in the subdaily wind file (if .true., read header).
c   usrid     - This character variable is an identification string
c               to aid the user in identifying the simulation run.
c   usrloc    - This character variable holds a location
c               description of the simulation site.
c   usrnam    - This character variable holds the user name.
c   winfil    - This variable holds the WINDGEN input file name.
c   wflag     - This logical variable controls output of warning
c               messages for mismatch of WINDGEN and simulation dates.
c   wcflag    - This logical variable controls output of warning
c               messages for mismatch of CLIGEN and WINDGEN dates.

c     + + + SUBROUTINES CALLED + + +
c not calcwu   -  Subdaily wind speed generation
c     caldat   -  Converts julian day to day, month, and year (cd,cm,cy)
c     cdbug    -  Prints global variables before and after call to CROP
c     crop     -  Crop submodel
c not ddbug    -  Prints global variables before and after call to DECOMP
c not decomp   -  Decomposition submodel
c not erode    -  Erosion submodel
c not genrep   -  Output the general report
c not hdbug    -  Prints global variables before and after call to HYDRO
c not hydro    -  Hydrology submodel main program
c     input    -  Open files and perform input
c not manage   -  Management (tillage) submodel main program
c not mfinit   -  Management initiation subroutines
c not sdbug    -  Prints global variables before and after call to SOIL
c not soil     -  Soil submodel

c     + + + FUNCTIONS DECLARATIONS + + +
      integer julday

c     + + + FUNCTIONS CALLED + + +
c     julday    -  This function determines the julian day given day,
c                  month, and year.

c     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
c     * = screen and keyboard
c     1 = simulation run file
c     2 = general report output file
c     3 = CLIGEN run file
c     4 = WINDGEN run file
c     5 = Reserved
c     6 = Reserved - screen
c     7 = Reserved
c     8 = sub-daily wind speed data file
c     9 = SOIL/HYDROLOGY run file
c    10 = management (tillage) run file
c    11 = decomp run file
c    12 = 'water.out'   - hourly hydrology output file
c    13 = 'temp.out'    - daily soil temperature output file
c    14 = 'hydro.out'   - daily hydrology output file
c    15 =    ?          - management output file
c    16 = 'soil.out'    - soil output file
c    17 = 'crop.out'    - crop output file
c    18 = 'dabove.out'  - decomp above ground output file
c    19 = 'dbelow.out'  - decomp below ground output file
c    20 = 'eros.out'    - erosion output file
c    21 =
c    24 = hourly wind distribution file
c    25 = debug hydro
c    26 = debug soil
c    27 = debug crop
c    28 = debug decomp
c    31 = 'crop_dbs.dat' - crop growth parameter database
c    32 = 'plot.out'    - plotting output file

c     + + + DATA INITIALIZATIONS + + +
      daysim = 0

c     set initialization flags
      am0dif = .true.
      am0eif = .true.
c     no crop growing at start of simulation
      am0cgf = .false.
      am0ifl = .true.
      cflag = .false.
      subflg = .true.
      wflag = .false.
      wcflag = .false.

c     + + + INPUT FORMATS + + +
 1020 format (a80)
 1025 format (32x, f7.2)
 1030 format (2(2x,i2),1x,i4,1x,2f6.2,f5.2,1x,f6.2,3f7.2,f6.2,2f7.2)
 1040 format (2(1x, i2), 1x, i4, 4f6.1)

c     + + + OUTPUT FORMATS + + +
 2030 format (' warning !',28x,' day       month       year')
 2040 format (' current simulation date -              ',i2,9x,i2,8x,i4,
     &      /,' does not match current WINDGEN date -  ',i2,9x,i2,8x,i4,
     &      /)
 2050 format (' current simulation date -              ',i2,9x,i2,8x,i4,
     &      /,' does not match current CLIGEN date -   ',i2,9x,i2,8x,i4,
     &      /)
 2060 format (' current CLIGEN date -                  ',i2,9x,i2,8x,i4,
     &      /,' does not match current WINDGEN date -  ',i2,9x,i2,8x,i4,
     &      /)
 2070 format (1x,'day ',i4,'  of ',i4)
 2080 format (1x, 5f10.3)

c     + + + END SPECIFICATIONS + + +
c     call ieee_flags('clear', 'exception', 'all', out)
c     ieeer = ieee_handler('set','division',common_handler)
c     ieeer = ieee_handler('set','division',SIGFPE_ABORT)
c     if (ieeer .ne. 0) print*, 'could not set common_handler'
c     ieeer = ieee_handler('set','inexact',SIGFPE_IGNORE)
c     ieeer = ieee_handler('set','underflow',SIGFPE_IGNORE)
c     ieeer = ieee_handler('set','all',continue_hdl)

c     open input files and read run files
      call input

c     initialize the management file
c     call mfinit(nsubr, tinfil)
c      do 5 isr = 1, nsubr
c         call decini(isr)
c    5 continue

c     convert soil layer thickness (mm) to depth to bottom of layer (mm)
c     and initialize applied NO3
      do 10 isr=1,nsubr
         aszlyd(1, isr) = aszlyt(1, isr)
         do 15  l = 2, nslay(isr)
            aszlyd(l,isr) = aszlyt(l,isr) + aszlyd(l-1, isr)
   15    continue
         asmno3(isr) = 0.
   10 continue

c   temporarily initialize residue to bypass an error in crop-jt 7/22/94
c   remove when problem is solved on crop
      do 11 isr=1,nsubr
         do 16  l = 1, nslay(isr)
            admbgz(1,l,isr) = 0.01
   16    continue
   11 continue

c     read cligen header - remove when user interface is installed
      do 20 i = 1, 3
         read(3,1020) header
   20 continue
      read(3,1025) awtyav
c          write(*,*) 'year av',awtyav
      do 25 i = 1, 3
         read(3,1020) header
   25 continue
c     read windgen header
      do 30 i = 1, 5
         read (4,1020) header
   30 continue

c     open plot data file
      open(unit = 32, file = '../plot/plot.out', status = 'unknown')

c     querry for crop number (for crop test routine)
      do 31 i = 1, nsubr
         write(*,*) ' Enter the number for the crop to plant in subregio
     cn number ',i, ' ?'
         read(*,*) ac0id(i)
c        write(*,*) ac0id(i)
         if ((ac0id(i) .lt. 1) .or. (ac0id(i) .gt. 22)) then
            write(*,*) ' crop number ',ac0id(i),' is not available '
            goto 501
         end if
         write(*,*) ' Enter the planting date (day month year) for the c
     crop (eg. 09 22 1994) '
         read(*,*) pd,pm,py
         pdate = julday(pd, pm, py)
         write(*,*) '  Enter the harvest date (day month year) for the c
     crop (eg. 06 27 1995) '
         read(*,*) hd,hm,hy
         hdate = julday(hd, hm, hy)
   31 continue

c     begin simulation days
      do 50 am0jd = ijday,ljday

c        test for plant date (for crop test routine)
c        write(*,*) am0jd,pdate
         if (am0jd .eq. pdate) then
            am0cif = .true.
            call cinput
         endif
         if (am0jd .eq. hdate) am0cgf = .false.

         call caldat (am0jd, cd, cm, cy)

c        read weather cligen and windgen daily data
         read(3,1030) ccd,ccm,ccy,awzdpt,dur,tp,xmav,
     &                awtdmx,awtdmn,grad,wvel,wdir,awtdpt
         read(4,1040) cwd,cwm,cwy,awadir,awudmx,awudmn,awhrmx
         aweirr = grad * 0.04186
         awudav = (awudmx + awudmn) / 2.

c        calculate air density from temperature and pressure
         awtdav = (awtdmx + awtdmn) / 2.
         awdair = 348.56 * (1.013-0.1183*(amzele/1000.)
     &           + 0.0048 * (amzele/1000.)**2.) / (awtdav + 273.1)

c        test for date mismatch with weather files
c        print out warning on first mismatch only, skip thereafter

         if ((wflag .eqv. .true.) .and. (cflag .eqv. .true.) .and.
     &       (wcflag .eqv. .true.)) go to 57

         if (wflag .eqv. .true.) go to 55
         if ((cd .ne. cwd) .or. (cm .ne. cwm) .or. (cy .ne. cwy)) then
            write (6,2030)
            write (6,2040) cd,cm,cy,cwd,cwm,cwy
            wflag = .true.
         end if

   55    if (cflag .eqv. .true.) go to 56
         if ((cd .ne. ccd) .or. (cm .ne. ccm) .or. (cy .ne. ccy)) then
            write (6,2030)
            write (6,2050) cd, cm, cy, ccd, ccm, ccy
            cflag = .true.
         end if

   56    if (wcflag .eqv. .true.) go to 57
         if ((ccd .ne. cwd) .or. (ccm .ne. cwm) .or. (ccy .ne. cwy))
     &      then
            write (6,2030)
            write (6,2060) ccd, ccm, ccy, cwd, cwm, cwy
            wcflag = .true.
         end if

c        print current day of simulation to screen periodically
   57    daysim = daysim + 1
         diff = ljday - ijday + 1
         div = 10
         if (mod(daysim, div) .eq. 0) then
            write(*,2070) daysim, diff
         end if

c        do subregions
   36    do 60 isr = 1, nsubr
            am0csr = isr

c           call HYDROLOGY submodel

c           ieeer = ieee_handler('set','underflow',SIGFPE_DEFAULT)
c           if (am0hdb .eq. 1) then
c              call hdbug(cd, cm, cy, isr, nslay(isr))
c           end if
c           call hydro( nslay(isr), amrslp(isr),
c    &                  acftcv(isr), acrlai(isr),
c    &                  admf(isr), aczrtd(isr), ahfwsf(isr),
c    &                  aszlyd(1, isr), asdblk(1, isr),
c    &                  ahrwc(1, isr), ahrwcs(1, isr),
c    &                  ahrwcf(1, isr), ahrwcw(1, isr), ahrwca(1,isr),
c    &                  ah0cb(1,isr), aheaep(1,isr),
c    &                  asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),
c    &                  ah0cng(isr), ah0cnp(isr),
c    &                  ahzper(isr), ahzirr(isr), ahzrun(isr),
c    &                  awudav, ahrsk(1, isr),
c    &                  ahtsmx(1, isr), ahtsmn(1, isr),
c    &                  ahrwc0(1, isr), daysim,
c    &                  asfald(isr), asfalw(isr) )
c           if (am0hdb .eq. 1) then
c              call hdbug(cd, cm, cy, isr, nslay(isr))
c           end if

c           call MANAGEment (tillage) submodel
c            call manage (isr, cd, cm, cy, iy)

c           call SOIL submodel
c            if (am0sdb .eq. 1) then
c               call sdbug(cd, cm, cy, isr, nslay(isr))
c            end if
c            call soil (daysim, am0irr(isr), ahzirr(isr), ahzsmt(isr),
c     &                 ahtsmx(1,isr), ahtsmn(1,isr),
c     &                 ahrwc(1,isr), ahrwca(1,isr), ahrwcw(1,isr),
c     &                 asfom(1,isr), aszlyt(1,isr), nslay(isr),
c     &                 asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),
c     &                 asxrgs(isr), aszrgh(isr),
c     &                 aslrrc(isr), as0rrk(isr),
c     &                 aszcr(isr), asfcr(isr), asecr(isr),
c     &                 asdcr(isr), asmlos(isr), asflos(isr),
c     &                 asdblk(0,isr), asdagd(0,isr),
c     &                 aslagm(0,isr), aslagn(0,isr),
c     &                 as0ags(0,isr), aslagx(0,isr), aseags(0,isr),
c     &                 acftcv(isr), ac0rg(isr))
c            if (am0sdb .eq. 1) then
c               call sdbug(cd, cm, cy, isr, nslay(isr))
c            end if

            aszlyd(1, isr) = aszlyt(1, isr)
            do 70  l = 2, nslay(isr)
               aszlyd(l,isr) = aszlyt(l,isr) + aszlyd(l-1, isr)
   70       continue

c           call CROP submodel if between planting and killing harvest
             if (am0cgf .eqv. .true.) then
                if (am0cdb .eq. 1) then
                   call cdbug(cd, cm, cy, isr, nslay(isr))
                end if

               call crop  (nslay(isr), aszlyd(1,isr), asdblk(1,isr),
     &      asfcce(1,isr), asfom(1,isr), asfcec(1,isr), asfsmb(1,isr),
     &      asfcla(1,isr), as0ph(1,isr), asftan(1,isr), asftap(1,isr),
     &      asfnoh(1,isr), asfpoh(1,isr), asfpsp(1,isr), acrlai(isr),
     &      aczrtd(isr), acmst(isr), asmno3(isr),ac0id(isr),
     &      ac0alt(isr), ac0bn1(isr), ac0bn2(isr), ac0bn3(isr),
     &      ac0bp1(isr), ac0bp2(isr), ac0bp3(isr), ac0ck(isr),
     &      acrhi(isr), acehu0(isr), aczmxc(isr), ac0idc(isr),
     &      acephu(isr), acrbed(isr), aczmrt(isr), acrdla(isr),
     &      acrmla(isr), actmin(isr), actopt(isr), ac0bev(isr),
     &      ac0pt1(1,isr), ac0pt2(1,isr), ac0pt1(2,isr), ac0pt2(2,isr),
     &      ac0fd1(1,isr), ac0fd2(1,isr), ac0fd1(2,isr), ac0fd2(2,isr),
     &      ac0be1(1,isr), ac0be2(1,isr), ac0be1(2,isr),ac0be2(2,isr),
     &      acrmhi(isr), admbgz(1,1,isr))

                if (am0cdb .eq. 1) then
                   call cdbug(cd, cm, cy, isr, nslay(isr))
                end if
             end if

c           call the DECOMPosition submodel
c            if (am0ddb .eq. 1) then
c               call ddbug(cd, cm, cy, isr, nslay(isr))
c            end if
c            call decomp(isr)
c           call decman (isr)
c            if (am0ddb .eq. 1) then
c               call ddbug(cd, cm, cy, isr, nslay(isr))
c            end if
c            if ((am0dfl .eq. 1).or.(am0dfl .eq. 2).or.(am0dfl .eq.3))
c     &         then
c               call decout (cd, cm, cy)
c            end if
   60    continue

c         if (awudmx .gt. 8.) then
c            call calcwu
cc           call erosion
c           am0eif = .false.
c        else if (cd .eq. lstday(cm, cy)) then
c           call erosion
c          end if
c          else
c         end if

c        set initialization flag to .false. after first day
          am0ifl = .false.

c        open and print to plot data file
         write (32, 2080) awudmx, awzdpt, ahrwc0(1, 1),
     &                    aszrgh(1), acrlai(1)

   50 continue

c      if ((cd .eq. lstday(cm, cy) .and. ((am0psm .eq. 1) .or.
c     &   (am0psm .eq. 2) .or. (am0psm .eq. 3)) then
c         call period summaries
c         call persum (am0psm)
c      end if

c     output simulation general report

c      call genrep

  500 stop 'The WERM simulation run is finished'
  501 end

