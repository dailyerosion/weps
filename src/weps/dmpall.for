!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!     file: 'input.for'

      subroutine   dmpall(filnam)

!     + + + PURPOSE + + +
!     dumps all variables read in by input in main
!     author: wjr
!
!     EDIT History
!     17-Feb-99   wjr   original coding
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
      include 'c1gen.inc'
      include 'c1glob.inc'
      include 'b1glob.inc'
      include 'd1gen.inc'
      include 'd1glob.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      integer       i, isr, iar, l, ibr

!     + + + LOCAL DEFINITIONS + + +

!   amo*fl    - These are switches for production of submodel
!               output, where the asterisk represents the first
!               letter of the submodel name.
!   *infil    - These character variables hold the run file names
!               where the asterisk represents the first
!               letter of the submodel name.
!   clifil    - This variable holds the CLIGEN input file name.
!   gnrpt    - This variable holds flags to select the various general
!               report forms.
!   i         - Generic loop counter.
!   id,im,iy  - The initial day, month, and year of simulation.
!   ijday     - This variable contains the initial julian day of
!               the simulation run.
!   isr       - This variable holds the subregion index.
!   l         - This variable is a index on soil layers.
!   lchar     - This variable holds the character position in a string
!               so to ignore leading blanks in that string.
!   ld,lm,ly  - The last day, month, and year of simulation.
!   ljday     - This variable contains the last julian day of
!               the simulation run.
!   nslay     - The number of soil layers.
!   nsubr     - This variable holds the total number of subregions.
!   runfil    - This variable holds the simulation run input file name.
!   series    - This character variable holds the soil series name.
!   simout    - This variable holds the simulation output file name.
!   sinfil    - This variable holds the SOIL/HYDROLOGY input file name.
!   subfil    - This variable holds the subdaily wind information
!               ('real data') file name for use by subroutine 'calcwu'.
!   usrid     - This character variable is an identification string
!               to aid the user in identifying the simulation run.
!   usrloc    - This character variable holds a location
!               description of the simulation site.
!   usrnam    - This character variable holds the user name.
!   winfil    - This variable holds the WINDGEN input file name.

        character*(*) filnam

      call fopenk(luo1, filnam, 'unknown')

      write (luo1, *) 'rootp ',      rootp

      write(luo1, *) 'rootp is: ', rootp(1:len_trim(rootp))

      write (luo1, *) 'runfil ',        runfil 


      do 30, isr = 1, nsubr
      write (luo1, *) 'am0monirr ',    am0monirr(isr) 
      write (luo1, *) 'ahzirr ',       ahzirr(isr) 
         do 33 l = 1, nslay(isr)
      write (luo1, *) 'abmbgz ',          abmbgz(l,isr)
   33    continue
   30   continue

      write (luo1, *) 'usrnam ',      usrnam 
      write (luo1, *) 'usrid ',      usrid 
      write (luo1, *) 'usrloc ',      usrloc 
      write (luo1, *) 'amalat ',      amalat
      write (luo1, *) 'amalon ',       amalon
      write (luo1, *) 'amzele ',      amzele
      write (luo1, *) 'id,im,iy ',      id,im,iy
      write (luo1, *) 'ld,lm,ly ',      ld,lm,ly
      write (luo1, *) 'ntstep ',      ntstep
      write (luo1, *) 'clifil ',      clifil 
      write (luo1, *) 'winfil ',      winfil 
      write (luo1, *) 'subfil ',        subfil 
      write (luo1, *) 'sinfil ',      sinfil 
      write (luo1, *) 'tinfil ',      tinfil 
      write (luo1, *) 'simout ',      simout 
      write (luo1, *) 'gnrpt ',       (gnrpt(i), i=1,6)
      write (luo1, *) 'erosrpt ',       erosrpt
      write (luo1, *) 'am0hfl, etc. ',am0hfl,am0sfl,am0tfl,am0cfl,      &
     &am0dfl,am0efl
      write (luo1, *) 'am0hdb, etc. ',      am0hdb,am0sdb,am0cdb,       &
     &am0ddb,am0tdb
      write (luo1, *) 'amasim ',       amasim
      write (luo1, *) 'amxsim ',       amxsim(1,1), amxsim(2,1)
      write (luo1, *) 'amxsim2 ',       amxsim(1,2),amxsim(2,2)
      write (luo1, *) 'nacctr ',       nacctr
      do 10 iar=1,nacctr
      write (luo1, *) 'iar ', iar
      write (luo1, *) 'amxar ', amxar(1,1,iar), amxar(2,1,iar)
      write (luo1, *) 'amxar2 ',       amxar(1,2,iar), amxar(2,2,iar)
   10 continue
      write (luo1, *) 'nsubr (should be 1) ',nsubr
      do 20 isr=1,nsubr
        write (luo1, *) 'isr ', isr
      write (luo1, *) 'amxsr ',       amxsr(1,1,isr), amxsr(2,1,isr)
      write (luo1, *) 'amxsr2 ',       amxsr(1,2,isr), amxsr(2,2,isr)
      write (luo1, *) 'nbr ',       nbr
      ibr = 1
      write (luo1, *) 'amxbr ',       amxbr(1,1,ibr), amxbr(2,1,ibr)
      write (luo1, *) 'amxbr2 ',       amxbr(1,2,ibr), amxbr(2,2,ibr)
      write (luo1, *) 'amzbr ',       amzbr(ibr)
      write (luo1, *) 'amxbrw ',       amxbrw(ibr)
      write (luo1, *) 'ampbr ',       ampbr(ibr)
      write (luo1, *) 'amrslp ',       amrslp(isr)
  20  continue

      do 200 isr = 1,nsubr
      write (luo1, *) 'isr ', isr
      write (luo1, *) 'nslay ',         nslay(isr)      
      write (luo1, *) 'aszlyt ',         (aszlyt(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfsan ',         (asfsan(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfsil ',         (asfsil(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfcla ',         (asfcla(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asvroc ',         (asvroc(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asdblk ',         (asdblk(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aslagm ',         (aslagm(l,isr), l=1,nslay(isr))
      write (luo1, *) 'as0ags ',         (as0ags(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aslagx ',         (aslagx(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aslagn ',         (aslagn(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asdagd ',         (asdagd(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aseags ',         (aseags(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aszcr ',         aszcr(isr)
      write (luo1, *) 'asdcr ',         asdcr(isr)
      write (luo1, *) 'asecr ',         asecr(isr)
      write (luo1, *) 'asfcr ',         asfcr(isr)
      write (luo1, *) 'asmlos ',         asmlos(isr)
      write (luo1, *) 'asflos ',         asflos(isr)
      write (luo1, *) 'aslrr ',         aslrr(isr)
      write (luo1, *) 'asargo ',         asargo(isr)
      write (luo1, *) 'aszrgh ',         aszrgh(isr)
      write (luo1, *) 'asxrgs ',         asxrgs(isr)
      write (luo1, *) 'asxrgw ',         asxrgw(isr)
      write (luo1, *) 'ahrwc ',         (ahrwc(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ahrwcs ',         (ahrwcs(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ahrwc ',         (ahrwcf(l,isr), l=1,nslay(isr))
      write (luo1, *) 'agrwcw ',         (ahrwcw(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ah0cb ',     (ah0cb(l,isr), l=1,nslay(isr))
      write (luo1, *) 'aheaep ',         (aheaep(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ahrsk ',         (ahrsk(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ah0cnp ',         ah0cnp(isr)
      write (luo1, *) 'ah0cng ',       ah0cng(isr)
      write (luo1, *) 'asfald ',         asfald(isr)
      write (luo1, *) 'asfalw ',      asfalw(isr)
      write (luo1, *) 'asfom ',         (asfom(l,isr), l=1,nslay(isr))
      write (luo1, *) 'as0ph ',         (as0ph(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfcce ',         (asfcce(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfcec ',         (asfcec(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asfsmb ',         (asfsmb(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asftan ',         (asftan(l,isr), l=1,nslay(isr))
      write (luo1, *) 'asftap ',         (asftap(l,isr), l=1,nslay(isr))
      write (luo1, *) 'ad0nam ',       ad0nam(1,isr) 
      write (luo1, *) 'addstm adzht ',addstm(1,isr), adzht(1,isr)
      write (luo1, *) 'admst ',          admst(1,isr)
      write (luo1, *) 'admf ',         admf(1,isr)
! Only printing the first decomp pool results here, I think that is ok  - LEW
      write (luo1, *) 'admbgz ', (admbgz(l,1,isr), l = 1, nslay(isr))
      write (luo1, *) 'admrtz ', (admrtz(l,1,isr), l = 1, nslay(isr))
  200 continue
      close (luo1)
      return
      end

