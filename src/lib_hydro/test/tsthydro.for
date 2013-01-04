!$Author$
!$Date$
!$Revision$
!$HeadURL$

!***********************************************************************
!     MAIN for TSTHYDRO
!***********************************************************************

!     +++  PURPOSE +++

!     To start a standalone version of the HYDROLOGY submodel

!     +++ ARGUMENT DECLARATIONS +++
     
!     + + + GLOBAL COMMON BLOCKS + + +
! not sure if this one is needed
!      include 'main/main.inc'

      include 'p1werm.inc'
      include 'b1glob.inc'
      include 'c1glob.inc'
      include 'c1gen.inc'
      include 'd1glob.inc'
      include 'm1sim.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'
      include 's1surf.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'h1scs.inc'
      include 'h1balance.inc'
      include 'w1wind.inc'
      include 'w1clig.inc'
      include 'timer.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS      

!     ++++ ARGUMENT DEFINITIONS +++

!     +++ SUBROUTINES CALLED+++
     
!     ++++ LOCAL VARIABLES +++
      integer isr
      integer mxdasm
      integer idx
      integer cd, cm, cy
      real grad
      real temp

!     +++ END SPECIFICATIONS +++

! use isr so that we don't change all of the parameters in the function calls

      am0ifl = .true.
      isr = 1
      am0csr = 1
      am0hfl = 2
      am0dfmfl = 0
      am0drmfl = 1
      nsubr = 1

C read in parameters

      write(*,*) 'Enter average slope '
      read(*,*) amrslp(isr)
      write(*,*) 'Enter crop biomass cover fraction '
      read(*,*) acftcv(isr)
      write(*,*) 'Enter crop leaf area index '
      read(*,*) acrlai(isr)
      write(*,*) 'Enter total flat biomass '
      read(*,*) bdmft
      write(*,*) 'Enter root depth '
      read(*,*) aczrtd(isr)
      write(*,*) 'Enter average daily wind speed '
      read(*,*) awudav
      write(*,*) 'Enter minimum air temperature '
      read(*,*) awtdmn
      write(*,*) 'Enter maximum air temperatures '
      read(*,*) awtdmx
      write(*,*) 'Enter number of days to simulate '
      read(*,*) mxdasm
      write(*,*) 'Enter ifc file name '
      read(*,*) sinfil
      write(*,*) 'Enter cli_gen file name '
      read(*,*) clifil
      call fopenk (luicli, clifil, 'old')
      call fopenk(luo1, 'h1.out', 'unknown')

! read in the ifc file        

!     call inpsub
      call input_ifc

! re-calc the layers
      
      call recalc

! calculate layer depths

      awtdav = (awtdmn + awtdmx) / 2
      amzele = 35.0
      write(*,*) ' awtdav ', awtdav
      aszlyd(1,isr) = aszlyt(1,isr)
      do 20 idx = 2,nslay(isr)
        aszlyd(idx,isr) = aszlyd(idx-1,isr) + aszlyt(idx,isr)
   20 continue

! initialize the hydro variables
      call hydrinit(isr)

! actually call hydro, printing out variables as we go

      daysim = 0
      write(luo1,1001) daysim, (ahrwca(idx,isr), idx=1,10)
      write(luo1,1004) daysim, (ahrwc(idx,isr), idx=1,10)
      write(luo1,1002) daysim, (ahrwcf(idx,isr), idx=1,10)
      write(luo1,1003) daysim, (ahrwcw(idx,isr), idx=1,10)
      write(luo1,1007) daysim, (ahrwcs(idx,isr), idx=1,10)

      write(luo1,1005) 
 1005 format('      awzdpt  ',                                          &
     &  '   ahrwc     ahrwc     ahrwc     ahrwc  ')
      write(*,*) ' ah0cnp, ah0cng ', ah0cnp(isr), ah0cng(isr)
      write(luo1,1006) 0, 0.0, (ahrwc(idx,isr), idx=1,7)
      do 10 daysim=(1+365*2000),mxdasm+365*2000
        awzdpt=0.0
        call caldat (daysim, cd, cm, cy)

        call getcli(cd, cm, cy, awzdpt, awtdmx, awtdmn, grad, awtdpt)
        aweirr = grad * 0.04186

        call hydro( nslay(isr), amrslp(isr),
     &                  acftcv(isr), acrlai(isr),
     &                  bdmft, aczrtd(isr), ahfwsf(isr),
     &                  aszlyd(1, isr), asdblk(1, isr),
     &                  ahrwc(1, isr), ahrwcs(1, isr),
     &                  ahrwcf(1, isr), ahrwcw(1, isr),
     &                  ah0cb(1,isr), aheaep(1,isr),
     &                  asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),
     &                  ah0cng(isr), ah0cnp(isr),
     &                  ahzper(isr), ahzirr(isr), ahzrun(isr),
     &                  awudav, ahrsk(1, isr),
     &                  ahtsmx(1, isr), ahtsmn(1, isr),
     &                  ahrwc0(1, isr), daysim,
     &                  asfald(isr), asfalw(isr), aszlyt(1,isr),
     *                  awzdpt, awtdmx, awtdmn, ahzwid(isr) )

      temp =  sum(ahrwc0(1:24,isr))/(24 *  asdblk(1,isr))

      write(luo1,1006) daysim, awzdpt, temp, (ahrwc(idx,isr), idx=1,7)
1006  format(i8, 9f10.4) 
   10 continue
      write(luo1,1002) daysim, (ahrwcf(idx,isr), idx=1,10)
      write(luo1,1001) daysim, (ahrwca(idx,isr), idx=1,10)
1001  format(i4, ' ahrwca ', 10f8.4) 
1004  format(i4, ' ahrwc  ', 10f8.4) 
1002  format(i4, ' ahrwcf ', 10f8.4) 
      write(luo1,1003) daysim, (ahrwcw(idx,isr), idx=1,10)
1003  format(i4, ' ahrwcw ', 10f8.4) 
      write(luo1,1007) daysim, (ahrwcs(idx,isr), idx=1,10)
1007  format(i4, ' ahrwcs ', 10f8.4) 
 
      close (luo1)
      end
