!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
      subroutine npcy
!
!     + + + PURPOSE + + +
!     This subroutine is the master nutrient cycling subroutine.
!     calls NPMIN, NYNIT, NLCH, NMNIM, AND NDNIT for each soil
!     layer.
!
!     + + + KEWORDS + + +
!     nutrient cycling

!     + + + COMMON BLOCKS + + +

       include 'p1werm.inc'

! local includes
      include 'crop/cgrow.inc'
      include 'crop/cenvr.inc'
      include 'crop/cparm.inc'
      include 'crop/csoil.inc'
      include 'crop/chumus.inc'
      include 'crop/cfert.inc'

!     + + + LOCAL VARIABLES + + +
      real xx
      integer j
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     ssfn - sum of N leached from each layer (kg/ha)
!     xx - depth to previous layer (m)
!
!     + + + SUBROUTINES CALLED + + +
!     npmin
!     nynit
!     nlch
!     nmnim
!     ndnit
!
!     + + + OUTPUT FORMATS + + +
!
!     + + + END OF SPECIFICATIONS + + +
!
      smr=0.
      shm=0.
      sim=0.
      sdn=0.
      smp=0.
      sip=0.
      tsfn=0.
      xx=0.
      do 5 j=1,ir
!        J1=LID(J)
!        J2=J1
!     calculate relative moisture content of each layer
!        SUT=ST(J)/(FC(J)+1.E-10)
         sut=.8
         if (sut.gt.1.) sut=1.
!     calculate mineral P transformations
         call npmin (j)
!        IF (J1.NE.LD1) GO TO 2
!        calculate leaching from the top layer
!        CALL NYNIT (RQ)
!        GO TO 3
!     calculate leaching from layers other than the top
!   2    CALL NLCH (RQ)
!   3    TSFN=TSFN+SSFN
!         IF (T(J).LE.0.) GO TO 5
!     calculate soil temperature factor for each layer
!         CDG=T(J)/(T(J)+20551.*EXP(-.312*T(J)))
          cdg=1.
!        IF (RZ.LT.XX) GO TO 4
!         if (rz.gt.xx) then
!      calculate organic N & P transformations in layers where there are roots
            call nmnim (j)
            shm=shm+hmn
            smr=smr+rmnr
            sim=sim+wim
            smp=smp+wmp
            sip=sip+wip
!         endif
!     calculate N denitrification
!   4    IF (ST(J1)/PO(J1).LT..9) GO TO 5
!        if (st(j)/po(j).gt..9) then
!           CALL NDNIT
!           SDN=SDN+WDN
!        endif
!         XX=Z(J)
    5 continue
      return
      end
