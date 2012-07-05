!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file: decout.for

      subroutine  decout()

!       + + +  PURPOSE + + + +
!      This subroutine writes decomposition output

!  +  + +  COMMON  BLOCKS + + +

      include 'file.inc'
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'd1glob.inc'
      include 's1layr.inc'

      include 'decomp/decomp.inc'

! + + + DECLARE VARIABLES + + +

!      integer outday(13)
      integer cd, cm, cy

! + + + FORMATS + + +

 2000 format (i2,2x,i2,'/',i2,'/',i4,'  1',1x,f5.0,1x,f7.2,1x,f7.2,1x,  &
     &        f7.2,1x,f7.2,2x,f6.2,3x,f6.2,3x,2(f6.2,3x),1x,3(f6.2,3x), &
     &        /,15x,' 2',1x,f5.0,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,/,     &
     &        15x,' 3',23x,f7.2,1x,f7.2)
 2001 format (i2,2x,i2,'/',i2,'/',i4,'  1',1x,f5.0,1x,f7.2,1x,f7.2,1x,  &
     &        f7.2,1x,f7.2,2x,f6.2,3x,2(f6.2,3x),1x,f6.2,3x,            &
     &        /,15x,' 2',1x,f5.0,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,/,     &
     &        15x,' 3',23x,f7.2,1x,f7.2)
 2005 format (1x,'subr',2x,i2,'/',i2,'/',i4,4x,'cumddg',13x,'admbgz',   &
     & 13x,'admrtz',/,1x,i2,3x,'layer',7x,3(' 1',6x,' 2',8x))
 2010 format (6x,i3,4x,3(f7.2,1x,f7.2,3x))

! + + + END SPECIFICATIONS + + +

       call caldatw(cd, cm, cy)

! write output for subregion 1 only
! standing and surface residues

!  test code  use goto to get output for specific days only

!  and remove 'c' for lines following 25 and 'end if' at 2025

!  buried = goto 10
!  stemcts = goto 15
!  sir residues = goto 20
!  no outdays = goto 25

!      goto 25

!  Buried residue South Section experiment outdays

!10    outday(1)=1
!      outday(2)=34
!      outday(3)=65
!      outday(4)=99
!      outday(5)=126
!      outday(6)=161
!      outday(7)=191
!      outday(8)=216
!
!      goto 25

!    STEMCT experiment  OUTDAYS

!15    outday(1)=18
!      outday(2)=98
!      outday(3)=158
!      outday(4)=223
!      outday(5)=289
!      outday(6)=379
!
!      goto 25
!
!c     surface sir residues output days
!
!20    outday(1)=1
!      outday(2)=34
!      outday(3)=65
!      outday(4)=99
!      outday(5)=126
!      outday(6)=161
!      outday(7)=191
!      outday(8)=216
!      outday(9)=251
!      outday(10)=279
!      outday(11)=307
!      outday(12)=336
!      outday(13)=370

!25    continue

!      if(daysim.eq.outday(1)) i=1
!      if(daysim.eq.outday(i)) then
!     output above ground data on a daily basis
      if ((am0dfl .eq. 1) .or. (am0dfl .eq. 3)) then
         write (luod_above,2001) am0csr, cd, cm, cy,                    &
     &   addstm(1,am0csr), cumdds(1,am0csr), admst(1,am0csr),           &
     &   cumddf(1,am0csr), admf(1,am0csr), adfscv(1,am0csr),            &
     &   adffcv(1,am0csr), adrsai(1,am0csr),                            &
     &   adrsaz(1,1,am0csr),                                            &
     &   addstm(2,am0csr), cumdds(2,am0csr), admst(2,am0csr),           &
     &   cumddf(2,am0csr), admf(2,am0csr), cumddf(3,am0csr),            &
     &   admf(3,am0csr)
      end if

!     output below ground residues
      if ((am0dfl .eq. 2) .or. (am0dfl .eq. 3)) then
         write(luod_below,2005) cd, cm, cy, am0csr
         do 100 isz = 1, nslay(am0csr)
         write (luod_below,2010)                                        &
     &         isz,cumddg(isz,1,am0csr),cumddg(isz,2,am0csr),           &
     &         admbgz(isz,1,am0csr),admbgz(isz,2,am0csr),               &
     &         admrtz(isz,1,am0csr),admrtz(isz,2,am0csr)
  100    continue
      end if

! root residues

!      write (43,2020) daysim,
!     &(cumddg(isz,1,1), admrtz(isz,1,1),  isz = 1,nslay(1)),
!     &(cumddg(isz,2,1), admrtz(isz,2,1),  isz = 1,nslay(1))

!2020  format (i3,16(2x,f6.2,1x,f5.2))

! cover estimates  for flat and standing residue

!2025  format (2x,i3,8x,f5.4,2x,f6.4,8x,2(f6.4,3x),3x,3(f6.4,3x))

!     end if

      return
      end
