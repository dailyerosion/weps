!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine updlay(daysim, szlyd,                                  &
     &  bhrwc0, bhrwc, bhrwcdmx,                                        &
     &  bseagmx, bseagmn, bseags,                                       &
     &  bhrwca, bhrwcw, bhrwcs,                                         &
     &  bhtsmn, bhtmx0, bhtsmx,                                         &
     &  bsecr,                                                          &
     &  bsk4d, bslmin, bslmax,                                          &
     &  bslagm,                                                         &
     &  bs0ags, bslagx, bsdblk,                                         &
     &  bszlyt, bsdagd, bslay, bsdcr,                                   &
     &  bsdsblk, bsdwblk,                                               &
     &  bhzinf, bhzwid, trigger)

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'

!     + + + ARGUMENT DECLARATIONS + + +

      integer daysim
      real szlyd(0:mnsz)
      real  bhrwc0(mnsz), bhrwc(mnsz), bhrwcdmx(mnsz)
      real  bseagmx(mnsz), bseagmn(mnsz), bseags(0:mnsz)
      real  bhrwca(mnsz), bhrwcw(mnsz),bhrwcs(mnsz)
      real  bhtsmn(mnsz), bhtmx0(mnsz), bhtsmx(mnsz)
      real  bsecr
      real bsk4d(mnsz), bslmin(mnsz), bslmax(mnsz)
      real bslagm(0:mnsz)
      real bs0ags(0:mnsz), bslagx(0:mnsz)
      real bsdblk(0:mnsz), bhzinf
      real bszlyt(mnsz), bsdagd(0:mnsz)
      real bsdcr, bsdsblk(mnsz), bsdwblk(mnsz)
      real bhzwid

      integer bslay, trigger(bslay)

!     + + + LOCAL VARIABLES + + + 
      real k4f, k4fs, k4fd, k4td, k4w,k4d
      parameter(k4f=1.4, k4fs=4.25, k4fd=5.08, k4w=1)
      real se0, se1
      integer ldx

!     + + + LOCAL DEFINITIONS + + +
!   k4f  - water migration & freezing expansion coef. for agg. stab.
!   k4fs - freezing solidification coef. for agg. stability
!   k4fd - drying while frozen coef. for agg. stability
!   k4td - after thaw drying coef. a fn of depth in soil
!   k4w  - wetting process coef. modified by current dry stability
!   k4d  - drying coef. a fn of depth in soil
!   se0  - relative aggregate stability prior to SOIL update
!   se1  - relative aggregate stability after SOIL update

!  + + +  UPDATE LAYERS: + + +

      do ldx = 1, bslay
!        check for dry soil - then no changes
         if ((bhrwc(ldx) .lt. bhrwcw(ldx)) .and.                        &
     &       (bhrwcdmx(ldx) .lt. bhrwcw(ldx)) .and.                     &
     &       (bhrwc0(ldx) .lt. bhrwcw(ldx))) go to 90
!
       !estimate parameters as fn of depth
        k4td = 0.4*(1+0.00333*szlyd(ldx))
        k4td = min(k4td,0.667)
        k4d  = 0.6*(1+0.00333*szlyd(ldx))
        k4d  = min(k4d, 1.0)
!Tmp ####
!       write (*,*)'ldx=',ldx,'szlyd=',szlyd(ldx),'k4d=',k4d
!
         call aggsta(daysim, bseags(ldx), bseagmn(ldx), bseagmx(ldx),   &
     &    bhrwc0(ldx), bhrwc(ldx), bhrwcdmx(ldx),                       &
     &    bhrwcw(ldx), bhrwca(ldx),bhrwcs(ldx),                         &
     &    bhtmx0(ldx), bhtsmn(ldx), bhtsmx(ldx), bsk4d(ldx),            &
     &    se0,se1,  trigger(ldx),                                       &
     &    k4f, k4fs, k4fd, k4td, k4w, k4d)

          call asd(bslagm(ldx), bslmin(ldx),                            &
     &    bslmax(ldx), bhtsmx(ldx), bhtmx0(ldx), bs0ags(ldx),           &
     &    bslagx(ldx), se0, se1)
!
         call den(bsdblk(ldx), bsdsblk(ldx), bsdwblk(ldx),              &
     &    bszlyt(ldx), bsdagd(ldx), bhrwc0(ldx), bhrwc(ldx),            &
     &    bhrwca(ldx), bhrwcw(ldx), bhzinf, bhzwid, trigger(ldx))
   90    continue

      end do

!     calc. new crust stability
      bsecr = bseags(1)

!     update crust density                                       (S-58)
      bsdcr = 0.576 + 0.603 * bsdsblk(1)
      end
