!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file: chumus.inc

!     This file contains the declaration of the common block of variables
!     related to N&P transformations from humus and fresh o.m.(residue)

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real rsd,rsdi,rc,fon,fop,wn,wp,wim,wip,cmn,hum,rmnr,hmn,wmp,rtn,  &
     & wmn,yc,twn,twmn,tap,tno3,shm,smr,sim,sdn,smp,sip,tsfn,trmn,trmp, &
     & thmn,thmp,trsd,tfon,tfop,tmp,top,tp

      common / humus /                                                  &
     &                cmn,                                              &
     &                fon(mnsz),                                        &
     &                fop(mnsz),                                        &
     &                hmn,                                              &
     &                hum(mnsz),                                        &
     &                rc,                                               &
     &                rsd(mnsz),                                        &
     &                rsdi(mnsz),                                       &
     &                rmnr,                                             &
     &                rtn(mnsz),                                        &
     &                shm,                                              &
     &                smr,                                              &
     &                sim,                                              &
     &                sdn,                                              &
     &                smp,                                              &
     &                sip,                                              &
     &                tap,                                              &
     &                tno3,                                             &
     &                tsfn,                                             &
     &                trmn,                                             &
     &                trmp,                                             &
     &                thmn,                                             &
     &                thmp,                                             &
     &                trsd,                                             &
     &                tfon,                                             &
     &                tfop,                                             &
     &                tmp,                                              &
     &                top,                                              &
     &                tp,                                               &
     &                twn,                                              &
     &                twmn,                                             &
     &                wim,                                              &
     &                wip,                                              &
     &                wmn(mnsz),                                        &
     &                wmp,                                              &
     &                wn(mnsz),                                         &
     &                wp(mnsz),                                         &
     &                yc

      save :: /humus/

!    rsdi,rc, are new variables not included in EPIC1910.
!
!     + + + VARIABLE DEFINITIONS + + +
!     cmn - ???
!     fon - amount of N in residue (kg/ha)
!     fop - amount of P in residue (kg/ha)
!     hmn - amount of N mineralized from active N pool (kg/ha/d)
!     hmp - amount of P mineralized from active P pool (kg/ha/d)
!     hum - amount of humus (t/ha)
!     rc - residue composition factor (0.8, or 0.05, or 0.0095)
!     rmnr - net mineralized n from all sources of o.m. (kg/ha)
!     rsd - current amount of residue (t/ha) in a layer
!     rsdi - initial amount of residue in a layer (t/ha)
!     rtn - active pool fraction
!     sdn - sum of denitrified N (kg/ha)
!     shm - sum of mineralized N from humus (kg/ha)
!     sim - sum of immobilized N (kg/ha)
!     sip - sum of immobilized P (kg/ha)
!     smp - sum of mineralized P from humus and fresh residue (kg/ha)
!     smr - sum of mineralized N from humus & fresh residue (kg/ha)
!     tap - total labile P (kg/ha)
!     tfon - total amount of N from fresh organic matter (kg/ha)
!     tfop - total amount of P from fresh organic matter (kg/ha)
!     tmp - total active mineral P from all layers (kg/ha)
!     top - total stable mineral P from all layers (kg/ha)
!     tp - total P from all humus (kg/ha)
!     tno3 - total no3_n (kg/ha)
!     trmn - total mineralized N from fresh residue (kg/ha)
!     trmp - total mineralized N from fresh residue (kg/ha)
!     thmn - total mineralized N from humus (kg/ha)
!     trmp - total mineralized P from humus (kg/ha)
!     trsd - sum of residue of all layers (t/ha)
!     tsfn - sum of N leached from all layers (kg/ha)
!     twmn - total organic N from active humus pool (kg/ha)
!     twn - total organic N from humus (kg/ha)
!     wim - immobilized n (kg/ha)
!     wip - immobilized p 9kg/ha)
!     wmn - active humus n pool (kg/ha)
!     wmp - active humus p pool (kg/ha)
!     wn - organic N concentration of humus (g/t)
!     wp - organic P concentration of humus (g/t)
!     yc - period of cultivation before simualtion starts (yr)
