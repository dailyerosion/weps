!$Author$
!$Date$
!$Revision$
!$HeadURL$

!    file: csoil.inc

!     This file contains the declaration of the common block of variables
!     related to initial values of soil N&P, rootzone parameters, etc.
!     note: cn,cp are new variables added. They replace the variables
!     wno3,ap at the initial stage(units of t/g). Variables wno3 and ap are now
!     reserved for values of total no3_n and labile P (units of kg/ha).

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real cnt,cpt,cdg,cn,cp,fc,gsw
      real pdsw,po,psp,rq,rz,st,suno3,sut,sws,t,wt,za

      integer ids

      common / csoil /                                                  &
     &                cnt, cpt, cdg,                                    &
     &                cn(mnsz), cp(mnsz), fc(mnsz),                     &
     &                gsw, ids, pdsw, po,                               &
     &                psp(mnsz), rq, rz, st(mnsz),                      &
     &                suno3, sut, sws, t(mnsz),                         &
     &                wt(mnsz), za(mnsz)

      save :: /csoil/

!     + + + VARIABLE DEFINITIONS + + +
!     alt - idex of crop toleramce to aluminum saturation (1-5;1=sensitive,
!           5=tolerant)
!     ino3 - amount of no3_n applied (kg/ha)
!     cdg - soil temperature factor for nutrient cycling
!     cec - cation exchange capacity (cmol/kg) --- blank if unknown
!     cn - concentration of soil test N (g/t) --- blank if unknown
!     cp - concentration of soil test N (g/t) --- blank if unknown
!     fc - field capacity water content of layer (mm)
!     gsw - soil water below which germination will not occur (m)
!     ids - soil weathering code
!     pdsw - plant available water at the layer where seed is placed (m)
!     po - soil porosity (mm)
!     psp - fraction of fertilizer P that is labile
!     rq - no3_n lost by pecolation,runoff,lateral flow from top layer (kg/ha)
!     rz - maximum depth of root zone (cm)
!     smb - sum of bases (cmol/kg) --- blank if unknown
!     st - soil water content of layer (mm)
!     sut - soil moisture factor
!     sws - soil water stress factor
!     t - soil temperature at each soil layer (C)
!     wt - weight of soil in a layer (t/ha-mm)
!     za - depth to the middle of a layer (m)
