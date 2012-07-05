!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!     file: nmnim.for
!
      subroutine nmnim (k)
!
!     + + + PURPOSE + + +
!     This subroutine estimates daily N and P mineralization and immobilization
!     considering fresh organic material (crop residue) and active and stable
!     humus material. Goto statements in the original code were replaced by if-
!     then-else statements as required by WEPP coding convention.
!
!     + + + KEYWORDS + + +
!     mineralization
!
!     + + + COMMON BLOCKS + + +

       include 'p1werm.inc'

! local includes
      include 'crop/chumus.inc'
      include 'crop/cenvr.inc'
      include 'crop/cfert.inc'
      include 'crop/csoil.inc'

!     + + + LOCAL VARIABLES + + +
      real tkg,cs,rwn,xx,hmp,r4,cnr,cpr,cnrf,cprf,ca,decr,rmn,rmp,rm2
      real rdc

      integer k

!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     ca (cnp) - takes the value of 1.0,cnrf or cprf whichever is the smallest
!     cmn (cmn) - humus rate constant(1/d) - 2.160
!     cnr (cnr) - C:N ratio in a soil layer -  2.155
!     cnrf (cnp) - C:N ratio factor -  2.154
!     cpr (cpr) - C:P ratio in a soil layer -  2.156
!     cprf (cnp) - C:P ratio factor -  2.154
!     cs - temp and soil moisture factor of - 2.153
!     decr (dcr) - decay rate constant for fresh organic matter -  2.153
!     hmp (hmp) - amount of P mineralized from humus - kg/ha/d
!     r4 - numerator of - 2.155
!     rc (rc) - residue decomposition factor (.8,.05,.0095)
!     rdc - amount of decayed residue from fresh residue - kg/ha
!     rm2 - 20% of N mineralized from fersh residue - kg/ha/day
!     rmn (rmn) - amount of N mineralized from fresh residue - kg/ha/d - 2.152
!     rmp - amount of P mineralized from fresh residue - kg/ha/d - 2.152
!     rwn (ron) - flow rate between active and stable humus N pools - kg/ha/d
!     tkg - residue - kg/ha
!     xx - sum of active and stable N pools
!
!
!     + + + OUTPUT FORMATS + + +
 2000 format(1x,2(i3,1x),11(f7.3,1x))
!
!     + + + END OF SPECIFICATIONS + + +
!
!     convert residue to kg/ha.
      cmn=0.0003
      tkg=rsd(k)*1000.
!     calculate parts of eq. 2.153
      cs=sqrt(cdg*sut)
!     This section of code calculates amount of N&P mineralized from humus.
!     calculate N that becomes part of the stable N pool using eq. 2.159.
!     This eq. is not the same as in the manual--->(-1.0) added.
      rwn=.1e-4*(wmn(k)*(1./rtn(k)-1.)-wn(k))
!     Add RWN to the stable organic N pool (WN(K)).
      wn(k)=wn(k)+rwn
      wim=0.
      wip=0.
!     Calculate amount of N mineralized from the active N pool. This eq is
!     not the same as in the manual--->BD*BD left out.
!     next line replaced by the line following it
!     HMN=CMN*CS*WMN(K)/(BDP(K)*BDP(K))
      hmn=cmn*cs*wmn(k)
!     Calculate mineralized P (HMP) in the following 2 lines.
      xx=wn(k)+wmn(k)
      hmp=1.4*hmn*wp(k)/xx

!     calculate N&P mineralization from humus when there is not enough residue
!      IF (TKG.le.1.) then
!        calculate remaining amount of humus
!         HUM(K)=HUM(K)*(1.-HMN/XX)
!        subtract N flow to stable pool(RWN) and humus mineralized N(HMN) from
!        the active N pool (WMN)
!         WMN(K)=WMN(K)-HMN-RWN
!        subtract humus P from stable organic pool
!         WP(K)=WP(K)-HMP
!         RMNR=HMN
!        add humus N to the NO3_N supply
!         WNO3(K)=WNO3(K)+RMNR
!        add humus P to the labile P supply
!         AP(K)=AP(K)+HMP
!         WMP=HMP
!      else
!        calculate N&P mineral./immobil. from fresh residue and humus material
!        this section of code calculates N & P mineralization from fresh OM
!        numerator of eq 2.155
         r4=.58*tkg
!        calculate C:N ratio
         cnr=r4/(fon(k)+wno3(k))
!        calculate C:P ratio
         cpr=r4/(fop(k)+ap(k))
!        calculate CNP (C:N and C:P ratio factor)--eq 2.154
         cnrf=1.
         if(cnr.gt.25.) cnrf=exp(-.693*(cnr-25.)/25.)
         cprf=1.
         if(cpr.gt.200.) cprf=exp(-.693*(cpr-200.)/200.)
         ca=amin1(cnrf,cprf)
!        calculate the decay rate constant using eq. 2.153
!        RC=0.05 is it constant ? when does it become 0.8 and 0.0095 ?
!        new code added to determine residue composition factor
!         if (rsdi(k).le.0.) rsdi(k)=1.
!         rfom=rsd(k)/rsdi(k)
!         if (rfom.ge.0.8) rc=0.8
!         if (rfom.lt.0.8) rc=0.05
!         if (rfom.lt.0.1) rc=0.0095
          rc=0.05
!        end of new code additions
         decr=rc*ca*cs
!        calculate N mineralization rate using eq. 2.152
         rmn=decr*fon(k)
!        calculate P mineralization rate using eq. 2.152
         rmp=decr*fop(k)
!        calculate 20% of fresh OM N
         rm2=.2*rmn
!        calculate amount of remaining humus
         hum(k)=hum(k)*(1.+(rm2-hmn)/xx)
!        update amount of active humus N pool
         wmn(k)=wmn(k)+rm2-hmn-rwn
!        update amount of stable organic N pool
         wp(k)=wp(k)-hmp+.2*rmp
!        calulate amount of decayed residue
         rdc=decr*tkg
!        update amount of residue and convert to t/ha
         rsd(k)=.001*(tkg-rdc)
!        calculate net minerlized N
         rmnr=.8*rmn+hmn
!        calculate net mineralized P
         wmp=.8*rmp+hmp

!        WIM=AMAX1(.0232*RDC-RMN,0.)
!        WIM=AMIN1(RMNR+WNO3(K),WIM)
!        WIP=AMAX1(.0029*RDC-RMP,0.)
!        WIP=AMIN1(WMP+AP(K),WIP)

!        add immobilized P and subtract mineralized P to fresh organic P pool
         fop(k)=fop(k)+wip-rmp
!        add immobilized N and subtract mineralized N to fresh organic N pool
         fon(k)=fon(k)+wim-rmn
!        update total NO3_N in soil layer
         wno3(k)=wno3(k)-wim+rmnr
!        update total labile P in soil layer
         ap(k)=ap(k)-wip+wmp
!        keep running totals of mineralized N & P from fresh residue(rmn*.8,
!        rmp*.8) and humus(hmn,hmp)
         trmn=trmn+.8*rmn
         trmp=trmp+.8*rmp
         thmn=thmn+hmn
         thmp=thmp+hmp
!      endif

!     write(37,2000)jd,k,wno3(k),ap(k),fon(k),rmn,rmp,hmn,hmp,trmn,trmp,
!    1thmn,thmp
!     write(312,2001)jd,k,cnr,cpr,cnrf,cprf,ca,decr,cs,cdg,sut
!     write(313,2002)jd,k,hum(k),wmn(k),wn(k),wp(k),rwn,rmnr,wmp
!2001 format (1x,2(i3,1x),9(f8.3,1x))
!2002 format (1x,2(i3,1x),7(f10.5,1x))
      return
      end
