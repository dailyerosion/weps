!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

      subroutine nuse (bn1, bn2, bn3, bp1, bp2, bp4)

!     + + + PURPOSE + + +
!     This subroutine calculates the daily soil supply of N & P from each
!     soil layer in which there are roots.

!     + + + ARGUMENT DECLARATIONS + + +
      real bn1, bn2, bn3, bp1, bp2, bp4

!     + + + COMMON BLOCKS + + +

       include 'p1werm.inc'

! local includes
      include 'crop/cgrow.inc'
      include 'crop/csoil.inc'
      include 'crop/cfert.inc'
      include 'crop/cparm.inc'
      include 'crop/cenvr.inc'
      include 'crop/chumus.inc'
      include 'crop/p1crop.inc'

!     + + + LOCAL VARIABLES + + +
      real vt, xx, clp, flu, xy
      integer j, k
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     Note:variable names in brackets are the names used in the EPIC manual
!     cnt(cnb) - optimal plant N concentration(kg/t) on day i
!     bn1,bn2,... - crop parameters for plant N concentration equation
!     hui         - heat unit index(0-1) on day i
!     un2(undef.) - optimal crop N concentration (kg/ha) on day i
!     un1(undef.) - sum of soil supplied plus fixed N (kg/ha) on day i ??
!     uno3(und)   - N demand rate for crop (kg/ha/d)
!     dm(b)       - accumulated plant biomass(tops+roots) (t/ha)
!     ddm(undef.) - daily plant biomass accumulation (t/ha/d)
!     cpt(cpb)    - optimal plant P concentration(kg/t) on day i
!     bp          - crop parameters for plant P concentration equation
!     hui         - heat unit index(0-1) on day i
!     up2(undef.) - optimal crop P concentration (kg/ha) on day i
!     up1(undef.) - actual crop P concentration  (kg/ha) on day i
!     upp(UPD)    - P demand rate for crop (kg/ha/d)
!     dm(b)       - accumulated plant biomass(tops+roots) (t/ha)
!     upp(upd)  = P demand rate (kg/ha/d)
!     rw(rwt)   = total root weight upto day i (t/ha)
!     ir        = deepest layer number to which roots have extended
!     un        = rate of N supplied by the soil from layer J (kg/ha/d)
!     wno3      = amount of N in a layer (kg/ha)
!     u         = water uptake from a layer by evpt (mm)
!     st(sw)    = soil water content of a layer (mm)
!     sunn(uns)  = sum of N supplied from all rooted layers (kg/ha)
!     clp(clp)  = P concentration in a layer(g/ton)
!     flu(lfu)  = labile P factor for crop uptake(0-1)
!     up        = soil supply of P from a layer (kg/ha)
!     sup(ups)  = soil supply of P from all rooted layers (kg/ha)
!     ap        = amount of labile P in a layer (kg/ha)
!     xy        =

!     + + + OUTPUT FORMATS + + +
!2000 format(1x,i3,1x,9(f7.3,1x))

!     + + + END OF SPECIFICATIONS + + +
!     This section is the EPIC subroutine NUP
!      dimension fr(10)
      un1=un2
!      sunn=0.
      sup=0.
!     calculate optimal N concentration for a crop using a modified version of
!     eq. 2.215 in the next 2 lines.
      cnt=bn1+bn2*exp(-bn3*hui)
      un2=cnt*dm*1000.
!      if (un2.lt.un1) un2=un1
!     allow positive N demand late in the season ?
!      uno3=amin1(4000.*.0023*ddm,un2-un1)
      uno3=un2-un1
      if (uno3.le.0.) uno3=0.
      vt=uno3
!     This section is the EPIC subroutine NPUP
!     Calculate P concentration for a crop using a modified form of eq.2.229.
      cpt=bp2+bp1*exp(-bp4*hui)
      up2=cpt*dm*1000.
      if (up2.lt.up1) up2=up1
      upp=up2-up1
!
!     This section is the EPIC subroutine NUSE
!     calculate parts of eq. 2.231 --- P demand rate
      if (rw.eq.0.) goto 45
      xx=1.5*upp/rw
!     loop for computing soil supply of N and P from each layer
      do 4 j=1,ir
!        compute soil supply of N in the next 2 lines
!        next line commented out unitl water use data is available
!        un(j)=wno3(j)*u(j)/(st(j)+.001)
!         un(j)=wno3(j)*0.05
!         sunn=sunn+un(j)
!        above 3 lines replaced by the following 5 lines
!         un(j)=uno3*rwt(j)/rw
          if (vt.le.0.) goto 75
          if (wno3(j).eq.0.) goto 75
          xy=wno3(j)-vt
          if (xy.ge.0.) then
             wno3(j)=wno3(j)-vt
             sunn=sunn+vt
             vt=0.
           endif
           if (xy.lt.0.) then
             sunn=sunn+wno3(j)
             vt=vt-wno3(j)
             wno3(j)=0.
           endif
!         xy=wno3(j)-un(j)
!         if (xy.le.0.) un(j)=wno3(j)
!         sunn=sunn+un(j)
!         wno3(j)=wno3(j)-un(j)
!        compute soil supply of P in the next 8 lines
!        F in the next line replaced by CLP
  75  continue
         clp=1000.*ap(j)/wt(j)
!        F in the next and subsequent lines replaced by FLU --- eq 2.232
!        *********** re-arranged to avoid the goto statement **************
         flu=clp/(clp+exp(a_s11-b_s11*clp))
         if (clp.gt.30.) flu=1.
!!       use equation 2.231
!         rwt(j) = ???  if this is root mass by layer - change to bcmbgr(j)
!         bcmbgr should also be passed as an argument
         up(j)=xx*flu*rwt(j)
         if (up(j).ge.ap(j)) up(j)=.9*ap(j)
         sup=sup+up(j)
    4 continue
!     the following algorithms may be temporary depending whether subroutine
!     najn & najp are eliminated.
!     adjust soil supply and plant demand for N
!      sum=0.
!      rt=uno3/(sunn+1.e-20)
!      if (rt.lt.1.) then
!         do 2 k=1,ir
!            un(k)=un(k)*rt
!            sum=sum+un(k)
!   2     continue
!        sunn=sum
!      endif
!c     adjust soil supply and plant demand for P
!      sum=0.
!      rt=upp/(sup+1.e-20)
!      if (rt.lt.1.) then
!         do 3 k=1,ir
!            up(k)=up(k)*rt
!            sum=sum+up(k)
!   3     continue
!        sup=sum
!      endif
!     acummulate plant uptake of N and P
!      un1=un1+sunn
      up1=up1+sup
!     a new variable un3 added for debugging purposes
      suno3=suno3+uno3
!     update remaining no3 and labile p in the soil and ouput for debugging
      do 8 k=1,ir
!         wno3(k)=wno3(k)-un(k)
         ap(k)=ap(k)-un(k)
!        write(310,2133) jd,k,ap(k),rwt(k),wno3(k),un(k),up(k)
!2133 format (i3,1x,i3,1x,5(f8.4,1x))
   8  continue
      tno3=tno3-sunn+rmnr
      tap=tap-sup+wmp
      j=j-1
!     write(39,2000)jd,cnt,un1,un2,uno3,suno3,sunn,vt,dm,ddm
  45  continue
      return
      end
