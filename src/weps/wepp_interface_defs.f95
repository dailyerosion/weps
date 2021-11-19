!$Author$
!$Date$
!$Revision$
!$HeadURL$

!   Add this definition file in every source file to insure that the compiler can
!   verify subroutine and function signatures.
  
!   Can't seem to use constants in interface block? This effects two dimensional arrays where the
!   first dimension must be specified 
       
       MODULE wepp_interface_defs
          
       interface
!---------------- WEPP Routines ----------------------------
      real function cross(x1,y1,x2,y2)
      real, intent(in) :: x1, y1, x2, y2              
      end function cross
!-----------------------
      real function depc(xu,a,b,phi,theta,du,ktrato,qostar)
      real, intent(in) ::  xu, a, b, phi, theta, du, ktrato, qostar  
      end function depc
!-----------------------
      real function depend(xu,xl,a,b,cdep,phi,theta,ktrato,qostar)
      real, intent(in) :: xu, xl, a, b, cdep, phi, theta, ktrato, qostar          
      end function depend
!------------------------
      subroutine depeqs(xu,cdep,a,b,phi,theta,x,depeq,ktrato,qostar)
      real, intent(in) :: xu, cdep, a, b, phi, theta,ktrato,qostar
      real, intent(inout) :: x
      real, intent(out) :: depeq      
      end subroutine depeqs
!-------------------------
      subroutine depos(xb,xe,cdep,a,b,c,phi,theta,ilast,dl,ldlast,      &
     &    xinput,ktrato,detach,load,tc,qostar)
      real, intent(in) :: xb, cdep, phi, theta, ktrato, qostar
      real, intent(in) :: a, b, c
      real, intent(inout) :: xe, xinput(101), load(101)
      real, intent(out) :: dl, ldlast, detach(101)
      real, intent(out) :: tc(101)
      integer, intent(inout) :: ilast
      end subroutine depos
!--------------------------
      subroutine enrich(kk,xtop,xbot,xdetst,ldtop,ldbot,lddend,theta,   &
     &    iendfg,slplen,ktrato,qin,qout,qostar,ainftc,binftc,cinftc,    &
     &    npart,frac,fall,frcly,frslt,frsnd,frorg,sand,silt,clay,orgmat,&
     &    fidel,tcf1,frcflw,enrato)

      integer, intent(in) :: kk, iendfg, npart
      real, intent(in) :: xtop, xbot, xdetst, theta, ldtop,ldbot,lddend,&
     &     slplen, ktrato, qin, qout, qostar, ainftc(*),                & 
     &     binftc(*), cinftc(*), frac(*), fall(*),                      &
     &     frcly(*), frslt(*), frsnd(*), frorg(*),                      &
     &     fidel(*), tcf1(*),                                           &
     &     sand(*), silt(*), clay(*), orgmat(*)  
      real, intent(inout) :: frcflw(*)    
      real, intent(out) ::  enrato 
      end subroutine enrich
!--------------------------
      subroutine enrprt(jun,npart,frac,frcflw,dia,spg,frsnd,            &
     &    frslt,frcly,frorg,enrato)
      
      integer, intent(in) :: jun, npart
      real, intent(in) ::  frac(*), frcflw(*), dia(*),                  & 
     &      spg(*),                                                     &
     &     frsnd(*), frslt(*), frcly(*), frorg(*),                      &
     &     enrato   
      end subroutine enrprt
!---------------------------
      subroutine eprint(slplen,avgslp,runoff,peakro,effdrn,efflen,      &
     &    effint,effdrr)
       real, intent(in) :: slplen, avgslp, runoff, peakro, effdrn,       &
     &     efflen, effint, effdrr 
      end subroutine eprint
!---------------------------
      subroutine erod(xb,xe,a,b,c,atc,btc,ctc,eata,tauc,theta,phi,ilast,&
     &    dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,detach,qostar)
 
      real, intent(in) :: xb, xe, a, b, c, eata, tauc, theta
      real, intent(inout) :: xdbeg
      real, intent(in) ::  atc, btc, ctc, phi, qostar
      real, intent(in) :: xinput(101), ktrato
      real, intent(inout) :: detach(101)
      integer, intent(inout) ::  ilast
      integer, intent(out) :: ndep
      real, intent(inout) :: ldlast, tc(101), load(101), dl 
      end subroutine erod
!---------------------------
      real function falvel(spg,dia)
      real, intent(in) :: spg, dia
      end function falvel
!---------------------------
      subroutine getfromweps(sr, sand,silt,clay,orgmat, &
       thetdr,rrc,dg,st,thdp,frdp,thetfc,por,rh, &
       frctrl, frcsol, precip, soil)
      use soil_data_struct_defs, only: soil_def
      integer, intent(in) :: sr
      real, intent(out):: sand(*), silt(*), clay(*)
      real, intent(out):: orgmat(*)
      real, intent(out):: thetdr(*), rrc
      real, intent(out):: dg(*), st(*), thdp, frdp
      real, intent(out):: thetfc(*), por(*), rh
      real, intent(out):: frctrl, frcsol
      real, intent(out):: precip
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      end subroutine getfromweps
!---------------------------------
      SUBROUTINE init_wepp(isr, afterWarmup, soil)
      use soil_data_struct_defs, only: soil_def
      integer, intent(in) :: isr, afterWarmup
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      end subroutine init_wepp
!---------------------------------
      subroutine param(qin,qout,qostar,qshear,qsout,a,b,avgslp,         &
     &    width,rspace,ktrato,shrsol,tcend,frcsol,frctrl,rrc,npart,frac,&
     &    dia,spg,fall,runoff,effdrn,effint,effdrr,strldn,tcf1,fidel,    &
     &    eata ,tauc,theta,phi,slpend,ainf,binf,cinf,ainftc,binftc,     &
     &    cinftc,sand,slplen,kiadj,kradj,shcrtadj,nslpts,efflen,        &
     &    anflst, bnflst, cnflst, atclst, btclst, ctclst,slpprv, wdhtop,&
     &    rwflag)
  
      real, intent(out) :: ktrato, shrsol, tcend, strldn, eata, tauc
        real, intent(out) :: phi, tcf1(*), theta, fidel(*)
      real, intent(out) :: slpend
      real, intent(out) ::  ainftc(*), binftc(*),cinftc(*)

      real, intent(inout) :: width,avgslp

      real, intent(in):: a(*), b(*),qin,qout,qostar
      real, intent(in):: qsout, qshear, rspace, frcsol, frctrl, rrc           
      real, intent(in):: frac(*), dia(*),spg(*)
      real, intent(in):: fall(*), runoff,effdrn, effint, effdrr
      real, intent(in):: sand(*), slplen, kiadj, kradj, shcrtadj
      real, intent(in) ::efflen

      real, intent(inout):: ainf(*),binf(*),cinf(*)
  
      integer, intent(in):: npart, nslpts,rwflag  
      
      real, intent(inout) :: anflst, bnflst, cnflst, atclst, btclst
      real, intent(inout) :: ctclst, slpprv, wdhtop
      
      end subroutine param
!------------------------------
      subroutine print(slplen,avgslp,runoff,peakro,effdrn,efflen,       &
     &    effint,effdrr)

      real, intent(in) :: slplen, avgslp, runoff, peakro, effdrn,       &
     &     efflen, effint, effdrr 
      end subroutine print
!------------------------------
      SUBROUTINE PRINT_BUG(DT, NS,RECUM, T, S, SI, SLEN,ALPHA, M,       &
     &    DUREXR, A1, A2, TSTAR)                                   

      real, intent(inout) :: T(*), S(*), SI(*)
      integer, intent(in) :: NS
      real, intent(in) :: RECUM(*), ALPHA, M, DUREXR, A1, A2
      real, intent(in) :: TSTAR, DT, SLEN  
      end subroutine print_bug
!-------------------------------
      subroutine profil(a,b,avgslp,nslpts,slplen,xinput,slpinp,xu,xl,   &  
     & y,x,totlen)
  
      real, intent(out) :: a(*), b(*), avgslp, xu(*)
      real, intent(out) :: xl(*), y(*), x(*), totlen
      real, intent(in) :: slplen, xinput(*), slpinp(*)
      integer, intent(in) :: nslpts 
      end subroutine profil
!-------------------------------
      subroutine prtcmp(npart,spg,dia,frac,frcly,frslt,frsnd,frorg,     &
     & sand1,silt1,clay1,orgmat1)
      
      real, intent(out) :: spg(10), dia(10), frcly(10),frslt(10),       &
     & frsnd(10),frorg(10), frac(10)
      real, intent(in) :: clay1, sand1, silt1, orgmat1
      integer, intent(in) :: npart 
      end subroutine prtcmp
!--------------------------------
      subroutine root(a,b,c,x1,x2)
     
      real, intent(in) :: a, b
      double precision, intent(in) :: c
      double precision, intent(out) :: x1, x2
      end subroutine root
!---------------------------------
      subroutine route(qin,qout,qostar,strldn,ktrato,ainf,binf,         &
     &    cinf,ainftc,binftc,cinftc,npart,frac,frcly,frslt,frsnd,frorg, &
     &    fall,frcflw,nslpts,xinput,xu,xl,load,enrato,tcf1,fidel,sand,  &
     &    silt,clay,orgmat,eata,tauc,theta,phi,slplen)

      real, intent(in):: qin, qout, qostar,strldn,ktrato
      real, intent(in):: ainf(*), binf(*), cinf(*)
      real, intent(in) :: ainftc(*), binftc(*), cinftc(*)
      real, intent(in) :: frac(*), frcly(*), frslt(*)
      real, intent(in) :: frsnd(*)
      real, intent(in) :: frorg(*), fall(*)
      real, intent(in) :: fidel(*)
      real, intent(inout) :: xinput(101)
      real, intent(out) :: enrato
      real, intent(in) :: sand(*), silt(*), clay(*)
      real, intent(in) :: orgmat(*)
      real, intent(in) :: eata, tauc, theta, phi, slplen, tcf1(*)
      real, intent(out) ::  load(101), frcflw(*)
      real, intent(inout) :: xu(*), xl(*)
      integer, intent(in) :: npart, nslpts 
      end subroutine route
!----------------------------------
      subroutine runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,x,ldold,    &
     &    ldnew,xx,eatax,taucx,shr,dcap,ktrato)
    
      real, intent(in) :: atc, btc, ctc, a, b, c, ktrato
      real, intent(in) :: eata, tauc, theta, dx, ldold, x
      real, intent(out) :: dcap, ldnew 
      real, intent(inout) ::  xx, eatax, taucx, shr 
      end subroutine runge
!-----------------------------------
      real function sedia(spg,eqfall)
      
      real, intent(in) :: spg,eqfall                                                   
      end function sedia
!----------------------------------
      subroutine sedist(dslost,dstot,stdist,delxx,slplen,avgslp,        &
     &    y,ysdist)
     
      real, intent(in) :: slplen, avgslp, y(101), dslost(100)
      real, intent(out) :: ysdist(1000),dstot(1000),stdist(1000),delxx  
      end subroutine sedist
!----------------------------------
      subroutine sedmax(jnum,amax,amin,ptmax,ptmin,dstot,stdist,ibegin, &
     &    iend,jflag,lseg)
      
      integer, intent(in) :: jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) ::  amax(100), amin(100), ptmax(100), ptmin(100)
      real, intent(in) :: dstot(1000), stdist(1000)
      end subroutine sedmax
!----------------------------------
      subroutine sedout(sumfile,irdgdx,dslost,avsole,                   &
     &    npart,frac,                                                   &
     &    dia,spg,slplen,fwidth,avgslp,                                 &
     &    y,totlen,years)
     
      integer, intent(in) :: npart, sumfile, years
      real, intent(in) :: irdgdx, dslost(100), avsole,                  &
     & frac(*),                                                         &
     &     dia(*), spg(*),                                              &
     &     slplen,                                                      &
     &     fwidth, avgslp
      real, intent(in) :: y(101), totlen
      end subroutine sedout
!---------------------------------
      subroutine sedseg(dslost,jun,iyear,noout,dstot,stdist,irdgdx,     &
     &    ysdist,avgslp,slplen,y,avedet,maxdet,ptdet,avedep,maxdep,     &
     &    ptdep,                                                        &
     &    detpt1, detpt2, dtavls, detstd, detmax, pdtmax, detmin,       &
     &    pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,       &
     &    depmin, pdpmin,ndetach,ndepos)  
 
!
      integer, intent(in) :: jun, iyear, noout
      real, intent(in) :: dslost(100)                                 
      real, intent(in) :: irdgdx, avgslp, slplen, y(101)
      real, intent(out) :: dstot(1000), stdist(1000), ysdist(1000)
      real, intent(out) :: avedet,maxdet,ptdet
      real, intent(out) :: avedep,maxdep,ptdep
      real, intent(out) :: detpt1(100), detpt2(100), dtavls(100)
      real, intent(out) :: detstd(100), detmax(100), pdtmax(100)
      real, intent(out) :: detmin(100), deppt1(100), deppt2(100)
      real, intent(out) :: dpavls(100), depstd(100), depmax(100)
      real, intent(out) :: pdpmax(100), depmin(100), pdpmin(100)
      real, intent(out) :: pdtmin(100)
      integer, intent(out) :: ndetach, ndepos                        
      end subroutine sedseg
!--------------------------------
      subroutine sedsta(jnum,dloss,dsstd,vmax,pmax,vmin,pmin,ibegin,    &
     &    iend,jflag,lseg,dstot,stdist,delxx)
      
      integer, intent(in) ::  jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) :: pmax(100), pmin(100)
      real, intent(out) :: vmax(100), vmin(100)
      real, intent(in) :: dstot(1000), stdist(1000), delxx
      real, intent(out) :: dloss(100), dsstd(100)  
      end subroutine sedsta
!--------------------------------
      real function shear(a,b,c,x)
      
      real, intent(in) :: a, b, c, x    
      end function shear
!--------------------------------
      subroutine shears(q,sslope,rspace,width,frcsol,frctrl,shearq,     &
     &  rwflag)
      
      real, intent(inout) :: width,sslope
      real, intent(in) :: q, rspace, frcsol, frctrl
      integer, intent(in) :: rwflag
      real, intent(out) :: shearq      
      end subroutine shears
!---------------------------------
      subroutine sheart(q,sslope,rspace,wdhtop,frcsol,frctrl,shearq)
      
      real, intent(inout) :: q,sslope,wdhtop
      real, intent(in) :: rspace,frcsol,frctrl
      real, intent(out) :: shearq
      end subroutine sheart
      
      
!---------------------------------      
      real function shield(reyn)
      
      real, intent(in) :: reyn
      end function shield
!--------------------------------
      subroutine sloss(load,tcend,width,rspace,effdrn,theta,            &
     &    slplen,irdgdx,qsout,dslost,dsmon,dsyear,dsavg,avsole,qout,    &
     &    frcflw,npart,enrato)
  
      real, intent(in) :: load(101), tcend, width, rspace, effdrn
      real, intent(in) :: theta, slplen, frcflw(*)
      real, intent(in) :: qout,enrato
      real, intent(out) :: dslost(100)
      real, intent(out) :: avsole, irdgdx, qsout
      real, intent(inout) :: dsmon(100), dsyear(100), dsavg(100)
      integer, intent(in) :: npart 
      end subroutine sloss
!-----------------------------
      subroutine soil_adj(ki,kr,shcrit,kiadj,kradj,shcrtadj,            &
     & rrc, canhgt,cancov,inrcov,rtm15,rtm,bconsd,daydis,rh,rspace,     &
     & avgslp,smrm,krcrat,tccrat,kicrat,dg,thetdr,st,thdp,frdp,ifrost,  &
     & thetfc,por,tens,cycle)
       real, intent(in):: canhgt,cancov,inrcov,rtm15,rtm
       real, intent(in):: bconsd,rh,rspace,avgslp
       real, intent(in):: smrm,krcrat,tccrat,rrc,kicrat
       real, intent(in):: dg(10), thetdr(10), st(10),thdp,frdp
       real, intent(in):: thetfc(10), por(10)
       integer, intent(in):: cycle, daydis
       integer, intent(inout):: ifrost
       real, intent(out):: tens, kiadj, kradj, shcrtadj
       real, intent(in):: ki, kr, shcrit
       end subroutine soil_adj
!--------------------------------
      subroutine trcoeff(trcoef,shrsol,sand,dia,spg,tcf1,npart,frac)
     
      real, intent(in) :: sand(*), dia(*), spg(*),        &
     &   frac(*),shrsol
      integer, intent(in) ::  npart
      real, intent(out) :: trcoef
      real, intent(inout):: tcf1(*)
      end subroutine trcoeff
!----------------------------------
      subroutine undflo(factor,expon)
   
      real, intent(inout) :: factor, expon
      end subroutine undflo
!----------------------------------
      subroutine write_main_event(sumfile,cd, cm, cy, precp,            &
     &        runoff,                                                   &
     &      irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,      &
     &      enrato,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,      &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     &     depmin, pdpmin,ndetach,ndepos, npart,frac,frcflw, dia,       &
     &     spg,frsnd,frslt, frcly,frorg,                                &
     &     slplen,fwidth,avgslp,stdist,dslost,years,avgannual)
  
      integer, intent(in) :: sumfile,cd,cm,cy,ndetach,ndepos,npart
      real, intent(in) :: precp,runoff,avedet,maxdet,ptdet,avedep
      real, intent(in) :: maxdep,ptdep,avsole,enrato,irdgdx
      real, intent(in) :: detpt1(*), detpt2(*), dtavls(*)
      real, intent(in) :: detstd(*), detmax(*), pdtmax(*)
      real, intent(in) :: detmin(*), deppt1(*), deppt2(*)
      real, intent(in) :: dpavls(*), depstd(*), depmax(*)
      real, intent(in) :: pdpmax(*), depmin(*), pdpmin(*)
      real, intent(in) :: pdtmin(*)
      real, intent(in) :: frac(*),frcflw(*),dia(*), spg(*)
      real, intent(in) :: frcly(*), frslt(*), frsnd(*), frorg(*)
      real, intent(in) :: slplen,fwidth,avgslp
      real, intent(in) :: stdist(*),dslost(*)
      integer, intent(in) :: years
      integer, intent(in) :: avgannual
      
      end subroutine write_main_event
      
!-----------------------------------
      subroutine  write_event(luowepperod,cd, cm, cy, precp,runoff,     &
     &     irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,       &
     &     enrato)
      
      integer, intent(in) :: luowepperod,cd,cm,cy
      real, intent(in) :: precp,runoff,irdgdx,avedet,maxdet,ptdet
      real, intent(in) :: avedep,maxdep,ptdep,avsole,enrato
      end subroutine write_event
!-----------------------------------
      subroutine write_hydro_summary(sumfile,totalPrecip,precipEvents,  &
     & totalRunoff,runoffEvents,totalSnowrunoff, snowmeltEvents,years)
      
      
      integer, intent(in) :: sumfile
      real, intent(in) :: totalPrecip,totalRunoff, totalSnowrunoff
      integer, intent(in):: precipEvents, runoffEvents,snowmeltEvents
      integer, intent(in) :: years
     
      end subroutine write_hydro_summary
!---------------------------------------      
      subroutine xcrit(a,b,c,tauc,xb,xe,xc1,xc2,mshear)
     
      real, intent(in) :: a, b, c, tauc, xb, xe
      integer, intent(out) :: mshear
      real, intent(out) :: xc1,xc2
      end subroutine xcrit
!-----------------------------------
      subroutine xinflo(xinput,efflen,slplen,a,b,qin,qout,peakro,       &
     &    qostar,ainf,binf,cinf,ainftc,binftc,cinftc,qshear,rspace,     &
     &    nslpts)
     
      real, intent(inout) :: qout
      real, intent(out) :: xinput(*), qin, qostar, qshear
      real, intent(in) :: peakro, efflen, slplen, a(*)
      real, intent(in) :: b(*), rspace
      real, intent(out) :: ainf(*), binf(*), cinf(*)
      real, intent(out) :: ainftc(*), binftc(*), cinftc(*)
      integer, intent(in) :: nslpts
      end subroutine xinflo
!-----------------------------------
      subroutine yalin(effsh,tottc,sand,dia,spg,tcf1,npart,frac)
      real, intent(in) :: effsh
      real, intent(in) :: dia(*), spg(*), sand(*)
      real, intent(in) :: frac(*)      
      integer, intent(in) ::  npart
      real, intent(out) :: tottc, tcf1(*) 
      end subroutine yalin
!------------------------------------   
!------------- WEPP Hydro Routines ---------------------
      SUBROUTINE BGNRND(X0, X, A, MRND)
      
      REAL, intent(in) :: X0
      real, intent(inout) :: X
      real, intent(out) :: A, MRND                            
      end subroutine bgnrnd
!-----------------------
      SUBROUTINE HDEPTH(T2, X, A1, A2, TSTAR, T, S, SI, NS, II, M,      & 
     &                 HDPTHO, A, MRND)


      INTEGER II, NS
      REAL SI(*),TSTAR, M, A, MRND
      real, intent(in) :: X, A1, A2, S(*), T(*)
      DOUBLE PRECISION, intent(in) :: T2
      real, intent(out) ::hdptho  
      end subroutine hdepth
!-------------------------
      SUBROUTINE HDRIVE(NQT, DURPQ, QTP, TPEE,                          &
     &                  DT, NS, QTOT, Q, TQ1,                           &
     &                  RECUM, T, S, SI, SLEN, ALPHA, M, DUREXR, A1,A2, &
     &                  TSTAR, PEAKRO, DURRUN)

      integer, intent(out) :: NQT
      real, intent(out) :: DURPQ, QTP, TPEE, QTOT(*), Q(*)
      real, intent(out) :: TQ1(*), PEAKRO, DURRUN
      real, intent(inout) :: T(*), S(*), SI(*)
      integer, intent(in) :: NS
      real, intent(in) :: RECUM(*), ALPHA, M, DUREXR, A1, A2
      real, intent(in) :: TSTAR, DT, SLEN    
      end subroutine hdrive
!---------------------------------
      SUBROUTINE HDRIVEFLOW(NS,NF,RECUM,SLEN,SLOPE,DUREXR,DT,TF,RE,     &     
     & SC,PEAKRO,DURRUN)

      integer, intent(in) :: NF
      integer, intent(inout) :: NS
      real, intent(in) :: RECUM(*), SLEN, DUREXR, DT, TF(*),            &
     & RE(*), SLOPE, SC
      real, intent(out) :: PEAKRO,DURRUN
      end subroutine hdriveflow
!---------------------------------
      SUBROUTINE PHI_SUB(TIME, TSTAR, SI, NS, II, M, A2, S, T, OPHI)
     
      INTEGER, intent(in) :: NS
      integer, intent(inout) :: II
      real, intent(in) :: TSTAR, SI(*), M, A2, S(*)
      real, intent(in) :: T(*)
      DOUBLE PRECISION, intent(in) :: TIME
      real, intent(out) :: OPHI 
      end subroutine phi_sub
!----------------------------
      SUBROUTINE PSIINV(TIME, X, TSTAR, T, S, SI, OSINT, NS, A2,        & 
     &                  II, M, PSI, DPSI, OPSII, A, MRND)
      
      integer, intent(inout) :: II, NS
      real, intent(in) :: A, MRND, M, A2, TSTAR, T(*)
      real, intent(in) :: S(*), SI(*)
      real, intent(out) :: OSINT
      double precision, intent(inout) :: PSI, DPSI, OPSII
      DOUBLE PRECISION, intent(in) ::  TIME   
      real, intent(inout) :: X  
      end subroutine psiinv
!------------------------------
      SUBROUTINE PSIS(TIME, UU, TSTAR, T, S, SI, OSINT, NS, A2, II,     &
     &                M, PSI, DPSI)
      
      INTEGER, intent(in) :: NS
      integer, intent(inout) :: II
      real, intent(in) :: TSTAR, A2, M, SI(*), S(*)
      real, intent(in) ::  T(*)
      real, intent(out) :: OSINT
      double precision, intent(in) :: TIME, UU
      double precision, intent(out) :: DPSI, PSI
      end subroutine psis
!-------------------------------
      SUBROUTINE RANDM(X, A, MRND, RNUMB)
      
      real, intent(in) :: MRND, A
      real, intent(out) :: RNUMB
      real, intent(inout) :: X 
      end subroutine randm
!------------------------------

      SUBROUTINE RDAT(NF, SI, SC, S, T, NS, ALPHA, M, A1, A2,           &
     &                ACV, HCV, SCV, TSTAR, SLEN, SLOPE)
      
      real, intent(in) :: SLEN, SLOPE, T(*), SC
      real, intent(out) :: ALPHA, M, A1,A2, TSTAR, SI(*)
      real, intent(out) ::  ACV(3), HCV(3), SCV(3)
      real, intent(inout) :: S(*)
      integer, intent(inout) :: NS
      integer, intent(in) :: NF    
      end subroutine rdat
!-------------------------------
      SUBROUTINE SINT(TIME, T, TSTAR, II, S, SI, NS, OSINT)
      DOUBLE PRECISION, intent(in) :: TIME
      real, intent(in) :: TSTAR, T(*), S(*), SI(*)
      integer, intent(inout) :: II
      integer, intent(in) :: NS
      real, intent(out) :: OSINT  
      end subroutine sint
!------------------------------------      
       end interface
       end module
