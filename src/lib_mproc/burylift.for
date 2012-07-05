!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine burylift                                               &
     &              (nlay,dflat,dstand,droot,                           &
     &               dblwgnd,buryf,liftf,fltcoef) 


!     + + + PURPOSE + + +
!     
!     This subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  It deals only with the biomass
!     pools (ie no live crop is involved) 
!
!
!     + + + KEYWORDS + + +
!     bury, lift, biomass manipulation  

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real    buryf,liftf,fltcoef 
      real    dflat(mnbpls),dstand(mnbpls)
      real   dblwgnd(mnbpls,mnsz), droot(mnbpls,mnsz)
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     buryp     - percent of flat material buried
!     dblwgnd   - (decomp) below ground residue / layer and decomp
!                 pool (kg / m^2)
!     dflat     - (decomp) flat residue pools (kg / m^2) 
!     droot     - (decomp) root mass / layer and decomp pool  
!     dstand    - (decomp) standing residue pools (kg/ m^2) 
!     fltcoef   - flattening coefficient of an implement 
!     liftp     - percent of buried material lifted to the surface
!     nlay      - number of soil layers used in the operation(s)
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     
!     mnbpls    - max number of biomass pools 
!     mnsz      - max number of soil layers
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer  lay,i 
      real    bury(mnbpls),liftlay(mnbpls,mnsz),lifttot(mnbpls)  
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     bury      - mass of biomass that is buried
!     i         - biomass pools (1-3) 
!     lay       - number of layers in a specified subregion 
!     liftlay   - buried material lifted to the surface in each layer
!     lifttot   - total buried material lifted to the surface
!
!     + + + END SPECIFICATIONS + + + 
!
!     perform the flatting of standing residue based upon the flatten
!     coefficient (fltcoef) 


!     perform the lifting and burying of biomass simulataneously

      do 110 i=1,mnbpls 
        dflat(i) = dflat(i)+dstand(i)*fltcoef
!     need to use temporary variables when performing the lifting
!     process.  This is done so we do not lift something that has
!     just been buried. 
! 
      bury(i)=dflat(i)*buryf 
          do 100 lay=1,nlay 
             liftlay(i,lay)=dblwgnd(i,lay)*liftf
             lifttot(i)=lifttot(i)+droot(i,lay)*                        &
     &       liftf+liftlay(i,lay) 
100       continue
110   continue
!     Now let's update the 4 pool types using the temporary variables
!     we calculated above.  
        do 201 i=1,mnbpls
          do 200 lay=1,nlay 
            dblwgnd(i,lay) = dblwgnd(i,lay)+bury(i)/                    &
     &                       nlay-liftlay(i,lay) 
            droot(i,lay) = droot(i,lay)*(1.0-liftf)
200       continue
        dflat(i)=dflat(i)-bury(i)+lifttot(i)
        dstand(i)=dstand(i)*(1.0-fltcoef) 
201     continue

      end
