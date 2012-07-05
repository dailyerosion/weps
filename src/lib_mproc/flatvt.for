!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
      subroutine flatvt                                                 &
     &                 (fltcoef, tillf, bcrbc, bdrbc,                   &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           bcdstm,                                                &
     &           bdmstandstem, bdmstandleaf, bdmstandstore,             &
     &           bdmflatstem, bdmflatleaf, bdmflatstore,                &
     &           bddstm, bflg)

!     + + + PURPOSE + + +
!     Process # 33 called from doeffect.for
!
!     This subroutine performs the biomass manipulation process of transferring
!     standing biomass to flat biomass based upon a flatenning coefficient.
!     The standing component (either crop or a biomass pool) flattened
!     is determined by a flag which is set before the call to this
!     subroutine.  The flag may contain any number of combinations
!     found below.

!     The implicit assumption in this routine is that if you flatten it,
!     it is removed from the living crop and put into the temporary pool
!     to become residue

!            Flags values (binary #'s actually)
!   bit no.                                          decimal value
!     x  - flatten standing material in all pools         (0)
!     0  - flatten standing crop                          (1)
!     1  - flatten standing residue in decomp pool #1     (2)
!     2  - flatten standing residue in decomp pool #2     (4)
!     3  - flatten standing residue in decomp pool #3     (8)
!
!     Note that biomass for any of these pools that are flattened
!     is transfered to the cooresponding flat pool.
!
!     + + + KEYWORDS + + +
!     flatten, biomass manipulation

      include 'p1werm.inc'
!
!     + + + ARGUMENT DECLARATIONS + + +
      real    fltcoef(mnrbc)
      real    tillf
      integer bcrbc
      integer bdrbc(mnbpls)

      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore

      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore

      real    bcdstm

      real    bdmstandstem(mnbpls)
      real    bdmstandleaf(mnbpls)
      real    bdmstandstore(mnbpls)

      real    bdmflatstem(mnbpls)
      real    bdmflatleaf(mnbpls)
      real    bdmflatstore(mnbpls)

      real    bddstm(mnbpls)
      integer bflg
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     fltcoef   - flattening coefficients of implement for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     bcrbc     - residue burial class for standing crop
!     bdrbc     - residue burial class for residue

!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)

!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)

!     bcdstm - Number of crop stems per unit area (#/m^2)

!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)

!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)

!     bddstm - Number of residue stems per unit area (#/m^2)

!     dstand    - (decomp pool) standing biomass by age pool (kg/m^2)
!     dflat     - (decomp pool) surface biomass by age pool  (kg/m^2)
!     dstems    - (decomp pool) number of standing residue stems (#/m^2)
!
!     bflg      - flag indicating what to flatten
!       0 - All standing material is flatttened (both crop and residue)
!       1 - Crop is flattened
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool

!       Note that any combination of pools or crop may be used
!       A bit test is done on the binary number to see what to modify
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     mnrbc         - max number of residue burial classes
!     mnbpls        - max number of biomass pools
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      integer  idy
      integer  tflg
      real     flatfrac
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     idy   - loop variable for decomp pools
!     tflg  - temporary biomass flag
!     flatfrac - fraction of material to be flattened

!     + + + END SPECIFICATIONS + + +

!     set tflg bits correctly for "all" pools if bflg=0
      if (bflg .eq. 0) then
          tflg = 1                   ! crop pool
          do 10 idy = 1,mnbpls
             tflg = tflg + 2**idy      ! decomp pools
10        continue
      else
          tflg = bflg
      endif

!     check for proper indexes in bcrbc
      if( (bcrbc.ge.1).and.(bcrbc.le.mnrbc) ) then
          if (BTEST(tflg,0)) then              ! flatten standing crop
              flatfrac = fltcoef(bcrbc) * tillf
              ! increase flat pools
              btmflatstem = btmflatstem + bcmstandstem * flatfrac
              btmflatleaf = btmflatleaf + bcmstandleaf * flatfrac
              btmflatstore = btmflatstore + bcmstandstore * flatfrac
              ! decrease standing pools
              bcmstandstem = bcmstandstem * (1.0 - flatfrac)
              bcmstandleaf = bcmstandleaf * (1.0 - flatfrac)
              bcmstandstore = bcmstandstore * (1.0 - flatfrac)
              ! reduce # of crop stems
              bcdstm = bcdstm * (1.0 - flatfrac)
          endif
      endif

      do idy = 1,mnbpls               ! flatten standing residue
!         check for proper indexes in bdrbc
          if( (bdrbc(idy).ge.1).and.(bdrbc(idy).le.mnrbc) ) then
              if (BTEST(tflg,idy)) then    ! from specified decomp pools 
                  flatfrac = fltcoef(bdrbc(idy)) * tillf
                  bdmflatstem(idy) = bdmflatstem(idy)                   &
     &                             + bdmstandstem(idy) * flatfrac
                  bdmflatleaf(idy) = bdmflatleaf(idy)                   &
     &                             + bdmstandleaf(idy) * flatfrac
                  bdmflatstore(idy) = bdmflatstore(idy)                 &
     &                              + bdmstandstore(idy) * flatfrac
                  bdmstandstem(idy) = bdmstandstem(idy)*(1.0 - flatfrac)
                  bdmstandleaf(idy) = bdmstandleaf(idy)*(1.0 - flatfrac)
                  bdmstandstore(idy) = bdmstandstore(idy)*(1.0-flatfrac)
                  bddstm(idy) = bddstm(idy) * (1.0 - flatfrac)
              endif
          endif
      end do

 60   return
      end
