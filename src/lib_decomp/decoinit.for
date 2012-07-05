!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     decini.for

      subroutine decoinit(isr)

!     + + +  PURPOSE + + +
!     This subroutine initalizes values needed in the decomposiiton
!     submodel. The values are read from a file that indicate the previous
!     crop harvested and quantities of biomass remaining in the field, for
!     standing, surface, buried, and root residues.

!     The subroutine also sets all other age pools and decompdays to 0.


!     + + + COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'd1gen.inc'
      include 's1layr.inc'
      include 'm1subr.inc'
      include 'c1db1.inc'
      include 'd1glob.inc'

      include 'decomp/decomp.inc'

!      + + + LOCAL VARIABLE DECLARATION + + +

      integer isr
      integer j, l

!     + + +  LOCAL VARIABLE DEFINITIONS + + +

!     isr     current subregion

!   + + + FUNCTION DECLARATION + + +

!   + + + DATA INITIALIZATION + + +

!      + + + FORMAT STATEMENT  + + +

 2500 format ('  Problem reading the DECOMPOSITION parameters from file &
     &crop.db - WEPS EXECUTION HALTED')

!     + + + END SPECIFICATION + + +

!     Set harvest flag to 0 and pool values to 1
      hrvflag = 0
      ipool = 1
      ipoolf = 1

!     default initalization values for crop type, stem no. stem biomass
!     surface biomass, below ground biomass, and root biomass. ie. nothing

!     water coefficent parameters
      diwcsy(isr) = 0.0
      dweti(isr) = 0.0

!     cummulative ddays for surface residues
      do 90 iage= 1,mnbpls
         cumdds(iage,isr) = 0.0
   90 continue

!     standing stem biomass, stem number, stem diam, stem height, stem fall threshold
      do iage = 1,mnbpls
         ad0nam(iage,isr) = "No Crop"
         ad0sla(iage,isr) = 0.0
         ad0ck(iage,isr) = 0.0
         adrbc(iage,isr) = 1

         admst(iage,isr) = 0.0
         addstm(iage,isr) = 0.0
         adxstm(iage,isr) = 0.0
         adxstmrep(iage,isr) = 0.0
         ddsthrsh(iage,isr) = 0.0

         admstandstem(iage,isr) = 0.0
         admstandleaf(iage,isr) = 0.0
         admstandstore(iage,isr) = 0.0

         admflatstem(iage,isr) = 0.0
         admflatleaf(iage,isr) = 0.0
         admflatstore(iage,isr) = 0.0

         admflatrootstore(iage,isr) = 0.0
         admflatrootfiber(iage,isr) = 0.0

         adgrainf(iage,isr) = 1.0
         adhyfg(iage,isr) = 0

         addstm(iage,isr) = 0.0
         adzht(iage,isr) = 0.0

         adm(iage,isr) = 0.0
         admst(iage,isr) = 0.0
         admf(iage,isr) = 0.0
         admbg(iage,isr) = 0.0
         admrt(iage,isr) = 0.0

         adrsai(iage,isr) = 0.0
         adrlai(iage,isr) = 0.0
         adffcv(iage,isr) = 0.0
         adfscv(iage,isr) = 0.0
         adftcv(iage,isr) = 0.0
      end do

!     cumulative ddays and biomass for all layers below ground
      do 40 isz = 1,nslay(isr)
         do 41 iage = 1,mnbpls
            cumddg(isz,iage,isr) = 0.0
   41    continue
         do 42 iage = 1,mnbpls
            admbgz(isz,iage,isr) = 0.0
            admrtz(isz,iage,isr) = 0.0

            admbgstemz(isz,iage,isr) = 0.0
            admbgleafz(isz,iage,isr) = 0.0
            admbgstorez(isz,iage,isr) = 0.0

            admbgrootstorez(isz,iage,isr) = 0.0
            admbgrootfiberz(isz,iage,isr) = 0.0
   42    continue
   40 continue

!     flat biomass, cummddays, and covfact for surface residues
      do 30 iage = 1,mnbpls
         cumddf(iage,isr) = 0.0
         covfact(iage,isr) = 0.0
   30 continue

!  decomp pool variables
      do iage=1,mnbpls
          do isz=1, mncz
              adrsaz(isz,iage,isr) = 0.0
              adrlaz(isz,iage,isr) = 0.0
          end do
      end do

!     set biomass decomposition rates to 0.0 for all pools
!     residue type counter
      do 50 j = 1,5
!        residue pool counter
         do 60 l = 1,mnbpls
            dkrate(j,l,isr) = 0.0
   60    continue
   50 continue

      ! set biomass surface evaporation suppression coefficients
      do iage = 1,mnbpls
          adresevapa(iage,isr) = 0.0
          adresevapb(iage,isr) = 1.0
      end do

!     Open output files
      call decopen

      return
   80 write(*,2500)
      end
