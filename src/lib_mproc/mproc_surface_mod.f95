!
!$Author$
!$Date$
!$Revision$
!$HeadURL$

module mproc_surface_mod

  contains

    subroutine crust (crustf_rm,tillf,crustf,lmosf, lmosm)

!     + + + PURPOSE + + +
!     This subroutine destroys the surface crust after a tillage event.

!     + + + KEYWORDS + + +
!     crust, tillage (primary/secondary)

!     + + + ARGUMENT DECLARATIONS + + +
      real tillf, crustf, crustf_rm, lmosf, lmosm
!
!     + + + ARGUMENT DEFINITIONS + + +
!     lmosf - fraction of crusted surface containing loose erodible material
!     lmosm - mass of loose erodible material on crusted portion of surface
!     crustf - Current fraction of surface crusted (before & after operation)
!     crustf_rm - Fraction of crust removed (0 <= crustf_rm <= 1)
!     tillf - Fraction of the surface tilled (0 <= tillf <= 1)

!     crf = cri * ( (1.0 - tillf) + (tillf * (1.0-crustf_rm)))

      ! determine fraction of surface that remains crusted
      crustf = crustf * (1.0 - tillf * crustf_rm)

! Currently the crust function doesn't modify the loose erodible
! material variables on the crusted surface.  That could be changed
! in the future if it was deemed necessary.

! The following should be removed.  Need to check SOIL and EROSION
! first to make sure they aren't adversely affected. - LEW
! 8/25/1999

!     check to see if the loose material on the surface is still there
!     if enough of the crust is removed set lmosf to zero (loose material)
!     This was done according to L. Hagen

      ! just clear them out if it close to zero 
      ! (LH shouldn't have erosion or soil submodels this sensitive)
      if (crustf .lt. 0.01) then
         lmosf = 0.0
         lmosm = 0.0
      endif

      return
    end subroutine crust

    subroutine rough                                                  &
     &              (roughflg, rrimpl,till_i,tillf,                     &
     &               rr, tillay, clayf, siltf,                          &
     &               rootmass, resmass,                                 &
     &               ldepth ) 

!     + + + PURPOSE + + +
!     
!     This subroutine performs a random roughness calculation 
!     after a tillage operation.
!
!     + + + KEYWORDS + + +
!     random roughness (RR), tillage (primary/secondary)
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer roughflg
      real    tillf,rrimpl,rr,till_i
      integer tillay
      real    clayf(*), siltf(*)
      real    rootmass(:), resmass(:)
      real    ldepth(*)
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     tillf	    - Fraction of the surface tilled (0-1)
!	  till_i	- Tillage intensity factor (0-1)
!     rrimpl	- Assigned nominal RR value for the tillage operation (mm) 
!     rr		- current surface random roughness (mm) 
!     tillay    - number of layers affected by tillage
!     clayf     - clay fraction of soil
!     siltf     - silt fraction of soil
!     rootmass  - mass of roots by layer, pools (kg/m^2)
!     resmass   - mass of buried crop residue by layer, pools (kg/m^2)
!     ldepth    - depth from soil surface of lower layer boundaries
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     
!
!     + + + PARAMETERS + + +
      real rrmin
      parameter ( rrmin = 6.096 ) !(mm) = 0.24 inches
!
!     + + + LOCAL VARIABLES + + +
      integer laycnt, laymax
      real    rradj, soiladj
      real    biomass

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     laycnt   - counter for layers
!     laymax   - maximum layer index
!     rradj    - adjusted implement random roughness
!     soiladj  - soil texture adjustment multiplier
!     biomass  - total biomass in the tillage zone
!
!     + + + END SPECIFICATIONS + + + 
!
!	  Perform the calculation of the surface RR after a tillage
!     operation.  Check to see if the tillage intensity factor is
!     needed before performing the calculation. 
!
!     adjust the input random roughness value based on flag
!     roughflg.eq.0 does not adjust the implement random roughness for soil type or biomass amount
      rradj = rrimpl
      if( (roughflg.eq.1).or.(roughflg.eq.2)) then
!         adjust for soil type
          soiladj = 0.16*siltf(1)**0.25+1.47*clayf(1)**0.27
          soiladj = max(0.6,soiladj)
          rradj = rradj * soiladj
      endif

      ! random roughness for a non tillage operation (tillay=0) (roller, wheel traffic)
      ! is still adjusted for soil type and biomass in the first layer
      ! since tillage group process is still required tillf and till_i will be set
      laymax = max(tillay, 1)

      if( (roughflg.eq.1).or.(roughflg.eq.3)) then
!         adjust for buried residue amounts, handbook 703, eq 5-17
!         this equation is originally in lbs/ac/in
!         rradj = rrmin+(rradj-rrmin)*(0.8*(1-exp(-0.0012*biomass))+0.2)
!         This was modified in Wagners correspondence with Foster to use
!         the factor exp(-0.0015*biomass)
!         lbs/ac/in = 226692 * kg/m^2/mm
!         sum up total biomass in the tillage depth
          if( rrimpl.gt.rrmin ) then
              biomass = 0.0
              do laycnt = 1, laymax
                  biomass = biomass + rootmass(laycnt)
                  biomass = biomass + resmass(laycnt)
              end do
!             make it kg/m^2/mm
              biomass = biomass / ldepth(laymax)          
!             if value is below min, don't adjust since it would
!             increase it with less residue. 
              if(rradj.gt.rrmin) then
!                 this equation uses biomass in kg/m^2/mm
                  rradj = rrmin + (rradj-rrmin)                         &
     &                  *(0.8*(1-exp(-339.92*biomass))+0.2)
              endif
          endif
      endif

      ! Is RR going to be increased?  If so, then just do it.
      if (rradj .ge. rr) then 
	      rr = tillf*rradj + (1.0-tillf)*rr
      else
          rr = tillf*(till_i*rradj + (1.0-till_i)*rr)                   &
     &       + (1.0-tillf)*rr
      end if

      return
    end subroutine rough

    subroutine orient                                                 &
     &              (rh,rw,rs,rd,dh,ds,                                 &
     &              impl_rh,impl_rw,impl_rs,impl_rd,                    &
     &              impl_dh,impl_ds,tilld,rflag)


!     + + + PURPOSE + + +
!
!     This subroutine performs an oriented roughness calculation
!     after a tillage operation.  Actually it performs a check of the
!     ridge flag (rflag) and does the coresponding manipulation
!     of the ridge parameters.  The three valid values of the
!     ridge flag are:
!     0 - operation has no effect if a ridge currently exists.
!     1 - set all oriented roughness parameters to the implement values.
!     2 - Modification depends on the current ridge height,
!         specified tillage depth, and ridging characteristics
!         of the tillage implement.
!         If the tillage depth is great enough to remove the ridges,
!         ridge values are set according to the implement values.
!         If the tillage depth is too shallow to remove the current
!         ridges alone, then the two following situations occur:
!         a) if the difference between the original ridge height and
!         specified tillage depth is less than the implement specified
!         ridging height, the ridge values are set according to the
!         implement values.
!         b) if not, then the current ridge remains but at a reduced
!         height dependent upon the implement tillage depth.
!
!     + + + KEYWORDS + + +
!     oriented roughness (OR), tillage (primary/secondary)
!
!     + + + ARGUMENT DECLARATIONS + + +
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!
!     + + + PARAMETERS + + +
!
!     + + + LOCAL VARIABLES + + +
!
      real     rh,rw,rs,rd,dh,ds
      real     impl_rh,impl_rw,impl_rs,impl_rd
      real     impl_dh,impl_ds
      real     tilld
      integer  rflag
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     rh      - current ridge height (mm)
!     rw      - current ridge width (mm)
!     rs      - current ridge spacing (mm)
!     rd      - current ridge direction (clockwise from true north)
!     rh      - current dike height (mm)
!     rs      - current dike spacing (mm)
!     impl_rh - implement ridge height (mm)
!     impl_rw - implement ridge width (mm)
!     impl_rs - implement ridge spacing (mm)
!     impl_rd - implement ridge direction (clockwise from true north)
!     impl_dh - implement dike height (mm)
!     impl_ds - implement dike spacing (mm)
!     tilld  - implement tillage depth (mm)
!     rflag  - flag (0-2) telling what needs to be done

!     + + + END SPECIFICATIONS + + +
!
!  Perform the calculation of the oriented OR after a tillage
!     operation.
!
      select case (rflag)

        case (0) !typical of a row cultivator in a ridged field
          if (rh .lt. 0.1) then !if ridges don't exist, create'em
            rh = impl_rh
            rw = impl_rw
            rs = impl_rs
            rd = impl_rd
            dh = impl_dh
            ds = impl_ds
          else   !don't disturb ridges if they exist in field 
          endif

        case (1) !always set ridge values to those specified for tool
          rh = impl_rh
          rw = impl_rw
          rs = impl_rs
          rd = impl_rd
          dh = impl_dh
          ds = impl_ds

        case (2) !adjust ridge height based upon tillage depth
          if (tilld .ge. (rh/2.0)) then
            !tillage depth is deep enough
            rh = impl_rh
            rw = impl_rw
            rs = impl_rs
            rd = impl_rd
            dh = impl_dh
            ds = impl_ds
          else                          
			if (impl_rh .ge. (2.0 * (rh/2.0 - tilld))) then
			   !tillage implement ridging great enough
               rh = impl_rh
               rw = impl_rw
               rs = impl_rs
               rd = impl_rd
               dh = impl_dh
               ds = impl_ds
            else                        
               !tdepth too shallow to completely remove original ridges
               rh = 2.0 * (rh/2.0 - tilld)
               dh = impl_dh
               ds = impl_ds
            endif
          endif

        case default
            print *, 'The ridge flag (for oriented roughness)'
            print *, ' was not set correctly'

      end select
	  
      return
    end subroutine orient

end module mproc_surface_mod

