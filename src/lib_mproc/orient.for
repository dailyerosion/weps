!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
!
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
      end
