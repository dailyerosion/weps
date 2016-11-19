!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine updres(soil, residue, restot)

      use weps_interface_defs, only: poolupdate
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal

!     + + +   ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot

!     Update geometric properties of the decomp residue pools

!     + + + END SPECIFICATIONS + + +

      ! update derived globals for all decomposition pools
      call poolupdate(soil%nslay, soil%aszlyd, residue, restot)

      return
      end
