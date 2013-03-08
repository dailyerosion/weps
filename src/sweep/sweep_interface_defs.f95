!$Author$
!$Date$
!$Revision$
!$HeadURL$

MODULE sweep_interface_defs

   interface

!------------------------------------
      subroutine erodin (i_unit,o_unit,cmdebugflag,already_read_inputs, subrsurf)
      use erosion_data_struct_defs, only: subregionsurfacestate
      integer i_unit, o_unit, cmdebugflag, already_read_inputs
      type(subregionsurfacestate), dimension(:), allocatable :: subrsurf
      end subroutine erodin
!------------------------------------
!------------------------------------
!------------------------------------

   end interface

end MODULE sweep_interface_defs

