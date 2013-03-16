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
      subroutine erodout (o_unit, o_E_unit, sgrd_u, input_filename, hagen_plot_flag, cellstate)
      use erosion_data_struct_defs, only: cellsurfacestate
      integer o_unit, o_E_unit, sgrd_u
      character*1024 input_filename
      logical hagen_plot_flag
      type(cellsurfacestate), dimension(0:,0:), intent(out) :: cellstate     ! initialized grid cell state values
      end subroutine erodout
!------------------------------------
!------------------------------------

   end interface

end MODULE sweep_interface_defs

