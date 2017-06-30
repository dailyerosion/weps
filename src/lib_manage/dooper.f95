!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   dooper (manFile)

!     + + + PURPOSE + + +
!     Dooper reads in any coefficients associated with the
!     operation.

!     + + + KEYWORDS + + +
!     tillage, operation, management

      use weps_interface_defs, ignore_me=>dooper
      use manage_data_struct_defs, only: lastoper, man_file_struct 
      use manage_data_struct_mod, only: getManVal

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      type(man_file_struct), intent(in) :: manFile

!     + + + LOCAL VARIABLES + + +
      integer :: sr  ! the subregion being processed

!     + + + DATA INITIALIZATIONS + + +
      sr = manFile%isub

!     + + + END SPECIFICATIONS + + +

      lastoper(sr)%code = manFile%oper%operType
      lastoper(sr)%name = manFile%oper%operName
      if( (lastoper(sr)%code.eq.0).and.(manFile%mcount.gt.0) ) then
          lastoper(sr)%skip = 1
          print*, 'SR',sr,' Skip operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
      else
          print*, 'SR',sr,' Do operation', lastoper(sr)%code,' ', trim(lastoper(sr)%name)
      end if

      ! set value of tlayer to zero before operation begins. Compaction occurs from tlayer
      ! downward, so operations without tillage need this set to zero to model surface compaction.
      tlayer = 0

      ! assign default fuel as blank.  Treated as default in reports
      lastoper(sr)%fuel = ''

      select case (lastoper(sr)%code)

      case (1)  ! original ground engaging operation
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'ospeed', ospeed)
          call getManVal(manFile%oper, 'odirect', odir)
          call getManVal(manFile%oper, 'ostdspeed', ostdspeed)
          call getManVal(manFile%oper, 'ominspeed', ominspeed)
          call getManVal(manFile%oper, 'omaxspeed', omaxspeed)

      case (3) ! added energy and stir to O1
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'oenergyarea', lastoper(sr)%energyarea)
          call getManVal(manFile%oper, 'ostir', lastoper(sr)%stir)
          call getManVal(manFile%oper, 'ospeed', ospeed)
          call getManVal(manFile%oper, 'odirect', odir)
          call getManVal(manFile%oper, 'ostdspeed', ostdspeed)
          call getManVal(manFile%oper, 'ominspeed', ominspeed)
          call getManVal(manFile%oper, 'omaxspeed', omaxspeed)
          ! Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
            ! get fuel line
            call getManVal(manFile%oper, 'ofuel', lastoper(sr)%fuel)
          end if

      case (4) ! added energy and stir to O2
          ! read tillage speed and direction
          call getManVal(manFile%oper, 'oenergyarea', lastoper(sr)%energyarea)
          call getManVal(manFile%oper, 'ostir', lastoper(sr)%stir)
          ! Version 1.5 added ofuel
          if (manFile%mversion .ge. 1.50) then
            ! get fuel line
            call getManVal(manFile%oper, 'ofuel', lastoper(sr)%fuel)
          end if

      case default
          ! set energy and stir values to default
          lastoper(sr)%energyarea = -1
          lastoper(sr)%stir = -1
          ! set fuel to blank (default)
          lastoper(sr)%fuel = ''
      end select

      ! initialize row spacing and ridge flag to zero. They are needed
      ! by P51, (set in P3 or P5) but may be set and not cleared by a previous operation.
      imprs = 0.0
      rdgflag = 0

      return

      end

