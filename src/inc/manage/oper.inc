!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the variables used in the MANAGEMENT
!     process subprograms.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer          opcode
      integer          opskip
      character*80     opname
      character*80     ofuel

      character*80     grname
      integer          grcode

      real             cutht

      real             oenergyarea, ostir

      common / oper1 / opcode, opskip
      common / oper2 / opname

      common / proc1 / grcode
      common / proc2 / grname

      common / harvp / cutht

      common / energystir / oenergyarea, ostir, ofuel

      save :: /oper1/, /oper2/, /proc1/, /proc2/, /harvp/, /energystir/

!     + + + VARIABLE DEFINITIONS + + +
!     opcode   - code indicating operation type
!            0 - indicates an operation that will be run only mcount times
!                (normally used for initialzation)
!            1 - triggers a read of tillage related operation parameters
!                (speed and direction)
!     opskip   - used to skip all groups and processes in an operation that
!                has already completed mcount invocations
!            0 - do not skip
!            1 - skip
!     opname   - name of current operation read from management file

!     grname   - name of group read from management file
!     grcode   - group code designating which parameters will follow name
!            1 - soil distrubance parameters
!            2 - biomass manipulation
!            3 - crop growth
!            4 - ammendments
!     cutht    - read from process as fraction or distance (flag controlled).
!                Converted to distance from ground up in meters by cut.for
!     oenergyarea - diesel fuel equivalent energy required for operation Liters per hectare
!     ostir    - Operation Stir value (assigned from RUSLE2)
!     ofuel    - Name of fuel used for operation.

