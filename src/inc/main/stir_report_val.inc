!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This block common contains local variables used to store
!     variables needed to calculate the soil tillage intensity rating (stir).

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      integer oper_cnt(mnsub), proc_cnt(mnsub)
      logical done_flg(mnsub), man_eof(mnsub)
      real    stir_op_sum(mnsub)
      real    stir_op_energy(mnsub)

      integer mxphops
      parameter (mxphops = 400)

      integer phopcnt(mnsub), phopidx(mnsub)
      integer phopday(mxphops,mnsub)
      integer phopmon(mxphops,mnsub)
      integer phopyr(mxphops,mnsub)
      character*80 stir_opname(mxphops,mnsub)
      character*80 stir_cropname(mxphops,mnsub)
      character*80 stir_fuelname(mxphops,mnsub)
      integer phop_type(mxphops, mnsub)
      real phop_stir(mxphops, mnsub)
      real phop_energy(mxphops, mnsub)
      integer crop_num(mxphops, mnsub)
      integer last_harv(mxphops, mnsub)

      common / stir_report_val /                                        &
     &      oper_cnt, proc_cnt,                                         &
     &      done_flg, man_eof,                                          &
     &      stir_op_sum, stir_op_energy,                                &
     &      phopcnt, phopidx,                                           &
     &      phopday, phopmon, phopyr,                                   &
     &      stir_opname, stir_cropname, stir_fuelname,                  &
     &      phop_type, phop_stir, phop_energy,                          &
     &      crop_num, last_harv

      save :: /stir_report_val/

!     + + + VARIABLE DEFINITIONS + + +

!    oper_cnt   - stir count of operations, used to trigger accumulation
!    proc_cnt   - stir count of processes, used in averaging
!    done_flg   - flag to indicate that stir had completed one pass through management file
!    man_eof    - flag to indicate that man file has reached end once
!    stir_op_sum - accumulated stir values, divide by proc count to get average
!    stir_op_energy - operation energy value from operation input

!    mxphops    - maximum number of planting and harvest operations that
!                 can be tracked with these arrays

!    phopcnt    - actual number of (p)lanting and (h)arvest (op)erations tabulated
!    phopidx    - operation index, location in the array

!    phopday    - day of the operation
!    phopmon    - month of the operation
!    phopyr     - year of the operation

!    stir_opname - operation name from operation input
!    stir_cropname - crop name assoicated with this operation if a harvest
!    stir_fuelname -fuel name associated with this operation, may be blank to indicate use of the default fuel as defined by the interface
!    phop_type  - operation type flag
!                 0 = not yet initialized
!                 1 = planting operation
!                 2 = harvest operation
!    phop_stir  - STIR value for that operation
!    phop_energy - energy value for that operation
!    crop_num   - number of crop in the crop rotation cycle
!                 0 = not yet initialized
!                 1-n = number of crop
!    last_harv  - is this the last harvest in a crop rotation cycle
!                 0 = not the last harvest
!                 1 = this is the last harvest (ie. end of the cycle
