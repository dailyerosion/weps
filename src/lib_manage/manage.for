!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine manage( sr, syear, lopdd, lopmm, lopyy,                &
     &                   crop, residue, biotot, mandate)

!     + + + PURPOSE + + +
!     This is the main routine of the MANAGEMENT submodel. The date passed
!     to this routine is checked with the next operation date in the
!     management file. If the dates match, then an operation is to be
!     performed today on the given subregion.
!     The date of last operation (op*) is also passed for output purposes.jt

!     Edit History
!     19-Feb-99   wjr   rewrote
!     20-Feb-99   wjr   made date return

!     + + + KEYWORDS + + +
!     tillage, management

      use weps_interface_defs
      use datetime_mod, only: difdat, get_simdate
      use file_io_mod, only: luomanage
      use biomaterial, only: biomatter, biototal
      use mandate_mod, only: opercrop_date
      use stir_report_mod, only: stir_report
      use manage_data_struct_defs, only: am0tfl

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
      include 'manage/oper.inc'

! for debugging
! ***      include 's1layr.inc'      

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, syear
      integer lopdd, lopmm, lopyy
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate

!     + + + ARGUMENT DEFINITIONS + + +
!        sr - the subregion number
!     syear - starting year of the simulation run
!     lopdd - day of last operation
!     lopmm - month of last operation
!     lopyy - year of last operation

!     + + + LOCAL VARIABLES + + +
      integer dd, mm, yyyy, myear, month, day, year
      character*256   line

!        dd - current simulation day
!        mm - current simulation month
!      yyyy - current simulation year
!     myear - determines the "year offset" within a period of years
!     month - month from the management file
!       day - day from the management file
!      year - year from the management file

!     + + + SUBROUTINES CALLED + + +
!     dooper - DO OPERation is called when dates match
!     dogroup - DO GROUP is called when G code encountered
!     doproc - DO PROCess is called when P code encountered

!     + + + OUTPUT FORMATS + + +
2015     format ('Op Date ', i2,1x,i2,1x,i4,' Rot yr ',i2,' sr #',i2)
!2015     format ('Operation Date ',i2,1x,i2,1x,i4,', subregion #',i2)

!     + + + END SPECIFICATIONS + + +

!     Don't do anything if the subregion isn't in the data file.
      if (mbeg(sr).eq.mbeg(sr+1)) then
        write(*,*) 'Sub-region not in data file', sr
        return
      endif

      ! get current simulation day, month, year
      call get_simdate( dd, mm, yyyy )

      ! reset any global variables whose setting should only be valid
      ! for one day
      call mgdreset(sr)

      line = mtbl(mcur(sr))

!     If we aren't pointing at a date, we have a problem
      
      if (line(1:1).ne.'D') goto 901

      myear=mod (yyyy-syear, mperod(sr))+1

!     Must be a space between 'D' and date in dd/mm/yyyy format
      read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) day,month,year

!     if not today then return

      if (difdat (dd,mm,myear,day,month,year).ne.0) return

      if (am0tfl(sr) .eq. 1) then
        write (luomanage(sr),*)
        write (luomanage(sr),2015) dd,mm,yyyy,year,sr
      endif

!     pass date of operation to MAIN for output purposes, used by STIR also
      lopdd = day
      lopmm = month
      lopyy = year

!     Move the tbl ptr to the first operation after the date

  10  mcur(sr) = mcur(sr) + 1
      line = mtbl(mcur(sr))
      select case (line(1:1))
      case ('O')
        opskip = 0
        call dooper(sr)
      case ('G')
        if(opskip.eq.0) call dogroup(sr)
      case ('P')
        if(opskip.eq.0) then
           call doproc(sr, mcount(sr), crop, residue, biotot, mandate)
        endif
      case ('D')
        call stir_report(sr, .false., ostir, oenergyarea)
        read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) day,month,year
        if (difdat (dd,mm,myear,day,month,year).ne.0) return
      case ('*')
        call stir_report(sr, .true., ostir, oenergyarea)
        mcount(sr) = mcount(sr) + 1
        mcur(sr) = mbeg(sr)
  101   mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        if (line(1:1).ne.'D') goto 101
        return
      case ('+')
        continue
      case default
        goto 903
      end select
      goto 10

! Error stops
      
901   write(0,*) 'Enter manage not pointing at date'
      call exit (1)
902   write(0, 9902) line, sr
9902  format('Bad date format ',a,' in region ',i2)
      call exit (1)
903   write(0,*) 'Invalid management code -', line (1:1)      
      call exit (1)
      end
