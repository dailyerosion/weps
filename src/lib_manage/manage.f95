!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine manage( sr, startyr, soil, crop, cropprev, residue, biotot, mandate, h1et)

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

      use weps_interface_defs, ignore_me=>manage
      use datetime_mod, only: difdat, get_simdate
      use file_io_mod, only: luomanage
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, bio_prevday
      use mandate_mod, only: opercrop_date
      use stir_report_mod, only: stir_report
      use manage_data_struct_defs, only: am0tfl, lastoper
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'manage/man.inc'
      include 'manage/asd.inc'
      include 'manage/mproc.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, startyr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev    ! structure containing crop previous day values
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et

!     + + + ARGUMENT DEFINITIONS + + +
!        sr - the subregion number
!     startyr - starting year of the simulation run

!     + + + LOCAL VARIABLES + + +

      integer simdd, simmm, simyr, mansimyr, manmon, manday, manyr
      character*256   line

!        simdd - current simulation day
!        simmm - current simulation month
!        simyr - current simulation year
!     mansimyr - the simulation year which corresponds to the year from the management file
!       manmon - month from the management file
!       manday - day from the management file
!        manyr - year from the management file

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
      call get_simdate( simdd, simmm, simyr )

      ! reset any global variables whose setting should only be valid
      ! for one day
      call mgdreset(h1et%zirr)

      line = mtbl(mcur(sr))

!     If we aren't pointing at a date, we have a problem
      
      if (line(1:1).ne.'D') goto 901

!     Must be a space between 'D' and date in dd/mm/yyyy format
      read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) manday,manmon,manyr

      ! find simulation year to which management year corresponds
      mansimyr = simyr - mod (simyr-startyr, mperod(sr)) + manyr - 1

      if (difdat (simdd,simmm,simyr,manday,manmon,mansimyr).ne.0) then
        ! management date does not match simulation date
        return
      end if

      if (am0tfl(sr) .eq. 1) then
        write (luomanage(sr),*)
        write (luomanage(sr),2015) simdd,simmm,simyr,manyr,sr
      endif

!     pass date of operation to MAIN for output purposes, used by STIR also
      lastoper(0)%day = manday
      lastoper(0)%mon = manmon
      lastoper(0)%yr = manyr
      lastoper(sr)%day = manday
      lastoper(sr)%mon = manmon
      lastoper(sr)%yr = manyr

!     Move the tbl ptr to the first operation after the date

  10  mcur(sr) = mcur(sr) + 1
      line = mtbl(mcur(sr))
      select case (line(1:1))
      case ('O')
        lastoper(sr)%skip = 0
        call dooper(sr)
      case ('G')
        if(lastoper(sr)%skip.eq.0) call dogroup(sr, soil)
      case ('P')
        if(lastoper(sr)%skip.eq.0) then

           call doproc(sr, mcount(sr), soil, crop, cropprev, residue, biotot, mandate, h1et)
        endif
      case ('D')
        call stir_report(sr, .false., lastoper(sr)%stir, lastoper(sr)%energyarea)
        read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) manday,manmon,manyr
        ! find simulation year to which management year corresponds
        mansimyr = simyr - mod (simyr-startyr, mperod(sr)) + manyr - 1
        if( difdat (simdd,simmm,simyr,manday,manmon,mansimyr).ne.0) then
           ! initialize end of season / hydrobal reporting flag to true to generate a report
           rpt_season_flg(sr) = .true.
           return
        end if
      case ('*')
        call stir_report(sr, .true., lastoper(sr)%stir, lastoper(sr)%energyarea)
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
