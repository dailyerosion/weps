!$Author$
!$Date$
!$Revision$
!$HeadURL$

module decomp_out_mod

  contains

    subroutine decout(isr, plant)

      ! This subroutine writes decomposition output

      use datetime_mod, only: get_psimdate
      use file_io_mod, only: luod_above, luod_below
      use biomaterial, only: plant_pointer, residue_pointer
      use decomp_data_struct_defs, only: am0dfl

      ! + + + ARGUMENT DECLARATIONS + + +
      integer :: isr
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      ! + + + LOCAL VARIABLES + + +
      integer :: cd   ! simulation day
      integer :: cm   ! simulation month
      integer :: cy   ! simulation year
      integer :: nslay ! number of soil layers
      integer :: isz  ! soil layer index

      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      integer :: iplt ! plant index
      integer :: ires ! residue pool index
      integer :: iallres ! all residue pools index

      real, dimension (:), allocatable :: tcumddg
      real, dimension (:), allocatable :: tmbgz
      real, dimension (:), allocatable :: tmrtz

      integer :: alloc_stat  ! allocation status return
      integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

      ! + + + END SPECIFICATIONS + + +

      call get_psimdate(isr, cd, cm, cy)

      if ((am0dfl(isr) .eq. 1) .or. (am0dfl(isr) .eq. 3)) then
        write (luod_above(isr), FMT="(4x,i2,'/',i2,'/',i4)", ADVANCE="NO") cd, cm, cy
      end if

      if ((am0dfl(isr) .eq. 2) .or. (am0dfl(isr) .eq. 3)) then
        write(luod_below(isr), FMT="(7x,i2,'/',i2,'/',i4,4x,'cumddg',13x,'admbgz', &
          &13x,'admrtz',/,6x,'layer',7x,3(' 1',6x,' 2',8x))") cd, cm, cy
      end if

      iplt = 0
      iallres = 0

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists
        ! all plant biomass is living, not residue.

        iplt = iplt + 1
        nslay = size(plant%mass%rootfiberz)

        ires = 0

        ! output residues in this plant
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )

          ires = ires + 1
          iallres = iallres + 1

          ! output above ground data on a daily basis
          if ((am0dfl(isr) .eq. 1) .or. (am0dfl(isr) .eq. 3)) then
            select case (iallres)
            case (1)
              write (luod_above(isr), FMT="(1x,i1,1x,f5.0,8(1x,f7.2))") iallres, &
                thisResidue%dstm, thisResidue%cumdds, thisResidue%deriv%mst, &
                thisResidue%cumddf, thisResidue%deriv%mf, thisResidue%deriv%fscv, &
                thisResidue%deriv%ffcv, thisResidue%deriv%rsai, &
                thisResidue%deriv%rsaz(1)
            case (2)
              write (luod_above(isr), FMT="(15x,i1,1x,f5.0,4(1x,f7.2))") iallres, &
                thisResidue%dstm, thisResidue%cumdds, thisResidue%deriv%mst, &
                thisResidue%cumddf, thisResidue%deriv%mf
            case(3)
              write (luod_above(isr), FMT="(15x,i1,23x,2(1x,f7.2))") iallres, &
                thisResidue%cumddf, thisResidue%deriv%mf
            case default
              exit
            end select
          end if

          ! output below ground residues
          if ((am0dfl(isr) .eq. 2) .or. (am0dfl(isr) .eq. 3)) then
            if( iallres .eq. 1 ) then
              ! save first residue pool in temporary arrays
              ! allocate temporary arrays for below ground residue
              sum_stat = 0
              allocate(tcumddg(nslay), stat=alloc_stat)
              sum_stat = sum_stat + alloc_stat
              allocate(tmbgz(nslay), stat=alloc_stat)
              sum_stat = sum_stat + alloc_stat
              allocate(tmrtz(nslay), stat=alloc_stat)
              sum_stat = sum_stat + alloc_stat
              if( sum_stat .gt. 0 ) then
                write(*,*) 'ERROR: unable to allocate memory decout temporary arrays'
              end if
              ! save values
              do isz = 1, nslay
                tcumddg(isz) = thisResidue%cumddg(isz)
                tmbgz(isz) = thisResidue%deriv%mbgz(isz)
                tmrtz(isz) = thisResidue%deriv%mrtz(isz)
              end do
            else if( iallres .eq. 2 ) then
              ! write first and second pool layers
              do isz = 1, nslay
                write (luod_below(isr),"(6x,i3,4x,3(f7.2,1x,f7.2,3x))") &
                  isz, tcumddg(isz), thisResidue%cumddg(isz), &
                       tmbgz(isz), thisResidue%deriv%mbgz(isz), &
                       tmrtz(isz), thisResidue%deriv%mrtz(isz)
              end do
            end if
          end if

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        if( iallres .ge. 3 ) then
          ! only printing first three pools
          exit
        end if

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      if ((am0dfl(isr) .eq. 2) .or. (am0dfl(isr) .eq. 3)) then
        if( iallres .eq. 1 ) then
          ! only one residue pool present, print below ground output that was not printed
          ! write first and second pool layers
          do isz = 1, nslay
            write (luod_below(isr),"(6x,i3,4x,3(f7.2,1x,f7.2,3x))") &
               isz, tcumddg(isz), 0.0, &
                    tmbgz(isz), 0.0, &
                    tmrtz(isz), 0.0
          end do
        end if
        if( iallres .gt. 0 ) then
          ! deallocate temporary arrays
          sum_stat = 0
          deallocate( tcumddg, stat=alloc_stat )
          sum_stat = sum_stat + alloc_stat
          deallocate( tmbgz, stat=alloc_stat )
          sum_stat = sum_stat + alloc_stat
          deallocate( tmrtz, stat=alloc_stat )
          sum_stat = sum_stat + alloc_stat
          if( sum_stat .gt. 0 ) then
            write(*,*) 'ERROR: unable to deallocate memory decout temporary arrays'
          end if
        end if
      end if

      return
    end subroutine decout

    subroutine ddbug(isr, nslay, plant)

      ! This program prints out many of the global variables before
      ! and after the call to DECOMP provide a comparison of values
      ! which may be changed by DECOMP

      use weps_main_mod, only: am0ifl
      use datetime_mod, only: get_psimdate
      use file_io_mod, only: luoddb
      use biomaterial, only: plant_pointer, residue_pointer
      use debug_mod, only: tddbug

      !  + + + ARGUMENT DECLARATIONS + + +
      integer :: isr     ! subregion number
      integer :: nslay   ! number of soil layers
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older plant data

      !  + + + LOCAL VARIABLES + + +
      integer :: cd  ! The current day of simulation month.
      integer :: cm  ! The current year of simulation run.
      integer :: cy  ! The current year of simulation run.
      integer :: idx ! index on soil layers.

      type(plant_pointer), pointer :: thisPlant
      type(residue_pointer), pointer :: thisResidue

      integer :: iplt ! plant index
      integer :: ires ! residue pool index
      integer :: iallres ! all residue pools index

     !  + + + DATA INITIALIZATIONS + + +
      if (am0ifl(isr)  .eqv. .true.) then
          tddbug(isr)%tday = -1
          tddbug(isr)%tmo = -1
          tddbug(isr)%tyr = -1
      end if
      call get_psimdate (isr, cd, cm, cy)

      !  + + + END SPECIFICATIONS + + +
      iplt = 0
      iallres = 0

      thisPlant => plant
      do while( associated(thisPlant) )
        ! plant exists
        ! all plant biomass is living, not residue.

        iplt = iplt + 1
        nslay = size(plant%mass%rootfiberz)

        ires = 0

        ! output residues in this plant
        thisResidue => thisPlant%residue
        do while( associated(thisResidue) )

          ires = ires + 1
          iallres = iallres + 1

          ! write weather cligen and windgen variables ??

          if ((cd .eq. tddbug(isr)%tday) .and. (cm .eq. tddbug(isr)%tmo) .and. (cy .eq. tddbug(isr)%tyr)) then
            write(luoddb(isr),"('**',1x,2(i2,'/'),i4,'    After  call to DECOMP       Subregion No. ',i3)") cd,cm,cy,isr
          else
            write(luoddb(isr),"('**',1x,2(i2,'/'),i4,'    Before call to DECOMP       Subregion No. ',i3)") cd,cm,cy,isr
          end if
          write(luoddb(isr),*)

          ! header
          write(luoddb(isr), &
            "('  #p  #r #ar #STstm mST_ST mST_LF mST_RP cumDDS lay massST massLF massRP massRS massRF cumDDF/G')")

          ! first line
          idx = 0
          write(luoddb(isr),"(3(1x,i3),4f7.3,f9.3,1x,i3,5f7.3,f9.3)") iplt, ires, iallres, thisResidue%dstm, &
            thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore, thisResidue%cumdds, &
            idx, thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
            thisResidue%flatrootstore, thisResidue%flatrootfiber, thisResidue%cumddf
          ! soil layers
          do idx=1,nslay
             write(luoddb(isr),"(50x,i3,5f7.3,f9.3)") idx, &
               thisResidue%stemz(idx), thisResidue%leafz(idx), thisResidue%storez(idx), &
               thisResidue%rootstorez(idx), thisResidue%rootfiberz(idx), thisResidue%cumddg(idx)
           end do

          ! go to next older residue in thisPlant
          thisResidue => thisResidue%olderResidue
        end do

        ! go to next older plant
        thisPlant => thisPlant%olderPlant
      end do

      tddbug(isr)%tday = cd
      tddbug(isr)%tmo = cm
      tddbug(isr)%tyr = cy

      return
    end subroutine ddbug

    subroutine decopen(isr)

! + + + Purpose  + + +
!   Write headers for output files
!       dabove.out
!       dbelow.out

      use file_io_mod, only: luod_above, luod_below
      use decomp_data_struct_defs, only: am0dfl

      integer :: isr

! + + + FORMATS + + +
 2030 format( 29x,'Standing',9x,'Flat',9x,'Surface Cover    Silhouett ' &
     &,'Area     Total Residue Amounts')
 2035 format (14x,'Pool',1x,'Stem',1x,2(2x,'decomp',3x,'bio-',1x),2x,   &
     &       15('-'),3x,14('-'),4x,24('-'))
 2040 format ('sr day/mo/yr   no.  no.    days    mass    days    mass',&
     &        '   Stems      Flat    Total      /5     Stand    Flat  ',&
     &'  Buried    ')

!    + + + END SPECIFICATIONS + + +

!     write headers for above ground residues file if requested
      if ((am0dfl(isr) .eq. 1) .or. (am0dfl(isr) .eq. 3)) then
         write (luod_above(isr),*)                                      &
     &         'Above Ground Residue Decomposition Output File'
         write (luod_above(isr),*) 'Standing and Surface Residues'
         write (luod_above(isr),*) '  '
         write (luod_above(isr),2030)
         write (luod_above(isr),2035)
         write (luod_above(isr),2040)
         write (luod_above(isr),*) '  '
      end if

!     write headers for below ground residues file if requested
      if ((am0dfl(isr) .eq. 2) .or. (am0dfl(isr) .eq. 3)) then
         write (luod_below(isr),*)                                      &
     &         'Below Ground Residue Decomposition Output File'
         write (luod_below(isr),*)                                      &
     &         'Data by soil layer for age pools 1 and 2'
         write (luod_below(isr),*) '  '
         write (luod_below(isr),*) '     day/mo/year '
      end if

      return
    end subroutine decopen

end module decomp_out_mod

