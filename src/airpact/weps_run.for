! This is the main program of weps driver for airquality
! 
      subroutine weps_run (a,b,isr)

      USE pd_dates_vars
      USE pd_update_vars
      USE pd_report_vars
      USE pd_var_tables
      USE mandate_vars    ! Load shared mandates() array

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'p1unconv.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1sgeo.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'w1pavg.inc'
      include 'file.inc'


      include 'b1glob.inc'
      include 'd1glob.inc'
      include 'c1glob.inc'
      include 'c1db1.inc'
      include 'h1hydro.inc'
      include 'timer.inc'
      include 'command.inc'   !declarations for commandline args
      include 'precision.inc' !declaration for portable math range checking

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'manage/man.inc'
      include 'manage/oper.inc'
      include 'decomp/decomp.inc'
      include 'erosion/p1erode.inc' !Needs the SURF_UPD_FLG variable
      include 'erosion/m2geo.inc'   !Need tsterode cmdline arg vars(xgdpt,ygdpt)
      include 'erosion/e2erod.inc'
      include 'airpact/emit.inc'
      include 'airpact/spatialGIS.inc' 
 ! parameter variables   
      integer a,b, isr

! define local variables
      logical init_flag
      integer get_nperiods
      integer pd, nperiods
      integer ndiy             
     
      integer cd,cm,cy
      integer dayear, julday, lstday
      logical isleap
      integer iostat
  
    
! a - an regional index representing the number of row        
! b - an regional index representing the number of column   
! isr - the index within the regional grid

! initalizae
             
!         call input(isr)  ! open runfile for each grid 
! no need for input function, but not space dimension info and altitute info 
!          call save_soil(isr)
!         call input_ifc(isr)  
!read directly from soil attribute tables
   
      
         ac0shoot(1) = 0.0; acdpop(1) = 0.0
        
!      call fopenk(luo_erod, rootp(1:len_trim(rootp))//'erosion.out',     &
!     &  'unknown') 
        
         tinfil = rootp(1:len_trim(rootp))//tinfil
         call mfinit(isr, tinfil, maxper)
     
!     check for consistency maxper, n_rot_cycles, number of years to run
      if( maxper*n_rot_cycles .ne. ly-iy+1 ) then
          write(*,*) 'Warning: Number of rotations (',n_rot_cycles,') ',&
     &               'times Years in rotation (',maxper,') ',           &
     &               ' does not match Number of simulation years (',    &
     &               ly-iy+1,') '
          n_rot_cycles = (ly-iy+1) / maxper
          if( mod( (ly-iy+1), maxper ) .gt. 0 ) then
              write(*,*) 'Warning: Not simulating complete rotations'
              n_rot_cycles = n_rot_cycles + 1
          end if
      end if
    
      if (calibrate_rotcycles .eq. 0) then
         calibrate_rotcycles = n_rot_cycles
      endif
     
!     This is all the initialization for the new output reporting code
      call mandates(isr)  !Get man dates, op names, and crop names
    
      nperiods = get_nperiods(maxper)   !Get # of periods for reports
      if( report_debug >= 1 ) then
          write(*,*) '# rot years', maxper, "nperiods", nperiods,       &
     &    '# cycles', n_rot_cycles
      end if

!      call init_report_vars(nperiods, maxper, n_rot_cycles)
     
      call weps_init(isr)  ! running the submodel each day
   
!      write(*,*) 'aseags1:',aseags(1,isr)
!     calculate first and last Julian dates for simulation
      ijday = julday(id, im, iy)
      ljday = julday(ld, lm, ly)
  
!     calculate last julian date for initialization cycle
    
      am0csr = 1  ! set global current subregion variable?

      if (init_cycle > 0) then   ! to avoid printing it when not being done
          write(6,*) "Starting initialization phase"
      else
          lopyr = 1
      endif

      ! begin initialization simulation phase
      init_loop = .true. ! Signifies that we are in the "initialization" loop
      
      daysim = 0
! running the date loop 

      do am0jd = ijday, ljday
         daysim = daysim + 1
        call caldatw (cd, cm, cy)
        call getcli(cd,cm,cy)
        call getwin(cd,cm,cy)
 
       
!  Initial erosion variables           
        egt(1,1) = 0.0
        egtss(1,1) = 0.0
        egt10(1,1) = 0.0
       if (am0jd .eq. ijday) then
                    
         ndiy = 365; if (isleap(cy) .eqv. .true.) ndiy = 366
         
         call submodels(isr,cd,cm,cy)
   
        ! write decomposition biomass pool amounts to files
        ! if last day of year, check for end of rotation
         if (dayear(cd,cm,cy) .eq. ndiy) then
            ! check if at end of subregion's rotation cycle
            if (mod(amnryr(isr),maxper) == 0) then
               amnryr(isr) = 1
               lopyr = amnryr(isr)
               amnrotcycle(isr) = amnrotcycle(isr) + 1
            else
               amnryr(isr) = amnryr(isr) + 1
               lopyr = amnryr(isr)
            end if
         end if
! From else part
         amnrotcycle(isr) = 1   ! set here for use in confidence interval calculation (no other use?)
         am0sif = .false.  ! Done with all initialization and calibration phases
         report_loop = .true.
       else 
          
          call caldat(am0jd-1,cd,cm,cy)  ! call the data from previous day
          call inpSaveFile(a,b,isr,cd,cm,cy)
          call caldat(am0jd,cd,cm,cy)
      
          call submodels(isr,cd,cm,cy)        
        end if
          call erodinit(isr)
      if (run_erosion > 0) then
       if (awudmx .gt. 8.0) then
      
        call calcwu
        call erosion(isr,5.0)
      
          if (egt(1,1) .ne. 0) then 
       write(luo_erod,*) cd,cm,cy,egt(1,1),egtss(1,1),egt10(1,1)
         end if       
!             if (btest(am0efl,0) .or. btest(am0efl,1)) then
              call daily_erodout (luo_egrd, luo_erod)
 !             write(*,*) 'done daily on date!',cm,cd,cy
!          endif
         end if
      end if
       
      call caldatw(cd,cm,cy)

           if (egt(1,isr).gt. 0 .or. egtss(1,isr) .gt. 0 .or.            &
     &     egt10(1,isr) .gt. 0)  then
       write(luo_egrd,*)cm, cd,cy,egt(1,isr),egtss(1,isr),egt10(1,isr)  
           end if                  
     
      call plotstate(a,b,isr,cd,cm,cy)  ! save into a text file for testing
      call plotstate2(a,b,isr,cd,cm,cy) ! write into a temp file which will be
!				     called by inpSaveFile()
!       if (egtss(1,1) .NE. 0)  then
!      write(*,*) 'EROSION:',isr,cd,cm,cy,egtss(1,1),egt(1,1),egt10(1,1)
!       end if
     

      end do 
     
      deallocate (mandate)
  !    end do  
   
! end of subregions	
! Done with simulation here ..................
      end
          
