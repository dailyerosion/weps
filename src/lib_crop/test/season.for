!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!     + + + PURPOSE + + +
!     This is the MAIN program for growth season parameter optimization
!     Its purpose is to search parameter space for values which result
!     in a crop growth period most closely matching the actual planting
!     and harvest dates for multiple locations

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'w1clig.inc'
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'util/misc/f2kcli.inc'

!     + + + LOCAL VARIABLES + + +
      integer MAX_LOCATIONS, MAX_DAYS
      parameter (MAX_LOCATIONS = 20)
      parameter (MAX_DAYS = 730)
      character*512 argv    ! For Fortran 2k commandline parsing
      integer       index, numarg, ln, ll, ss
      logical       fexist

      ! planting and harvest dates
      integer plantdate(MAX_LOCATIONS), harvdate(MAX_LOCATIONS)

      ! entered low and optimum growth temperatures
      real lowgrowt_db(MAX_LOCATIONS), optgrowt_db(MAX_LOCATIONS)
      real lowgrowt_orig, optgrowt_orig

      ! project run file names
      character*65 projname(MAX_LOCATIONS)

      ! minimum and maximum monthly air temperature arrays
      integer ntemp, jloop, kloop
      real dy_mon(14), jday
      real max_mon_airt(14), min_mon_airt(14)
      real max_mon_airt2(14), min_mon_airt2(14)
      real yp1, ypn
      real max_air_temp(MAX_LOCATIONS,MAX_DAYS)
      real min_air_temp(MAX_LOCATIONS,MAX_DAYS)

      real lowgrowt, optgrowt
      real minrangelowt, maxrangelowt
      real minrangeoptt, maxrangeoptt
      real genran
      integer seed(1)

      real heat_sum(MAX_LOCATIONS)
      real best_heat_sum(MAX_LOCATIONS)
      real best_lowgrowt, best_optgrowt

      real loc_avg_heat_sum, max_heat_sum, min_heat_sum
      real best_loc_nvar_heat_sum, loc_nvar_heat_sum

!     + + + LOCAL DEFINITIONS + + +


!     + + + SUBROUTINES CALLED + + +
!     inprun   -  read input run file and set file names for other input files
!     mfinit   -  Management initialization subroutines
!     GET_COMMAND_ARGUMENT - pull in command line arguments

!     + + + FUNCTIONS CALLED + + +
!     COMMAND_ARGUMENT_COUNT - find out how many command line arguments
      real huc1   ! heat unit calculations

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!     1 = simulation run file
!     5 = Reserved
!     6 = Reserved - screen
!     7 = Reserved
!    10 = management (tillage) run file

!     + + + DATA INITIALIZATIONS + + +
      ! for spline fit, middle of month day
      data dy_mon /-15,15,45,74,105,135,166,196,227,258,288,319,349,380/

!     + + + END SPECIFICATIONS + + +

!     read project names from command line and process one at a time

      numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call

      if (numarg .gt. 0) then
        do index = 1, numarg
          call GET_COMMAND_ARGUMENT(index,argv,ll,ss)  !Fortran 2k compatible call

          ! read project name and place in rootp array
          ! Check to see if trailing '/' is there - LEW
          ln = len_trim(argv(1:))
          if (ln .ne. 0) then
             rootp = trim(argv(1:))
             projname(index) = trim(rootp(1:65))
             if (rootp(ln:ln) .ne. '/') then
                rootp = trim(argv(1:)) // '/'   !add trailing '/' character
             endif
          else
             write(*,*) 'Ignoring invalid WEPS root dir: ', trim(argv)
          endif

          ! check for existence of simulation run file
          runfil = rootp(1:len_trim(rootp)) // 'weps.run'
          inquire(file = runfil(1:len_trim(runfil)), exist = fexist)
          if (.not.fexist) then
              stop ' simulation run file not found '
          end if

          ! load the simulation run file
          call inprun

          ! Initialize the management file and rotation counters
          maxper = 1
          call mfinit(1, tinfil, maxper)
          close(lui1)
          close(luiwin)

          ! find planting and harvest dates from management file
          call read_man( plantdate(index), harvdate(index),             &
     &        lowgrowt_db(index), optgrowt_db(index) )

          ! load monthly temperatures from cligen file
          call cliginit
          close(luicli)

          ! initialize spline fit to cligen data
          do jloop=1,12
              max_mon_airt(jloop+1) = awtmxav(jloop)
              min_mon_airt(jloop+1) = awtmnav(jloop)
          end do
          max_mon_airt(1) = awtmxav(12)
          max_mon_airt(14) = awtmxav(1)
          min_mon_airt(1) = awtmnav(12)
          min_mon_airt(14) = awtmnav(1)

          ntemp = 14      ! number of monthly temperature values used in interpolation
          yp1 = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)
          ypn = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)

          ! initialize interpolation scheme
          call spline(dy_mon,max_mon_airt,ntemp, yp1,ypn, max_mon_airt2)
          call spline(dy_mon,min_mon_airt,ntemp, yp1,ypn, min_mon_airt2)

          ! interpolate into 2 year daily temperature arrays
          do jloop = 1, 365
              jday = jloop
              ! calculate daily temps. and he
              call splint(dy_mon, max_mon_airt, max_mon_airt2, ntemp,   &
     &                    jday, max_air_temp(index,jloop))
              call splint(dy_mon, min_mon_airt, min_mon_airt2, ntemp,   &
     &                    jday, min_air_temp(index,jloop))
              max_air_temp(index,jloop+365) = max_air_temp(index,jloop)
              min_air_temp(index,jloop+365) = min_air_temp(index,jloop)
          end do

        end do

        ! check that all growth temperatures in file are equal
        lowgrowt_orig = lowgrowt_db(1)
        optgrowt_orig = optgrowt_db(1)
        do index = 2, numarg
          if( (lowgrowt_orig.ne.lowgrowt_db(index)) .or.                &
     &        (optgrowt_orig.ne.optgrowt_db(index)) ) then
            write(*,*) "Not all projects are using same database"
            stop 1003
          end if
        end do
      else
        write(*,*) "No project file specified on the command line"
      endif

      ! write results for tests
!      write(*,*) "maximum daily air temperatures"
!      write(*,*) "day"
!      do jloop = 1, 365
!           write(*,*) jloop, max_air_temp(1:numarg,jloop)
!      end do
!      write(*,*) "minimum daily air temperatures"
!      write(*,*) "day"
!      do jloop = 1, 365
!           write(*,*) jloop, min_air_temp(1:numarg,jloop)
!      end do
!      write(*,*) "planting days"
!      write(*,*) plantdate(1:numarg)
!      write(*,*) "harvest days"
!      write(*,*) harvdate(1:numarg)
!      write(*,*) "Low growth Temp ", lowgrowt_orig,                     &
!     &           "Optimum Growth Temp", optgrowt_orig

      ! test different high and low temperature combinations
      ! for best fit to areas where crop is grown
      minrangelowt = lowgrowt_orig - 3.0
      maxrangelowt = lowgrowt_orig + 3.0
      minrangeoptt = optgrowt_orig - 3.0
      maxrangeoptt = optgrowt_orig + 3.0

      seed = 1
      call random_seed(put=seed)

      best_loc_nvar_heat_sum = 1.0e37
      do jloop = 1, 100000
        call random_number(genran)
        lowgrowt = minrangelowt + genran * (maxrangelowt-minrangelowt)
        call random_number(genran)
        optgrowt = minrangeoptt + genran * (maxrangeoptt-minrangeoptt)
        if( lowgrowt+5.0.lt.optgrowt ) then

          ! sum growing degree days for each location between
          ! planting and harvest dates
          do index = 1, numarg
            heat_sum(index) = 0
            do kloop = 1, MAX_DAYS
              if( (kloop.gt.plantdate(index)) .and.                     &
     &            (kloop.lt.harvdate(index)) ) then
                heat_sum(index) = heat_sum(index) +                     &
     &            huc1 (max_air_temp(index,kloop),                      &
     &                  min_air_temp(index,kloop), optgrowt, lowgrowt)
              end if
            end do
          end do

          ! check normalized measure difference between locations

          ! average heat sum across locations
          loc_avg_heat_sum = 0
          max_heat_sum = 0
          min_heat_sum = 1.0e37
          do index = 1, numarg
            loc_avg_heat_sum = loc_avg_heat_sum + heat_sum(index)
            min_heat_sum = min(min_heat_sum, heat_sum(index))
            max_heat_sum = max(max_heat_sum, heat_sum(index))
          end do
          loc_avg_heat_sum = loc_avg_heat_sum / numarg

          ! normalized variation of this set of heat sums
          if( loc_avg_heat_sum.gt.0.0 ) then
            loc_nvar_heat_sum = (max_heat_sum - min_heat_sum)           &
     &                        /  loc_avg_heat_sum
            if( loc_nvar_heat_sum .lt. best_loc_nvar_heat_sum ) then
              best_loc_nvar_heat_sum = loc_nvar_heat_sum
              do index = 1, numarg
                best_heat_sum(index) = heat_sum(index)
              end do
              best_lowgrowt = lowgrowt
              best_optgrowt = optgrowt
              ! write out progress on search
              write(*,*) jloop, loc_nvar_heat_sum, lowgrowt, optgrowt,  &
     &                   loc_avg_heat_sum
            end if
          end if
        end if

      end do

      ! write out a summary of the best values found
      do index = 1, numarg
        write(*,*) projname(index), best_heat_sum(index)
      end do

      stop 
      end

      subroutine read_man( pdate, hdate, lowgrowt, optgrowt )

      integer pdate, hdate
      real lowgrowt, optgrowt

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1sim.inc'
      include 'main/main.inc'
      include 'manage/man.inc'
      include 'manage/oper.inc'

!     + + + LOCAL VARIABLES + + +
      integer cd, cm, cy, end_init_jday, sr, day, month, year
      integer prcode
      character*1 prdumy
      character*30 prname
      character*256 line
      real temp1, temp2, temp3, temp4, temp5

!     + + + FUNCTIONS CALLED + + +
      integer julday
      integer difdat

      sr = 1
      pdate = 0
      hdate = 0

      ! determine initial day of the simulation
      ijday = julday(id, im, iy)

      ! calculate last julian date for full management cycle
      cd = 31
      cm = 12
      cy = iy + maxper - 1
      end_init_jday = julday(cd, cm, cy)

      ! loop through management file until the first planting
      ! and harvest date pair have been identified.
      do am0jd = ijday, end_init_jday
         call caldatw (cd, cm, cy)
         line = mtbl(mcur(sr))

         ! If we aren't pointing at a date, we have a problem
         if (line(1:1).ne.'D') goto 901

         ! read day, month and year from date line (checks format)
         read (line (3:12),'(i2,1x,i2,1x,i4)', err=902) day,month,year

         ! if today then evaluate, else next day
         if (difdat (cd,cm,cy,day,month,year).eq.0) then

             ! Move the tbl ptr to the first operation after the date
  10         mcur(sr) = mcur(sr) + 1
             line = mtbl(mcur(sr))
             select case (line(1:1))
             case ('O')
                 continue
             case ('G')
                 continue
             case ('P')
                 line = mtbl(mcur(sr))
                 read(line, 1001, err=901) prdumy, prcode, prname
 1001            format(a1,1x,i2,1x,a)
                 select case (prcode)
                 case (32, 33, 37, 38, 61)
                     hdate = am0jd - ijday + 1
                 case (51)
                     pdate = am0jd - ijday + 1
                     ! parse for growth temperatures
                     mcur(sr) = mcur(sr) + 4
                     line = mtbl(mcur(sr))
                     read(line(2:len_trim(line)), *, err=904)           &
     &                   temp1, temp2, lowgrowt, optgrowt,              &
     &                   temp3, temp4, temp5
                 end select
                 if( (pdate.gt.0) .and. (hdate.gt.0) ) then
                     if( hdate.le.pdate ) then
                         hdate = hdate + 365
                     end if
                     return
                 end if
             case ('D')
                 read (line (3:12),'(i2,1x,i2,1x,i4)', err=902)         &
     &                                                    day,month,year
                 if (difdat (cd,cm,cy,day,month,year).ne.0) goto 20
             case ('*')
                 mcount(sr) = mcount(sr) + 1
                 mcur(sr) = mbeg(sr)
  101            mcur(sr) = mcur(sr) + 1
                 line = mtbl(mcur(sr))
                 if (line(1:1).ne.'D') goto 101
                 return
             case ('+')
                 continue
             case default
                 goto 903
             end select
             goto 10

         end if
   20    continue

      end do

      ! Valid planting and harvest dates were not found
      return

901   write(*,*) 'Enter manage not pointing at date'
      stop 1002
902   write(*, 9902) line, sr
9902  format('Bad date format ',a,' in region ',i2)
      stop 1002
903   write(*,*) 'Invalid management code -', line (1:1)      
      stop 1002
904   write(*,*) 'planting operation error -', line (1:1)      
      stop 1002

      end
