! This is the main program of weps driver for air quality
! 
      program weps_driver

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

      include  'hydro/dvolwparam.inc 
! define local variables



      integer isr             
     
      integer cd,cm,cy
      integer julday
      integer cnt
      real ci     

! local variables
      integer i,j,idx
      integer m,n
      logical flag_idx(landc_type,soil_comp)  ! 8 is manage tpyes and 200 is soil types
! temp erosion variable to save subregion grid erosion 
      real total_tmp(landc_type,soil_comp)
      real ssp_tmp(landc_type,soil_comp)
      real pm10_tmp(landc_type,soil_comp)
       
      integer*4 start_date(3), start_time(3),end_date(3),end_time(3)
        
       id = 1
       im = 1
       iy = 0002 
       ld = 3
       lm = 1
       ly = 0002
      call idate(start_date)
      call itime(start_time)  
      max_real = huge(1.0) * 0.999150
      max_arg_exp = log(max_real) 
      SURF_UPD_FLG = 1
      xgdpt = 0         !use default grid spacing values if
      ygdpt = 0         !these are not specified on the commandline  

      erod_interval = 0 !default value for updating eroding soil surface
                        !(currently only used in standalone erosion submodel)
      calib_cycle = 0
      max_calib_cycles = 3  ! Default value unless increased via cmdline option
      calib_done = .false.
      wc_type = 0       ! default of water content type
      ntstep = 24
      nsubr= 1
  
      SoilRockFragments(1) = -1  ! Setting default value to -1 (single subregion only for now!!!)
      ci = 0.90    ! default confidence interval value

!     temporarily initialize old random roughness
!      aslrrc(1) = 10.
!      as0rrk(1) = 0.9

      run_erosion = 1 
      init_cycle = 1
      rootp ='c:\usr\test_erosion\test1\'
      
      report_loop = .false.
 
  !    if (btest(am0efl,0)) then
  !     write(*,*) 'Luo_erod',luo_erod
      call fopenk(luo_erod, rootp(1:len_trim(rootp))//                   &
     &  'daily_erosionSP.out','unknown')
  !    endif
      if (btest(am0efl,1)) then
       call fopenk (luo_egrd,rootp//'daily_egrd.out', 'unknown')
      endif
 
      if (calibrate_crops > 3) max_calib_cycles = calibrate_crops

      ijday = julday(id, im, iy)
      ljday = julday(ld, lm, ly)
 !     call cliginit()  ! open clig file
 !     call erodinit()
      
      cnt = 0
     
      call read_soilGIS()
      close (47)
  
      call read_landcov()
    
!      call read_man() ! read the management file based on landcov code

! do loops for 2-D dimension 
! create an index array
! isr = rdx(i,j) 
      idx = 1  
! initial flag index and temp erosion variables
! For 10 landcov types and 200 soil components
      do i=1,landc_type
        do j = 1,soil_comp
         flag_idx(i,j) = .false.
         total_tmp(i,j) = 0.0
         ssp_tmp(i,j) = 0.0
         pm10_tmp(i,j) = 0.0         
        end do
      end do

! soilDIM(i,j) is the unique soil ID
! Assign a temp index for each subregion grid      
       do i=1,12 !row
         do j=1,12 !col

          rdx(i,j) = idx
          idx = idx+1
         end do
       end do
      
      cnt = 0
! run the whole region with 95x95 12-km grids and 12x12 1-km grids      
      do m=1,95
        do n=1,95
 ! initial
           etotal(m,n) = 0.0
           essp(m,n) = 0.0
           epm10(m,n) = 0.0
! run each subgrid with 1-km grid 
            do i = 1,12
             do j = 1,12 
         if (landcov(m,n,i,j) .gt. 0 .AND. rdx(i,j) .gt. 0 .and.        &
     &       soilDim(m,n,i,j) .gt. 0 ) then

          if (flag_idx(landcov(m,n,i,j),soilDim(m,n,i,j)).neqv. .TRUE.) &
     &     then
                  
                  call read_manage(landcov(m,n,i,j)) ! loading manage file for this grid
       
                  call read_soil(m,n,i,j,rdx(i,j))   !loading soil features
!        write(*,*) 'Cond & ID in weps_driver:',cond(1),soilDim(m,n,i,j)    
                  call open_cli_win(rdx(i,j))
   ! If layer is 0, we will not run the weps model
                  if ( nslay(rdx(i,j)) .gt. 0) then 
                  call weps_run(m,n,rdx(i,j))
                  end if 
             
                  rewind (luicli)
 !                 call save_erosion(rdx(i,j))      ! adding codes for this function
                 total_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))=egt(1,1)
                 ssp_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))=egtss(1,1)
                 pm10_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))=egt10(1,1)                
                         
                 flag_idx(landcov(m,n,i,j),soilDim(m,n,i,j)) = .TRUE.
                         
                 cnt = cnt+1

                else
                egt(1,1)=total_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))
                egtss(1,1)=ssp_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))
                egt10(1,1)=pm10_tmp(landcov(m,n,i,j),soilDim(m,n,i,j))       
    !           write(*,*) 'The same soil and landcov is already run!' 
          ! get the erosion from the same landcov and soil
                end if
! add the erosion together in a grid
                 etotal(m,n) = etotal(m,n)+egt(1,1)         
                 essp(m,n) = essp(m,n)+egtss(1,1)
                 epm10(m,n) = epm10(m,n)+egt10(1,1)                                 
              end if                
              end do
             end do
!    write(luo_erod,*)'Eros:',m,n,etotal(m,n),essp(m,n),epm10(m,n) 
        end do
       end do
  !       write(*,*) 'Total running is:',cnt,idx 
      close (luiwin)
      close (luicli)
      close (luo_egrd) 
      close (luo_erod)
      close (luimandate)
      close(42)       
      call idate(end_date)
      call itime(end_time)
      write(*,1000)start_date(2),start_date(1),start_date(3),start_time
      write(*,1000)end_date(2),end_date(1),end_date(3),end_time
1000  format('D:',i2.2,'/',i2.2,'/',i4.4,';T:',i2.2,':',i2.2,':',i2.2)    
! end of subregions   luo_erod	
! Done with simulation here ..................
      end
          
