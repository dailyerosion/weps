! This is a function to read soil spatial data from file
! into soil array called 'soilDIM'
! @ read from adm_soil.dat
! Updated version by change soilDim and landcov from 2-D into 4D
! updated on 3/12/10 

      subroutine read_soilGIS()

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'airpact/spatialGIS.inc'
      
! define the spatial variable. m as rows and n as columns         
       integer m,n,cellsize
       character *125 soilFile
       real xllcorner, yllcorner
       character *2512 line         
!       real temp(hight,width,12,12)    ! This is a temp variable to hold data from readline
       real temp(401,500)
       integer i, lineNum, colNum
       integer s,t,temp1,temp2

!        soilFile = 'adm_soils.dat'
        soilFile = 'wa_soil_grid.dat' 
        lineNum = 1
        temp1 = 1
        temp2 = 1
                 
       call fopenk(47,rootp(1:len_trim(rootp))//soilFile,'old')
     
  100   read (47,'(a)',end=80) line    
            
          if (line(1:1) .eq. '#') then
             goto 100
          else if ((line(1:) .eq. 'EOF') .or. (line(1:).eq.'End')) then
             goto 300
          end if

           read(line,*) m,n,cellSize  ! read in nrows,colns,and cell size
!           write(*,*) 'Size:',m,n, cellSize
           read (47,'(a)',err=80) line   
!           read(line,*) m   ! re


   ! read the soil spatial data into array     
          s = 1        
                   
  ! read the soil spatial data into soilDim   
  200  do while (lineNum .le. m) 
          read (47,'(a)',end=80) line
          read(line,*) (temp(lineNum,i), i=1,n)
          temp1 = mod(lineNum,12)

          if (temp1 .eq. 0) then 
               s = s+1
               temp1 = 12
          end if  
          colNum = 1 
          t = 1 
          do while (colNum .le. n)
            temp2 = mod(colNum,12)
          
            if ( temp2 .eq. 0) then
                  t = t+1
                  temp2 = 12
            end if       
   
            soilDim(s,t,temp1,temp2)=temp(lineNum,colNum) 
            colNum = colNum + 1
            end do
            lineNum = lineNum+1
        end do
    
     
 300   close(47)                     
 80    write (0,9001)
 9001  format('Done to read the soilGIS file!')
! 1001  format (3i6)
! 1002  format (2f16.8)    
       end                