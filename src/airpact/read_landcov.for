! This is a function to read landcov spatial data from file
! into landcov array called 'landcov'
! @ read from adm_soil.dat
 
      subroutine read_landcov()

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'airpact/spatialGIS.inc'
      
! define the spatial variable. m as rows and n as columns         
       integer m,n,cellsize
       character *125 landcovFile
       real xllcorner, yllcorner
       character *2512 line         
       integer i,j, lineNum
       integer s,t, colNum, temp1, temp2
  !     real temp(hight,width,12,12)    ! This is a temp variable to hold data from readline
! There is a char limitation for each line to read in
        real temp(401,500)  
        landcovFile = 'wa_landc_clip.dat'
        lineNum = 1
        temp1 = 1
        temp2 = 1
        
       call fopenk(48,rootp(1:len_trim(rootp))//landcovFile,'old')
     
  100   read (48,'(a)',err=80) line    
            
          if (line(1:1) .eq. '#') then
             go to 100
          else if ((line(1:) .eq. 'EOF') .or. (line(1:).eq.'End')) then
             goto 300           
          end if

           read(line,*) m,n,cellSize  ! read in nrows,colns,and cell size
           
        
           read (48,'(a)',err=80) line
!           read(line,*) m   ! read in nrows
           read(line,*) xllcorner, yllcorner
          
          s = 1
  
      
 200    do while (lineNum .le. m) 
          read (48,'(a)',err=300) line
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
            landcov(s,t,temp1,temp2)=temp(lineNum,colNum) 
            colNum = colNum + 1
          end do                      
          lineNum = lineNum+1
        end do
  !     write(*,*) 'landcov data at (s,t)', landcov(48,2)                    
 80    write (0,9001)
 9001  format('Done in Landcov file!')
! 1001  format (3i6)
! 1002  format (2f16.8)      
 300   close (48)
       end                