 

      subroutine write_plotfile(plotfile,stdist,ydist,dstot,count)
!
!     write_plotfile()
!
!     This writes information to the WEPP plot output file.
!
      
      integer, intent(in) :: plotfile
      real, intent(in) :: stdist(*)
      real, intent(in) :: ydist(*)
      real, intent(in) :: dstot(*)
      integer, intent(in) :: count
      
      integer i
      
      write (plotfile,2200)
!     
      do 40 i = 1, count
         write (plotfile,2300) stdist(i), ydist(i), dstot(i)
   40 continue
   
      return
      
 2200 format (1x,'dist. downslope',1x,'elevation',1x,'soil loss'/,/,    &
     &    '   (meters) ',1x,' (meters) ',1x,'(kg/m**2)'/)
 2300 format (1x,f10.3,1x,f10.3,1x,f10.3)
 
      end