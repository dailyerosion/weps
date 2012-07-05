
      subroutine inpwepp

      include 'wepp_erosion.inc'
      
      integer linnum,typidx,luiwp
      character     line*256
      integer l
      character weppfil*512
      integer ios,av,pts
      real len,wid,s1,s2
      
      data weppfil /"wepp.run"/
      
      luiwp = 300
      write (*,*) 'WEPP runfil is ', '>>',                              &
     &  weppfil(1:len_trim(weppfil)), '<<'
     
      open(luiwp,FILE=weppfil(1:len_trim(weppfil)),STATUS='old',        &
     &  ERR=99, IOSTAT=ios, recl=1024)
      goto 101
    
!     wepp.run file does not exist, need to create one in the current
!     directory. This is the last file loaded so shoule be able to use the 
!     average slope and length, width of field to create a slope file.
  99  open(luiwp,FILE='wepp.run',STATUS='REPLACE', IOSTAT=ios)
      len = 20.9
      wid = 6.0
      s1 = 0.0
      s2 = 0.05
      av = 0
      pts = 2
      write(luiwp,2000) pts,len,s1,s1,len,s2,av
      close(luiwp)
       
 101  call fopenk (luiwp, weppfil(1:len_trim(weppfil)), 'old')
      linnum = 0
      typidx = 0
 100  linnum = linnum + 1
      if (typidx.ge.3) goto 200
      read (luiwp,'(a)',err=80) line
!
! skip comment lines
      if (line(1:1) .eq. '#') go to 100
      
      typidx = typidx + 1
  
      if (typidx.gt.3) goto 200
      
      select case (typidx)
     
      case (1)
!       read WEPP Slope inputs, this is two lines of data
!       first, number of slope points
        read (line,*,err=80) wp_nslpts
           
      case(2)
!       second, fractional distance and steepness pairs      
        read (line,*,err=80) (wp_xinput(l),wp_slpinp(l),l = 1,wp_nslpts)
           
      case(3)
!       type of wepp output
        read (line,*,err=80) wp_detailout 
      
      end select
     
      goto 100
      
      
     
80    write(0,9001) weppfil, linnum, typidx, line
9001  format('Error in file ',a,' on line #',i4,i3,' ',a)
      call exit(1)     
      
      
200   close (luiwp)
      return
2000  format(i2,' ',f10.2, /,f10.2,' ',f10.2,' ',f10.2,' ',f10.2,/,     &
     &     i2,/)
      end