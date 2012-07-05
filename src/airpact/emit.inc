! Defintion of emit variable

      real aegt(mnsub)
      real aegtss(mnsub)
      real aegt10(mnsub)
     
      real aegtp(mnsub)
      real aegtssp(mnsub)
      real aegt10p(mnsub)

      common /emit/aegt, aegtss,aegt10
      common /prevemit/aegtp, aegtssp, aegt10p

      save :: /emit/, /prevemit/

!   aegt - total soil loss  
!   aegtss - total suspension of soil loss 
!   aegt10 - total  loss of <10 pm soil particle
!   aegtp - previous day total loass of soil 
!   aegtssp - previous day suspension loss 
!   aegt10p - previous day loass of <10 pm particle
