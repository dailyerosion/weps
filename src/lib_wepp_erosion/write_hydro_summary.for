      subroutine write_hydro_summary(sumfile,totalPrecip,precipEvents,  &
     & totalRunoff,runoffEvents,totalSnowrunoff, snowmeltEvents,years)
      
      
      integer, intent(in) :: sumfile
      real, intent(in) :: totalPrecip,totalRunoff, totalSnowrunoff
      integer, intent(in):: precipEvents, runoffEvents,snowmeltEvents
      integer, intent(in) :: years
      
      write(sumfile,1050)
      write(sumfile,1200) 1,years
      write(sumfile,1350) precipEvents,totalPrecip,runoffEvents,        &
     &    totalRunoff,snowmeltEvents,totalSnowrunoff
     
      write(sumfile,1650) years,totalPrecip/years,totalRunoff/years,    &
     &  totalSnowrunoff/years
      
      return 
      
 1050 format(//'AVERAGE ANNUAL SUMMARIES',/,72('-'))
 1200 format (//'I.   RAINFALL AND RUNOFF SUMMARY',/,5x,8('-'),1x,3('-'
     1    ),1x,6('-'),1x,7('-'),//,6x,'total summary: ',' years ',i4,
     1    ' - ',i4)
 1350 format(/5x,i5,
     1    ' storms produced                       ',f9.2,
     1    ' mm of precipitation',/,5x,i5,
     1    ' rain storm runoff events produced     ',f9.2,
     1    ' mm of runoff',/,5x,i5,
     1    ' snow melts and/or',/,10x,
     1    '   events during winter produced       ',f9.2,
     1    ' mm of runoff',/)  
 1650 format (6x,'annual averages'/6x,'---------------'//6x,
     1    '  Number of years                              ',
     1    3x,i4,/,6x,
     1    '  Mean annual precipitation                    ',
     1    f7.2,1x,'mm',/,6x,
     1    '  Mean annual runoff from rainfall             ',
     1    f7.2,1x,'mm',/,6x,
     1    '  Mean annual runoff from snow melt',/,6x,
     1    '    and/or rain storm during winter            ',
     1    f7.2,1x,'mm',/)   
      
      
      end
      
      
      