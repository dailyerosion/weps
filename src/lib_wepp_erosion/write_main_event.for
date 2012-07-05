      subroutine write_main_event(sumfile,cd, cm, cy, precp,            &
     &        runoff,                                                   &
     &      irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,      &
     &      enrato,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,      &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     &     depmin, pdpmin,ndetach,ndepos, npart,frac,frcflw, dia,       &
     &     spg,frsnd,frslt, frcly,frorg,slplen,fwidth,avgslp,           &
     &     stdist,dslost,years,annualavg)
     
     
      use wepp_interface_defs
      
      integer, intent(in) :: sumfile,cd,cm,cy,ndetach,ndepos,npart
      real, intent(in) :: precp,runoff,avedet,maxdet,ptdet,avedep
      real, intent(in) :: maxdep,ptdep,avsole,enrato,irdgdx
      real, intent(in) :: detpt1(*), detpt2(*), dtavls(*)
      real, intent(in) :: detstd(*), detmax(*), pdtmax(*)
      real, intent(in) :: detmin(*), deppt1(*), deppt2(*)
      real, intent(in) :: dpavls(*), depstd(*), depmax(*)
      real, intent(in) :: pdpmax(*), depmin(*), pdpmin(*)
      real, intent(in) :: pdtmin(*)
      real, intent(in) :: frac(*),frcflw(*),dia(*), spg(*)
      real, intent(in) :: frcly(*), frslt(*), frsnd(*), frorg(*)
      real, intent(in) :: slplen,fwidth,avgslp
      real, intent(in) :: stdist(*),dslost(*)
      integer, intent(in) :: years
      integer, intent(in) :: annualavg
      
!

!     write_main_event()
!
!     This subroutine writes out the information in the main WEPP summary
!     file. It is a collection from various places in the original WEPP
!     code.
!     There are two options for this output: detailed and summary
!     For detailed output this can be called after each event, for summary
!     output it is called at the end of the simulation.
!
      integer i
      character*4 mths(12)
      integer defaultHill,defaultOFE
      real areaHa
      data mths /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
      
      defaultHill = 1
      defaultOFE = 1 
      
!      real hyd0, hyd1, hyd2, hyd3, hyd4, hyd5, hyd6, hyd7, hyd8, hyd9,
!     1     hyd10

!
!    section 1 - hydrology event data   
      if (annualavg.eq.0) then   
        write(sumfile,1000) defaultHill
        write(sumfile,1100) defaultOFE,mths(cm), cd, cy
        write(sumfile,1700)
      endif

!     section - 2 on-site effects
      write(sumfile,2500)
!     detachment section(s)      
      write(sumfile,2600)
      write(sumfile,2750) avedet
      write(sumfile,2850) maxdet,ptdet
      write(sumfile,2950) irdgdx,defaultOFE
      write(sumfile,3000)
      
      do i=1,ndetach
         write (sumfile,3350)detpt1(i),detpt2(i),
     1               dtavls(i),detstd(i),detmax(i), pdtmax(i),
     1               detmin(i), pdtmin(i)
         
      end do
      
!     deposition section(s) only printed if there was at least one deposition region      
      if (ndepos.gt.0) then
        write(sumfile,3700)
        write(sumfile,3750) avedep
        write(sumfile,3800) maxdep,ptdep
        write(sumfile,3850)
      
        do i=1,ndepos
          write (sumfile,3350)deppt1(i), deppt2(i),                      &
     &                   dpavls(i), depstd(i), depmax(i),               &
     &                   pdpmax(i), depmin(i), pdpmin(i) 
         
        end do
      end if

!     section 3 - plot info
      write(sumfile,3100)
      write(sumfile,3200)
      write(sumfile,3300)
      write(sumfile,3400)
      write(sumfile,3500)
      
!     3 columns, 34 entries in first 2, 32 entries in last column      
      do i=1,32
         write(sumfile,3550) stdist(i), dslost(i),                      &
     &                       stdist(i+34), dslost(i+34),                &
     &                       stdist(i+68), dslost(i+68)
      end do
          
      write(sumfile,3580) stdist(33), dslost(33), stdist(67), dslost(67)
      write(sumfile,3580) stdist(34), dslost(34), stdist(68), dslost(68)

      write(sumfile,3600)
      
!     section 4 -     sediment leaving and characteristics
      write(sumfile,4000)
      areaHa = (fwidth * slplen) / 10000.0
      if (annualavg.eq.0) then
          write(sumfile,4100) mths(cm), cd, cy, avsole
      else
          write(sumfile,4110) avsole,avsole*fwidth,fwidth,              &
     & ((avsole*fwidth)/1000.0)/areaHa, areaHa
      endif
      
!      write(sumfile,4150) defaultOFE,0
      write(sumfile,4600)
      write(sumfile,4200)
      
      do i=1,npart        
         write(sumfile,4400) i,dia(i)*1000.0,spg(i),frsnd(i)*100,       &
     &      frslt(i)*100, frcly(i)*100,frorg(i)*100,frac(i),frcflw(i)
      end do
            
      write(sumfile,4300)
      if (annualavg.eq.0) then
         write(sumfile,4550) mths(cm), cd, cy,enrato
      else
         write(sumfile,4500) enrato
      endif
!     --------------

 
 
 !
 !    Main event information, only printed if summary file has detailed event output
 !
 1000 format (///'HILLSLOPE ',i1,' RESULTS',/,19('-'),//
     1    'I.   ABBREVIATED EVENT-BY-EVENT HYDROLOGY',/,5x,11('-'),1x,14
     1    ('-'),1x,9('-'))
 1100 format (/7x,'Overland flow element number:',i3/7x,'Event date: ',1
     1    x,a3,1x,i2,', year ',i4)
 1300 format (/,7x,'precipitation amount',f8.2,7x,'rainfall amount    ',
     1    f8.2/,7x,'snow melt amount    ',f8.2,7x,'runoff amount      ',
     1    f8.2/,7x,'rain/melt duration  ',f8.2,7x,'effective duration ',
     1    f8.2/,7x,'peak runoff rate    ',f8.2,7x,'effective length   ',
     1    f8.2/)
     
 1700 format (7x,'note: amounts = mm, durations = min, rates = mm/hr',
     1    ', length = meters'/)
 

 !
 !  Areas of net soil or deposition along profile
 !
 2500 format (//'II.  ON SITE EFFECTS  ON SITE EFFECTS',
     1    '  ON SITE EFFECTS',/,5x,(3(15('-'),2x))/)  
 2600 format (/2x,'A.  AREA OF NET SOIL LOSS')     
 2750 format (/6x,'** Soil Loss (Avg. of Net Detachment',' Areas) = ',f8
     1    .3,' kg/m2 **')
 2850 format (6x,'** Maximum Soil Loss  = ',f8.3,' kg/m2 at ',f7.2,
     1    ' meters **'/)
 2950 format (6x,'** Interrill Contribution = ',f8.3,' kg/m2 ',
     1    ' for OFE #',i2)   
 3000 format (/,6x,'Area of',4x,'Soil Loss',3x,'Soil Loss',3x,'MAX',3x,
     1    'MAX Loss',3x,'MIN',3x,'MIN Loss',/,6x,'Net Loss',6x,'MEAN',6
     1    x,'STDEV',6x,'Loss',4x,'Point',4x,'Loss',3x,'Point',/,8x,
     1    '(m)',7x,'(kg/m2)',5x,'(kg/m2)',2x,'(kg/m2)',4x,'(m)',4x,
     1    '(kg/m2)',2x,'(m)',/,72('-'))   
 3350 format (f7.2,'-',f7.2,1x,f8.3,2x,f8.3,2x,f9.3,1x,f7.2,2x,f8.3,2x,
     1    f7.2)  
 3700 format (/2x,'B.  AREA OF SOIL DEPOSITION') 
 3750 format (/6x,'** Soil Deposition (Avg. of Net Deposition',
     1    ' Areas) = ',f9.3,' kg/m2 **')  
 3800 format (6x,'** Maximum Soil Deposition  = ',f9.3,' kg/m2 at ',f7
     1    .2,' meters **'/) 
 3850 format (6x,'Area of',4x,'Soil Dep',3x,'Soil Dep',5x,'MAX',4x,
     1    'MAX Dep',3x,'MIN',3x,'MIN Dep',/,6x,'Net Dep',7x,'MEAN',5x,
     1    'STDEV',7x,'Dep',5x,'Point',4x,'Dep',4x,'Point',/,8x,'(m)',7x,
     1    '(kg/m2)',4x,'(kg/m2)',3x,'(kg/m2)',4x,'(m)',4x,'(kg/m2)',2x,
     1    '(m)',/,72('-'))              
     
!

!    Plot of soil loss or depsition along profile
!
 3100 format (///2x,'C.  SOIL LOSS/DEPOSITION ALONG SLOPE PROFILE',//,10
     1    x,'Profile distances are from top to bottom of hillslope'//)
 3200 format (1x,'distance',1x,'soil',10x,'distance',3x,'soil',
     1    10x,'distance',3x,'soil')  
 3300 format (4x,'(m)',3x,'loss',13x,'(m)',5x,'loss',13x,
     1    '(m)',5x,'loss')  
 3400 format (9x,'(kg/m2)',18x,'(kg/m2)',18x,'(kg/m2)')
 3500 format (72('-'),/)  
 3550 format (3(5x,f7.2,5x,f7.3))
 3580 format (2(5x,f7.2,5x,f7.3))
 3600 format (/'note:  (+) soil loss - detachment     (-) soil loss',
     1    ' - deposition')

!
!    Offsite effects and sediment enrichment information
!
 4000 format (///'III. OFF SITE EFFECTS  OFF SITE EFFECTS',
     1    '  OFF SITE EFFECTS',/,5x,(3(16('-'),2x))/)
 4100 format (5x,'A.  SEDIMENT LEAVING PROFILE for ',a3,1x,i3,1x,i4,1x,
     1    f9.3,' kg/m'/)
 4110 format (5x,'A.  AVERAGE  ANNUAL SEDIMENT LEAVING PROFILE  ',/,    &
     &    14x,f10.3, ' kg/m of width',/,14x,f10.3,' kg (based on a ',   &
     &    'profile width of ',f7.3,' m)',/,14x,f10.3,' t/ha (assuming'  &
     &    ' contributions from ',f7.3,' ha)',/)     
 4150 format (9x,'Predicted sediment leaving side of OFE ',i2,1x,
     1        'is ',f10.3,' kg/m width')     
 4200 format (/5x,'sediment particle information leaving profile',/,    &
     &    '-------------------------------------------------------',    &
     &    '------------------------',/,                                 &
     &    '                                 particle composition',      &
     &    '         detached fraction',/,'class  diameter  specific  ---&
     &------------------------------','  sediment  in flow',/,9x,       &
     &    '(mm)    gravity   % sand   % silt   % clay   % o.m.',        &
     &    '  fraction  exiting',/,                                      &
     &    '-------------------------------------------------------',    &
     &    '------------------------')                
 4300 format ('---------------------------------------------------',    &
     &    '----------------------------'/)
 4400 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x, &
     &    f5.3,4x,f5.3)
 4500 format (/5x,'SSA enrichment ratio leaving profile = ',f6.2)
 4550 format (/5x,'SSA enrichment ratio leaving profile for ',a3,1x,i3,1
     1    x,i4,' = ',f6.2)
 4600 format (//5x,'B.  SEDIMENT CHARACTERISTICS AND ENRICHMENT')
     
!     
      end
