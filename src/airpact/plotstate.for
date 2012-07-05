! The purpose of this subroutine is to save the variable statement into a file

      subroutine plotstate(a,b,sr,day,month,year)


      include 'p1werm.inc'
! ***      include 'm1sim.inc'
      include 'm1flag.inc'
      include 'c1glob.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'c1report.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1db3.inc'

      include 'd1glob.inc'
      include 'd1gen.inc'
      include 'b1glob.inc'
      include 'h1db1.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1surf.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'h1hydro.inc'
      include 'file.inc'
      include 'm1subr.inc'
      include 'm1geo.inc'
      include 'erosion/m2geo.inc'
      include 'erosion/e2erod.inc'
      include 'erosion/e3grid.inc'
      include 'erosion/w2wind.inc' 
      include 'erosion/s2agg.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/s2surf.inc'

      include 'erosion/threshold.inc'
      include 'manage/oper.inc'
      include 'main/main.inc'
      include 'main/plot.inc'
      include 'wpath.inc'
      include 's1dbc.inc'
       include 's1layr.inc'
      include 's1sgeo.inc'
      include 's1dbh.inc'
      include 'h1temp.inc'
      include 'h1balance.inc'
      include 'h1scs.inc'
      include 'decomp/decomp.inc'
      include 'manage/tcrop.inc'
      include 'manage/asd.inc' 
      include 'p1const.inc' 
      include 'soil/prevday.inc'
      include 'm1sim.inc'
      include 'crop/prevstate.inc' 

! parameter variables 
! a - row of regional grid
! b - column of regional grid
! sr - subregion index of regional grid
 
      integer a,b, sr
      integer day, month, year
!   local variable 
      integer doy

      integer ngdpt  !number of grid cells within field
      integer idx, jdy   !local loop vars

      integer dayear, i, j,hidx 
      character *5 fileName,temp_name,name_a,name_b

      character *128 temp,sub_temp
      character *1 tmp, sub_tmp, sub_tmp1
      character *2 tmp2,sub_tmp2,sub_tmp2b
      character *3 tmp3,sub_tmp3
           
!     + + + OUTPUT FORMATS + + +
!     format for header of save file
!  
 !     linenum = 0
! To write the subregion index into the saved filename
      if (a > 0 .and. a < 10) then
        write(temp_name, FMT=20) a
        sub_tmp = temp_name(1:1)
        name_a = sub_tmp 
      else 
        write(temp_name,FMT=22) a
        sub_tmp2 = temp_name(1:2)
        name_a = sub_tmp2
       end if
         
      if (b > 0 .and. b < 10) then
        write(temp_name, FMT=20) b
        sub_tmp1 = temp_name(1:1)
        name_b = sub_tmp1 
      else 
        write(temp_name,FMT=22) a
        sub_tmp2b = temp_name(1:2)
        name_b = sub_tmp2b
      end if

      sub_temp = name_a(1:len_trim(name_a))//'_'//                      &
     &  name_b(1:len_trim(name_b))
! To write the region index into saved file
     
      if (sr <10) then
        write(fileName,FMT=20) sr
        tmp = fileName(1:1)
        temp = 'N'//tmp//'_'//sub_temp//'.out'
      else if (sr .ge. 10 .and. sr < 100) then
        write(fileName,FMT=22) sr
        tmp2 = fileName(1:2) 
        temp ='N'//tmp2//'_'//sub_temp//'.out'
       else if (sr .ge. 100 .and. sr < 1000) then
         write(fileName,FMT=24) sr
         tmp3 = fileName(1:3)
         temp ='N'//tmp3//'_'//sub_temp//'.out'
        else
         write(fileName,FMT=28) sr
         temp ='NS'//sub_temp
       end if
! To join the region and subregion index into file name
    !    temp = temp//'.out'

         
   20 format (I1)  
   22 format (I2) 
   24 format (I3)
   26 format (I4)
   28 format (I5)
 
 !      fileName = trim(char(sr))
          
       
       call fopenk(42,rootp(1:len_trim(rootp))//temp,'unknown') 
!       call fopenk(luo_erod, rootp(1:len_trim(rootp))//'.out','unknown') 
!      call caldatw(day,month,year)
     
!      doy = dayear(day,month,year)
  
!      call calfromdoy(doy,day,month,year)
 
 2101 format ('-Day Month Year')
 2102 format(2(1x,i2),2x,i4)
! print the head file 
      write(42,1005)
      write(42,2101)
      write(42,2102) day,month,year
 1005 format ('#',65('*'),/,'#     file:  SaveStatement.out ',/,         &
     & '#',/,                                                            &
     & '# +++ Purpose +++',/,                                            &
     & '#'                                                               &
     & 'This is to print out the variables after running the Weps'       &
     & ' model at each calendar day.'                                    &
     & '#')           
 2049 format('# Soil daily erosion: tot_loss | suspen | pm10 ')          
 2050 format ('# soil surface properties # ',/,                          &
     &      '# lay_thk ','|',' lay_san','|','  clay ',                   & 
     &     '|',' finesand', '|','  asfsil ','|',' asvroc ',              &
     &     '|',' asfvcs ','|','  asfcs    ','|',' asfms  ',              &
     &     '|', ' asffs ', '|','asdwblk',                                &
     &     '|','asfom','|',' aseags',  '|','  as0ph',                    &
     &     '|','asfcce','|', ' asfcec ','|',' asfcle')
! soil aggregate properties
 2051 format ('# soil aggregate properties #',/,                         &
     &      '# aslagm ','|','aslagn','|','aslagx ',                      &
     &     '|',' as0ags ','|','asdagd','|','aseags ',                    &
     &     '|',' asdblk ','|','ahrwc','|','ahocb ',                      &
     &     '|',' aheaep ','|','ahrsk','|','ahrwcs ',                     &
     &     '|',' ahrwcw ','|','ahrwcf','|','ahrwca ',                    &
     &     '|',' asdsblk ','|','asdpart','|','ahfredsat ',               &
     &     '|',' ahtsav ','|','ahtsmx','|','ahtsmn ',                    &
     &     '|',' ahfice ')

! hydrologica balance variables
 2052 format ('# hydrologica balance variables', /,                      &
     &        '# ahzsnd','|','ahzwid ','|',' ahzeasurf ','|','ahzsno',   &
     &     '|','ahtsno','|',' ahfsnfrz ','|','presswc','|','pressnow ',  &
     &      'cumptrcip','|','cumrunoff','|','cumevap ',                  & 
     &     '|',' cumtrans ','|','cumdrain','|','hprevrotation ')
!     header of irrigation variables
 2053 format ('# Irrigation variables #', /,                             &
     & '# ahzirr','|',' ahratirr ','|',' ahdurirr ',                     &
     &        '|',' ahlocirr ','|','am0monirr','|','ahmadirr',           &
     &        '|',' ahminirr ','|','ahndayirr','|','ahmintirr')

!     header of daily relative humidity
 2054 format ('# Daily relative humidity #', /,                          &
     & '# awrrh','|','ahzrun','|','ahzsmt','|','wh0cng','|','ah0cnp',    &
     &  'acthum|prevhucum')

!     header of soil surface variables (crust properties)
 2055 format ('# Crust properties #',/,                                  &
     &         '# asfcr','|','aszcr', '|','asflos',                      &
     &        '|','asmlos','|','asdcr','|','asecr','|','nslay',          &
     &         '|aseagmn|aseagmx|aseagm')
!     header of surface roughness properties ()
 2056 format ('# surface roughness properties #',/,                      &
     &        '# aslrr','|',' aslrro','|','asargo' ,'|','sxprg',         &
     &        '|','aszrgh','|','asxrgs ', '|',                           &
     &        '|','asxrgw','|','as0rrk ', '|','aslrrc',                  &
     &        '|','asxdks','|','asxdkh','|','asf10an ', '|','asf10en',   &
     &        '|','asf10bk','|','asfald ', '|','asfalw',                 &
     &        '|','amrslp','|','SFCov ', '|','bedroc_dpth',              &  
     &        '|','restrict_dpth')
!     header of
 2057 format ('# ne_sf84','|',' ne_rock',                                &
     &        '|','ne_wzzo','|','ne_sfcv ')
!     operation name(s) at end of line
 2058 format ('# operation    ','|','    crop  ')

!     + + + END SPECIFICATIONS + + +

 ! Don't print plotdata "plot.out" file unless a debug flag is set
 !     if((am0hfl.gt.0).or.(am0sfl.gt.0).or.(am0tfl.gt.0)                &
 !    &  .or.(am0cfl.gt.0).or.(am0dfl.gt.0).or.(am0efl.gt.0)) then

! write file header if still initializing        

        ! initialize erosion totals
        total = 0.0
        suspen = 0.0
        pmten = 0.0
        imax = 2
        jmax = 2
        ntstep = 24
        hidx = 0
        if( report_loop ) then
           ngdpt = (imax-1) * (jmax-1)  !Number of grid cells
           do idx = 1, imax-1
              do jdy = 1, jmax-1
                 total = total + egt(idx,jdy)
                !salt = salt + (egt(idx,jdy) - egtss(idx,jdy)
                 suspen = suspen + egtss(idx,jdy)
                 pmten = pmten + egt10(idx,jdy)
              end do
           end do
           total = total/ngdpt
           suspen = suspen/ngdpt
           pmten = pmten/ngdpt
        end if
     
        doy = dayear (day, month, year)
  
        ! make operation name available for this day
        !  amnryr(sr)
       
        if ((lopday .eq. day) .and. (lopmon .eq. month) .and.           &
     &      (amnryr(sr) .eq. year)) then
           operat = opname
           crname = ac0nam(sr)
!          write(*,*) 'Same Date:',day,month,year,lopday,lopmon,amnryr
        else
           operat = '                                               '
           crname = '                                               '
        end if

        ! insert double blank lines to demarcate years
!        if( doy .eq. 1 ) then
!            write (42,*)
!            write (42,*)
!        end if
! print header of daily erosion   
        write (42, 2049)           
        write(42,2079) total,total-suspen, suspen,pmten
! Print the erosion result into the Output file 
!        write(luo_erod,4001) day,month,year,total,total-suspen,suspen,  &
!     &       pmten
! print header of soil properties 
        write (42, 2050)
        do i=1, nslay(sr)
        write (42, 2080)                                                &
     &                    aszlyt(i,sr), asfsan(i,sr), asfcla(i,sr),     &
     &                    asfvfs(i,sr),asfsil(i,sr),asvroc(i,sr),       &
     &                    asfvcs(i,sr), asfcs(i,sr),asfms(i,sr),        &
     &                    asffs(i,sr), asdwblk(i,sr),asfom(i,sr),       &
     &                    aseags(i,sr),as0ph(i,sr), asfcce(i,sr),       &
     &                    asfcec(i,sr),asfcle(i,sr)
        end do
 
!print header 
        write (42, 2051)
        do i = 1, nslay(sr)
        write (42, 2081)                                                &
     &       aslagm(i,sr), aslagn(i,sr), aslagx(i,sr), as0ags(i,sr),    &
     &       asdagd(i,sr),aseags(i,sr),asdblk(i,sr),ah0cb(i,sr),        &
     &       aheaep(i,sr),ahrsk(i,sr),ahrwcs(i,sr),ahrwcw(i,sr),        &
     &       ahrwcf(i,sr), ahrwca(i,sr),asdsblk(i,sr),asdpart(i,sr),    &
     &       ahfredsat(i,sr),ahtsav(i,sr),ahtsmx(i,sr),ahtsmn(i,sr),    &
     &       ahfice(i,sr)
        end do

        write(42,2052)
        write(42,2082) 
     &       ahzsnd(sr),ahzwid(sr),ahzeasurf(sr),ahzsno(sr),            & 
     &       ahtsno(sr),ahfsnfrz(sr),presswc(sr),pressnow(sr),          &
     &       cumprecip(sr),cumrunoff(sr),cumevap(sr),                   &
     &       cumtrans(sr),cumdrain(sr),hprevrotation(sr)

! irrigation variables
        write (42, 2053)
        write (42, 2083)                                                &
     &       ahzirr(sr), ahratirr(sr), ahdurirr(sr), ahlocirr(sr),      &
     &       am0monirr(sr), ahmadirr(sr),ahminirr(sr),ahrwc1(1,sr),     &
     &       ahrwcr(1,sr),ahndayirr(sr),ahmintirr(sr)

 !daily relative humidity
       write (42, 2054)
       write (42, 2084) awrrh,                                          &
     &       ahzrun(sr), ahzsmt(sr), ah0cng(sr), ah0cnp(sr),acthum(sr), &
     &       prevhucum(sr)
 
   ! crust properties
         write (42, 2055) 
         write (42, 2085)                                               &
     &       asfcr(sr), aszcr(sr), asflos(sr),asmlos(sr),               &
     &       asdcr(sr),asecr(sr),nslay(sr),aseagmn(1,sr),               &
     &       aseagmx(1,sr),aseagm(1,sr)
      
  ! surface roughness 

          write (42, 2056)
          write (42, 2086)                                              &
     &       aslrr(sr), aslrro(sr),asargo(sr),sxprg(sr),aszrgh(sr),     &
     &       asxrgs(sr),asxrgw(sr), as0rrk(sr),aslrrc(sr),              &
     &       asxdks(sr),asxdkh(sr),asf10an(sr),asf10en(sr),asf10bk(sr), &
     &       asfald(sr),asfalw(sr),amrslp(sr),SFCov(sr),                &
     &       bedrock_depth(sr), restrict_depth(sr)
 
! soil related threshold values
! Those variables should be removed as there is no effect on erosion result 
        write (42, 2057)
        write (42,*) '  ' 
!        write (42, 2087) ne_sfd84(sr), ne_asvroc(sr),                   &
!     &    ne_wzzo(sr), ne_sfcv(sr)
! crop operation / not used here 
  
        write(42,2058) 
        write (42, 2090, ADVANCE="NO") operat
        write (42, 2091, ADVANCE="YES") crname
! write erosion threshold variables
        write (42,2059)
        write(42,2092)                                                   &
     & 	svroc(1,sr),abrsai(sr),adrsaitot(sr),admftot(sr),abmf(sr),       &                 
     &  admsttot(sr),acanag(sr),acancr(sr) !,sfcr(1,sr),sflos(1,sr),szrgh(1,sr)	


! write decomp variables 
      write(42,2060)
 
! # The following variables do affect erosion variables 
      do  i = 1, mnbpls
      write(42,3093)   adzht(i,sr),ad0sla(i,sr),ad0ck(i,sr),             &
     &            admst(i,sr),addstm(i,sr),adxstm(i,sr),                 &
     &            adxstmrep(i,sr),ddsthrsh(i,sr),admstandstem(i,sr),     &
     &            admstandleaf(i,sr),admstandstore(i,sr),                &
     &            admflatstem(i,sr),admflatleaf(i,sr),                   &
     &            admflatstore(i,sr), admflatrootstore(i,sr),            & 
     &            admflatrootfiber(i,sr), adgrainf(i,sr),                &
     &            adm(i,sr),admst(i,sr),admf(i,sr),                      &
     &      dkrate(1,i,sr),dkrate(2,i,sr),dkrate(3,i,sr),acdkrate(2,sr)  
    
      end do
! split the erosion variables into multiple lines
       write(42,2069)
      do  i = 1, mnbpls
       write(42,2093)                                                    &
     &            admbg(i,sr),admrt(i,sr),adrsai(i,sr),adrlai(i,sr),     &   
     &            adffcv(i,sr),adfscv(i,sr),adftcv(i,sr),admbgz(1,i,sr), &
     &            admrtz(1,i,sr),admbgstemz(1,i,sr),admbgleafz(1,i,sr),  &
     &            admbgstorez(1,i,sr),admbgrootstorez(1,i,sr),           &
     &            admbgrootfiberz(1,i,sr),cumddf(i,sr),covfact(i,sr),    &
     &            adrsaz(1,i,sr),  adrlaz(1,i,sr), adresevapa(i,sr),     &
     &            adresevapb(i,sr),adhyfg(i,sr), adrbc(i,sr),acrsfg(sr)
 
      end do


! crop variables
      write(42,2061) 
      write(42, 2094) acmstandstem(sr),acmstandleaf(sr),                 &
     &           acmstandstore(sr), acmflatstem(sr), acmflatleaf(sr),    &
     &           acmflatstore(sr),acmrootstorez(1,sr),                   &
     &           acmrootfiberz(1,sr), acmbgstemz(1,sr),                  &
     &           aczht(sr),acdstm(sr),aczrtd(sr),acdayap(sr),            &
     &           acthucum(sr),actrthucum(sr),acgrainf(sr),               &
     &           acmrootstore(sr),acmrootfiber(sr),acxstmrep(sr),        &
     &           acfliveleaf(sr),acm(sr),acmst(sr),acmf(sr),acmrt(sr),   &
     &           acmrtz(1,sr),acrsai(sr), acrlai(sr),                    &
     &           acrsaz(1,sr),acrlaz(1,sr),acffcv(sr),                   &
     &           acfscv(sr), acxstm(sr),accovfact(sr),                   &
     &           ac0ck(sr),acftcv(sr),abdstm(sr),                        &
     &           abffcv(sr),abfscv(sr),acfcancov(sr),                    &
     &           acrcd(sr),acdkrate(1,sr) 
        
      write(42,2062)
      write(42,2095) acddsthrsh(sr), atmstandstem(sr),atmstandleaf(sr),  &
     &           atmstandstore(sr),atmflatstem(sr),atmflatleaf(sr),      &
     &           atmflatstore(sr),atmflatrootstore(sr),                  &
     &           atmflatrootfiber(sr),atmbgstemz(1,sr),atmbgleafz(1,sr), &
     &           atmbgstorez(1,sr),atmbgrootstorez(1,sr),                &
     &           atmbgrootfiberz(1,sr),atzht(sr), atdstm(sr),            &
     &           atxstmrep(sr), atzrtd(sr), atgrainf(sr),actdtm(sr),     &
     &           adrlaitot(sr),adrsaitot(sr),abzht(sr),acxrow(sr),       &
     &           ac0rg(sr), acrbc(sr), cprevrotation(sr)
      write(42,2063) 
      write(42,2096)                                                     & 
     &           sdia(msieve), mnsize, mxsize,                           &
     &           mdia(msieve+1), nsieve, logcas    

      write(42,2066) 
!      write(42, 2099)    dmlos(1,1),sf84mn(1,1),smaglos(1,1) 
        do i=1,ntstep-1
         hidx = int(i*23.75/ntstep)+1
         write(42,3001,ADVANCE="NO") ahrwc0(hidx,sr)
        end do
         hidx = 24
         write(42,3001,ADVANCE="YES") ahrwc0(hidx,sr)
! save the hydro information from hydro function 
       write(42, 2067)
       write(42, 3002)                                                   &
     & abfcancov(sr),ahfwsf(sr),aszlyd(1,sr),abevapredu(sr),             &
     & asdblk0(1,sr),ahrwc(1,sr),ahrwcdmx(1,sr),ahzper(sr),              &
     & ahzdmaxirr(sr),ahzoutflow(sr),initswc(sr),initsnow(sr)      
! save crop growing variables
!       write(42,2068) 
!       write(42,3003)                                                    &
!     &   asfsmb(1,sr), as0ph(1,sr), asftan(1,sr), asftap(1,sr),          &
!     &   asmno3(sr),ac0bn1(sr), ac0bn2(sr), ac0bn3(sr),                  &
!     &   ac0bp1(sr), ac0bp2(sr), ac0bp3(sr), ac0ck(sr),                  &
!     &   acgrf(sr), acehu0(sr), aczmxc(sr),aczmrt(sr), actmin(sr),       &
!     &   actopt(sr),ac0bceff(sr),admbgz(1,1,sr), ac0alf(sr),ac0blf(sr),  &
!     &   ac0clf(sr),ac0dlf(sr), ac0arp(sr), ac0brp(sr), ac0crp(sr),      &
!     &   ac0drp(sr), ac0aht(sr), ac0bht(sr),ac0sla(sr),                  &
!     &   ac0hue(sr), actverndel(sr),acbaf(sr),                           &
!     &   acyraf(sr), acthum(sr), acdpop(sr),acdmaxshoot(sr),             &
!     &   ac0storeinit(sr),acfshoot(sr),ac0growdepth(sr),                 &
!     &   acfleafstem(sr),ac0shoot(sr),achyfg(sr),acbaflg(sr),ac0idc(sr), &
!     &   actdtm(sr),acthudf(sr), ac0transf(sr)  
! save crop variables 
!      write(42,2068) 
!      write(42,3004)                                                     &
!     &   ac0diammax(sr), ac0ssa(sr), ac0ssb(sr),                         &
!     &   acfleaf2stor(sr), acfstem2stor(sr), acfstor2stor(sr),           &
!     &   acyld_coef(sr), acresid_int(sr),acmshoot(sr),acmtotshoot(sr),   &
!     &   aczht(sr), aczshoot(sr), acdstm(sr), aczrtd(sr),                &
!     &   acthucum(sr), actrthucum(sr), acgrainf(sr), aczgrowpt(sr),      &
!     &   acfliveleaf(sr), acleafareatrend(sr), actchillucum(sr),        &
!     &   acthu_shoot_beg(sr),acthu_shoot_end(sr), acxstmrep(sr),        &                                               &
!     &   prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr),      &
!     &   prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),         &
!     &   prevmshoot(sr), prevmtotshoot(sr), prevbgstemz(1,sr),          &
!     &   prevrootstorez(1,sr), prevrootfiberz(1,sr), prevht(sr),        &
!     &   prevzshoot(sr), prevstm(sr), prevrtd(sr),                      &
!     &   prevhucum(sr), prevrthucum(sr),                                &
!     &   prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),           &
!     &   aczloc_regrow(sr),acdayspring(sr),acdayap(sr),acdayam(sr),     &
!     &   actwarmdays(sr),prevdayap(sr)
           

 !      write(42,3004) amxbr(1,1,sr),amxbr(2,1,sr),amxbr(1,2,sr),          &
 !    &    amxbr(2,2,sr),amzbr(sr),ampbr(sr),amxbrw(sr)
           
!       write(42, 3002) sf84(1,1),sf84mn(1,1)
!     asdagd(1,sr),asfcr(sr), asvroc(1,sr),    &
!     &   asflos(sr),abffcv(sr),dmlos(1,1),sf84mn(1,1),smaglos(1,1),       &
!     &   ahrwcw(1,sr),smaglosmx(1,1)
!      write(42,3003) aslagm(1,sr),as0ags(1,sr),aslagn(1,sr),aslagx(1,sr)  
!     write(42,3003)  sf84ic, sf10ic,sf84mn(1,1), sf84(1,1)       
      

! bhtmx0(sr),bhrwc0(sr),bszrh0,bszrr0,anemht,awzzo

 !     write(42,2064)
!      if (ne_erosion(1) .eq. 1) then
!       do i = i1,i2,i3
!       do j = i4,i5,i6 
!       write(42,2097)                                                    & 
!     &          sf10(i,j),sf84(i,j),sf200(i,j),sfcr(i,j),szcr(i,j),      &
!     &          smlos(i,j),szrgh(i,j),sflos(i,j),slrr(i,j),svroc(i,j),   &
!     &          smaglos(i,j), dmlos(i,j), sf84mn(i,j), smaglosmx(i,j)
!        end do
!        end do
!       end if 
!      write(42,3001) wust
      write(42,2077)
      write(42,2078)  
       
 2077 format('EOF')
 2078 format('End')
   
 2080   format (   e12.6, ' ',3(e12.6,' '),                             &
     &            3(e12.6,' '),                                         &
     &            3(e12.6,' '),                                         &
     &            3(e12.6,' '),                                         &
     &            2(e12.6,' '),                                         &
     &            3(e12.6,' '))
 2059   format ('# Erosion threshold variables',/,                      &
     &  '# svroc|abrsai|adrsaitot|admftot|abmf|admsttot|acamag|acancr')
!    &          '# ne_erosion|ne_snowdepth|ne_wus_anemom','|',          &
!     &          'ne_wus_random|ne_wus_ridge|ne_wus_biodrag|ne_wus','|', &
!     &          'ne_bare|ne_flat_cov|ne_surf_wet|ne_ag_den|ne_wust|',   &
!     &          'ne_sfd84|ne_asvroc|ne_wzzo|ne_sfcv|acanag|acancr') 
 2060   format ('# decomposion variables',/,                            &
     &         '#  adzht|ad0sla|ad0ck|admst|addstm|adxstm|adxstmrep|',  &
     &         'ddsthrsh|admstandstem|admstandleaf|admstandstore','|',  &
     &         'admflatstem|admflatleaf|admflatstore|admflatrootstore', &
     &         '|admflatrootfiber|adgrainf|adm|admst|admf',             &
     &         '|dkrate(1)|dkrate(2)|dkrate(3)|acdkrate(2,sr)')
 2069  format ('|admbg|admrt|adrsai|adrlai|adffcv|adfscv|adftcv|admbgz',&
     &          '|admrtz','|admbgstemz|admbgleafz|admbgstorez|',        &
     &          'admbgrootstorez','|admbgrootfiberz|cumddf|covfact|',   &
     &          'adrsaz|adrlaz','|adresevapa|adresevapb|adhyfg|adrbc|', &
     &           'acrsfg')
 2061   format ('# Crop variables',/,                                   &
     &          '#acmstandstem,acmstandleaf,acmstandstore,acmflatstem,' &
     &           'acmflatleaf,acmflatstore,acmrootstorez,'              &
     &           'acmrootfiberz,acmbgstemz,aczht,acdstm,aczrtd,'        &
     &           'acdayap,acthucum,actrthucum,acgrainf,acmrootstore,'   &
     &           'acmrootfiber,acxstmrep,acfliveleaf,acm,acmst,acmf,'   &
     &           'acmrt,acmrtz,acrsai,acrlai,acrsaz,acrlaz,acffcv,'     &
     &           'acfscv,acxstm,accovfact,ac0ck,acftcv,abdstm,'         &
     &           'abffcv,abfscv,acfcancov,acrcd,acdkrate')
                    
 2062   format ('# write temporary crop',/,                             &
     &           '# acddsthrsh, atmstandstem,atmstandleaf,'             &
     &           'atmstandstore,atmflatstem,atmflatleaf,'               &
     &           'atmflatstore,atmflatrootstore,atmflatrootfiber,'      &
     &           'atmbgstemz,atmbgleafz,atmbgstorez,atmbgrootstorez,'   &
     &           'atmbgrootfiberz,atzht,atdstm,atxstmrep,atzrtd,'       &
     &           'atgrainf,actdtm,adrlaitot,adrsaitot,abzht,acxrow,'    &
     &           'ac0rg','acrbc,cprevrotation') 
 2063   format ('# Asd info.'                                           &
     &           'sdia, mnsize, mxsize, mdia, nsieve, logcas'   )
 2064   format ('# Soil surface information')
 2066   format ('# Hourly surface soil water content with ahrwc0')
 2067   format ('# Soil hydro information',/,                           &
     &          '# abfcancov, abmf, ahfwsf, aszlyd, abevapredu,'        &
     &          'asdblk0, ahrwc, ahrwcdmx, ahzper,'                     &
     &          'ahzdmaxirr, ahzoutflow, initswc, initsnow')  
 2068   format ( '# Crop variables')      
 2079   format ( 4(e12.6,' '))
 2081   format ( 21(e12.6,' '))
 2082   format ( 13(e12.6,' '),i2, ' ')
 2083   format ( 4(e12.6,' '),i2,' ',4(e12.6,' '),i5,' ',i2,' ')
 2084   format ( 7(e12.6,' '))
 2085   format ( 6(e12.6,' '),i2,' ',3(e12.6,' '))
 2086   format ( e12.6,' ', 19(e12.6,' '))
 2087   format ( 4(e12.6,' '))
 2090   format ( a35,' ')
 2091   format ( a35,' ')
 2092   format (8(e12.6,' '))
! ( 2(i2,' '),11(e12.6,' '),5(e12.6,' '))            
 2093   format (20(e12.6,' '),3(i2,' '))
 3093   format (24(e12.6,' '))

 2094   format (12(e12.6,' '),i4,' ',28(e12.6,' '))   
 2095   format (19(e12.6,' '),i4,' ',4(e12.6,' '),3(i2,' '))
 2096   format (4(e12.6,' '),2(i2,' ')) 
 2097   format (14(e12.6,' '))
 2098   format (3(e12.6,' '))
 2099   format (3(e12.6,' '))
 3001   format (e12.6,' ')
 3002   format (13(e12.6,' '))
 3003   format (43(e12.6,' '),6(i2,' '))
 3004   format (45(e12.6,' '),5(i2,' '))
 4001   format  (3(i2,' '),4(e12.6,' ')) 
 !       close(42)
!      endif
      return
      end

