! The purpose is to open the saved statement file and to initialize to the variables 
! of current day to run erosion model
      subroutine inpSaveFile(a,b,sr,day,mon,year) 
!*******************************************************************Jin
! Read save statement file
!  date: June 9, 2009
! wrote by Jin Gao
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
      include 'soil/prevday.inc'
      include 'p1const.inc'
      include 'crop/prevstate.inc' 

! parameter variables 
! a - row of regional grid
! b - column of regional grid
! sr - subregion index of regional grid
 
      integer a,b, sr
      integer day, mon,year


! define local variables
      integer dd,mm,yy
      integer sr, idx, linenum, i, j, sub_sr 
      character line*512
      
      
      character *5 fileName,temp_name,name_a,name_b

      character *128 temp,sub_temp
      character *1 tmp, sub_tmp, sub_tmp1
      character *2 tmp2,sub_tmp2,sub_tmp2b
      character *3 tmp3,sub_tmp3
           
!    format for header of save file
!  
      linenum = 0
   
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
! Join the region and subregion into file name
  !       temp = temp//'a.out'            

   20 format(I1)  
   22 format (I2) 
   24 format (I3)
   26 format (I4)
   28 format (I5)

      call fopenk(44,rootp(1:len_trim(rootp))//temp,'old')
       
  100 linenum = linenum+1
      idx = 0
      read (44,'(a)',err=80) line  
          if (line(2:2) .eq. 'D') then
           read (44,'(a)',err=80) line
           read(line,*) dd, mm, yy
         if ((day .eq. dd) .and. (mon .eq. mm) .and. (year .eq. yy))    &
     & then
            goto 200
         else
            goto 100
        end if     
      else 
           goto 100
      end if

  200 read (44,'(a)',err=80) line
           if (line(1:1) .eq. '#') then 
                goto 200          
           else if ((line(1:) .eq. 'EOF') .or. (line(1:).eq. 'End'))    &
     &     then
                goto 300
           else 
                idx = idx + 1
            end if
           select case(idx)
             case (1)
! this is for erosion
             goto 200               
             case (2)
          do i=1, nslay(sr)
          read (line,2080)                                              &
     &                    aszlyt(i,sr), asfsan(i,sr), asfcla(i,sr),     &
     &                    asfvfs(i,sr),asfsil(i,sr),asvroc(i,sr),       &
     &                    asfvcs(i,sr), asfcs(i,sr),asfms(i,sr),        &
     &                    asffs(i,sr), asdwblk(i,sr),asfom(i,sr),       &
     &                    aseags(i,sr),as0ph(i,sr), asfcce(i,sr),       &
     &                    asfcec(i,sr),asfcle(i,sr)
          read (44,'(a)',err=80) line
          end do
             case (3)
             do i = 1, nslay(sr)
             read(line,2081)                                            &
     &       aslagm(i,sr), aslagn(i,sr), aslagx(i,sr), as0ags(i,sr),    &
     &       asdagd(i,sr),aseags(i,sr),asdblk(i,sr),ah0cb(i,sr),        &
     &       aheaep(i,sr),ahrsk(i,sr),ahrwcs(i,sr),ahrwcw(i,sr),        &
     &       ahrwcf(i,sr), ahrwca(i,sr),asdsblk(i,sr),asdpart(i,sr),    &
     &       ahfredsat(i,sr),ahtsav(i,sr),ahtsmx(i,sr),ahtsmn(i,sr),    &
     &       ahfice(i,sr)
             read (44,'(a)',err=80) line
             end do
             case (4)
              read(line,2082)                                           &
     &       ahzsnd(sr),ahzwid(sr),ahzeasurf(sr),ahzsno(sr),            & 
     &       ahtsno(sr),ahfsnfrz(sr),presswc(sr),pressnow(sr),          &
     &       cumprecip(sr),cumrunoff(sr),cumevap(sr),                   &
     &       cumtrans(sr),cumdrain(sr),hprevrotation(sr)
 
             case (5)
              read(line,2083)                                           &
     &       ahzirr(sr), ahratirr(sr), ahdurirr(sr), ahlocirr(sr),      &
     &       am0monirr(sr), ahmadirr(sr),ahminirr(sr),ahrwc1(1,sr),     &
     &       ahrwcr(1,sr),ahndayirr(sr),ahmintirr(sr)
 
             case (6)
                 read(line,2084)                                        &
     &        awrrh, ahzrun(sr), ahzsmt(sr), ah0cng(sr), ah0cnp(sr),    &
     &        acthum(sr),prevhucum(sr)
           
             case (7)
                 read(line,2085)                                        &
     &       asfcr(sr), aszcr(sr), asflos(sr),asmlos(sr),               &
     &       asdcr(sr),asecr(sr),nslay(sr),aseagmn(1,sr),               &
     &       aseagmx(1,sr),aseagm(1,sr)
                
             case (8)
                 read(line,2086)                                        &          
     &       aslrr(sr), aslrro(sr),asargo(sr),sxprg(sr),aszrgh(sr),     &
     &       asxrgs(sr),asxrgw(sr), as0rrk(sr),aslrrc(sr),              &
     &       asxdks(sr),asxdkh(sr),asf10an(sr),asf10en(sr),asf10bk(sr), &
     &       asfald(sr),asfalw(sr),amrslp(sr),SFCov(sr),                &
     &       bedrock_depth(sr), restrict_depth(sr)

             case (9)
                 continue
!                 read(line,2087)                                        &            
!     &        ne_sfd84(sr), ne_asvroc(sr),ne_wzzo(sr), ne_sfcv(sr)     
                 
             case (10)
!                 goto 200
                 read(line,2088) operat, crname 
           
             case (11)                  
                  read(line,2089)                                       &
     &       svroc(1,sr),abrsai(sr),adrsaitot(sr),abmf(sr),             &
     &        admftot(sr),admsttot(sr),acanag(sr),acancr(sr)
!     ,sfcr(1,sr),sflos(1,sr), szrgh(1,sr),              


          
             case (12)
 !               continue
             
              do i=1, mnbpls 
                read(line,3090)                                          &
     &            adzht(i,sr),ad0sla(i,sr),ad0ck(i,sr),                  &
     &            admst(i,sr),addstm(i,sr),adxstm(i,sr),                 &
     &            adxstmrep(i,sr),ddsthrsh(i,sr),admstandstem(i,sr),     &
     &            admstandleaf(i,sr),admstandstore(i,sr),                &
     &            admflatstem(i,sr),admflatleaf(i,sr),                   &
     &            admflatstore(i,sr), admflatrootstore(i,sr),            & 
     &            admflatrootfiber(i,sr), adgrainf(i,sr),                &
     &            adm(i,sr),admst(i,sr),admf(i,sr),                      &
     &      dkrate(1,i,sr),dkrate(2,i,sr),dkrate(3,i,sr),acdkrate(2,sr)  
                 read (44,'(a)',err=80) line
             end do 

          do i=1, mnbpls 
                read(line,2090)                    admbg(i,sr),          &
     &            admrt(i,sr),adrsai(i,sr),adrlai(i,sr),                 &
     &            adffcv(i,sr),adfscv(i,sr),adftcv(i,sr),admbgz(1,i,sr), &
     &            admrtz(1,i,sr),admbgstemz(1,i,sr),admbgleafz(1,i,sr),  &
     &            admbgstorez(1,i,sr),admbgrootstorez(1,i,sr),           &
     &            admbgrootfiberz(1,i,sr),cumddf(i,sr),covfact(i,sr),    &
     &            adrsaz(1,i,sr), adrlaz(1,i,sr), adresevapa(i,sr),      &
     &            adresevapb(i,sr),adhyfg(i,sr), adrbc(i,sr),acrsfg(sr)
                 read (44,'(a)',err=80) line
          end do 
             
             case (13)
!                  continue
                 read(line,2091)                                         &
     &           acmstandstem(sr),acmstandleaf(sr),                      &
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

            case (14)
                 read(line,2092)                                         &
     &               acddsthrsh(sr), atmstandstem(sr),atmstandleaf(sr),  &
     &           atmstandstore(sr),atmflatstem(sr),atmflatleaf(sr),      &
     &           atmflatstore(sr),atmflatrootstore(sr),                  &
     &           atmflatrootfiber(sr),atmbgstemz(1,sr),atmbgleafz(1,sr), &
     &           atmbgstorez(1,sr),atmbgrootstorez(1,sr),                &
     &           atmbgrootfiberz(1,sr),atzht(sr), atdstm(sr),            &
     &           atxstmrep(sr), atzrtd(sr), atgrainf(sr),actdtm(sr),     &
     &           adrlaitot(sr),adrsaitot(sr),abzht(sr),acxrow(sr),       &
     &           ac0rg(sr),acrbc(sr), cprevrotation(sr) 

            case (15)
                 read (line, 2093)                                       &
     &           sdia(msieve), mnsize, mxsize,                           &
     &           mdia(msieve+1), nsieve, logcas    
           case (16)
               read(line, 2096) (ahrwc0(i,sr), i=1,24) 
!                    dmlos(1,1),sf84mn(1,1),smaglos(1,1),     &
!     &             (ahrwc0(i,sr), i=1,24)      
!                   bhtmx0(sr),bhrwc0(sr),bszrh0,bszrr0,      &
!     &            anemht,awzzo  
           case (17)
               read(line,3002)                                          &
     & abfcancov(sr), ahfwsf(sr),aszlyd(1,sr),abevapredu(sr),           &
     & asdblk0(1,sr),ahrwc(1,sr),ahrwcdmx(1,sr),ahzper(sr),             &
     & ahzdmaxirr(sr),ahzoutflow(sr),initswc(sr),initsnow(sr)
           case(18)
               read (line,3003)                                          &
     &   asfsmb(1,sr), as0ph(1,sr), asftan(1,sr), asftap(1,sr),          &
     &   asmno3(sr),ac0bn1(sr), ac0bn2(sr), ac0bn3(sr),                  &
     &   ac0bp1(sr), ac0bp2(sr), ac0bp3(sr), ac0ck(sr),                  &
     &   acgrf(sr), acehu0(sr), aczmxc(sr),aczmrt(sr), actmin(sr),       &
     &   actopt(sr),ac0bceff(sr),admbgz(1,1,sr), ac0alf(sr),ac0blf(sr),  &
     &   ac0clf(sr),ac0dlf(sr), ac0arp(sr), ac0brp(sr), ac0crp(sr),      &
     &   ac0drp(sr), ac0aht(sr), ac0bht(sr),ac0sla(sr),                  &
     &   ac0hue(sr), actverndel(sr),acbaf(sr),                           &
     &   acyraf(sr), acthum(sr), acdpop(sr),acdmaxshoot(sr),             &
     &   ac0storeinit(sr),acfshoot(sr),ac0growdepth(sr),                 &
     &   acfleafstem(sr),ac0shoot(sr),achyfg(sr),acbaflg(sr),ac0idc(sr), &
     &   actdtm(sr),acthudf(sr), ac0transf(sr)  
            case (19)
              read (line, 3004)                                          &
     &   ac0diammax(sr), ac0ssa(sr), ac0ssb(sr),                         &
     &   acfleaf2stor(sr), acfstem2stor(sr), acfstor2stor(sr),           &
     &   acyld_coef(sr), acresid_int(sr),acmshoot(sr),acmtotshoot(sr),   &
     &   aczht(sr), aczshoot(sr), acdstm(sr), aczrtd(sr),                &
     &   acthucum(sr), actrthucum(sr), acgrainf(sr), aczgrowpt(sr),      &
     &   acfliveleaf(sr), acleafareatrend(sr), actchillucum(sr),        &
     &   acthu_shoot_beg(sr),acthu_shoot_end(sr), acxstmrep(sr),        &
     &  prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr),       &
     &   prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),         &
     &   prevmshoot(sr), prevmtotshoot(sr), prevbgstemz(1,sr),          &
     &   prevrootstorez(1,sr), prevrootfiberz(1,sr), prevht(sr),        &
     &   prevzshoot(sr), prevstm(sr), prevrtd(sr),                      &
     &   prevhucum(sr), prevrthucum(sr),                                &
     &   prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),           &
     &   aczloc_regrow(sr),acdayspring(sr),acdayap(sr),acdayam(sr),     &
     &   actwarmdays(sr),prevdayap(sr)
     
              end select
              goto 200

 2080  format ( e13.6,17e13.6)
 2081  format ( 21e13.6)
 2082  format ( 13e13.6,i2)
 2083  format ( 4e13.6,i3,4e13.6,i6,i3)
 2084  format ( 7e13.6)
 2085  format ( 6e13.6,i3,3e13.6)
 2086  format ( e13.6,19e13.6)
 2087  format ( 4e13.6)    
 2088  format (2a36)
 2089  format (8e13.6)

 2090  format (20e13.6,3i3)
 3090  format (24e13.6)
 2091  format (12e13.6,i5,28e13.6 )
 2092  format (19e13.6,i5,4e13.6,3i3)
 2093  format (4e13.6,2i3)
 2094  format (8e13.6)
 2096  format (24e13.6)
 2097  format (14e13.6)
 3001  format (2e13.6)
 3002  format (13e13.6)
 3003  format (43e13.6,6i3)
 3004  format (45e13.6,5i3)   
   80  write (0,9001)
 9001  format('Error in open file!')   
  300  close(44)
       end
