!$Author$
!$Date$
!$Revision$
!$HeadURL$

!   Add this definition file in every source file to insure that the compiler can
!   verify subroutine and function signatures.
!  
!   Can't seem to use constants in interface block? This effects two dimensional arrays where the
!   first dimension must be specified 
!        26 = msieve variables mf()
!      
!    
!       integer mnsz_loc
!       integer mncz_loc
!       parameter (mnsz_loc = 100)
!       parameter (mncz_loc = 5)
!       parameter (mnbpls = 3)
!       parameter (mndk = 5)
       
       MODULE weps_interface_defs

       interface

!---------------------- ASD Routines ----------------------------      
!-----------------------      
      subroutine asd2m (mnot, minf, gmd, gsd, nlay, mf)
      real, intent(in) :: mnot(*), minf(*)
      real, intent(in) :: gmd(*), gsd(*)
      integer, intent(in) :: nlay
      real, intent(out) :: mf(26+1,*)
      end subroutine asd2m
!----------------------
      subroutine asdini()
      end subroutine asdini
!------------------------  
      subroutine m2asd (mf, nlay, mnot, minf, gmd, gsd)
      real, intent(in) :: mf(26+1, *)
      integer, intent(in) :: nlay
      real, intent(in) ::  mnot(*), minf(*)
      real, intent(out) :: gmd(*), gsd(*)
      end subroutine m2asd 
      
!-------------------- CROP Routines --------------------------------
!------------------------
      subroutine callcrop(daysim, sr, crop, residue, restot, croptot, h1et)
      use biomaterial, only: biomatter, biototal
      use hydro_data_struct_defs, only: hydro_derived_et
      integer daysim
      integer sr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue  ! structure containing full residue pool description
      type(biototal), intent(in) :: restot
      type(biototal), intent(inout) :: croptot
      type(hydro_derived_et), intent(in) :: h1et
      end subroutine callcrop
!----------------------
      subroutine  cdbug(isr, slay, crop, restot, h1et)
      use biomaterial, only: biomatter, biototal
      use hydro_data_struct_defs, only: hydro_derived_et
      integer, intent(in) :: isr    ! subregion index
      integer, intent(in) :: slay   ! number of soil layers
      type(biomatter), intent(in) :: crop    ! structure containing full crop description
      type(biototal), intent(in) :: restot   ! structure containing residue totals
      type(hydro_derived_et), intent(in) :: h1et
      end subroutine cdbug
!----------------------
      subroutine chillu(bctchillucum, day_max_temp, day_min_temp)
      real, intent(inout) :: bctchillucum
      real, intent(in) :: day_max_temp, day_min_temp            
      end subroutine chillu
!----------------------
      subroutine cinit(isr, bnslay, bszlyd,                             &
     &           bctopt, bctmin,                                        &
     &           bcthudf, bctdtm, bcthum, bc0hue, bcdmaxshoot,          &
     &           bc0shoot, bc0growdepth, bc0storeinit,                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmshoot, bcmtotshoot, bcmbgstemz,                     &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bczht, bczshoot, bcdstm, bczrtd,                       & 
     &           bcdayap, bcdayam, bcthucum, bctrthucum,                &
     &           bcgrainf, bczgrowpt, bcfliveleaf,                      &
     &           bcleafareatrend, bcstemmasstrend, bctwarmdays,         &
     &           bctchillucum, bcthardnx, bcthu_shoot_beg,              &
     &           bcthu_shoot_end, bcdpop, bcdayspring)
      integer, intent(in) :: isr   ! subregion number
      integer bnslay, bcthudf, bctdtm
      real bszlyd(*)
      real bctopt, bctmin
      real bcthum, bc0hue, bcdmaxshoot, bc0shoot
      real bc0growdepth, bc0storeinit
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      integer bcdayap, bcdayam
      real bcthucum, bctrthucum
      real bcgrainf, bczgrowpt, bcfliveleaf
      real bcleafareatrend, bcstemmasstrend
      integer bctwarmdays
      real bctchillucum, bcthardnx, bcthu_shoot_beg, bcthu_shoot_end
      real bcdpop
      integer bcdayspring
      end subroutine cinit
!-------------------------------
      subroutine cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,  &
     &                     bcmstandstem, bcmstandleaf, bcmstandstore,   &
     &                     bcmflatstem, bcmflatleaf, bcmflatstore,      &
     &                     bcmrootstorez, lost_mass,                    &
     &                     bcyld_coef, bcresid_int, bcgrf )
      integer bchyfg, bnslay
      real dlfwt, dstwt, drpwt, drswt
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), lost_mass
      real bcyld_coef, bcresid_int, bcgrf          
      end subroutine cookyield
!-------------------------------
      subroutine cpout( isr )
      integer, intent(in) :: isr   ! subregion number
      end subroutine cpout
!-------------------------------
      subroutine crop_endseason ( isr, bc0nam, bm0cfl,                  &
     &                 bnslay, bc0idc, bcdayam,                         &
     &                 bcthum, bcxstmrep,                               &
     &                 bprevstandstem, bprevstandleaf, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevbgstemz,                                    &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevstm, bprevrtd,                     &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum, bprevliveleaf,      &
     &                 bprevdayspring, mature_warn_flg )
      integer, intent(in) :: isr   ! subregion number
      character*(80) bc0nam
      integer bm0cfl, bnslay, bc0idc, bcdayam
      real bcthum, bcxstmrep
      real bprevstandstem, bprevstandleaf, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevstm, bprevrtd
      integer bprevdayap
      real bprevhucum, bprevrthucum
      real bprevgrainf, bprevchillucum, bprevliveleaf
      integer bprevdayspring, mature_warn_flg
      end subroutine crop_endseason
!-----------------------------
      subroutine cprnl (hmx,bcthucum,day,mo,yr)
      integer   day, mo, yr
      real hmx, bcthucum  
      end subroutine cprnl
!-----------------------------
      subroutine cropgrow (isr, bnslay, bszlyd,                         &
     &                 bc0ck, bcgrf, bcehu0, bczmxc,                    &
     &                 bc0nam, bc0idc, bcxrow,                          &
     &                 bctdtm, bczmrt, bctmin, bctopt,                  &
     &                 bc0fd1, bc0fd2, cc0fd1, cc0fd2,                  &
     &                 bc0bceff,                                        &
     &                 bc0alf, bc0blf, bc0clf,                          &
     &                 bc0dlf, bc0arp, bc0brp, bc0crp,                  &
     &                 bc0drp, bc0aht, bc0bht,                          &
     &                 bc0sla, bc0hue, bctverndel,                      &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhfwsf,                                          &
     &                 bm0cif,                                          &
     &                 bcthudf, bcbaf,                                  &
     &                 bchyfg, bcthum, bcdpop, bcdmaxshoot,             &
     &                 bc0storeinit, bcfshoot,                          &
     &                 bc0growdepth, bcfleafstem, bc0shoot,             &
     &                 bc0diammax, bc0ssa, bc0ssb,                      &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int, bcxstm,                 &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bcdayap, bcdayam, bcthucum, bctrthucum,          &
     &                 bcgrainf, bczgrowpt, bcfliveleaf,                &
     &                 bcleafareatrend, bcstemmasstrend, bctwarmdays,   &
     &                 bctchillucum, bcthardnx, bcthu_shoot_beg,        &
     &                 bcthu_shoot_end, bcxstmrep,                      &
     &                 bprevstandstem, bprevstandleaf, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevmshoot, bprevbgstemz,                       &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevzshoot, bprevstm, bprevrtd,        &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum, bprevliveleaf,      &
     &               bprevdayspring, daysim, bcdayspring, bczloc_regrow,&
     &                 bgmstandstem, bgmstandleaf, bgmstandstore,       &
     &                 bgmflatstem, bgmflatleaf, bgmflatstore,          &
     &                 bgmbgstemz,                                      &
     &                 bgzht, bgdstm, bgxstmrep, bggrainf )
      integer, intent(in) :: isr   ! subregion number
      integer bnslay, bctdtm, bcthudf
      real bszlyd(*)
      real bc0ck, bcgrf, bcehu0, bczmxc
      character*(80) bc0nam
      integer bc0idc
      real bcxrow
      real bczmrt, bctmin, bctopt
      real bc0fd1, bc0fd2
      real cc0fd1, cc0fd2, bc0bceff
      real bc0alf, bc0blf, bc0clf, bc0dlf, bc0arp, bc0brp
      real bc0crp, bc0drp, bc0aht, bc0bht
      real bc0sla, bc0hue, bctverndel
      real bhtsmx(*), bhtsmn(*)
      real bhfwsf
      integer bchyfg
      real bcthum, bcdpop, bcdmaxshoot
      real bc0storeinit, bcfshoot
      real bc0growdepth, bcfleafstem, bc0shoot
      real bc0diammax, bc0ssa, bc0ssb
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int, bcxstm
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      integer bcdayap, bcdayam
      real bcthucum, bctrthucum
      real bcgrainf, bczgrowpt, bcfliveleaf
      real bcleafareatrend, bcstemmasstrend
      integer bctwarmdays
      real bctchillucum, bcthardnx, bcthu_shoot_beg, bcthu_shoot_end
      real bcxstmrep
      real bprevstandstem, bprevstandleaf, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevmshoot, bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevzshoot, bprevstm, bprevrtd
      integer bprevdayap
      real bprevhucum, bprevrthucum
      real bprevgrainf, bprevchillucum, bprevliveleaf
      integer bprevdayspring
      logical bm0cif
      real    bcbaf
      integer daysim, bcdayspring
      real    bczloc_regrow
      real    bgmstandstem, bgmstandleaf, bgmstandstore
      real    bgmflatstem, bgmflatleaf, bgmflatstore
      real    bgmbgstemz(*)
      real    bgzht, bgdstm, bgxstmrep, bggrainf
      end subroutine cropgrow
!---------------------------
      subroutine cropinit(isr, crop)
      use biomaterial, only: biomatter
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      end subroutine cropinit
!---------------------------
      subroutine growth(isr, bnslay, bszlyd, bc0ck, bcgrf,              &
     &                 bcehu0, bczmxc, bc0idc, bc0nam,                  &
     &                 a_fr, b_fr, bcxrow, bc0diammax,                  &
     &                 bczmrt, bctmin, bctopt, cc0be,                   &
     &                 bc0alf, bc0blf, bc0clf, bc0dlf,                  &
     &                 bc0arp, bc0brp, bc0crp, bc0drp,                  &
     &                 bc0aht, bc0bht, bc0ssa, bc0ssb,                  &
     &                 bc0sla, bcxstm, bhtsmn,                          &
     &                 bwtdmx, bwtdmn, bweirr, bhfwsf,                  &
     &                 hui, huiy, huirt, huirty, hu_delay, bcthardnx,   &
     &                 bcbaf, bchyfg,                                   &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int,                         &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bcmbgstemz,                                      &
     &                 bczht, bcdstm, bczrtd, bcfliveleaf,              &
     &                 bcdayap, bcgrainf, bcdpop, daysim, regrowth_flg, &
     &                 bc0shoot, bcdmaxshoot )
      integer, intent(in) :: isr   ! subregion number
      integer bnslay
      real bszlyd(*), bc0ck, bcgrf
      real bcehu0, bczmxc
      integer bc0idc
      character*(80) bc0nam
      real a_fr, b_fr, bcxrow, bc0diammax
      real bczmrt, bctmin, bctopt, cc0be
      real bc0alf, bc0blf, bc0clf, bc0dlf
      real bc0arp, bc0brp, bc0crp, bc0drp
      real bc0aht, bc0bht, bc0ssa, bc0ssb
      real bc0sla, bcxstm, bhtsmn(*)
      real bwtdmx, bwtdmn, bweirr, bhfwsf
      real hui, huiy, huirt, huirty, hu_delay, bcthardnx
      real bcbaf
      integer bchyfg
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bcmbgstemz(*)
      real bczht, bcdstm, bczrtd, bcfliveleaf
      integer bcdayap
      real bcgrainf, bcdpop
      integer daysim, regrowth_flg
      real bc0shoot, bcdmaxshoot
      end subroutine growth
!----------------------------------
      real function heatunit( tmax, tmin, thres )
      real tmax, tmin, thres                
      end function heatunit
!----------------------------------
      real function huc1 (bwtdmx, bwtdmn, bctmax, bctmin)
      real bwtdmx, bwtdmn, bctmax, bctmin      
      end function huc1
!---------------------------------- 
      subroutine nconc (po, p5,p1, a)
      real a, po, p5, p1
      end subroutine nconc
!----------------------------------
      subroutine nmnim (k)           
      integer k
      end subroutine nmnim
!----------------------------------
      subroutine npcy()
      end subroutine npcy
!----------------------------------
      subroutine npmin (j)          
      integer j
      end subroutine npmin
!----------------------------------
      subroutine nuse (bn1, bn2, bn3, bp1, bp2, bp4, un1, un2, sup, cnt,&
     & hui,dm, uno3, cpt, up2, up1, upp, rw, ir, wno3,sunn, ap, wt,     &
     & a_s11,b_s11,up,rwt,suno3,un,tno3,rmnr,tap,wmp)
    
      real, intent(in) :: bn1, bn2, bn3, bp1, bp2, bp4
      real, intent(out) :: un1,sup,cnt,uno3,cpt,up2,upp
      real, intent(in) :: hui,dm,rw,un(*),rmnr,wmp
      real, intent(in) :: wt(*),a_s11,b_s11,rwt(*)
      integer, intent(in) :: ir
      real, intent(inout) :: sunn,un2,wno3(*),up(*),up1
      real, intent(inout) :: suno3,ap(*),tno3,tap   
      end subroutine nuse
!----------------------------------
      subroutine nuts (y1, y2, uu, a_s8, b_s8)
      real, intent(in) :: y1, y2, a_s8, b_s8
      real, intent(inout) :: uu   
      end subroutine nuts
!----------------------------------
      subroutine scrv1 (x1, y1, x2, y2, a, b)
      real a,b,x1,x2,y1,y2 
      end subroutine scrv1
!---------------------------------
      subroutine sdst (x,dg,dg1,i)
      integer i
      real dg, dg1, x(*)  
      end subroutine sdst
!---------------------------------
      subroutine shoot_grow( isr, bnslay, bszlyd, bcdpop,               &
     &                 bczmxc, bcfleafstem,                             &
     &                 bcfshoot, bc0ssa, bc0ssb, bc0diammax,            &
     &                 hui, huiy, bcthu_shoot_beg, bcthu_shoot_end,     &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bczgrowpt, bcfliveleaf, bc0nam,                  &
     &                 bchyfg, bcyld_coef, bcresid_int, bcgrf,          &
     &                 daysim, bcdayap )
      integer, intent(in) :: isr   ! subregion number
      integer bnslay
      real bszlyd(*), bcdpop
      real bczmxc, bcfleafstem
      real bcfshoot, bc0ssa, bc0ssb, bc0diammax
      real hui, huiy, bcthu_shoot_beg, bcthu_shoot_end
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      real bczgrowpt, bcfliveleaf
      character*(80) bc0nam
      integer bchyfg
      real bcyld_coef, bcresid_int, bcgrf
      integer daysim, bcdayap
      end subroutine shoot_grow
!----------------------------------
      subroutine shootnum( shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
     &           bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )
      integer shoot_flg, bnslay, bc0idc
      real bcdpop, bc0shoot, bcdmaxshoot
      real bcmtotshoot
      real bcmrootstorez(*)
      real bcdstm
      end subroutine shootnum
!------------------------------------
      subroutine spline(x,y,n,yp1,ypn,y2)
      real x(*), y(*), yp1, ypn, y2(*)
      integer n                           
      end subroutine spline
!-------------------------------------
      subroutine splint(xa,ya,y2a,n,x,y)

      real x, y, y2a(*), ya(*), xa(*)
      integer n  
      end subroutine splint
!-------------------------------------
      real function temps(bwtdmx, bwtdmn, bctopt, bctmin)

      real bwtdmx, bwtdmn, bctopt, bctmin
      end function temps
!--------------------------------------  

!-------------- DECOMP Routines ------------------------------
      subroutine  ddbug(isr, slay, residue)
      use biomaterial, only: biomatter
      integer isr, slay
      type(biomatter), dimension(:), intent(in) :: residue
      end subroutine ddbug
!---------------------------
      subroutine decoinit(residue, decompfac)
      use biomaterial, only: biomatter, decomp_factors
      type(biomatter), intent(inout) :: residue
      type(decomp_factors), intent(inout) :: decompfac
      end subroutine decoinit
!---------------------------
      subroutine decomp(isr, crop, residue, decompfac)
      use biomaterial, only: biomatter, decomp_factors
      integer, intent(in) :: isr
      type(biomatter), intent(inout) :: crop
      type(biomatter), dimension(:), intent(inout) :: residue
      type(decomp_factors), intent(inout) :: decompfac
      end subroutine decomp
!---------------------------
      subroutine decopen(isr)
      integer :: isr
      end subroutine decopen  
!----------------------------
      subroutine  decout(isr, residue)
      use biomaterial, only: biomatter
      integer    isr
      type(biomatter), dimension(:), intent(in) :: residue
      end subroutine decout
!---------------- EROSION Routines ---------------------------
      subroutine calcwu()
      end subroutine calcwu
!---------------------------

!---------------  HYDRO Routines -----------------------------
      real function acplwu (awcr, awcr_crit, wup)
      real awcr
      real awcr_crit
      real wup                   
      end function acplwu
!------------------------

      subroutine addsnow(dprecip, dirrig, bwzdpt, bhzirr, bhlocirr,     &
     &                   bwtdmn, bwtdmx, bwtdpt, bmzele,                &
     &                   bhzsno, bhtsno, bhfsnfrz, bhzsnd )

      real, intent(in) :: bwzdpt, bhzirr, bhlocirr
      real, intent(in) :: bwtdmn, bwtdmx, bwtdpt, bmzele
      real, intent(in) :: bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real, intent(inout) :: dirrig, dprecip
      end subroutine addsnow
!----------------------
      real function airtempsin(tsec, tmax, tmin)

      real, intent(in) ::  tsec, tmax, tmin            
      end function airtempsin
!----------------------
      real function albedo (bcrlai, snwc, sndp, bsfalw, bsfald)

      real bcrlai
      real snwc
      real sndp
      real bsfalw
      real bsfald      
      end function albedo
!------------------------
      real function atmpreselev( elevation )
      real, intent(in) :: elevation      
      end function atmpreselev
!------------------------
      real function availwc (theta, thetaw, thetaf)

      real theta, thetaw, thetaf      
      end function availwc
!------------------------
      real function calctht0( bszlyd, theta, thetaw, eratio )

      real bszlyd(*)
      real theta(0:*)
      real thetaw(*)
      real eratio      
      end function calctht0
!---------------------------
      subroutine callhydr(daysim, isr, crop, restot, biotot, h1et, wp)
      use biomaterial, only: biototal, biomatter
      use hydro_data_struct_defs, only: am0hdb, hydro_derived_et
      use wepp_param_mod, only: wepp_param
      integer daysim
      integer isr                   
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      end subroutine callhydr
!---------------------------
      subroutine darcy(isr, daysim, numeq, bszlyt, bszlyd, bulkden,     &
     &       theta, thetadmx, bthetas, bthetaf, bthetaw, bthetar,       &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav,               &
     &       bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,            &
     &       rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt,       &
     &       dirrig, bhdurirr, bhlocirr, bhzoutflow,                    &
     &       bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0,             &
     &       bhzea, bhzper, bhzrun, bhzinf, bhzwid,                     &
     &       bhzeasurf, evaplimit, vaptrans, bmrslp )
      integer, intent(in) :: isr   ! subregion number
      integer daysim, numeq
      real bszlyt(*), bulkden(*), bszlyd(*), theta(0:*)
      real thetadmx(*), bthetas(*), bthetaf(*), bthetar(*), bthetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)
      real bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt
      real rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt
      real dirrig, bhdurirr, bhlocirr, bhzoutflow
      real bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0(*)
      real bhzea, bhzper, bhzrun, bhzinf, bhzwid
      real bhzeasurf, evaplimit, vaptrans, bmrslp    
      end subroutine darcy
!----------------------
      real function depstore( ranrough, soilslope, bhzoutflow )
      real ranrough, soilslope, bhzoutflow        
      end function depstore
!----------------------
      real function diffusive( theta, porosity, airtemp, atmpres )

      real, intent(in) :: theta, porosity, airtemp, atmpres
      end function diffusive
!----------------------
      subroutine drainsnow(dh2o, bhzsno, bhfsnfrz, bhzsnd )

      real, intent(inout) :: dh2o, bhzsno, bhfsnfrz, bhzsnd      
      end subroutine drainsnow
!----------------------
      subroutine dvolw(neqn,tsec,volw,wfluxn)

      integer neqn(*)
      real tsec, volw(*), wfluxn(*)      
      end subroutine dvolw
!-----------------------
      subroutine jac (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd
      real*4 t, y(*), pd(*)
      end subroutine jac      
!-----------------------
      subroutine  et(rn, g_soil, vel_wind, bmzele, bwtdmx, bwtdmn,      &
     &            bwtdav, bwtdpt, bhzetp, loc_za, loc_zo, loc_zd)

      real rn
      real g_soil
      real vel_wind
      real bmzele
      real bwtdmn
      real bwtdmx
      real bwtdav
      real bwtdpt
      real bhzetp
      real loc_za, loc_zo, loc_zd   
      end subroutine et
!-------------------------
      real function evapredu( bhzeasurf, evaplimit, vaptrans, bhzep )

      real bhzeasurf, evaplimit, vaptrans, bhzep         
      end function evapredu
!--------------------------
      real function extra (bszlyd, theta)

      real, intent(in) :: bszlyd(*)
      real, intent(in) :: theta(0:*)
      end function extra
!--------------------------
      real function fricfact(ref_ranrough, ranrough,                    &
     &                  tot_stems, tot_flat_cov )

      real ref_ranrough, ranrough
      real tot_stems, tot_flat_cov            
      end function fricfact
!-------------------------
      real function furrowcut ( bszrgh, bsxrgw, bsxrgs )
      real bszrgh, bsxrgw, bsxrgs
      end function furrowcut
!-------------------------
      subroutine  hdbug(isr, slay, crop, restot, h1et)
      use biomaterial, only: biototal, biomatter
      use hydro_data_struct_defs, only: hydro_derived_et
      integer isr                   
      integer slay                   
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(hydro_derived_et), intent(in) :: h1et
      end subroutine hdbug
!-------------------------
      subroutine heat(isr, layrsn, bszlyd, bszlyt, theta, thetas,       &
     &                bsfsan, bsfsil, bsfcla, bsfom, bsdblk,            &
     &                bwtdmn, bwtdmx, bwtyav, rad_net, bdmres,          &
     &                bhtsmn, bhtsmx, bhtsav, bhfice,                   &
     &                bhzsno, bhtsno, bhfsnfrz, bhzsnd,                 &
     &                bhzsmt, soil_heat_flux )
      integer, intent(in) :: isr   ! subregion number
      integer layrsn
      real bszlyd(*), bszlyt(*), theta(0:*), thetas(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsdblk(*)
      real bwtdmn, bwtdmx, bwtyav, rad_net, bdmres
      real bhtsmn(*), bhtsmx(*), bhtsav(*), bhfice(*)
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, soil_heat_flux
      end subroutine heat
!-------------------------
      real function snowcond( snow_den )
      real snow_den
      end function snowcond
!-------------------------------      
      real function heatcap(bsdblk, theta, bhfice,                      &
     &                      bsfsan, bsfsil, bsfcla, bsfom)
      real, intent(in) :: bsdblk
      real, intent(in) :: theta
      real, intent(in) :: bhfice
      real, intent(in) :: bsfsan
      real, intent(in) :: bsfsil
      real, intent(in) :: bsfcla
      real, intent(in) :: bsfom              
      end function heatcap
!-----------------------
      real function heatcond(bsdblk, theta, thetas, bhtsav, bhfice,     &
     &                       bsfsan, bsfsil, bsfcla, bsfom)
      real, intent(in) :: bsdblk, theta, thetas, bhtsav, bhfice
      real, intent(in) :: bsfsan, bsfsil, bsfcla, bsfom      
      end function heatcond
!-----------------------
      subroutine hinit(layrsn, bsdblk, bsdblk0, bsdpart, bsdwblk,       &
     &                 bhrwc, bhrwcs, bhrwcf, bhrwcw, bhrwcr,           &
     &                 bhrwca, bh0cb, bheaep, bhrsk, bhfredsat,         &
     &                 bsfsan, bsfsil, bsfcla, bsfom, bsfcec,           &
     &                 bszlyd, bszlyt, vaptrans, evaplimit)
      integer layrsn
      real bsdblk(*), bsdblk0(*), bsdpart(*), bsdwblk(*)
      real bhrwc(*), bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*), bhfredsat(*)
      real bsfsan(*), bsfsil(*), bsfcla(*), bsfom(*), bsfcec(*)
      real bszlyd(*), bszlyt(*), vaptrans, evaplimit 
      end subroutine hinit
!------------------------
      subroutine hydrinit(isr, h1et, wp)
      use hydro_data_struct_defs, only: hydro_derived_et
      use wepp_param_mod, only: wepp_param
      integer isr
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      end subroutine hydrinit
!-------------------------
      subroutine hydro ( isr, layrsn, bmrslp, bbzht,                    &
     &                   bcrlai, bcrsai, bczht, bcdayap,                &
     &                   bcxrow, bc0rg, bbfcancov, bcfliveleaf,         &
     &                   bdmres, bbevapredu, bczrtd, bhfwsf,            &
     &                   bszlyd, bsdblk, bsdblk0, bsdpart, bsdwblk,     &
     &                   bhrwc, bhrwcdmx, bhrwcs, bhrwcf,               &
     &                   bhrwcw, bhrwcr, bhrwca,                        &
     &                   bh0cb, bheaep, bhfredsat,                      &
     &                   bsfsan, bsfsil, bsfcla,                        &
     &                   bsvroc, bsfom, bsfcec,                         &
     &                   bhtsav, bbdstm, bbffcv,                        &
     &                   bsxrgs, bszrgh, bsfcr,                         &
     &                   bslrro, bslrr, bmzele,                         &
     &                   bhzper,                                        &
     &                   bhzirr, bhzdmaxirr, bhratirr, bhdurirr,        &
     &                   bhlocirr, bhminirr, bm0monirr,                 &
     &                   bhmadirr, bhndayirr, bhmintirr,                &
     &                   bhzoutflow, bhzrun, bhzinf,                    &
     &                   bhzsno, bhtsno, bhfsnfrz, &
     &                   bhzsmt, bhfice, bhrsk,                         &
     &                   bhtsmx, bhtsmn, bhrwc0,                        &
     &                   daysim, bsfald, bsfalw, bszlyt,                &
     &                   bwudav, bhzwid, &
     &                   bhzeasurf,                                     &
     &                   cumprecip, cumirrig,                           &
     &                   cumrunoff, cumevap,                            &
     &                   cumtrans, cumdrain,                            &
     &                   presswc, pressnow, presday,                    &
     &                   bhztranspdepth, restot, h1et, wp)
      use biomaterial, only: biototal
      use hydro_data_struct_defs, only: am0hfl, hydro_derived_et
      use wepp_param_mod, only: wepp_param
      integer, intent(in) :: isr   ! subregion number
      integer layrsn
      real bmrslp
      real bbzht
      real bcrlai, bcrsai, bczht
      integer bcdayap
      real bcxrow
      integer bc0rg
      real bbfcancov, bcfliveleaf
      real bdmres, bbevapredu, bczrtd, bhfwsf
      real bszlyd(*), bsdblk(*), bsdblk0(*), bsdpart(*), bsdwblk(*)
      real bhrwc(*), bhrwcdmx(*), bhrwcs(*), bhrwcf(*)
      real bhrwcw(*), bhrwcr(*), bhrwca(*)
      real bh0cb(*), bheaep(*), bhfredsat(*)
      real bsfsan(*), bsfsil(*), bsfcla(*)
      real bsvroc(*), bsfom(*), bsfcec(*)
      real bhtsav(*), bbdstm, bbffcv
      real bsxrgs, bszrgh, bsfcr
      real bslrro, bslrr, bmzele
      real bhzper
      real bhzirr, bhzdmaxirr, bhratirr, bhdurirr
      real bhlocirr, bhminirr
      integer bm0monirr
      real bhmadirr
      integer bhndayirr, bhmintirr
      real bhzoutflow, bhzrun, bhzinf
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, bhfice(*), bhrsk(*)
      real bhtsmx(*), bhtsmn(*), bhrwc0(*)
      integer daysim
      real bsfald, bsfalw, bszlyt(*)
      real bwudav, bhzwid
      real bhzeasurf
      real cumprecip, cumirrig
      real cumrunoff, cumevap
      real cumtrans, cumdrain
      real presswc, pressnow, presday
      real bhztranspdepth
      type(biototal), intent(in) :: restot
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      end subroutine hydro
!-----------------------
      real function internode_wt_bc(cond_up, cond_low,                  &
     &              ksat_up, ksat_low, lambda_up, lambda_low,           &
     &              thick_up, thick_low, airentry_up, airentry_low )

      real cond_up, cond_low
      real ksat_up, ksat_low, lambda_up, lambda_low
      real thick_up, thick_low, airentry_up, airentry_low 
      end function internode_wt_bc
!-----------------------
      subroutine matricpot_bc(theta, thetar, thetas, airentry, lambda,  &
     &                        thetaw, theta80rh, soiltemp,              &
     &                        matricpot, soilrh )

      real  theta, thetar, thetas, airentry, lambda
      real  thetaw, theta80rh, soiltemp
      real  matricpot, soilrh                           
      end subroutine matricpot_bc
!------------------------
      real function matricpot_from_rh( soilrh, soiltemp )

      real  soilrh, soiltemp      
      end function matricpot_from_rh
!-------------------------
      real function movewind( meas_wind, meas_za, meas_zo, meas_zd,     &
     &                          loc_za, loc_zo, loc_zd)

      real meas_wind, meas_za, meas_zo, meas_zd
      real loc_za, loc_zo, loc_zd  
      end function movewind    
!---------------------------
      subroutine param_blkden_adj( nlay, bsdblk, bsdblk0,               &
     &                         bsdpart, bhrwcf, bhrwcw, bhrwca,         &
     &                         bsfcla, bsfom,                           &
     &                         bh0cb, bheaep, bhrsk )

      integer nlay
      real bsdblk(*), bsdblk0(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*), bhrwca(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*), bhrsk(*)      
      end subroutine param_blkden_adj
!----------------------------
      subroutine param_pot_bc( nlay, bsdblk,                            &
     &                         bsdpart, bhrwcf, bhrwcw,                 &
     &                         bsfcla, bsfom,                           &
     &                         bh0cb, bheaep )

      integer nlay
      real bsdblk(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*) 
      end subroutine param_pot_bc
!-----------------------------
      subroutine param_prop_bc( nlay, bszlyd, bsdblk, bsdpart,          &
     &                          bsfcla, bsfsan, bsfom, bsfcec,          &
     &                          bhrwcs, bhrwcf, bhrwcw, bhrwcr,         &
     &                          bhrwca, bh0cb, bheaep, bhrsk,           &
     &                          bhfredsat )

      integer nlay
      real bszlyd(*), bsdblk(*), bsdpart(*)
      real bsfcla(*), bsfsan(*), bsfom(*), bsfcec(*)
      real bhrwcs(*), bhrwcf(*), bhrwcw(*), bhrwcr(*)
      real bhrwca(*), bh0cb(*), bheaep(*), bhrsk(*)
      real bhfredsat(*) 
      end subroutine param_prop_bc
!------------------------------
      real function plant_wat_g( begind, endd, bhrwcf, bhrwcw, bsdblk,  &
     &                           bszlyt, nlay )

      integer nlay
      real bhrwcf(nlay), bhrwcw(nlay), bsdblk(nlay), bszlyt(nlay)
      real begind, endd  
      end function  plant_wat_g
!--------------------------------
      real function plant_wat_t( begind, endd, thetaf, thetaw,          &
     &                           bszlyd, nlay )

      real begind, endd
      integer nlay
      real thetaf(nlay), thetaw(nlay), bszlyd(nlay)                   
      end function plant_wat_t
!-------------------------------
      real function preslaps( elevation )
      real elevation      
      end function preslaps
!------------------------------
      subroutine printlayval( isr, daysim, layrsn,                      &
     &       bszlyt, bszlyd, bulkden,                                   &
     &       theta, thetas, thetaf, thetaw, thetar,                     &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )
      integer, intent(in) :: isr   ! subregion number
      integer daysim, layrsn
      real bszlyt(*), bszlyd(*), bulkden(*)
      real theta(0:*), thetas(*), thetaf(*), thetar(*), thetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)
      end subroutine printlayval
!--------------------------------      
      subroutine propsaxt( sandf, clayf, sat, fc, pwp )

      real sandf, clayf, sat, fc, pwp
      end subroutine propsaxt
!-------------------------------
      subroutine proptext( nlay, clayf, sandf, organf, &
     &                 settled_bulkden, proctor_bulkden, partden )

      integer nlay
      real sandf(*), clayf(*), organf(*)
      real settled_bulkden(*)
      real proctor_bulkden(*)
      real partden(*)      
      end subroutine proptext
!--------------------------------
      subroutine   psd (sandm, siltm, claym, pgmd, pgsd)

      real claym
      real pgmd
      real pgsd
      real sandm
      real siltm  
      end subroutine psd
!---------------------------------
      real function radnet( bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn, &
     &                      bmalat, bsfalw, bsfald, idoy, bwtdpt )
      real bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn
      real bmalat, bsfalw, bsfald
      integer idoy
      real bwtdpt
      end function radnet
!---------------------------------
      subroutine ratedura(bhzirr, bhratirr, bhdurirr)

      real bhzirr, bhratirr, bhdurirr              
      end subroutine ratedura
!----------------------------------
      subroutine report_hydrobal( isr, bmrotation )

      integer isr, bmrotation      
      end subroutine report_hydrobal
!----------------------------------
      real function resevapredu(                                           &
     &           prev_redu_ratio, biomass, coeff_a, coeff_b)

      real prev_redu_ratio
      real biomass
      real coeff_a
      real coeff_b      
      end function resevapredu
!-----------------------------------
      real function satvappres( airtemp )

      real airtemp      
      end function satvappres
!-----------------------------------
      real function   scsq (rain,cniip,cniig,canp,slp,theta1,thetf1)

      real rain
      real cniip
      real cniig
      real canp
      real slp
      real theta1
      real thetf1      
      end function scsq
!------------------------------------
      subroutine set_prevday_blk( nlay, bsdblk, bsdblk0 )

      integer nlay
      real bsdblk(*), bsdblk0(*)      
      end subroutine set_prevday_blk
!----------------------------------
      subroutine setlsnow(snow_wat, snow_froz_old, snow_froz_new,       &
     &                    snow_depth, snow_temp, bwtdmx )

      real snow_wat, snow_froz_old, snow_froz_new
      real snow_depth, snow_temp, bwtdmx      
      end subroutine setlsnow
!------------------------------
      real function soilrelhum(theta, thetaw, theta80rh, soiltemp,      &
     &                           matricpot)

      real*4 theta, thetaw, theta80rh, soiltemp, matricpot
      end function soilrelhum
!------------------------------
      subroutine statesnow( dh2o, new_mass, new_energy, new_depth,      &
     &                      bhzsno, bhtsno, bhfsnfrz, bhzsnd )

      real dh2o, new_mass, new_energy, new_depth
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd      
      end subroutine statesnow
!-------------------------------
      real function store (minlay, maxlay, prevvolw, volw, laydepth)

      integer minlay, maxlay
      real prevvolw(*), volw(*), laydepth(*)      
      end function store
!-------------------------------
      subroutine transp (layrsn, actflg, bszlyd, bszlyt, rootd,         &
     &                   theta, thetas, thetaf, thetaw,                 &
     &                   theta80rh, thetar, airentry, lambda,           &
     &                   ksat, soiltemp, potwu, actwu, wsf)

      integer layrsn, actflg
      real bszlyd(*), bszlyt(*), rootd
      real theta(0:*), thetas(*), thetaf(*), thetaw(*)
      real theta80rh(*), thetar(*), airentry(*), lambda(*)
      real ksat(*), soiltemp(*), potwu, actwu, wsf      
      end subroutine transp
!------------------------------
      real function transpdepth ( bczrtd, bhzfurcut,                    &
     &                            bhztransprtmin, bhztransprtmax )
      real bczrtd, bhzfurcut
      real bhztransprtmin, bhztransprtmax
      end function transpdepth
!------------------------------
      real function unsatcond_bc(theta, thetar, thetas, ksat, lambda)

      real  theta, thetar, thetas, ksat, lambda      
      end function unsatcond_bc
!-------------------------------
      real function vaporden( airtemp, relhum )

      real airtemp, relhum      
      end function vaporden
!--------------------------------
      real function volwat_matpot_bc(matricpot,thetar,thetas,           &
     &                                 airentry,lambda)

      real matricpot, thetar, thetas, airentry, lambda      
      end function volwat_matpot_bc
!---------------------------------
      real function volwatadsorb(bulkden, clayfrac, orgfrac,            &
     &                             claygrav80rh, orggrav80rh )

      real bulkden, clayfrac, orgfrac, claygrav80rh, orggrav80rh      
      end function volwatadsorb
!-------------------------------
      real function waterk (bd, cb, clay, silt)

      real bd
      real cb
      real clay
      real silt      
      end function waterk
!------------------------------
      real function wetbulb( airtemp, dewtemp, elevation )

      real airtemp, dewtemp, elevation
      end function wetbulb
!------------------------------      

!---------------- MAIN Routines ------------------------------
      subroutine bpools (isr, residue, restot, biotot, decompfac)
      use biomaterial, only: biomatter, biototal, decomp_factors
      integer isr
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(decomp_factors), intent(in) :: decompfac
      end subroutine bpools
!------------------------------
      subroutine   cmdline()
      end subroutine cmdline
!------------------------------
    subroutine confidence_interval(ci, nrot_yrs, n1cycles, ci_year, yrly_report, yr_report)
    USE pd_var_type_def
    real,    intent (in) :: ci ! confidence interval value (decimal)
    integer, intent (in) :: nrot_yrs ! number of year in a rotation cycle
    integer, intent (in) :: n1cycles ! one more than the number of rotation cycles completed
    integer, intent (inout) :: ci_year ! indicates how many years of data have been printed into ci.out
    TYPE (pd_var_type), DIMENSION(:,0:), intent(in) :: yrly_report
    TYPE (pd_var_type), DIMENSION(:,:), intent(in) :: yr_report
    end subroutine confidence_interval
!------------------------------
      subroutine   dmpall(filnam)   
      character*(*) filnam
      end subroutine dmpall
!------------------------------
    subroutine erodsubr_update( sr, restot, croptot, biotot, h1et, subrsurf )
    use biomaterial, only: biototal
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate
    integer sr                               ! subregion index (eventually obsolete)
    type(biototal), intent(in) :: restot
    type(biototal), intent(in) :: croptot
    type(biototal), intent(in) :: biotot
    type(hydro_derived_et), intent(in) :: h1et
    type(subregionsurfacestate) :: subrsurf  ! subregion surface conditions (erosion specific set)
    end subroutine erodsubr_update
!-----------------------------
      integer   function g_argc()    
      end function g_argc     
!-------------------------------
        FUNCTION get_nperiods (nrot_yrs, mandate)
        USE mandate_mod, only: opercrop_date
        INTEGER :: get_nperiods
        INTEGER :: nrot_yrs            ! Number of rotation years
        type(opercrop_date), dimension(:), intent(in) :: mandate ! array of mandates from management file
        end function get_nperiods
!-------------------------------
      subroutine inpsub (isr)
      integer isr
      end subroutine inpsub
!------------------------------      
       subroutine   input( n_rot_cycles )
      integer, intent(out) :: n_rot_cycles
      end subroutine input
!------------------------------
      subroutine input_ifc(isr)
      integer, intent(in) :: isr
      end subroutine input_ifc
!------------------------------
      subroutine mandates(sr, mandate)
      use mandate_mod, only: opercrop_date, create_mandate    ! Load shared mandate() array
      integer sr
      type (opercrop_date), dimension(:), allocatable :: mandate
      end subroutine mandates
!-----------------------------
      subroutine openfils(residue)
      use biomaterial, only: biomatter
      type(biomatter), dimension(:,:), intent(out) :: residue
      end subroutine openfils
!--------------------------------
      subroutine closefils(residue)
      use biomaterial, only: biomatter
      type(biomatter), dimension(:,:), intent(in) :: residue
      end subroutine closefils
!--------------------------------
      subroutine plotdata(sr, crop, restot, croptot, biotot, noerod, cellstate)
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: threshold
      use erosion_data_struct_defs, only: cellsurfacestate
      integer, intent(in) :: sr
      type(biomatter), intent(inout) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(threshold), intent(in) :: noerod
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
      end subroutine plotdata
!--------------------------------
      subroutine save_soil(isr)
      integer isr
      end subroutine save_soil
!--------------------------------
      subroutine sci_stir_init(isr)
      integer isr
      end subroutine sci_stir_init
!--------------------------------
      subroutine sci_cum( isr, restot, cellstate )
      use biomaterial, only: biototal
      use erosion_data_struct_defs, only: cellsurfacestate
      integer, intent(in) :: isr
      type(biototal), intent(in) :: restot
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
      end subroutine sci_cum
!--------------------------------
      subroutine sort (iarr,n,p1,p5,p9)
      integer  n
      real iarr(*),p1, p5, p9
      end subroutine sort
!--------------------------------
      subroutine spllay(isr)
      integer       isr
      end subroutine spllay
!--------------------------------
      subroutine spllay_ifc (isr)
      integer       isr
      end subroutine spllay_ifc
!--------------------------------
      subroutine submodels (isr, crop, residue, restot, croptot,        &
     &                      biotot, decompfac, mandate, h1et, wp)
      use biomaterial, only: biomatter, biototal, decomp_factors
      use mandate_mod, only: opercrop_date
      use hydro_data_struct_defs, only: hydro_derived_et
      use wepp_param_mod, only: wepp_param
      integer isr
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot, croptot, biotot
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      end subroutine submodels
!-------------------------------
      subroutine sumbio(isr, crop, residue, restot, croptot, biotot)
      use biomaterial, only: biomatter, biototal
      integer, intent(in) :: isr
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(inout) :: restot, biotot
      end subroutine sumbio
!-------------------------------
      subroutine updres(isr, residue, restot)
      use biomaterial, only: biomatter, biototal
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot
      integer isr
      end subroutine updres
!--------------------------------     
      subroutine wsum()
      end subroutine wsum
!--------------------------------  

!--------------- MANAGE Subroutines --------------------------
      subroutine cropupdate(                                            &
     &      bszrgh, bszlyd,                                             &
     &      bc0rg, bcxrow,                                              &
     &      bnslay, bc0ssa, bc0ssb,                                     &
     &      bcdpop,                                                     &
     &      bhztranspdepth, bhzfurcut,                                  &
     &      bhztransprtmin, bhztransprtmax, crop, croptot )
      use biomaterial, only: biomatter, biototal
      use p1unconv_mod, only: pi
      use wind_mod, only: biodrag
      real bszrgh, bszlyd(*)
      integer bc0rg
      real bcxrow
      integer bnslay
      real bc0ssa, bc0ssb
      real bcdpop
      real bhztranspdepth, bhzfurcut
      real bhztransprtmin, bhztransprtmax
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biototal), intent(inout) :: croptot  ! structure containing derived variables
      end subroutine cropupdate
!----------------------
      subroutine   dogroup (sr)      
      integer sr
      end subroutine dogroup
!----------------------
      subroutine   dooper (sr)
      integer sr
      end subroutine dooper
!---------------------------
      subroutine   doproc (sr, bmrotation, crop, residue, biotot, mandate)
      use biomaterial, only: biomatter, biototal
      use mandate_mod, only: opercrop_date
      integer sr, bmrotation
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      end subroutine doproc
!--------------------------
    SUBROUTINE get_calib_crops(sr, crop)
    use biomaterial, only: biomatter
    INTEGER :: sr
    type(biomatter), intent(in) :: crop    ! structure containing full crop description
    end subroutine get_calib_crops
!--------------------------
    SUBROUTINE get_calib_yield(sr,rotation_no,mass_removed, mass_left, crop)
    use biomaterial, only: biomatter
    INTEGER :: sr
    INTEGER :: rotation_no
    REAL    :: mass_removed
    REAL    :: mass_left
    type(biomatter), intent(inout) :: crop    ! structure containing full crop description
    end subroutine get_calib_yield
!--------------------------
      subroutine manage( sr, syear, crop, residue, biotot, mandate)
      use biomaterial, only: biomatter, biototal
      use mandate_mod, only: opercrop_date
      integer sr, syear
      integer lopdd, lopmm, lopyy
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      end subroutine manage
!-------------------------
      subroutine mfinit (sr, fname)
      integer sr
      character fname*(*)
      end subroutine mfinit
!--------------------------
      subroutine mgdreset (sr)
      integer sr
      end subroutine mgdreset
!---------------------------
      real function poolmass( nslay,                                    &
     &           mstandstem, mstandleaf, mstandstore,                   &
     &           mflatstem, mflatleaf, mflatstore,                      &
     &           mflatrootstore, mflatrootfiber,                        &
     &           mbgstemz, mbgleafz, mbgstorez,                         &
     &           mbgrootstorez, mbgrootfiberz )
      integer nslay
      real mstandstem
      real mstandleaf
      real mstandstore
      real mflatstem
      real mflatleaf
      real mflatstore
      real mflatrootstore
      real mflatrootfiber
      real mbgstemz(*)
      real mbgleafz(*)
      real mbgstorez(*)
      real mbgrootstorez(*)
      real mbgrootfiberz(*)
      end function poolmass      
!--------------------------
      subroutine poolupdate(bnslay, bszlyd, residue, restot)
      use biomaterial, only: biomatter, biototal
      integer :: bnslay
      real, dimension(:), intent(in) :: bszlyd
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot
      end subroutine poolupdate
!------------------------
      subroutine report_calib_harvest(sr,bmrotation,mass_rem, mass_left, crop)
      use biomaterial, only: biomatter
      integer sr, bmrotation
      real mass_rem, mass_left
      type(biomatter), intent(in) :: crop    ! structure containing full crop description
      end subroutine report_calib_harvest
!------------------------
      subroutine report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )
      use mandate_mod, only: opercrop_date
      use biomaterial, only: biomatter
      integer sr, bmrotation
      real mass_rem, mass_left
      integer harv_unit_flg
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      end subroutine  report_harvest
!-------------------------
    SUBROUTINE set_calib(sr, crop)
    use biomaterial, only: biomatter
    INTEGER :: sr
    type(biomatter), intent(in) :: crop    ! structure containing full crop description
    end subroutine set_calib
!-------------------------
      integer function skpnam(line)
      character line*80                
      end function  skpnam
!--------------------------
      subroutine tdbug(sr, slay, output, crop, residue)
      use biomaterial, only: biomatter
      integer sr, slay, output
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      end subroutine tdbug
!--------------------------
      integer function tillay (tdepth, lthick, nlay)
      real    tdepth
      integer nlay
      real    lthick(*)
      end function tillay
!---------------------------
            
!---------------  MPROC Routines -----------------------------
      subroutine buryadj( burycoef,mnrbc,                               &
     &                    speed,stdspeed,minspeed,maxspeed,             &
     &                    depth,stddepth,mindepth,maxdepth)
      integer mnrbc
      real    burycoef(mnrbc)
      real    speed,stdspeed,minspeed,maxspeed
      real    depth,stddepth,mindepth,maxdepth      
      end subroutine buryadj
!-----------------------------
      real function burydist( lay, burydistflg, lthick, ldepth, nlay)
      integer lay
      integer burydistflg
      real    lthick(*)
      real    ldepth(*)
      integer nlay      
      end function burydist
!-----------------------------
      subroutine burylift                                               &
     &              (nlay,dflat,dstand,droot,                           &
     &               dblwgnd,buryf,liftf,fltcoef) 
      integer nlay
      real    buryf,liftf,fltcoef 
      real    dflat(*),dstand(*)
      real   dblwgnd(3,*), droot(3,*)  
      end subroutine burylift
!-------------------------------
      subroutine crush (alpha, beta,nlay,mf)
      real    alpha, beta
      integer nlay
      real    mf(26+1,*) 
      end subroutine crush         
!-------------------------------
      subroutine crust (crustf_rm,tillf,crustf,lmosf, lmosm)
      real tillf, crustf, crustf_rm, lmosf, lmosm
      end subroutine crust
!-------------------------------
      subroutine cut (                                                  &
     &           cutflg, cutht, grainf, cropf, standf,                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bczht, bcgrainf, bchyfg,                               &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btzht, btgrainf, residue,                              &
     &           tot_mass_rem, sel_mass_left)
      use biomaterial, only: biomatter
      integer cutflg
      real    cutht, grainf, cropf, standf
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bczht, bcgrainf
      integer bchyfg
      real btmstandstem, btmstandleaf, btmstandstore
      real btmflatstem, btmflatleaf, btmflatstore
      real btzht, btgrainf
      type(biomatter), dimension(:), intent(inout) :: residue
      real tot_mass_rem, sel_mass_left
      end subroutine cut
!---------------------------------
      subroutine cut_pool (                                             &
     &           poolcutht, grainf, cropf,                                  &
     &           poolmstandstem, poolmstandleaf, poolmstandstore,       &
     &           poolzht, poolgrainf, poolhyfg,                         &
     &           poolmflatstem, poolmflatleaf, poolmflatstore,          &
     &           tot_mass_rem, sel_mass_left )
      real    poolcutht
      real    grainf
      real    cropf
      real    poolmstandstem
      real    poolmstandleaf
      real    poolmstandstore
      real    poolzht
      real    poolgrainf
      integer poolhyfg
      real    poolmflatstem
      real    poolmflatleaf
      real    poolmflatstore
      real    tot_mass_rem
      real    sel_mass_left
      end subroutine cut_pool
!---------------------------------
      subroutine fall_mod_vt ( rate_mult_vt, thresh_mult_vt,            &
     &                         sel_pool, fracarea,                      &
     &                         bcrbc, bcdkrate, bcddsthrsh,             &
     &                         residue )
      use biomaterial, only: biomatter
      real       rate_mult_vt(*)
      real       thresh_mult_vt(*)
      integer    sel_pool
      real       fracarea
      integer    bcrbc
      real       bcdkrate(*)
      real       bcddsthrsh
      type(biomatter), dimension(:), intent(inout) :: residue
      end subroutine fall_mod_vt  
!-----------------------------------
      subroutine flatvt                                                 &
     &                 (fltcoef, tillf, bcrbc,                          &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           bcdstm, residue, bflg)
      use biomaterial, only: biomatter
      real    fltcoef(*)
      real    tillf
      integer bcrbc
      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore
      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore
      real    bcdstm
      type(biomatter), dimension(:), intent(inout) :: residue
      integer bflg
      end subroutine flatvt
!---------------------------
      real function func(y) 
      real y
      end function func
!---------------------------
      subroutine invert                                                 &
     &              (nlay,density,laythk,                               &
     &               sand,silt,clay, rock_vol,                          &
     &               c_sand, m_sand, f_sand, vf_sand,                   &
     &               w_bd,                                              &
     &               organic, ph, calcarb, cation,                      &
     &               lin_ext,                                           &
     &               aggden, drystab,                                   &
     &               soilwatr,                                          &
     &               satwatr, thrdbar, ftnbar,                          &
     &               avawatr,                                           &
     &               soilcb,soilair,satcond,                            &
     &               residue, massf)
      use biomaterial, only: biomatter
      integer nlay
      real density(*),laythk(*)
      real sand(*),silt(*),clay(*), rock_vol(*)
      real c_sand(*), m_sand(*), f_sand(*), vf_sand(*)
      real w_bd(*)
      real organic(*), ph(*), calcarb(*), cation(*)
      real lin_ext(*)
      real aggden(*), drystab(*)
      real soilwatr(*)
      real satwatr(*), thrdbar(*), ftnbar(*)
      real avawatr(*)
      real soilcb(*), soilair(*), satcond(*)
      type(biomatter), dimension(:), intent(inout) :: residue
      real massf(26+1,*) 
      end subroutine invert
!--------------------------
      subroutine invproc(nlay,thick,xcomp) 
      real xcomp(*), thick(*)
      integer nlay  
      end subroutine invproc
!---------------------------
      subroutine kill_crop( am0cgf, nlay,                               &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bcmbgstemz,                                            &
     &           bczht, bcdstm, bcxstmrep, bczrtd,                      &
     &           bcgrainf,                                              &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           btmbgstemz,                                            &
     &           btzht, btdstm, btxstmrep, btzrtd,                      &
     &           btgrainf )

      logical am0cgf
      integer nlay

      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore

      real    bcmflatstem
      real    bcmflatleaf
      real    bcmflatstore

      real    bcmrootstorez(*)
      real    bcmrootfiberz(*)

      real    bcmbgstemz(*)

      real    bczht
      real    bcdstm
      real    bcxstmrep
      real    bczrtd

      real    bcgrainf

      real    btmstandstem
      real    btmstandleaf
      real    btmstandstore

      real    btmbgstemz(*)

      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore

      real    btmbgrootstorez(*)
      real    btmbgrootfiberz(*)

      real    btzht
      real    btdstm
      real    btxstmrep
      real    btzrtd

      real    btgrainf  
      end subroutine kill_crop
!---------------------------------
      subroutine liftvt (liftf, tillf, nlay, residue, resurface_roots, bflg)
      use biomaterial, only: biomatter
      real    liftf(*)
      real    tillf
      integer nlay
      type(biomatter), dimension(:), intent(inout) :: residue
      integer resurface_roots
      integer bflg
      end subroutine liftvt
!--------------------------------
      subroutine loosn (u,tillf,nlay,density,sbd,laythk)

      integer nlay
      real    u,tillf,density(*),laythk(*),sbd(*)                                     
      end subroutine loosn
!---------------------------------
      subroutine mburyvt                                                &
     &          (buryf,tillf,bcrbc,burydistflg,                         &
     &           nlay,lthick,ldepth,                                    &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmflatrootstore, btmflatrootfiber,                    &
     &           btmbgstemz, btmbgleafz, btmbgstorez,                   &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           residue, bflg)
      use biomaterial, only: biomatter
      real    buryf(*)
      real    tillf
      integer bcrbc
      integer burydistflg
      integer nlay
      real    lthick(*)
      real    ldepth(*)
      real   btmflatstem
      real   btmflatleaf
      real   btmflatstore
      real   btmflatrootstore
      real   btmflatrootfiber
      real   btmbgstemz(*)
      real   btmbgleafz(*)
      real   btmbgstorez(*)
      real   btmbgrootstorez(*)
      real   btmbgrootfiberz(*)
      type(biomatter), dimension(:), intent(inout) :: residue
      integer bflg
      end subroutine mburyvt
!-------------------------------
      subroutine mix                                                    &
     &              (u,tillf,nlay,density,laythk,                       &
     &               sand,silt,clay, rock_vol,                          &
     &               c_sand, m_sand, f_sand, vf_sand,                   &
     &               w_bd,                                              &
     &               organic, ph, calcarb, cation,                      &
     &               lin_ext,                                           &
     &               aggden, drystab,                                   &
     &               soilwatr,                                          &
     &               satwatr, thrdbar, ftnbar,                          &
     &               avawatr,                                           &
     &               soilcb,soilair,satcond,                            &
     &               residue, massf)
      use biomaterial, only: biomatter
      integer nlay
      real u,tillf,density(*),laythk(*)
      real sand(*),silt(*),clay(*), rock_vol(*)
      real c_sand(*), m_sand(*), f_sand(*), vf_sand(*)
      real w_bd(*)
      real organic(*), ph(*), calcarb(*), cation(*)
      real lin_ext(*)
      real aggden(*), drystab(*)
      real soilwatr(*)
      real satwatr(*), thrdbar(*), ftnbar(*)
      real avawatr(*)
      real soilcb(*), soilair(*), satcond(*)
      type(biomatter), dimension(:), intent(inout) :: residue
      real massf(26+1,*)  
      end subroutine mix
!-------------------------
      subroutine mixproc(u, nlay, xcomp, cmass, mass) 

      integer nlay  
          real xcomp(*), mass, cmass, u          
          end subroutine mixproc
!--------------------------
      subroutine orient                                                 &
     &              (rh,rw,rs,rd,dh,ds,                                 &
     &              impl_rh,impl_rw,impl_rs,impl_rd,                    &
     &              impl_dh,impl_ds,tilld,rflag)
      real     rh,rw,rs,rd,dh,ds
      real     impl_rh,impl_rw,impl_rs,impl_rd
      real     impl_dh,impl_ds
      real     tilld
      integer  rflag
      end subroutine orient
!----------------------------
      subroutine orient1                                                &
     &              (rh,rw,rs,rd,                                       &
     &              impl_rh,impl_rw,impl_rs,impl_rd,                    &
     &              tilld,rflag) 
      real     rh,rw,rs,rd
      real     impl_rh,impl_rw,impl_rs,impl_rd
      real     tilld
      integer  rflag
      end subroutine orient1
!-----------------------------
      subroutine orient2 (dh,ds,impl_dh,impl_ds)  
      real     dh,ds
      real     impl_dh,impl_ds    
      end subroutine orient2
!------------------------------
      subroutine remove (                                               &
     &           sel_position, sel_pool, bflg,                          &
     &           stemf, leaff, storef, rootstoref, rootfiberf,          &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bcmbgstemz,                                            &
     &           bczht, bcdstm, bcgrainf, bchyfg,                       &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btmflatrootstore, btmflatrootfiber,                    &
     &           btmbgstemz, btmbgleafz, btmbgstorez,                   &
     &           btmbgrootstorez, btmbgrootfiberz,                      &
     &           btzht, btdstm, btgrainf, residue,                      &
     &           nslay, tot_mass_rem, sel_mass_left)
      use biomaterial, only: biomatter
      integer sel_position, sel_pool, bflg
      real stemf, leaff, storef, rootstoref, rootfiberf
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bcdstm, bcgrainf
      integer bchyfg
      real btmstandstem, btmstandleaf, btmstandstore
      real btmflatstem, btmflatleaf, btmflatstore
      real btmflatrootstore, btmflatrootfiber
      real btmbgstemz(*), btmbgleafz(*), btmbgstorez(*)
      real btmbgrootstorez(*), btmbgrootfiberz(*)
      real bcmbgstemz(*)
      real btzht, btdstm, btgrainf
      type(biomatter), dimension(:), intent(inout) :: residue
      integer nslay
      real   tot_mass_rem, sel_mass_left
      end subroutine remove
!---------------------------------
      subroutine rem_stand_pool(                                        &
     &      stemf, leaff, storef, rootstoref, rootfiberf,               &
     &      pool_stem, pool_leaf, pool_store,                           &
     &      pool_rootstore, pool_rootfiber,                             &
     &      nslay, pool_hyfg, pool_grainf, pool_dstm,                   &
     &      tot_mass_rem, sel_mass_left )
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, pool_dstm, tot_mass_rem, sel_mass_left
      end subroutine rem_stand_pool
!---------------------------------
      subroutine rem_flat_pool(                                         &
     &           stemf, leaff, storef, rootstoref, rootfiberf,          &
     &           pool_stem, pool_leaf, pool_store,                      &
     &           pool_rootstore, pool_rootfiber,                        &
     &           pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store, pool_leaf, pool_stem
      real pool_rootstore, pool_rootfiber
      integer pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left
      end subroutine rem_flat_pool
!---------------------------------
      subroutine rem_bg_pool(                                           &
     &      stemf, leaff, storef, rootstoref, rootfiberf,               &
     &      pool_stem, pool_leaf, pool_store,                           &
     &      pool_rootstore, pool_rootfiber,                             &
     &      nslay, pool_hyfg, pool_grainf, tot_mass_rem, sel_mass_left )
      real stemf, leaff, storef, rootstoref, rootfiberf
      real pool_store(*), pool_leaf(*), pool_stem(*)
      real pool_rootstore(*), pool_rootfiber(*)
      integer nslay, pool_hyfg
      real pool_grainf, tot_mass_rem, sel_mass_left
      end subroutine rem_bg_pool
!---------------------------------
      subroutine adj_stand_pool(                                        &
     &      start_standstem, start_standleaf, start_standstore,         &
     &      start_rootstore, start_rootfiber,                           &
     &      pool_standstem, pool_standleaf, pool_standstore,            &
     &      pool_rootstore, pool_rootfiber,                             &
     &      pool_flatstem, pool_flatleaf, pool_flatstore,               &
     &      pool_dstm, nslay)
      real start_standstem, start_standleaf, start_standstore
      real start_rootstore(*), start_rootfiber(*)
      real pool_standstem, pool_standleaf, pool_standstore
      real pool_rootstore(*), pool_rootfiber(*)
      real pool_flatstem, pool_flatleaf, pool_flatstore
      real pool_dstm
      integer nslay
      end subroutine adj_stand_pool
!---------------------------------
      subroutine resinit(resmass, resdepth, nlay, resarray, laythick)

      real resmass
      real resdepth
      integer nlay
      real resarray(*)
      real laythick(*)      
      end subroutine resinit
!-----------------------------------
      integer function rootlay (rtdepth, lthick, nlay)

      integer nlay
      real    rtdepth
      real    lthick(*)      
      end function rootlay
!-----------------------------------
      subroutine rough                                                  &
     &              (roughflg, rrimpl,till_i,tillf,                     &
     &               rr, tillay, clayf, siltf,                          &
     &               rootmass, resmass,                                 &
     &               ldepth ) 
      integer roughflg
      real    tillf,rrimpl,rr,till_i
      integer tillay
      real    clayf(*), siltf(*)
      real    rootmass(:), resmass(:)
      real    ldepth(*)
      end subroutine rough
!---------------------
      subroutine thin (                                                 &
     &           thinflg, thinval, grainf, cropf, standf,               &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcdstm, bcgrainf, bchyfg,                              &
     &           btmstandstem, btmstandleaf, btmstandstore,             &
     &           btmflatstem, btmflatleaf, btmflatstore,                &
     &           btdstm, btgrainf, residue,                             &
     &           tot_mass_rem, sel_mass_left)
      use biomaterial, only: biomatter
      integer thinflg
      real    thinval, grainf, cropf, standf
      real    bcmstandstem
      real    bcmstandleaf
      real    bcmstandstore
      real    bcmflatstem
      real    bcmflatleaf
      real    bcmflatstore
      real    bcdstm
      real    bcgrainf
      integer bchyfg
      real    btmstandstem
      real    btmstandleaf
      real    btmstandstore
      real    btmflatstem
      real    btmflatleaf
      real    btmflatstore
      real    btdstm
      real    btgrainf
      type(biomatter), dimension(:), intent(inout) :: residue
      real    tot_mass_rem, sel_mass_left
      end subroutine thin
!----------------------------------
      subroutine thin_pool (                                            &
     &           thinval, grainf, cropf,                                &
     &           poolmstandstem, poolmstandleaf, poolmstandstore,       &
     &           poolmflatstem, poolmflatleaf, poolmflatstore,          &
     &           poolgrainf, poolhyfg, tot_mass_rem, sel_mass_left)
      real    thinval, grainf, cropf
      real    poolmstandstem, poolmstandleaf, poolmstandstore
      real    poolmflatstem, poolmflatleaf, poolmflatstore
      integer poolhyfg
      real    poolgrainf, tot_mass_rem, sel_mass_left
      end subroutine thin_pool
!----------------------------------
      subroutine trans(                                                 &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmflatrootstore, bcmflatrootfiber,                    &
     &           bcmbgstemz, bcmbgleafz, bcmbgstorez,                   &
     &           bcmbgrootstorez, bcmbgrootfiberz,                      &
     &           bczht, bcdstm, bcxstmrep, bcgrainf,                    &
     &         bc0nam, bcxstm, bcrbc, bc0sla, bc0ck,                    &
     &         bcdkrate, bccovfact, bcddsthrsh, bchyfg,                 &
     &         bcresevapa, bcresevapb,                                  &
     &         nslay, residue )
      use biomaterial, only: biomatter
      type(biomatter), dimension(:), intent(inout) :: residue
      real             bcmstandstem !added state
      real             bcmstandleaf !added state
      real             bcmstandstore !added state
      real             bcmflatstem !added state
      real             bcmflatleaf !added state
      real             bcmflatstore !added state
      real             bcmflatrootstore !added state
      real             bcmflatrootfiber !added state
      real             bcmbgstemz(*) !added state
      real             bcmbgleafz(*) !added state
      real             bcmbgstorez(*) !added state
      real             bcmbgrootstorez(*) !added state
      real             bcmbgrootfiberz(*) !added state
      real             bczht  !changed from tczht state
      real             bcdstm !changed from tcdstm state
      real             bcxstmrep !changed from tcxstmrep state
      real             bcgrainf !added state
      character*(80)  bc0nam
      real       bcxstm
      integer    bcrbc
      real       bc0sla
      real       bc0ck
      real       bcdkrate(*)
      real       bccovfact
      real       bcddsthrsh
      integer    bchyfg
      real       bcresevapa 
      real       bcresevapb
      integer    nslay
      end subroutine trans
!------------------------------
          subroutine trapzd(a,b,s,n) 
      integer n
          real a, b, s
          end subroutine trapzd
!------------------------------

!--------------- REPORTS Routines ----------------------------
    SUBROUTINE init_report_vars(nperiods, nrot_yrs, ncycles, mandate, rep_report, rep_update)
    USE pd_dates_vars
    USE pd_update_vars
    USE pd_report_vars
    USE mandate_mod, only: opercrop_date
    INTEGER, INTENT (IN) :: nperiods   ! 24 is minimum value per rotation year
    INTEGER, INTENT (IN) :: nrot_yrs   ! Minimum is 1
    INTEGER, INTENT (IN) :: ncycles    ! number of rotation cycles
    type (opercrop_date), dimension(:), intent(in) :: mandate
    type(reporting_report), intent(inout) :: rep_report
    type(reporting_update), intent(inout) :: rep_update
    end subroutine init_report_vars
!----------------------
    SUBROUTINE print_mandate_output(lun, mperod, mandate)
    use mandate_mod, only: opercrop_date
    INTEGER :: lun             ! output file unit number
    integer :: mperod             ! number of year in man rotation file
    type (opercrop_date), dimension(:), intent(in) :: mandate
    end subroutine print_mandate_output
!----------------------
    SUBROUTINE print_report_vars(nperiods, nrot_yrs, rep_report, mandate)
    USE pd_report_vars
    use mandate_mod, only: opercrop_date
    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_yrs
    type (reporting_report), intent(in) :: rep_report
    type (opercrop_date), dimension(:), intent(in) :: mandate
    end subroutine print_report_vars
!-----------------------
SUBROUTINE print_ui1_output(luogui1, nperiods, nrot_years, ncycles, rep_report, mandate)
    USE pd_report_vars
    use mandate_mod, only: opercrop_date
    integer, intent(in) :: luogui1         ! subregion number for output file selection
    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: ncycles
    type(reporting_report), intent(in) :: rep_report
    type (opercrop_date), dimension(:), intent(in) :: mandate
    end subroutine print_ui1_output
!-----------------------
    SUBROUTINE print_yr_report_vars(nperiods, nrot_yrs, ncycles, yr_report)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_yrs
    INTEGER, INTENT (IN) :: ncycles
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,:), intent(in) :: yr_report
    end subroutine print_yr_report_vars
!----------------------
      SUBROUTINE run_ave(pd_ave, new_val, cnt) 
      USE pd_var_type_def
      TYPE (pd_var_type),INTENT (INOUT) :: pd_ave
      REAL,    INTENT (IN) :: new_val
      INTEGER, INTENT (IN) :: cnt      
      end subroutine run_ave
!-----------------------
    SUBROUTINE update_hmonth_update_vars(isr, cd, cm, hmonth_update, hmrot_update, h1et)
    USE pd_var_type_def, only: pd_var_type
    USE pd_var_tables
    use hydro_data_struct_defs, only: hydro_derived_et
    INTEGER, intent (in) :: isr  ! current subregion
    INTEGER, INTENT (IN) :: cd  ! current day
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update
    type(hydro_derived_et), intent(in) :: h1et
    end subroutine update_hmonth_update_vars
!-----------------------
    SUBROUTINE update_hmonth_report_vars(cur_day, cur_month, cur_yr, nrot_years, hmonth_update, hmrot_update, hmonth_report)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: cur_day  
    INTEGER, INTENT (IN) :: cur_month  
    INTEGER, INTENT (IN) :: cur_yr  
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:), intent(inout) :: hmonth_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:), intent(inout) :: hmrot_update
    TYPE (pd_var_type), DIMENSION(Min_hmonth_vars:,:,0:), intent(inout) :: hmonth_report
    end SUBROUTINE update_hmonth_report_vars
!-----------------------
    SUBROUTINE update_monthly_update_vars(isr, cm, monthly_update, mrot_update, cellstate, h1et)
    USE pd_var_type_def
    USE pd_var_tables
    use erosion_data_struct_defs, only: cellsurfacestate, awdair, awudmx, subday, ntstep 
    use hydro_data_struct_defs, only: hydro_derived_et
    INTEGER, intent (in) :: isr  ! current subregion
    INTEGER, INTENT (IN) :: cm  ! current month
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
    type(hydro_derived_et), intent(in) :: h1et
    end subroutine  update_monthly_update_vars
!------------------------
SUBROUTINE update_monthly_report_vars(cur_month, cur_year, nrot_years, monthly_update, mrot_update, monthly_report)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: cur_month
    INTEGER, INTENT (IN) :: cur_year
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:,0:), intent(inout) :: monthly_report
    end SUBROUTINE update_monthly_report_vars
!------------------------
SUBROUTINE update_period_update_vars(sbr, period_update, restot, croptot, biotot, cellstate)
    USE pd_var_tables
    USE pd_var_type_def
    use biomaterial, only: biototal
    use erosion_data_struct_defs, only: cellsurfacestate
    INTEGER :: sbr              ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    type(biototal), intent(in) :: restot  ! contains:
    type(biototal), intent(in) :: croptot  ! contains:
    type(biototal), intent(in) :: biotot  ! contains:
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10
    end subroutine  update_period_update_vars
!-------------------------
SUBROUTINE update_period_report_vars(pd, npd, cur_yr, nrot_years, period_update, period_report)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: pd, npd
    INTEGER, INTENT (IN) :: cur_yr
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    TYPE (pd_var_type), DIMENSION(Min_period_vars:,:), intent(inout) :: period_report
    end SUBROUTINE update_period_report_vars
!-------------------------            
    SUBROUTINE update_yrly_update_vars(isr, yrly_update, yrot_update, yr_update, cellstate, h1et)
    USE pd_var_type_def
    USE pd_var_tables
    use erosion_data_struct_defs, only: cellsurfacestate, awdair, awudmx, subday, ntstep 
    use hydro_data_struct_defs, only: hydro_derived_et
    INTEGER, intent (in) :: isr  ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10
    type(hydro_derived_et), intent(in) :: h1et
    end subroutine update_yrly_update_vars
!-------------------------            
    SUBROUTINE update_yrly_report_vars(cur_year, nrot_years, yrly_update, yrot_update, yr_update, yrly_report, yr_report)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: cur_year
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,0:), intent(inout) :: yrly_report
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,:), intent(inout) :: yr_report
    end SUBROUTINE update_yrly_report_vars
!-------------------------            
!---------------  Soil Routines ------------------------------       
!-----------    
      subroutine aggsta(daysim,                                         &
     &  cseags, cseagmn, cseagmx,                                       &
     &  cbhrwc0, cbhrwc, cbhrwcdmx,                                     &
     &  chrwcw, chrwca,chrwcs,                                          &
     &  chtmx0, chtsmn, chtsmx, ck4d,                                   &
     &  se0, se1, trigger,                                              &
     &  k4f, k4fs, k4fd, k4td, k4w, k4d)
      integer daysim
      real cseags, cseagmn, cseagmx
      real  cbhrwc0, cbhrwc, cbhrwcdmx
      real  chrwcw, chrwca,chrwcs
      real  chtmx0, chtsmn, chtsmx, ck4d
      real  se0, se1
      integer trigger
      real k4f, k4fs, k4fd, k4td, k4w, k4d
      end subroutine aggsta
!------------         
      subroutine asd( cslagm, cslmin, cslmax, chtsmx, chtmx0, cs0ags,   &
     &  cslagx, se0, se1)
      real cslagm, cslmin
      real cslmax, chtsmx, chtmx0, cs0ags
      real cslagx, se0, se1
      end subroutine asd
!-------------         
      subroutine callsoil(daysim, isr, croptot, biotot)
      use biomaterial, only: biototal
      integer daysim
      integer isr                   
      type(biototal), intent(in) :: croptot, biotot
      end subroutine callsoil 
!-------------
      subroutine cru(bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,             &
     &  bsmlos,csfom,csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos)
      real bszcr,cumpa,csfcla,dcump,bsfcr,bhzsmt,bsmlos,csfom
      real csfcce,csfsan,bsmls0,bszrgh,bszrr,bsflos        
      end subroutine cru
!---------------      
      subroutine den(                                                   &
     &  csdblk, csdsblk, csdwblk, cszlyt, csdagd,                       &
     &  chrwc0, chrwc, chrwca, chrwcw,                                  &
     &  bhzinf, chzwid, trigger)
      real csdblk, csdsblk, csdwblk, cszlyt, csdagd
      real chrwc0, chrwc, chrwca, chrwcw
      real bhzinf, chzwid
      integer trigger
      end subroutine den
!----------------      
      subroutine depthini(nlay, bszlyt, bszlyd)
      integer nlay
      real    bszlyt(*), bszlyd(*)
      end subroutine depthini
!----------------
      subroutine ranrou(                                                &
     &  csfsil, csfsan, bszrr, bszrro, cumpa, dcump, cf2cov, csvroc)
      real csfsil, csfsan
      real bszrr, bszrro
      real cumpa, dcump, cf2cov, csvroc
      end subroutine ranrou
!------------------
      subroutine rid(cf2cov, bbfscv, bbffcv, bszrgh,                    &
     &  bsxrgs, bszrho, cumpa, dcump, bsvroc)
      real cf2cov, bbfscv, bbffcv, bszrgh, bsxrgs, bszrho
      real cumpa, dcump, bsvroc(*)  
      end subroutine rid
!------------------
      subroutine  sdbug(isr,slay, croptot, biotot)
      use biomaterial, only: biototal
      integer isr, slay
      type(biototal), intent(in) :: croptot, biotot
      end subroutine sdbug
!------------------
      subroutine sinit (daysim,                                         &
     &                 bhtsmx, bhrwc, bsfom, bszlyt,                    &
     &                 bslay, bsfsan, bsfsil, bsfcla,                   &
     &                 bszrgh, bszrr, bsfcce, bsfcec,                   &
     &                 cump, dcump, bsk4d,                              &
     &                 bhtmx0, bhrwc0, szlyd,                           &
     &                 bszrr0, bszrh0,                                  &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bslmin, bslmax,                                  &
     &                 rain, snow, sprink,                              &
     &                 bhzirr, bszrho,                                  &
     &                 bhlocirr, bhzsmt, bszrro,                        &
     &                 bsdsblk, bwzdpt, bwtdav, trigger)
      integer daysim
      real bhtsmx(*), bhrwc(*), bsfom(1:*), bszlyt(*)
      integer bslay
      real bsfsan(1:*), bsfsil(1:*), bsfcla(1:*)
      real bszrgh, bszrr, bsfcce(1:*), bsfcec(1:*)
      real cump, dcump, bsk4d(*)
      real bhtmx0(*), bhrwc0(*), szlyd(0:*)
      real bszrr0, bszrh0
      real bseagm(*), bseagmn(*), bseagmx(*)
      real bslmin(*),bslmax(*)
      real rain, snow, sprink
      real bhzirr, bszrho
      real bhlocirr, bhzsmt, bszrro
      real bsdsblk(*), bwzdpt, bwtdav
      integer trigger(*)
      end subroutine sinit
!-----------------
      subroutine soil (isr, daysim, bhlocirr, bhzirr, bhzsmt,           &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhrwc, bhrwcdmx, bhrwca,                         &
     &                 bhrwcw, bhrwcs, bszlyt, bslay,                   &
     &                 bsfsan, bsfsil, bsfcla, bsfom, bsvroc,           &
     &                 bsxrgs, bszrgh, bszrho,                          &
     &                 bszrr, bszrro,                                   &
     &                 bszcr, bsfcr, bsecr, bsdcr,                      &
     &                 bsmlos, bsflos,                                  &
     &                 bsdsblk, bsdwblk,                                &
     &                 bsdblk, bsdagd,                                  &
     &                 bslagm, bslagn,                                  &
     &                 bs0ags, bslagx, bseags,                          &
     &                 bseagm, bseagmn, bseagmx,                        &
     &                 bsk4d, bslmin, bslmax,                           &
     &                 bbffcv, bbfscv,                                  &
     &                 bsfcce, bsfcec, bhzinf, bhzwid)
      integer, intent(in) :: isr   ! subregion number
      integer daysim
      real bhlocirr, bhzirr, bhzsmt
      real bhtsmx(*), bhtsmn(*)
      real bhrwc(*), bhrwcdmx(*), bhrwca(*)
      real bhrwcw(*), bhrwcs(*), bszlyt(*)
      integer bslay
      real bsfsan(1:*), bsfsil(1:*), bsfcla(1:*)
      real bsfom(1:*), bsvroc(1:*)
      real bsxrgs, bszrgh, bszrho
      real bszrr, bszrro
      real bszcr, bsfcr, bsecr, bsdcr
      real bsmlos, bsflos
      real bsdsblk(*), bsdwblk(*)
      real bsdblk(0:*), bsdagd(0:*)
      real bslagm(0:*), bslagn(0:*)
      real bs0ags(0:*), bslagx(0:*), bseags(0:*)
      real bseagm(*), bseagmn(*), bseagmx(*)
      real bsk4d(*), bslmin(*), bslmax(*)
      real bbffcv, bbfscv
      real bsfcce(1:*), bsfcec(1:*)
      real bhzinf, bhzwid
      end subroutine soil
!------------------------
      subroutine soilinit(isr)                                
      integer isr
      end subroutine soilinit
!-----------------------
      subroutine updlay(daysim, szlyd,                                  &
     &  bhrwc0, bhrwc, bhrwcdmx,                                        &
     &  bseagmx, bseagmn, bseags,                                       &
     &  bhrwca, bhrwcw, bhrwcs,                                         &
     &  bhtsmn, bhtmx0, bhtsmx,                                         &
     &  bsecr,                                                          &
     &  bsk4d, bslmin, bslmax,                                          &
     &  bslagm,                                                         &
     &  bs0ags, bslagx, bsdblk,                                         &
     &  bszlyt, bsdagd, bslay, bsdcr,                                   &
     &  bsdsblk, bsdwblk,                                               &
     &  bhzinf, bhzwid, trigger)
      integer daysim
      real szlyd(0:*)
      real  bhrwc0(*), bhrwc(*), bhrwcdmx(*)
      real  bseagmx(*), bseagmn(*), bseags(0:*)
      real  bhrwca(*), bhrwcw(*),bhrwcs(*)
      real  bhtsmn(*), bhtmx0(*), bhtsmx(*)
      real  bsecr
      real bsk4d(*), bslmin(*), bslmax(*)
      real bslagm(0:*)
      real bs0ags(0:*), bslagx(0:*)
      real bsdblk(0:*), bhzinf
      real bszlyt(*), bsdagd(0:*)
      real bsdcr, bsdsblk(*), bsdwblk(*)
      real bhzwid
      integer bslay, trigger(*)
      end subroutine updlay  
!-----------------------
!---------------- WEPP in WEPS Routines ----------------------------
      subroutine waterbal(layrsn, thetas, thetes, thetaf, thetaw,       &
     &                   bszlyt, bszlyd, satcond,                       &
     &                   dprecip, bwdurpt, bwpeaktpt, bwpeakipt,        &
     &                   dirrig, bhdurirr, bhlocirr, bhzoutflow,        &
     &                   bhzsno, bslrr, bmrslp, bsfsan, bsfcla,         &
     &                   bsfcr, bsvroc, bsdblk, bsfcec,                 &
     &                   bbffcv, bbfcancov, bbzht, bcdayap,             &
     &                   bhzep, theta, thetadmx, bhrwc0,                &
     &                   bhzea, bhzper, bhzrun, bhzinf, bhzwid,         &
     &                   slen, cd, cm, cy, isr,                         &
     &                   wepp_hydro, init_loop, calib_loop, bhfice, wp)
      use wepp_param_mod, only: wepp_param
      integer, intent(in) :: layrsn
      real, intent(in) :: thetas(*), thetes(*), thetaf(*), thetaw(*)
      real, intent(in) :: bszlyt(*), bszlyd(*), satcond(*)
      real, intent(in) :: dprecip, bwdurpt, bwpeaktpt, bwpeakipt
      real, intent(in) :: dirrig, bhdurirr, bhlocirr, bhzoutflow
      real, intent(in) :: bhzsno, bslrr, bmrslp, bsfsan(*), bsfcla(*)
      real, intent(in) :: bsfcr, bsvroc(*), bsdblk(*), bsfcec(*)
      real, intent(in) :: bbffcv, bbfcancov, bbzht
      integer, intent(in) :: bcdayap
      real, intent(in) :: bhzep
      real, intent(inout) :: theta(0:*), thetadmx(*), bhrwc0(*)
      real, intent(inout) :: bhzea, bhzper, bhzrun, bhzinf, bhzwid
      logical, intent(in) :: init_loop,calib_loop
      integer, intent(in) :: cd, cm, cy, isr, wepp_hydro
      real, intent(inout) :: slen
      real, intent(in) :: bhfice(*)
      type(wepp_param), intent(inout) :: wp
      end subroutine waterbal
!-----------------------
      subroutine arraymerge( nr, dt, trf, rf, irrig, durirr,            &
     &                       nf, tr, r, rr)
      integer, intent(in) :: nr
      real, intent(in) :: dt, trf(*), rf(*), irrig, durirr
      integer, intent(inout) :: nf
      real, intent(inout) :: tr(*), r(*), rr(*)
      end subroutine arraymerge
!-----------------------
      SUBROUTINE CONST(NR, DELTFQ, TIMEDL, INTDL)
      INTEGER NR
      REAL DELTFQ, TIMEDL(*), INTDL(*) 
      end subroutine CONST
!-----------------------
      SUBROUTINE DBLEX(NR, DELTFQ, TIMEDL, INTDL, TPD, IP)
      INTEGER NR
      REAL DELTFQ, TIMEDL(*), INTDL(*), IP, TPD
      end subroutine DBLEX
!-----------------------
      SUBROUTINE  disag(NR, TRF, RF, P, DURD, TPD, IP)
      INTEGER NR
      REAL TRF(*), RF(*), P, DURD, TPD, IP
      end SUBROUTINE  disag
!-----------------------
      FUNCTION EQROOT(A,ERR)
      REAL EQROOT, A
      INTEGER ERR  
      end function EQROOT
!-----------------------
      SUBROUTINE grna( NF, DEPSTO, TR, R, RR, KS, SM,                   &
     &     NS, TF, RCUM, F, FF, RE, RECUM, TP,                          &
     &     RPRINT, DDEPSTO, RUNOFF, DUREXR, EFFINT, EFFDRR, IT )
      INTEGER MXTIME, MXPOND
      PARAMETER (MXTIME = 1500, MXPOND = 1000)
      INTEGER, intent(in) :: NF
      REAL, intent(in) :: DEPSTO, TR(MXTIME), R(MXTIME), RR(MXTIME),    &
     &     KS, SM
      INTEGER, intent(inout) :: NS
      REAL, intent(inout) :: TF(MXTIME), RCUM(MXTIME),                  &
     &     F(MXTIME), FF(MXTIME), RE(MXTIME), RECUM(MXTIME), TP(MXPOND),&
     &     RPRINT(MXTIME), DDEPSTO(MXTIME),                             &
     &     RUNOFF, DUREXR, EFFINT, EFFDRR
      INTEGER, intent(out) :: IT
      end SUBROUTINE grna
!-----------------------
      subroutine infparsub( nsl, ssc, sscv, dg, cec1, st, ul, frzw,     &
     &                      avclay, avsand, avbdin, avporin, avrocvol,  &
     &                      avsatin, rescov, cancov, canhgt,            &
     &                      rrc, dsnow, prcp, rkecum, bcdayap,          &
     &                      ks, sm, frdp )
      integer, intent(in) :: nsl
      real, intent(in) :: ssc(*), sscv(*), dg(*), cec1(*), st(*), ul(*)
      real, intent(in) :: avclay, avsand, avbdin, avporin, avrocvol
      real, intent(in) :: avsatin, rescov, cancov, canhgt
      real, intent(in) :: rrc, dsnow, prcp, rkecum
      integer, intent(in) :: bcdayap
      real, intent(inout) :: ks, sm
      real, intent(in) :: frzw(*),frdp
      end subroutine infparsub
!-----------------------
      SUBROUTINE NEWTON(TIME, FFPAST, FFNOW, KS, SM)
      REAL TIME, FFPAST, FFNOW, KS, SM
      end SUBROUTINE NEWTON
!-----------------------
      SUBROUTINE parestsub(SAND, CLAY, SAT, CC, SC, KS, SM)
      REAL, intent(in) :: SAND, CLAY, SAT, CC, SC
      REAL, intent(inout) :: KS, SM
      end SUBROUTINE parestsub
!-----------------------
      real function rainenergy( ninten, timem, intensity)
      integer, intent(in) :: ninten
      real, intent(in) :: timem(*), intensity(*)
      end function rainenergy
!-----------------------
      subroutine perc(vv, k1, nsl, st, ul, hk, ssc, sep)
      integer, intent(in) :: k1, nsl
      real, intent(in) :: vv, st(*), ul(*), hk, ssc
      real, intent(inout) :: sep
      end subroutine perc
!-----------------------
      subroutine purk(nsl, st, fc, ul, hk, ssc, sep)
      integer, intent(in) :: nsl
      real, intent(in)  :: fc(*),ul(*), hk(*), ssc(*)
      real, intent(inout) :: st(*), sep
      end subroutine purk
!-----------------------
      subroutine saxpar(sand,clay,orgmat,nsl,saxwp,saxfc,saxenp,saxpor, &
     &                  saxA,saxB,saxks)
      real, intent(in) :: sand(*),clay(*),orgmat(*)
      integer, intent(in) :: nsl
      real, intent(out) :: saxwp(*),saxfc(*),saxenp(*)
      real, intent(out) :: saxpor(*),saxA(*),saxB(*),saxks(*)
      end subroutine saxpar
!-----------------------
      subroutine usdatx( sand, clay, class)
      integer class
      real sand, clay
      end subroutine usdatx
!-----------------------
      real function effksat(uselan, clay, sand, cec, orgmat, rooty,     &
     &              rilcov, bascov, rescov, rrough, fbasr, fbasi, fresi)
      integer, intent(in) :: uselan
      real, intent(in) ::  clay, sand, cec, orgmat, rooty
      real, intent(in) ::  rilcov, bascov, rescov, rrough
      real, intent(in) ::  fbasr, fbasi, fresi
      end function effksat
!-----------------------
!---------------- WEPP Routines ----------------------------
      real function cross(x1,y1,x2,y2)
      real, intent(in) :: x1, y1, x2, y2              
      end function cross
!-----------------------
      real function depc(xu,a,b,phi,theta,du,ktrato,qostar)
      real, intent(in) ::  xu, a, b, phi, theta, du, ktrato, qostar  
      end function depc
!-----------------------
      real function depend(xu,xl,a,b,cdep,phi,theta,ktrato,qostar)
      real, intent(in) :: xu, xl, a, b, cdep, phi, theta, ktrato, qostar          
      end function depend
!------------------------
      subroutine depeqs(xu,cdep,a,b,phi,theta,x,depeq,ktrato,qostar)
      real, intent(in) :: xu, cdep, a, b, phi, theta,ktrato,qostar
      real, intent(inout) :: x
      real, intent(out) :: depeq      
      end subroutine depeqs
!-------------------------
      subroutine depos(xb,xe,cdep,a,b,c,phi,theta,ilast,dl,ldlast,      &
     &    xinput,ktrato,detach,load,tc,qostar)
      real, intent(in) :: xb, cdep, phi, theta, ktrato, qostar
      real, intent(in) :: a, b, c
      real, intent(inout) :: xe, xinput(101), load(101)
      real, intent(out) :: dl, ldlast, detach(101)
      real, intent(out) :: tc(101)
      integer, intent(inout) :: ilast
      end subroutine depos
!--------------------------
      subroutine enrich(kk,xtop,xbot,xdetst,ldtop,ldbot,lddend,theta,   &
     &    iendfg,slplen,ktrato,qin,qout,qostar,ainftc,binftc,cinftc,    &
     &    npart,frac,fall,frcly,frslt,frsnd,frorg,sand,silt,clay,orgmat,&
     &    fidel,tcf1,frcflw,enrato)
      integer, intent(in) :: kk, iendfg, npart
      real, intent(in) :: xtop, xbot, xdetst, theta, ldtop,ldbot,lddend,&
     &     slplen, ktrato, qin, qout, qostar, ainftc(*),                & 
     &     binftc(*), cinftc(*), frac(*), fall(*),                      &
     &     frcly(*), frslt(*), frsnd(*), frorg(*),                      &
     &     fidel(*), tcf1(*),                                           &
     &     sand(*), silt(*), clay(*), orgmat(*)  
      real, intent(inout) :: frcflw(*)    
      real, intent(out) ::  enrato 
      end subroutine enrich
!--------------------------
      subroutine enrprt(jun,npart,frac,frcflw,dia,spg,frsnd,            &
     &    frslt,frcly,frorg,enrato)
      integer, intent(in) :: jun, npart
      real, intent(in) ::  frac(*), frcflw(*), dia(*),                  & 
     &      spg(*),                                                     &
     &     frsnd(*), frslt(*), frcly(*), frorg(*),                      &
     &     enrato   
      end subroutine enrprt
!---------------------------
      subroutine eprint(slplen,avgslp,runoff,peakro,effdrn,efflen, effint,effdrr)
       real, intent(in) :: slplen, avgslp, runoff, peakro, effdrn, efflen, effint, effdrr 
      end subroutine eprint
!---------------------------
      subroutine erod(xb,xe,a,b,c,atc,btc,ctc,eata,tauc,theta,phi,ilast,&
     &    dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,detach,qostar)
      real, intent(in) :: xb, xe, a, b, c, eata, tauc, theta
      real, intent(inout) :: xdbeg
      real, intent(in) ::  atc, btc, ctc, phi, qostar
      real, intent(in) :: xinput(101), ktrato
      real, intent(inout) :: detach(101)
      integer, intent(inout) ::  ilast
      integer, intent(out) :: ndep
      real, intent(inout) :: ldlast, tc(101), load(101), dl 
      end subroutine erod
!---------------------------
      real function falvel(spg,dia)
      real, intent(in) :: spg, dia
      end function falvel
!---------------------------
      subroutine getFromWeps(isr,canhgt,cancov,sand,silt,clay,orgmat,   &
     & rtm15,thetdr,rrc,dg,st,thdp,frdp,ifrost,thetfc,por,rh,           &
     & frctrl, frcsol,rtm, smrm, precip)
      integer, intent(in):: isr
      real, intent(out):: canhgt,cancov
      real, intent(out):: sand(*), silt(*), clay(*)
      real, intent(out):: orgmat(*)
      real, intent(out):: thetdr(*), rrc, rtm15
      real, intent(out):: dg(*), st(*), thdp, frdp
      integer, intent(inout):: ifrost
      real, intent(out):: thetfc(*), por(*), rh
      real, intent(out):: frctrl, frcsol
      real, intent(out):: rtm(3), smrm(3), precip 
      end subroutine getFromWeps
!---------------------------------
      SUBROUTINE init_wepp(isr, afterWarmup)
      integer, intent(in) :: isr, afterWarmup
      end subroutine init_wepp
!---------------------------------
      subroutine param(qin,qout,qostar,qshear,qsout,a,b,avgslp,         &
     &    width,rspace,ktrato,shrsol,tcend,frcsol,frctrl,rrc,npart,frac,&
     &    dia,spg,fall,runoff,effdrn,effint,effdrr,strldn,tcf1,eata,    &
     &    fidel,tauc,theta,phi,slpend,ainf,binf,cinf,ainftc,binftc,     &
     &    cinftc,sand,slplen,kiadj,kradj,shcrtadj,nslpts,efflen,rwflag)
      real, intent(out) :: ktrato, shrsol, tcend, strldn, eata, tauc
      real, intent(out) :: phi, tcf1(*), theta, fidel(*)
      real, intent(out) :: slpend
      real, intent(out) ::  ainftc(*), binftc(*),cinftc(*)
      real, intent(inout) :: width
      real, intent(in):: a(*), b(*),avgslp,qin,qout,qostar
      real, intent(in):: qsout, qshear, rspace, frcsol, frctrl, rrc           
      real, intent(in):: frac(*), dia(*),spg(*)
      real, intent(in):: fall(*), runoff,effdrn, effint, effdrr
      real, intent(in):: sand(*), slplen, kiadj, kradj, shcrtadj
      real, intent(in) ::efflen
      real, intent(inout):: ainf(*),binf(*),cinf(*)
      integer, intent(in):: npart, nslpts,rwflag  
      end subroutine param
!------------------------------
      subroutine print(slplen,avgslp,runoff,peakro,effdrn,efflen,       &
     &    effint,effdrr)
      real, intent(in) :: slplen, avgslp, runoff, peakro, effdrn,       &
     &     efflen, effint, effdrr 
      end subroutine print
!------------------------------
      SUBROUTINE PRINT_BUG(DT, NS,RECUM, T, S, SI, SLEN,ALPHA, M,       &
     &    DUREXR, A1, A2, TSTAR)                                   
      real, intent(inout) :: T(*), S(*), SI(*)
      integer, intent(in) :: NS
      real, intent(in) :: RECUM(*), ALPHA, M, DUREXR, A1, A2
      real, intent(in) :: TSTAR, DT, SLEN  
      end subroutine print_bug
!-------------------------------
      subroutine profil(a,b,avgslp,nslpts,slplen,xinput,slpinp,xu,xl,   &  
     & y,x,totlen)
      real, intent(out) :: a(*), b(*), avgslp, xu(*)
      real, intent(out) :: xl(*), y(*), x(*), totlen
      real, intent(in) :: slplen, xinput(*), slpinp(*)
      integer, intent(in) :: nslpts 
      end subroutine profil
!-------------------------------
      subroutine prtcmp(npart,spg,dia,frac,frcly,frslt,frsnd,frorg,     &
     & sand1,silt1,clay1,orgmat1)
      real, intent(out) :: spg(10), dia(10), frcly(10),frslt(10),       &
     & frsnd(10),frorg(10), frac(10)
      real, intent(in) :: clay1, sand1, silt1, orgmat1
      integer, intent(in) :: npart 
      end subroutine prtcmp
!--------------------------------
      subroutine root(a,b,c,x1,x2)
      real, intent(in) :: a, b
      double precision, intent(in) :: c
      double precision, intent(out) :: x1, x2
      end subroutine root
!---------------------------------
      subroutine route(qin,qout,qostar,strldn,ktrato,ainf,binf,         &
     &    cinf,ainftc,binftc,cinftc,npart,frac,frcly,frslt,frsnd,frorg, &
     &    fall,frcflw,nslpts,xinput,xu,xl,load,enrato,tcf1,fidel,sand,  &
     &    silt,clay,orgmat,eata,tauc,theta,phi,slplen)
      real, intent(in):: qin, qout, qostar,strldn,ktrato
      real, intent(in):: ainf(*), binf(*), cinf(*)
      real, intent(in) :: ainftc(*), binftc(*), cinftc(*)
      real, intent(in) :: frac(*), frcly(*), frslt(*)
      real, intent(in) :: frsnd(*)
      real, intent(in) :: frorg(*), fall(*)
      real, intent(in) :: fidel(*)
      real, intent(inout) :: xinput(101)
      real, intent(out) :: enrato
      real, intent(in) :: sand(*), silt(*), clay(*)
      real, intent(in) :: orgmat(*)
      real, intent(in) :: eata, tauc, theta, phi, slplen, tcf1(*)
      real, intent(out) ::  load(101), frcflw(*)
      real, intent(inout) :: xu(*), xl(*)
      integer, intent(in) :: npart, nslpts 
      end subroutine route
!----------------------------------
      subroutine runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,x,ldold,    &
     &    ldnew,xx,eatax,taucx,shr,dcap,ktrato)
      real, intent(in) :: atc, btc, ctc, a, b, c, ktrato
      real, intent(in) :: eata, tauc, theta, dx, ldold, x
      real, intent(out) :: dcap, ldnew 
      real, intent(inout) ::  xx, eatax, taucx, shr 
      end subroutine runge
!-----------------------------------
      real function sedia(spg,eqfall)
      real, intent(in) :: spg,eqfall                                                   
      end function sedia
!----------------------------------
      subroutine sedist(dslost,dstot,stdist,delxx,slplen,avgslp, y,ysdist)
      real, intent(in) :: slplen, avgslp, y(101), dslost(100)
      real, intent(out) :: ysdist(1000),dstot(1000),stdist(1000),delxx  
      end subroutine sedist
!----------------------------------
      subroutine sedmax(jnum,amax,amin,ptmax,ptmin,dstot,stdist,ibegin, iend,jflag,lseg)
      integer, intent(in) :: jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) ::  amax(100), amin(100), ptmax(100), ptmin(100)
      real, intent(in) :: dstot(1000), stdist(1000)
      end subroutine sedmax
!----------------------------------
      subroutine sedout(sumfile,plotfile,irdgdx,dslost,avsole,          &
     &    enrato,npart,frac,                                            &
     &    dia,spg,frcly,frslt,frsnd,frorg,frcflw,slplen,fwidth,avgslp,  &
     &    y,totlen,years)
      integer, intent(in) :: npart, plotfile, sumfile, years
      real, intent(in) :: irdgdx, dslost(100), avsole, enrato,          &
     & frac(*),                                                    &
     &     dia(*), spg(*), frcly(*), frslt(*),                          &
     &     frsnd(*), frorg(*), frcflw(*), slplen, fwidth, avgslp  
      real y(101), totlen  
      end subroutine sedout
!---------------------------------
      subroutine sedseg(dslost,jun,iyear,noout,dstot,stdist,irdgdx,     &
     &    ysdist,avgslp,slplen,y,avedet,maxdet,ptdet,avedep,maxdep,     &
     &    ptdep)
      integer, intent(in) :: jun, iyear, noout
      real, intent(in) :: dslost(100)                                 
      real, intent(in) :: irdgdx, avgslp, slplen, y(101)
      real, intent(out) :: dstot(1000), stdist(1000), ysdist(1000)
      real, intent(out) :: avedet,maxdet,ptdet
      real, intent(out) :: avedep,maxdep,ptdep                          
      end subroutine sedseg
!--------------------------------
      subroutine sedsta(jnum,dloss,dsstd,vmax,pmax,vmin,pmin,ibegin,    &
     &    iend,jflag,lseg,dstot,stdist,delxx)
      integer, intent(in) ::  jnum, ibegin, iend, jflag(100), lseg
      real, intent(out) :: pmax(100), pmin(100)
      real, intent(out) :: vmax(100), vmin(100)
      real, intent(in) :: dstot(1000), stdist(1000), delxx
      real, intent(out) :: dloss(100), dsstd(100)  
      end subroutine sedsta
!--------------------------------
      real function shear(a,b,c,x)
      real, intent(in) :: a, b, c, x    
      end function shear
!--------------------------------
      real function shears(q,slp,rspace,width,frcsol,frctrl,rwflag)
      real, intent(inout) :: width
      real, intent(in) :: q, rspace, frcsol, frctrl, slp
      integer, intent(in) :: rwflag      
      end function shears
!---------------------------------
      real function shield(reyn)
      real, intent(in) :: reyn
      end function shield
!--------------------------------
      subroutine sloss(load,tcend,width,rspace,effdrn,theta,            &
     &    slplen,irdgdx,qsout,dslost,dsmon,dsyear,dsavg,avsole,qout,    &
     &    frcflw,npart,enrato)
      real, intent(in) :: load(101), tcend, width, rspace, effdrn
      real, intent(in) :: theta, slplen, frcflw(*)
      real, intent(in) :: qout,enrato
      real, intent(out) :: dslost(100)
      real, intent(out) :: avsole, irdgdx, qsout
      real, intent(inout) :: dsmon(100), dsyear(100), dsavg(100)
      integer, intent(in) :: npart 
      end subroutine sloss
!-----------------------------
      subroutine soil_adj(ki,kr,shcrit,kiadj,kradj,shcrtadj,            &
     & rrc, canhgt,cancov,inrcov,rtm15,rtm,bconsd,daydis,rh,rspace,     &
     & avgslp,smrm,krcrat,tccrat,kicrat,dg,thetdr,st,thdp,frdp,ifrost,  &
     & thetfc,por,tens,cycle)
      real, intent(in):: canhgt,cancov,inrcov,rtm15,rtm(3)
      real, intent(in):: bconsd,rh,rspace,avgslp
      real, intent(in):: smrm(3),krcrat,tccrat,rrc,kicrat
      real, intent(in):: dg(10), thetdr(10), st(10),thdp,frdp
      real, intent(in):: thetfc(10), por(10)
      integer, intent(in):: cycle, daydis
      integer, intent(inout):: ifrost
      real, intent(out):: tens, kiadj, kradj, shcrtadj
      real, intent(in):: ki, kr, shcrit
      end subroutine soil_adj
!--------------------------------
      subroutine trcoeff(trcoef,shrsol,sand,dia,spg,tcf1,npart,frac)
      real, intent(in) :: sand(*), dia(*), spg(*), frac(*),shrsol
      integer, intent(in) ::  npart
      real, intent(out) :: trcoef
      real, intent(inout):: tcf1(*)
      end subroutine trcoeff
!----------------------------------
      subroutine undflo(factor,expon)
      real, intent(inout) :: factor, expon
      end subroutine undflo
!----------------------------------
      SUBROUTINE water_erosion(isr, cd, cm, cy, restot, croptot)
      use biomaterial, only: biototal
      integer, intent(in):: isr,cd,cm,cy
      type(biototal), intent(in) :: restot, croptot
      end subroutine water_erosion
!----------------------------------
      subroutine weppsum(isr, years, wp)
      use wepp_param_mod, only: wepp_param
      integer, intent(in) :: isr, years
      type(wepp_param), intent(inout) :: wp
      end subroutine weppsum
!-----------------------------------
      subroutine xcrit(a,b,c,tauc,xb,xe,xc1,xc2,mshear)
      real, intent(in) :: a, b, c, tauc, xb, xe
      integer, intent(out) :: mshear
      real, intent(out) :: xc1,xc2
      end subroutine xcrit
!-----------------------------------
      subroutine xinflo(xinput,efflen,slplen,a,b,qin,qout,peakro,       &
     &    qostar,ainf,binf,cinf,ainftc,binftc,cinftc,qshear,rspace,     &
     &    nslpts)
      real, intent(out) :: xinput(*), qout, qostar, qshear
      real, intent(in) :: qin, peakro, efflen, slplen, a(*)
      real, intent(in) :: b(*), rspace
      real, intent(out) :: ainf(*), binf(*), cinf(*)
      real, intent(out) :: ainftc(*), binftc(*), cinftc(*)
      integer, intent(in) :: nslpts 
      end subroutine xinflo
!-----------------------------------
      subroutine yalin(effsh,tottc,sand,dia,spg,tcf1,npart,frac)
      real, intent(in) :: effsh
      real, intent(in) :: dia(*), spg(*), sand(*)
      real, intent(in) :: frac(*)      
      integer, intent(in) ::  npart
      real, intent(out) :: tottc, tcf1(*) 
      end subroutine yalin
!------------------------------------   

!-------------- UTIL Routines -----------------------
      integer   function begtrm (val)
      character*(*) val
      end function begtrm
!----------------------------------
      subroutine dbgdmp(day, sr, crop, residue, croptot, biotot)
      use biomaterial, only: biomatter, biototal
      integer, intent(in) :: day
      integer, intent(in) :: sr
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      end subroutine dbgdmp
!------------------------------------
      subroutine distriblay( nlay, bszlyd, bszlyt, layval, insertval, begind, endd )
      integer nlay
      real bszlyd(nlay), bszlyt(nlay), layval(nlay)
      real insertval, begind, endd 
      end subroutine distriblay
!------------------------------------
!------------------------------------      
      real function intersect( begind_a, endd_a, begind_b, endd_b )
      real begind_a, endd_a, begind_b, endd_b 
      end function intersect
!-------------------------------------
      subroutine move_ave_val( nlay_old, bszlyd, valuearr, nlay_new, laydepth_new )
      integer nlay_old, nlay_new
      real bszlyd(*), valuearr(*), laydepth_new(*) 
      end subroutine move_ave_val     
!-------------------------------------
      real function valbydepth(layrsn, bszlyd, lay_val, ai_flag, depthtop, depthbot)
      integer layrsn
      real bszlyd(layrsn), lay_val(layrsn)
      integer ai_flag
      real depthtop, depthbot      
      end function valbydepth
!------------------------------------
      subroutine   mvdate (delta, dd, mm, yyyy, nday, nmonth, nyear)
      integer delta, dd, mm, yyyy, nday, nmonth, nyear      
      end subroutine mvdate
!------------------------------------
      integer   function   wkday (dd, mm, yyyy)
      integer dd, mm, yyyy  
      end function wkday   
!------------------------------------
      integer   function   wkjday (jday)
      integer jday    
      end function wkjday   
!------------------------------------
      real function  dawn(dlat,dlong,idoy,riseangle)
      integer, intent(in) :: idoy
      real, intent(in) :: dlat
      real, intent(in) :: dlong
      real, intent(in) :: riseangle      
      end function dawn
!------------------------------------
      real function   daylen(dlat,idoy,riseangle)
      integer, intent(in) :: idoy
      real, intent(in) :: dlat
      real, intent(in) :: riseangle      
      end function daylen
!------------------------------------      
       end interface
       end module
