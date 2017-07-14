!$Author$
!$Date$
!$Revision$
!$HeadURL$

!   Add this definition file in every source file to insure that the compiler can
!   verify subroutine and function signatures.
       
       MODULE weps_interface_defs

       interface

!-------------- DECOMP Routines ------------------------------
!-----------------------
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
      subroutine decomp(isr, soil, crop, residue, decompfac, h1et)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, decomp_factors
      use hydro_data_struct_defs, only: hydro_derived_et
      integer, intent(in) :: isr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(inout) :: crop
      type(biomatter), dimension(:), intent(inout) :: residue
      type(decomp_factors), intent(inout) :: decompfac
      type(hydro_derived_et), intent(in) :: h1et
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
      subroutine callhydr(daysim, isr, soil, crop, restot, biotot, h1et, h1bal, wp)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal, biomatter
      use hydro_data_struct_defs, only: am0hdb, hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      integer daysim
      integer isr                   
      type(soil_def), intent(in) :: soil
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
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
      subroutine  hdbug(isr, soil, crop, restot, h1et)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal, biomatter
      use hydro_data_struct_defs, only: hydro_derived_et
      integer isr                   
      type(soil_def), intent(in) :: soil
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
      subroutine hydrinit(isr, soil, h1et, h1bal, wp)
      use soil_data_struct_defs, only: soil_def
      use hydro_data_struct_defs, only: hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      integer isr
      type(soil_def), intent(in) :: soil
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
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
     &                   bhzdmaxirr, bhratirr, bhdurirr,                &
     &                   bhlocirr, bhminirr, bm0monirr,                 &
     &                   bhmadirr, bhndayirr, bhmintirr,                &
     &                   bhzoutflow, bhzinf,                    &
     &                   bhzsno, bhtsno, bhfsnfrz, &
     &                   bhzsmt, bhfice, bhrsk,                         &
     &                   bhtsmx, bhtsmn, bhrwc0,                        &
     &                   daysim, bsfald, bsfalw, bszlyt,                &
     &                   bwudav, bhzwid, &
     &                   bhzeasurf,                                     &
     &                   bhztranspdepth, restot, h1et, h1bal, wp)
      use biomaterial, only: biototal
      use hydro_data_struct_defs, only: am0hfl, hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
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
      real bhzdmaxirr, bhratirr, bhdurirr
      real bhlocirr, bhminirr
      integer bm0monirr
      real bhmadirr
      integer bhndayirr, bhmintirr
      real bhzoutflow, bhzinf
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd
      real bhzsmt, bhfice(*), bhrsk(*)
      real bhtsmx(*), bhtsmn(*), bhrwc0(*)
      integer daysim
      real bsfald, bsfalw, bszlyt(*)
      real bwudav, bhzwid
      real bhzeasurf
      real bhztranspdepth
      type(biototal), intent(in) :: restot
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
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
     &                     bulkden, settled_bulkden, proctor_bulkden, &
     &                     wet_bulkden, wet_set_rat, partden )

      integer nlay
      real sandf(*), clayf(*), organf(*)
      real bulkden(*)
      real settled_bulkden(*)
      real proctor_bulkden(*)
      real wet_bulkden(*)
      real wet_set_rat(*)
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
      real function resevapredu(                                           &
     &           prev_redu_ratio, biomass, coeff_a, coeff_b)

      real prev_redu_ratio
      real biomass
      real coeff_a
      real coeff_b      
      end function resevapredu
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
    subroutine erodsubr_update( sr, soil, crop, restot, croptot, biotot, h1et, subrsurf )
      use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal, biomatter
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate
    integer sr                               ! subregion index (eventually obsolete)
    type(soil_def), intent(in) :: soil  ! soil for this subregion
    type(biomatter), intent(in) :: crop
    type(biototal), intent(in) :: restot
    type(biototal), intent(in) :: croptot
    type(biototal), intent(in) :: biotot
    type(hydro_derived_et), intent(in) :: h1et
    type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)
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
!------------------------------      
      subroutine mandates(mandate, manFile)
      use mandate_mod, only: opercrop_date, create_mandate    ! Load shared mandate() array
      use manage_data_struct_defs, only: man_file_struct
      type (opercrop_date), dimension(:), allocatable :: mandate
      type(man_file_struct), intent(inout) :: manFile
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
      subroutine plotdata(sr, soil, crop, restot, croptot, biotot, noerod, manFile, cellstate)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: threshold
      use erosion_data_struct_defs, only: cellsurfacestate
      use manage_data_struct_defs, only: man_file_struct
      integer, intent(in) :: sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(threshold), intent(in) :: noerod
      type(man_file_struct), intent(in) :: manFile
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values
      end subroutine plotdata
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
      subroutine submodels (isr, soil, crop, cropprev, residue, restot, croptot, &
     &                      biotot, decompfac, mandate, h1et, h1bal, wp, manFile)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal, decomp_factors, bio_prevday
      use mandate_mod, only: opercrop_date
      use hydro_data_struct_defs, only: hydro_derived_et
      use report_hydrobal_mod, only: hydro_balance
      use wepp_param_mod, only: wepp_param
      use manage_data_struct_defs, only: man_file_struct
      integer isr
      type(soil_def), intent(inout) :: soil     ! soil for this subregion
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(bio_prevday), intent(inout) :: cropprev
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot, croptot, biotot
      type(decomp_factors), intent(inout) :: decompfac
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et
      type(hydro_balance), intent(inout) :: h1bal
      type(wepp_param), intent(inout) :: wp
      type(man_file_struct), intent(inout) :: manFile
      end subroutine submodels
!-------------------------------
      subroutine sumbio(soil, crop, residue, restot, croptot, biotot)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(inout) :: restot, biotot
      end subroutine sumbio
!-------------------------------
      subroutine updres(soil, residue, restot)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(inout) :: restot
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
!--------------------------------
  subroutine compact( u, load, tillf, tlay, nlay, density, settled_bd, proc_bd_wc, proc_bd, laythk )
  integer :: tlay       ! starting soil layer for compaction
  integer :: nlay       ! total number of soil layers in horizon
  real :: u             ! Compaction coefficient
  real :: load          ! Compaction load (Mg, Megagrams) (also known as metric ton)
  real :: tillf         ! fraction of soil area tilled by the machine
  real :: density(*) ! present soil bulk density (Mg/m^3)
  real :: settled_bd(*) ! settled soil bulk density
  real :: proc_bd_wc(*) ! proctor soil bulk density adjusted for water content (Mg/m^3)
  real :: proc_bd(*) ! proctor soil bulk density (maximum dry density) (Mg/m^3)
  real :: laythk(*)  ! layer thickness (mm)
  end subroutine compact
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
    SUBROUTINE init_report_vars(nperiods, nrot_yrs, ncycles, mandate, rep_report, rep_update, rep_dates)
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
    type(reporting_dates), target, intent(inout) :: rep_dates
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
SUBROUTINE print_ui1_output(luogui1, nperiods, nrot_years, ncycles, rep_report, rep_dates, mandate)
    USE pd_dates_vars
    USE pd_report_vars
    use mandate_mod, only: opercrop_date
    integer, intent(in) :: luogui1         ! subregion number for output file selection
    INTEGER, INTENT (IN) :: nperiods
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: ncycles
    type(reporting_report), intent(in) :: rep_report
    type(reporting_dates), intent(in) :: rep_dates
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
SUBROUTINE update_monthly_report_vars(cur_month, cur_year, nrot_years, monthly_update, mrot_update, monthly_report, monthly_dates)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: cur_month
    INTEGER, INTENT (IN) :: cur_year
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:), intent(inout) :: monthly_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:), intent(inout) :: mrot_update
    TYPE (pd_var_type), DIMENSION(Min_monthly_vars:,:,0:), intent(inout) :: monthly_report
    TYPE (pd_dates_type), DIMENSION(:,:), intent(inout) :: monthly_dates
    end SUBROUTINE update_monthly_report_vars
!------------------------
SUBROUTINE update_period_update_vars(sbr, period_update, soil, restot, croptot, biotot, cellstate, h1et, h1bal)
    USE pd_var_tables
    USE pd_var_type_def
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal
    use erosion_data_struct_defs, only: cellsurfacestate
    use hydro_data_struct_defs, only: hydro_derived_et
    use report_hydrobal_mod, only: hydro_balance
    INTEGER :: sbr              ! current subregion
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    type(soil_def), intent(in) :: soil  ! soil for this subregion
    type(biototal), intent(in) :: restot  ! contains:
    type(biototal), intent(in) :: croptot  ! contains:
    type(biototal), intent(in) :: biotot  ! contains:
    type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate  ! egt, egtcs, egtss, egt10
    type(hydro_derived_et), intent(in) :: h1et
    type(hydro_balance), intent(in) :: h1bal
    end subroutine  update_period_update_vars
!-------------------------
SUBROUTINE update_period_report_vars(pd, npd, cur_yr, nrot_years, period_update, period_report, period_dates)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: pd, npd
    INTEGER, INTENT (IN) :: cur_yr
    INTEGER, INTENT (IN) :: nrot_years
    TYPE (pd_var_type), DIMENSION(Min_period_vars:), intent(inout) :: period_update
    TYPE (pd_var_type), DIMENSION(Min_period_vars:,:), intent(inout) :: period_report
    TYPE (pd_dates_type), target, DIMENSION(:), intent(inout) :: period_dates
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
    SUBROUTINE update_yrly_report_vars(cur_year, nrot_years, &
               yrly_update, yrot_update, yr_update, yrly_report, yr_report, yrly_dates, yr_dates)
    USE pd_var_type_def
    USE pd_var_tables
    INTEGER, INTENT (IN) :: nrot_years
    INTEGER, INTENT (IN) :: cur_year
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrly_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yrot_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:), intent(inout) :: yr_update
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,0:), intent(inout) :: yrly_report
    TYPE (pd_var_type), DIMENSION(Min_yrly_vars:,:), intent(inout) :: yr_report
    TYPE (pd_dates_type), DIMENSION(:), intent(inout) :: yrly_dates
    TYPE (pd_dates_type), DIMENSION(:), intent(inout) :: yr_dates
    end SUBROUTINE update_yrly_report_vars
!-------------------------            
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

!-------------- UTIL Routines -----------------------
      integer   function begtrm (val)
      character*(*) val
      end function begtrm
!----------------------------------
      subroutine dbgdmp(day, sr, soil, crop, residue, croptot, biotot, h1et)
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use hydro_data_struct_defs, only: hydro_derived_et
      use erosion_data_struct_defs, only: subregionsurfacestate
      integer, intent(in) :: day
      integer, intent(in) :: sr
      type(soil_def), intent(in) :: soil
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et
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
       end interface
       end module
