!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   doproc (sr, bmrotation, crop, residue, biotot, mandate, h1et)

!     + + + PURPOSE + + +
!     Doproc is called when a processline is found in the management file
!     Doproc reads in any coefficients associated with the
!     process. Doproc then makes a call to a subroutine which, in turn,
!     modifies the state variables to mimic the processes of doing the
!     process.

!     + + + KEYWORDS + + +
!     tillage, process, management

      use weps_interface_defs, ignore_me=>doproc
      use file_io_mod, only: luomanage, luotdb
      use biomaterial, only: biomatter, biototal
      use mandate_mod, only: opercrop_date
      use p1unconv_mod, only: mmtom
      use manage_data_struct_defs, only: am0tfl, am0tdb, lastoper
      use crop_data_struct_defs, only: am0cfl
      use soilden_mod, only: setbdproc_wc
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + + PARAMETERS AND COMMON BLOCKS + + +
      include 'command.inc'
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'm1sim.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 's1sgeo.inc'
      include 's1phys.inc'
      include 's1surf.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1info.inc'
      include 'h1hydro.inc'
      include 'h1db1.inc'
      include 'manage/asd.inc'
      include 'manage/man.inc'
      include 'manage/mproc.inc'
!     rdgflag  - flag indicating whether ridge modifications are needed
!     imprs  - implement ridge spacing (can be used to set row spacing)
      include 'manage/tcrop.inc'
!      include 'main/main.inc'
      include 'crop/prevstate.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, bmrotation
      type(biomatter), intent(inout) :: crop    ! structure containing full crop description
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: biotot
      type(opercrop_date), dimension(:), intent(inout) :: mandate
      type(hydro_derived_et), intent(inout) :: h1et

!     + + + ARGUMENT DEFINITIONS + + +
!     sr - the subregion being processed
!     bmrotation - rotation count updated in manage.for

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +

!     acdpop    - crop seeding density
!     acrlai    - crop leaf area index
!     aheaep    - soil air entery potential
!     ahrwc     - soil water content (mass bases)
!     ahrwca    - available soil water content
!     ahrwcf    - 1/3 bar soil water content
!     ahrwcs    - saturation soil water content
!     ahrwcw    - 15 bar soil water content
!     am0defoliatefl  - flag set by defoliate process
!                 0 - no defoliation
!                 1 - defoliation triggered
!     am0kilfl  - flag set by kill process
!                 0 - no kill being done
!                 1 - annual killed,perennial crop NOT killed
!                 2 - annual or perennial crop is killed
!                 3 - defoliation triggered
!     am0tdb    - flag for outputing debug information to a file
!                 0 - no output
!                 1 - output to file ../out/tdbug.out
!     am0tfl    - flag for outputing management operations to a file
!                 0 - no output
!                 1 - output to file ../out/manage.out
!     as0ags    - aggr. size geom. mean std. dev.
!     as0ph     - soil Ph
!     asargo    - ridge orientation (clockwise from true North) (degrees)
!     ascmg     - magnesium ion concentration
!     ascna     - sodium ion concentration
!     asdadg    - aggregrate density
!     asdblk    - soil layer bulk density
!     aseags    - dry aggregrate stability
!     asfcce    - fraction of calcium carbonate
!     asfcec    - cation exchange capcity
!     asfcla    - fraction of clay
!     asfesp    - exchangable sodium percentage
!     asfnoh    - organic N concentration of humus
!     asfom     - fraction of organic matter
!     asfpoh    - organic P concentration of humus
!     asfpsp    - fraction of fertilizer P that is labile
!     asfsan    - fraction of sand
!     asfsil    - fraction of silt
!     asfsmb    - sum of bases
!     aslagm    - aggr. size geom. mean diameter (mm)
!     aslagn    - min. aggr. size of each layer (mm)
!     aslagx    - max aggr. size of each layer (mm)
!     aslrr     - Allmaras random roughness parameter (mm)
!     asxrgs    - ridge spacing (mm)
!     asxrgw    - ridge width (mm)
!     aszlyt    - soil layer thickness (mm)
!     aszrgh    - ridge height (mm)
!     prcode    - the process id number
!     prname    - the process name

!     + + + LOCAL VARIABLES + + +
      integer cutflg
      real    massf (msieve+1,mnsz)
      real    alpha, beta, mu, rho
      integer roughflg
      real    rrimpl
!     real    intens, rrimpl
      real    kappa
      real    thinval
    real :: start_depth ! depth in soil at which tillage loosening/compaction begins (mm)
      real    pyieldf, pstalkf, rstandf
      integer harv_report_flg, harv_calib_flg, harv_unit_flg
      integer mature_warn_flg
      integer sel_position, sel_pool
      real    stemf, leaff, storef, rootstoref, rootfiberf
      real    rdght,rdgwt,dikeht,dikespac
!      real    af,cf,mf  ! used with disabled routines
      real    afvt(mnrbc), mfvt(mnrbc)
      integer burydistflg
      real    irrig
      real    rdght1
      character*1 prdumy
      character*256  line
      integer  idx, thinflg
      real    dmassres, zmassres, dmassrot, zmassrot
      real    mass_rem, mass_left
      integer crop_present, temp_present
      real    noparam1, noparam2, noparam3
      real    rate_mult_vt(mnrbc), thresh_mult_vt(mnrbc)
      real    dummy1(mnsz), dummy2(mnsz)
      ! temporary crop parameter values for process 65 and 66
      integer trbc, thyfg
      real    tdkrate(5), txstm, tddsthrsh, tcovfact
      real    tresevapa, tresevapb
      real    t0sla, t0ck
      ! temporary crop parameter values for process 66 only
      real    manure_buried_fraction, manure_total_mass
      real :: compact_load  ! 

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     alpha    - parameter reflecting the breakage of all soil
!                aggregrates regardless of size
!     beta     - parameter reflecting the uneveness of breakage among
!                aggregrates in different size classes
!     buryf    - fraction of mass to be buried
!     kappa    - fraction of the crust destroyed during a tillage operation
!     dikeht   - dike height (mm)
!     dikespac - dike spacing (mm)
!     fltcoef  - flattening coefficient of an implement
!     pyieldf  - fraction of crop and residue above ground plant reproductive mass removed
!     pstalkf  - fraction of crop stems, leaves and remaining reproductive mass removed
!     rstandf  - fraction of residue stems, leaves and remaining reproductive mass removed
!     harv_report_flg - place in harvest report flag
!                0 - do not place in harvest report
!                1 - place in harvest report
!     harv_calib_flg - Use harvested biomass in calibration flag
!                0 - do not use harvest in calibration
!                1 - use harvest amount in calibration
!     harv_unit_flg - overide units given in crop record
!                0  - use units given in crop record
!                1  - use lb/ac or kg/m^2
!     mature_warn_flg - flag to indicate use of crop maturity warning
!                0  - no crop maturity warning given for any crop
!                1  - Warnings generated for any crop unless supressed by crop type
!     sel_position - position to which percentages will be applied
!                0 - don't apply to anything
!                1 - apply to standing (and attached roots)
!                2 - apply to flat
!                3 - apply to standing (and attached roots) and flat
!                4 - apply to buried
!                5 - apply to standing (and attached roots) and buried
!                6 - apply to flat and buried
!                7 - apply to standing (and attached roots), flat and buried
!                this corresponds to the bit pattern:
!                msb(buried, flat, standing)lsb
                
!     sel_pool - pool to which percentages will be applied
!            0 - don't apply to anything
!            1 - apply to crop pool
!            2 - apply to temporary pool
!            3 - apply to crop and temporary pools
!            4 - apply to residue
!            5 - apply to crop and residue pools
!            6 - apply to temporary and residue pools
!            7 - apply to crop, temporary and residue pools
!                this corresponds to the bit pattern:
!                msb(residue, temporary, crop)lsb

!     storef   - fraction of storage (reproductive components) removed (kg/kg)
!     leaff    - fraction of plant leaves removed (kg/kg)
!     stemf    - fraction of plant stems removed (kg/kg)
!     rootstoref - fraction of plant storage root removed (kg/kg)
!     rootfiberf - fraction of plant fibrous root removed (kg/kg)
!     harvflag - flag indicating a harvest
!     intens   - tillage intensity factor
!     liftf    - fraction of mass to be lifted
!     massf    - mass fractions of aggregrates within sieve cuts
!                 (sum of all the mass fractions are expected to be 1.0)
!     fracarea - fraction of the surface affected by the process
!     rdght    - ridge height (mm)
!     rdght1   - tmp variable - ridge height (mm)
!     rdgwt    - ridge top width (mm)
!     rrimpl   - assigned nominal RR value for the tillage operation (mm)
!     start_depth - depth in soil at which tillage loosening/compaction begins (mm)
!     mu       - loosening coefficient (0 <= mu <= 1)
!     rho      - mixing coefficient (0 <= rho <= 1)
!     irrig    - irrigation quantity for a day (mm)
!     dmassres - Buried crop residue mass(kg/m^2)
!     zmassres - depth in soil of Buried crop residue mass (mm)
!     dmassrot - Buried root residue mass(kg/m^2)
!     zmassrot - depth in soil of Buried root residue mass (mm)
!     mass_rem - mass removed by harvest process (cut,remove)
!     mass_left - mass left behind in pool which mass was removed from by harvest process (cut,remove)
!     crop_present - flag to show crop biomass pool status
!                0 - no crop biomass present
!                1 - crop biomass present
!     temp_present - flag to show temporary crop biomass pool status
!                0 - no temporary crop biomass present
!                1 - temporary crop biomass present
!     noparam1-6   - variaable to allow reading in six non-used crop parameters in single read statement
!     rate_mult_vt - array of multipliers for modifying standing stem fall rate
!     thresh_mult_vt - array of multipliers for modifying standing stem fall threshold
!     dummy1(mnsz), dummy2(mnsz) - place holder variables (set to zero)
!                                  for call to poolmass

!     manure_total_mass - total mass of manure added to field (dry weight)
!     manure_buried_fraction - fraction of total manure applied that is buried

!     + + + SUBROUTINES CALLED + + +
!
!     asd2m     - aggregate size distribution to mass fraction converter
!     burylift  - performs the biomass transfer either into the soil
!                 or from the soil to the surface (deals with decomp
!                 pools only
!     crush     - the crushing process
!     crust     - destroys a crusted surface depending on the operation that
!                 is performed
!     invert    - performs an inversion of the vertical soil layers
!     loosn     - performs the loosen/compact process
!     m2asd     - mass fraction to aggregate size distribution converter
!     mix       - mixes components in specified layers
!     orient    - calculates the oriented roughness
!     remove    - performs the biomass removal during a harvest, burn, etc.
!                 and updates the decomposition pools accordingly.
!     rough     - calculated the post tillage random roughness
!     tdbug     - subroutine which writes out variables for debugging purposes

!     + + + DATA INITIALIZATIONS + + +
      noparam1 = 0.0
      noparam2 = 0.0
      dummy1 = 0.0  ! array, assigns all values
      dummy2 = 0.0  ! array, assigns all values

!     + + + OUTPUT FORMATS + + +
2015     format (' Process code ',i2,1x,'Process ',1x,a20 )

!     + + + END SPECIFICATIONS + + +

      ! set local flag to indicate whether a crop is growing or not
      ! this is used to eliminate spurious harvest reports from residue removal
      if( poolmass( nslay(sr), &
                 crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
                 crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
                 noparam1, noparam2, &
                 crop%mass%stemz, dummy1, dummy2, &
                 crop%mass%rootstorez, crop%mass%rootfiberz ) &
          .gt. 0.0) then
          crop_present = 1
      else
          crop_present = 0
      end if

      if( poolmass( nslay(sr), &
                 atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr), &
                 atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),    &
                 atmflatrootstore(sr), atmflatrootfiber(sr),            &
                 atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr), &
                 atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr) )         &
          .gt. 0.0 ) then
          temp_present = 1
      else
          temp_present = 0
      end if

      line = mtbl(mcur(sr))

      read(line, 1001, err=901) prdumy, prcode, prname
 1001 format(a1,1x,i2,1x,a)

      if (am0tfl(sr) .eq. 1) write (luomanage(sr),2015) prcode,prname

!     process calls follow
      select case (prcode)

      case (1)
!-----START crust breakdown process (process code 01)

!     pre-process stuff
        kappa = 1.0 ! *** NOTE that kappa is NOT being read from file

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before crust breakdown process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        am0til = .true.  !set flag for surface modification
!     do process
        call crust(kappa,fracarea,asfcr(sr),asflos(sr),asmlos(sr))

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After crust breakdown process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END crust breakdown process (process code 01)

      case (2)
!-----START random roughness process (process code 02)

!     pre-process stuff

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before random roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!     read the random roughness for the implement. tillage intensity
!     factor, and the fraction of the surface tilled come in as group parameter
!     get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) roughflg, rrimpl

        am0til = .true.  !set flag for surface modification
!     do process
        ! the biomass in the soil affects this calculation. Since it is 
        ! the integrated soil biomass, not fresh biomass that causes this,
        ! the best estimate is the number from sumbio from the previous day.
        call rough(roughflg,rrimpl,ti,fracarea,aslrr(sr),               &
     &             tlayer, asfcla(1,sr), asfsil(1,sr),                  &
     &             biotot%mbgz, biotot%mrtz,                            &
     &             aszlyd(1,sr))

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After random roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END random roughness process (process code 02)

      case (3)
!-----START oriented roughness ridge only process (process code 03)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before oriented roughness1 process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!     read the oriented roughness (ridge) parameters for the implement
!     get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    rdgflag, rdght, imprs, rdgwt

        rdght1 = aszrgh(sr) !keep initial ridge height value
        am0til = .true.  !set flag for surface modification
!     do process
        call orient1(aszrgh(sr),asxrgw(sr),asxrgs(sr),asargo(sr),       &
     &               rdght,rdgwt,imprs,odir,tdepth,rdgflag)

!     post-process stuff
        !if the ridge height changed or is very small,
        !then assume any dikes got destroyed
        if (rdght1 .ne. aszrgh(sr) .or. (aszrgh(sr) .le. 0.1)) then
          asxdkh(sr) = 0.0
          asxdks(sr) = 0.0
        end if

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After oriented roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END oriented roughness process (process code 03)

      case (4)
!-----START oriented roughness process dike only (process code 04)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before oriented roughness2 process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!     read the oriented roughness (dike) parameters for the implement
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    rdgflag, dikeht, dikespac
! NOTE: we don't need rdgflag anymore - LEW

        am0til = .true.  !set flag for surface modification
!     do process
        call orient2(asxdkh(sr),asxdks(sr),dikeht,dikespac)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After oriented roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END oriented roughness dike only process (process code 04)

      case (5)
!-----START oriented roughness process (process code 05)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before oriented roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!     read the oriented roughness parameters for the implement
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    rdgflag, rdght, imprs, rdgwt, dikeht, dikespac

        am0til = .true.  !set flag for surface modification
!     do process
        call orient(aszrgh(sr),asxrgw(sr),asxrgs(sr),asargo(sr),        &
     &              asxdkh(sr),asxdks(sr),                              &
     &              rdght,rdgwt,imprs,odir,dikeht,dikespac,             &
     &              tdepth,rdgflag)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After oriented roughness process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END oriented roughness process (process code 05)

      case (11)
!-----START crushing process (process code 11)
!    pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before crushing process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!        write (*,*) '//Before crushing process//'
        if( aslagm(5,sr).gt.aslagx(5,sr) ) then
            write (*,*) 'before crush:',aslagm(5,sr),aslagx(5,sr)
        end if
!        write (*,*) 'dia,sd',aslagm(1,sr),as0ags(1,sr)
!
!       Convert ASD from modified log-normal to sieve classes
        call asd2m(aslagn(1,sr), aslagx(1,sr), aslagm(1,sr),            &
     &           as0ags(1,sr), nslay(sr), massf)
!
!
!       read the crushing parameters for the implement
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) alpha, beta

!       check for valid crushing parameters
        if( alpha.lt.beta) then
           write(0,*) 'Process 11:Crushing:Alpha=',alpha,               &
     &                'must be greater than Beta=',beta
           call exit (-1)
        endif

        ! adjust parameters based on soil aggregate stability
        !aseags(1,sr)


!       do process
        call crush(alpha, beta, tlayer, massf)
!
!       post-process stuff
!
!       Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, nslay(sr),                                    &
     &    aslagn(1,sr), aslagx(1,sr), aslagm(1,sr), as0ags(1,sr))

        if( aslagm(5,sr).gt.aslagx(5,sr) ) then
            write (*,*) 'after crush:',aslagm(5,sr),aslagx(5,sr)
        end if
!        write (*,*) 'dia,sd',aslagm(1,sr),as0ags(1,sr)
!
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After crushing process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END crushing process (process code 11)

      case (12)
!-----START loosening process (process code 12)
!       pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before loosening process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        if( aslagm(5,sr).gt.aslagx(5,sr) ) then
            write (*,*) 'before loose:',aslagm(5,sr),aslagx(5,sr)
        end if


!       read the loosening parameter for the implement
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) mu

!        if( sr .eq. 3 ) then
!          write(*,*) mu,fracarea,tlayer,                                &
!     &    asdblk(1,sr),asdsblk(1,sr),aszlyt(1,sr)
!        end if

!       do process
        call loosn(mu,fracarea,tlayer,                                  &
     &    asdblk(1,sr),asdsblk(1,sr),aszlyt(1,sr))

!        if( sr .eq. 3 ) then
!          write(*,*) mu,fracarea,tlayer,                                &
!     &    asdblk(1,sr),asdsblk(1,sr),aszlyt(1,sr)
!          stop
!        end if

!       post-process stuff

        ! recalculate  depth to bottom of soil layer
        call depthini( nslay(sr), aszlyt(1,sr), aszlyd(1,sr) )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        tlayer, aszlyd(1,sr), asdblk(1,sr), asdpart(1,sr),        &
     &        asfcla(1,sr), asfsan(1,sr), asfom(1,sr), asfcec(1,sr),    &
     &        ahrwcs(1,sr), ahrwcf(1,sr), ahrwcw(1,sr),ahrwcr(1,sr),    &
     &        ahrwca(1,sr), ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr),     &
     &        ahfredsat(1,sr) )

        else
          ! adjust soil hydraulic properties for change in density
          call param_blkden_adj( tlayer, asdblk(1,sr), asdblk0(1,sr),   &
     &       asdpart(1,sr), ahrwcf(1,sr), ahrwcw(1,sr), ahrwca(1,sr),   &
     &       asfcla(1,sr), asfom(1,sr),                                 &
     &       ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr) )
        end if

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After loosening process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END loosening process (process code 12)

      case (13)
!-----START mixing process (process code 13)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mixing process//'
          write (luotdb(sr),*) 'Tillage layer depth is', tlayer
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!        write (*,*) '//Before mixing process//'
        if( aslagm(5,sr).gt.aslagx(5,sr) ) then
            write (*,*) 'before mix:',aslagm(5,sr),aslagx(5,sr)
        end if
!        write (*,*) 'dia,sd',aslagm(1,sr),as0ags(1,sr)

!       read the mixing coefficient from the data file
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) rho

!       Convert ASD from modified log-normal to sieve classes
        call asd2m(aslagn(1,sr), aslagx(1,sr), aslagm(1,sr),            &
     &           as0ags(1,sr), nslay(sr), massf)

!       do process
        call mix(rho,fracarea,tlayer,asdblk(1,sr),aszlyt(1,sr),         &
     &    asfsan(1,sr), asfsil(1,sr),asfcla(1,sr), asvroc(1,sr),        &
     &    asfcs(1,sr), asfms(1,sr), asffs(1,sr), asfvfs(1,sr),          &
     &    asdwblk(1,sr),                                                &
     &    asfom(1,sr), as0ph(1,sr), asfcce(1,sr), asfcec(1,sr),         &
     &    asfcle(1,sr),                                                 &
     &    asdagd(1,sr),aseags(1,sr),                                    &
     &    ahrwc(1,sr),                                                  &
     &    ahrwcs(1,sr),ahrwcf(1,sr), ahrwcw(1,sr),                      &
     &    ahrwca(1,sr),                                                 &
     &    ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr),                       &
     &    residue,                                                      &
     &    massf)

!     post-process stuff

!       With the change in composition of the layers, it is necessary
!       to update soil properties that are a function of texture
        call proptext( tlayer, asfcla(1,sr), asfsan(1,sr), asfom(1,sr), &
     &                 asdsblk(1,sr), asdprocblk(1,sr), asdpart(1,sr) )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        tlayer, aszlyd(1,sr), asdblk(1,sr), asdpart(1,sr),        &
     &        asfcla(1,sr), asfsan(1,sr), asfom(1,sr), asfcec(1,sr),    &
     &        ahrwcs(1,sr), ahrwcf(1,sr), ahrwcw(1,sr),ahrwcr(1,sr),    &
     &        ahrwca(1,sr), ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr),     &
     &        ahfredsat(1,sr) )

        else
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( tlayer, asdblk(1,sr), asdpart(1,sr),       &
     &                     ahrwcf(1,sr), ahrwcw(1,sr),                  &
     &                     asfcla(1,sr), asfom(1,sr),                   &
     &                     ah0cb(1,sr), aheaep(1,sr) )
        end if

!       set previous day bulk density for the changed layers since
!       this is a change in composition not in bulk density per se
        call set_prevday_blk( tlayer, asdblk(1,sr), asdblk0(1,sr) )

!       Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, nslay(sr),                                    &
     &    aslagn(1,sr), aslagx(1,sr), aslagm(1,sr), as0ags(1,sr))

!        write (*,*) 'dia,sd',aslagm(1,sr),as0ags(1,sr)

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After mixing process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!-----END mixing process (process code 13)
!
      case (14)
!-----START inversion process (process code 14)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before inversion process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!        write (*,*) '//Before inversion process//'
!        write (*,*) 'dia,sd',aslagm(1,sr),as0ags(1,sr)

!     Convert ASD from modified log-normal to sieve classes
        call asd2m(aslagn(1,sr), aslagx(1,sr), aslagm(1,sr),            &
     &           as0ags(1,sr), nslay(sr), massf)

!     do process
        call invert(tlayer,asdblk(1,sr),aszlyt(1,sr),                   &
     &    asfsan(1,sr), asfsil(1,sr),asfcla(1,sr), asvroc(1,sr),        &
     &    asfcs(1,sr), asfms(1,sr), asffs(1,sr), asfvfs(1,sr),          &
     &    asdwblk(1,sr),                                                &
     &    asfom(1,sr), as0ph(1,sr), asfcce(1,sr), asfcec(1,sr),         &
     &    asfcle(1,sr),                                                 &
     &    asdagd(1,sr),aseags(1,sr),                                    &
     &    ahrwc(1,sr),                                                  &
     &    ahrwcs(1,sr),ahrwcf(1,sr), ahrwcw(1,sr),                      &
     &    ahrwca(1,sr),                                                 &
     &    ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr),                       &
     &    residue,                                                      &
     &    massf)


!     post-process stuff

!     Convert ASD back from sieve classes to modified log-normal
        call m2asd(massf, nslay(sr),                                    &
     &    aslagn(1,sr), aslagx(1,sr), aslagm(1,sr), as0ags(1,sr))

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After inversion process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END inversion process (process code 14)

      case (21)
        !-----START Compaction (process code 21)
        ! pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before compaction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        if( aslagm(5,sr).gt.aslagx(5,sr) ) then
            write (*,*) 'before compaction:',aslagm(5,sr),aslagx(5,sr)
        end if

        ! read the compaction parameter for the implement
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) mu, compact_load

        ! do process
        ! compaction occurs below the tlayer depth
        ! find maximum bulk density (soil water content)
        ! find depth of compaction using water content adjusted proctor density
        call compact( mu, compact_load, fracarea, tlayer+1, nslay(sr), asdblk(1,sr), asdsblk(1,sr), &
                      setbdproc_wc( asfcla(1,sr), asfsan(1,sr), asfom(1,sr), asdpart(1,sr), ahrwc(1,sr) ), &
                      asdprocblk(1,sr), aszlyt(1,sr) )

        ! post-process stuff
        ! recalculate  depth to bottom of soil layer
        call depthini( nslay(sr), aszlyt(1,sr), aszlyd(1,sr) )

        if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        nslay(sr), aszlyd(1,sr), asdblk(1,sr), asdpart(1,sr),     &
     &        asfcla(1,sr), asfsan(1,sr), asfom(1,sr), asfcec(1,sr),    &
     &        ahrwcs(1,sr), ahrwcf(1,sr), ahrwcw(1,sr),ahrwcr(1,sr),    &
     &        ahrwca(1,sr), ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr),     &
     &        ahfredsat(1,sr) )

        else
          ! adjust soil hydraulic properties for change in density
          call param_blkden_adj( nslay(sr), asdblk(1,sr), asdblk0(1,sr), &
     &       asdpart(1,sr), ahrwcf(1,sr), ahrwcw(1,sr), ahrwca(1,sr),   &
     &       asfcla(1,sr), asfom(1,sr),                                 &
     &       ah0cb(1,sr), aheaep(1,sr), ahrsk(1,sr) )
        end if

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After compaction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        !-----END Compaction (process code 21)

      case (24)
!-----START flatten process variable toughness (process code 24)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flatten variable toughness proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) bioflg, afvt(1),       &
     &                      afvt(2), afvt(3), afvt(4), afvt(5)

!     do process
        call flatvt(afvt, fracarea, crop%database%rbc, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore,     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       crop%geometry%dstm, residue, bioflg)

!     post-process stuff
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1

        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After flatten variable toughness proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END flatten process variable toughness (process code 24)
!
      case (25)
!-----START mass bury process variable toughness (process code 25)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before mass bury variable toughness pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) burydistflg,           &
     &              mfvt(1), mfvt(2), mfvt(3), mfvt(4), mfvt(5)

        ! accumulation of STIR values
        call stir_cum(sr, ospeed, tdepth, burydistflg, fracarea)

!     Default all bury processes to "all" biomass for now.
      bioflg = 0

!     adjust all burial coefficients for speed and depth
      call buryadj(mfvt,mnrbc,                                          &
     &             ospeed,ostdspeed,ominspeed,omaxspeed,                &
     &             tdepth,tstddepth,tmindepth,tmaxdepth)

!     do process
        if( tlayer .gt. 0 ) then
          call mburyvt(mfvt,fracarea,crop%database%rbc, burydistflg,    &
     &             tlayer,aszlyt(1,sr),aszlyd(1,sr),                    &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atmflatrootstore(sr), atmflatrootfiber(sr),                &
     &       atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),     &
     &       atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),              &
     &       residue, bioflg)
        end if 

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After mass bury variable toughness pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END mass bury process variable toughness (process code 25)
!
      case (26)
!-----START re-surface process variable toughness (process code 26)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before re-surface vari. toughness proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) mfvt(1), mfvt(2),      &
     &                                mfvt(3), mfvt(4), mfvt(5)

      ! Lift processes only sees the decomp biomass pools. This default gets them all.
      bioflg = 0

!     do process
        if( tlayer .gt. 0 ) then
          call liftvt(mfvt, fracarea, tlayer, residue, resurf_roots, bioflg)
        end if

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After re-surface vari. toughness proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END re-surface process variable toughness (process code 26)

      case (30)
!-----START defoliate process (process code 30)

!     Derived from process 31 (kill) - LEW
!     Note that the "defoliate" process only drops leaves
!     and moves the "crop" parameters to the "temporary"
!     crop pool.  The "transfer" process does the final transfer
!     of the "temporary" crop pool values over to the "decomp"
!     pools where they can now begin to decay.

!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before defoliate process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!       Some operations will not kill certain types of crops,
!       ie., a mowing operation usually will not kill a perennial
!       crop like alfalfa but would kill many annual crops.

!       this flag remains set until a biomass transfer process (40)
!       occurs so any side effects can be triggered

!       This flag may get expanded in the future as new situations
!       arise.

!      set am0defoliatefl
!                 1 - defoliation triggered

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) am0defoliatefl

        if( crop%growth%am0cgf .and. .not. crop%growth%am0cif ) then
          ! crop growth flag on and not on initialization cycle
          if( am0defoliatefl .eq. 1 ) then
             ! defoliate by dropping all crop leaf mass into crop flat pool
             crop%mass%flatleaf = crop%mass%flatleaf + crop%mass%standleaf
             crop%mass%standleaf = 0.0
          end if
          ! crop pool state has been changed, force dependent variable update  
          am0cropupfl = 1
        else
            ! if no crop growing "defoliation" is not necessary and no biomass is
            ! present to transfer. Reset kill flag to zero, no report
            am0defoliatefl = 0
        end if

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After defoliate process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END defoliate process (process code 30)

      case (31)
!-----START killing process (process code 31)

!     Note that the "kill" process only stops the crop growth
!     submodel and moves the "crop" parameters to the "temporary"
!     crop pool.  The "transfer" process does the final transfer
!     of the "temporary" crop pool values over to the "decomp"
!     pools where they can now begin to decay.

!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before kill process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!       Some operations will not kill certain types of crops,
!       ie., a mowing operation usually will not kill a perennial
!       crop like alfalfa but would kill many annual crops.

!       this flag remains set until a biomass transfer process (40)
!       occurs so any side effects can be triggered

!       This flag may get expanded in the future as new situations
!       arise.

!      set am0kilfl
!                 0 - no kill being done
!                 1 - annual killed,perennial crop NOT killed
!                 2 - annual or perennial crop is killed
!                 3 - defoliation triggered

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) am0kilfl

        if( crop%growth%am0cgf .and. .not. crop%growth%am0cif ) then
          ! crop growth flag on and not on initialization cycle
          if ((am0kilfl.eq.2).or.((am0kilfl.eq.1).and.((ac0idc(sr).eq.1)&
     &       .or.(ac0idc(sr).eq.2).or.(ac0idc(sr).eq.4)                 &
     &       .or.(ac0idc(sr).eq.5)))) then
!            Stop the crop growth (ie. stop calling crop submodel) and
!            transfer crop state to temporary crop pool
             call kill_crop( crop%growth%am0cgf, nslay(sr), &
     &           crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &           crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &           crop%mass%rootstorez, crop%mass%rootfiberz, &
     &           crop%mass%stemz, &
     &           crop%geometry%zht, crop%geometry%dstm, crop%geometry%xstmrep, crop%geometry%zrtd, &
     &           crop%geometry%grainf, &
     &           atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr), &
     &           atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),    &
     &           atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),          &
     &           atmbgstemz(1,sr),                                      &
     &           atzht(sr), atdstm(sr), atxstmrep(sr), atzrtd(sr),      &
     &           atgrainf(sr) )
            call report_hydrobal( sr, bmrotation )
            call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
          else if( am0kilfl .eq. 3 ) then
             ! defoliate by dropping all crop leaf mass into crop flat pool
             crop%mass%flatleaf = crop%mass%flatleaf + crop%mass%standleaf
             crop%mass%standleaf = 0.0
          end if
          ! crop pool state has been changed, force dependent variable update  
          am0cropupfl = 1
        else
            ! if no crop growing kill is not necessary and no biomass is
            ! present to transfer. Reset kill flag to zero, no report
            am0kilfl = 0
        end if

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After kill process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END killing process (process code 31)

      case (32)
!-----START cutting to height process (process code 32)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting to height process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        ! set process parameters
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf

!     do process
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atzht(sr), atgrainf(sr), residue,                          &
     &       mass_rem, mass_left)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After cutting to height process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_harvest( sr, bmrotation, mass_rem, mass_left, 0,&
     &           mandate, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
        endif
!-----END cutting to height process (process code 32)

      case (33)
!-----START cutting by fraction process (process code 33)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before cutting by fraction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    lastoper(sr)%cutht, pyieldf, pstalkf, rstandf
!     do process
        cutflg = 2
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atzht(sr), atgrainf(sr), residue,                          &
     &       mass_rem, mass_left)
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After cutting by fraction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_harvest( sr, bmrotation, mass_rem, mass_left, 0,&
     &           mandate, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
        end if
!-----END cutting by fraction process (process code 33)

      case (34)
!-----START modify standing fall rate process variable toughness (process code 34)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before modify standing fall rate proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) sel_pool,              &
     &      rate_mult_vt(1), rate_mult_vt(2), rate_mult_vt(3),          &
     &      rate_mult_vt(4), rate_mult_vt(5)
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &      thresh_mult_vt(1), thresh_mult_vt(2), thresh_mult_vt(3),    &
     &      thresh_mult_vt(4), thresh_mult_vt(5)

!     do process
        call fall_mod_vt( rate_mult_vt, thresh_mult_vt,                 &
     &                    sel_pool, fracarea,                           &
     &                    crop%database%rbc, crop%database%dkrate, crop%database%ddsthrsh, &
     &                    residue )

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After modify standing fall rate proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END modify standing fall rate process variable toughness (process code 34)

      case (37)
!-----START thinning to population process (process code 37)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning to population process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    thinval, pyieldf, pstalkf, rstandf
!     do process
        thinflg = 1
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atdstm(sr), atgrainf(sr), residue,                         &
     &       mass_rem, mass_left)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After thinning to population process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_harvest( sr, bmrotation, mass_rem, mass_left, 0,&
      &          mandate, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
        end if
!-----END thinning to population process (process code 37)

      case (38)
!-----START thinning by fraction process (process code 38)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before thinning by fraction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    thinval, pyieldf, pstalkf, rstandf
!     do process
        thinflg = 0
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atdstm(sr), atgrainf(sr), residue,                         &
     &       mass_rem, mass_left)
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After thinning by fraction process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_harvest( sr, bmrotation, mass_rem, mass_left, 0,&
     &           mandate, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
        end if
!-----END thinning by fraction process (process code 38)

      case (40)
!-----START crop to biomass transfer process (process code 40)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass transfer process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

!     do process

      ! This checks if there is biomass in the temporary pool to be
      ! transferred into the residue pool. This check is here so that
      ! repeated calls to trans do not put all biomass in the 
      ! "slow decay" pool.

      if ( temp_present .gt. 0.0 ) then
          call trans(                                                   &
     &      atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),      &
     &      atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),         &
     &      atmflatrootstore(sr), atmflatrootfiber(sr),                 &
     &      atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),      &
     &      atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),               &
     &      atzht(sr), atdstm(sr),atxstmrep(sr),atgrainf(sr),           &
     &      crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck,   &
     &      crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg,  &
     &      crop%database%resevapa, crop%database%resevapb, &
     &      nslay(sr), residue )
      end if

      ! turn off kill flag, since temporary pool being emptied
      ! kill and transfer by necessity must be paired to properly handle
      ! temporary pool
      am0kilfl = 0

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After biomass transfer process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END crop to biomass transfer process (process code 40)

      case (42)
!-----START flagged cutting to height process (process code 42)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting to height proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        ! set process parameters
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &      harv_report_flg, harv_calib_flg, harv_unit_flg,             &
     &      mature_warn_flg, cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf

!     do process
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atzht(sr), atgrainf(sr), residue,                          &
     &       mass_rem, mass_left)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting to height proc.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( harv_calib_flg .gt. 0 ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
          end if
          if( harv_report_flg .gt. 0 ) then
            call report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
          end if
        endif
!-----END flagged cutting to height process (process code 42)

      case (43)
!-----START flagged cutting by fraction process (process code 43)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged cutting by fraction pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &      harv_report_flg, harv_calib_flg, harv_unit_flg,             &
     &      mature_warn_flg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf
!     do process
        cutflg = 2
        call cut(cutflg, lastoper(sr)%cutht, pyieldf, pstalkf, rstandf,              &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore,     &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore,        &
     &       crop%geometry%zht, crop%geometry%grainf, crop%geometry%hyfg,                       &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atzht(sr), atgrainf(sr), residue,                          &
     &       mass_rem, mass_left)
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After flagged cutting by fraction pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( harv_calib_flg .gt. 0 ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
          end if
          if( harv_report_flg .gt. 0 ) then
            call report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
          end if
        end if
!-----END flagged cutting by fraction process (process code 43)

      case (47)
!-----START flagged thinning to population process (process code 47)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write(luotdb(sr),*)'//Before flagged thinning to population pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &      harv_report_flg, harv_calib_flg, harv_unit_flg,             &
     &      mature_warn_flg, thinval, pyieldf, pstalkf, rstandf
!     do process
        thinflg = 1
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atdstm(sr), atgrainf(sr), residue,                         &
     &       mass_rem, mass_left)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write(luotdb(sr),*) '//After flagged thinning to population pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( harv_calib_flg .gt. 0 ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
          end if
          if( harv_report_flg .gt. 0 ) then
            call report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
          end if
        end if
!-----END flagged thinning to population process (process code 47)

      case (48)
!-----START flagged thinning by fraction process (process code 48)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before flagged thinning by fraction pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) &
     &      harv_report_flg, harv_calib_flg, harv_unit_flg, &
     &      mature_warn_flg, thinval, pyieldf, pstalkf, rstandf
!     do process
        thinflg = 0
        call thin(thinflg, thinval, pyieldf, pstalkf, rstandf, &
     &       crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &       crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &       crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &       atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),     &
     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
     &       atdstm(sr), atgrainf(sr), residue,                         &
     &       mass_rem, mass_left)
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After flagged thinning by fraction pr.//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
!       no harvest report if nothing removed or no crop present
        if( (pyieldf+pstalkf+rstandf.gt.0.0)                            &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          if( harv_calib_flg .gt. 0 ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
          end if
          if( harv_report_flg .gt. 0 ) then
            call report_harvest( sr, bmrotation, mass_rem, mass_left, &
     &                           harv_unit_flg, mandate, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
          end if
        end if
!-----END flagged thinning by fraction process (process code 48)

      case (50)
!-----START residue initialization process (process code 50)
        !New residue is assigned to residue pool 1.
        !Existing residue is set to 0.
        ! pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before residue initialization process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        ! do process
        ! Read surface residue counts and amount
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    residue(1)%geometry%dstm, residue(1)%geometry%zht, residue(1)%mass%standstem, &
     &    residue(1)%mass%flatstem, residue(1)%database%rbc

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read buried residue amounts
        read(line(2:len_trim(line)), *, err=901) dmassres, zmassres, dmassrot, zmassrot
        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, nslay(sr), residue(1)%mass%rootfiberz, aszlyt(1,sr))
        call resinit(dmassres,zmassres,nslay(sr), residue(1)%mass%stemz, aszlyt(1,sr))
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read decomposition parameters for type of residue buried
        read(line(2:len_trim(line)), *, err=901) residue(1)%database%dkrate(1), residue(1)%database%dkrate(2), &
             residue(1)%database%dkrate(3), residue(1)%database%dkrate(4), residue(1)%database%dkrate(5), &
             residue(1)%database%xstm, residue(1)%database%ddsthrsh, residue(1)%database%covfact
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read decomposition parameters for type of residue buried
        read(line(2:len_trim(line)), *, err=901)  residue(1)%database%resevapa, residue(1)%database%resevapa
        ! give residue the proper name
        residue(1)%bname = cropname
        ! post-process stuff
        ! set calendar days for residue to zero
        residue(1)%decomp%resday = 0
        residue(1)%decomp%resyear = residue(1)%decomp%resyear + 1
        ! set cumulative decomposition days for residue to zero
        residue(1)%decomp%cumdds = 0.0
        residue(1)%decomp%cumddf = 0.0
        do idx=1,nslay(sr)
          residue(1)%decomp%cumddg(idx) = 0.0
        end do

        ! zero out uninitialized mass pools
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        do idx = 2, mnbpls
            residue(idx)%mass%standstem = 0.0
            residue(idx)%mass%flatstem = 0.0
            call resinit(dmassrot, zmassrot, nslay(sr), residue(idx)%mass%rootfiberz, aszlyt(1,sr))
            call resinit(dmassres,zmassres,nslay(sr), residue(idx)%mass%stemz, aszlyt(1,sr))
        end do

        do idx = 1, mnbpls
            residue(idx)%mass%standleaf = 0.0
            residue(idx)%mass%standstore = 0.0
            residue(idx)%mass%flatleaf = 0.0
            residue(idx)%mass%flatstore = 0.0
            residue(idx)%mass%flatrootstore = 0.0
            residue(idx)%mass%flatrootfiber = 0.0
            call resinit(dmassres, zmassres, nslay(sr), residue(idx)%mass%leafz, aszlyt(1,sr))
            call resinit(dmassres, zmassres, nslay(sr), residue(idx)%mass%storez, aszlyt(1,sr))
            call resinit(dmassrot, zmassrot, nslay(sr), residue(idx)%mass%rootstorez, aszlyt(1,sr))
            ! set other state variables
            residue(idx)%geometry%xstmrep = residue(idx)%database%xstm
            residue(idx)%geometry%grainf = 1.0
            residue(idx)%geometry%hyfg = 0
        end do
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After residue initialization process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END residue initialization process (process code 50)
!
      case (51)
!-----START planting process (process code 51)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before planting process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!     kill and transfer only if existing crop and new crop
      if( crop%growth%am0cgf.and.(crop%geometry%dstm.gt.0.0) ) then
!         In a growth model growing only a single crop, any existing crop must
!         be killed and transferred to residue or all the residue will be lost
!         when the new crop is initialized
!        (remove when multiple species capable)
          call kill_crop( crop%growth%am0cgf, nslay(sr), &
     &           crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &           crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &           crop%mass%rootstorez, crop%mass%rootfiberz, &
     &           crop%mass%stemz, &
     &           crop%geometry%zht, crop%geometry%dstm, crop%geometry%xstmrep, crop%geometry%zrtd, &
     &           crop%geometry%grainf, &
     &           atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr), &
     &           atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),    &
     &           atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),          &
     &           atmbgstemz(1,sr),                                      &
     &           atzht(sr), atdstm(sr), atxstmrep(sr), atzrtd(sr),      &
     &           atgrainf(sr) )
          call trans(                                                   &
     &      atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),      &
     &      atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),         &
     &      atmflatrootstore(sr), atmflatrootfiber(sr),                 &
     &      atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),      &
     &      atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),               &
     &      atzht(sr), atdstm(sr),atxstmrep(sr),atgrainf(sr),           &
     &      crop%bname, crop%database%xstm, crop%database%rbc, crop%database%sla, crop%database%ck,   &
     &      crop%database%dkrate, crop%database%covfact, crop%database%ddsthrsh, crop%geometry%hyfg,  &
     &      crop%database%resevapa, crop%database%resevapb, &
     &      nslay(sr), residue )
      endif
      ! crop pool state has been changed, force dependent variable update  
      am0cropupfl = 1

!     read population, spacing and yield flags
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    acrsfg(sr), acxrow(sr), ac0rg(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    acdpop(sr), acdmaxshoot(sr), acbaflg(sr), acytgt(sr),         &
     &    acbaf(sr), acyraf(sr), crop%geometry%hyfg

!     get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
!     read yield reporting name
          acynmu(sr) = line(2:71)   !at present, line ends with < symbol at 72

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
!     read yield reporting values and growth characteristics
        read(line(2:len_trim(line)), *, err=901)                        &
     &    acywct(sr), acycon(sr), ac0idc(sr), acgrf(sr),                &
     &    crop%database%ck, acehu0(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
!     read crop growth parameters
        read(line(2:len_trim(line)), *, err=901)                        &
     &    aczmxc(sr), ac0growdepth(sr), aczmrt(sr), actmin(sr),         &
     &    actopt(sr), acthudf(sr), actdtm(sr), acthum(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901)                        &
     &    ac0fd1(1,sr), ac0fd2(1,sr), ac0fd1(2,sr), ac0fd2(2,sr),       &
     &    actverndel(sr), ac0bceff(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901)                        &
     &    ac0alf(sr), ac0blf(sr), ac0clf(sr), ac0dlf(sr),               &
     &    ac0arp(sr), ac0brp(sr), ac0crp(sr), ac0drp(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901)                        &
     &    ac0aht(sr), ac0bht(sr), ac0ssa(sr), ac0ssb(sr),               &
     &    crop%database%sla, ac0hue(sr), ac0transf(sr), ac0diammax(sr)

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901)                        &
     &    ac0storeinit(sr), ac0shoot(sr), acfleafstem(sr), acfshoot(sr),&
     &    acfleaf2stor(sr), acfstem2stor(sr), acfstor2stor(sr),crop%database%rbc

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901) &
          crop%database%dkrate(1),crop%database%dkrate(2),crop%database%dkrate(3),crop%database%dkrate(4), &
          crop%database%dkrate(5), crop%database%xstm, crop%database%ddsthrsh, crop%database%covfact

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)), *, err=901)                        &
     &    crop%database%resevapa, crop%database%resevapb, acyld_coef(sr), acresid_int(sr),&
     &    aczloc_regrow(sr), noparam3, noparam2, noparam1

        ! reading of process parameters complete

        ! input is residue yield ratio. internal use is total biomass yield ratio
        ! all input values are on a dry weight basis.
!        acyld_coef(sr) = acyld_coef(sr) + 1.0

        ! adjust yield coefficient to generate values on dry weight basis
        ! from total above ground biomass increments
        acyld_coef(sr) = (acyld_coef(sr) + 1.0 - acywct(sr)/100.0)      &
     &                 / (1.0-acywct(sr)/100.0)

        ! check crop type to see if yield coefficient and grain fraction are used
        if( cook_yield .eq. 1 ) then
            if(     (crop%geometry%hyfg .eq. 0)                                 &
     &         .or. (crop%geometry%hyfg .eq. 1)                                 &
     &         .or. (crop%geometry%hyfg .eq. 5) ) then
            ! grain fraction is used
                if(       (acyld_coef(sr) .gt. 1.0 )                    &
     &              .and. (acyld_coef(sr) * acgrf(sr) .lt. 1.0) ) then
                    ! these values will physically require the transfer of
                    ! biomass from stem or leaf pools to meet the incremental
                    ! need for reproductive mass to meet the residue yield ratio.
                    ! If acresid_int is not greateer than zero, this will
                    ! not be possible
                    write(*,*) 'Error: crop named (', trim(cropname),   &
     &         ') has bad grain fraction and residue yield ratio values'
                    write(*,*) 'Error: grf*(ryrat+1-mc)/(1-mc) must be > 1',&
     &                         ', Value is: ',acyld_coef(sr)*acgrf(sr)
                    stop
                end if
            end if
        end if

!       set planting date vars (day, month, rotation year)
        aplant_day(sr) = lastoper(sr)%day
        aplant_month(sr) = lastoper(sr)%mon
        aplant_rotyr(sr) = lastoper(sr)%yr

        ! initialize transpiration depth parameters
        ahzfurcut(sr) = 0.0
        ahztransprtmin(sr) = 0.0
        ahztransprtmax(sr) = 0.0
!       set row spacing based on flag
        select case( acrsfg(sr) )
        case(0) ! Broadcast Planting
            acxrow(sr) = 0.0
        case(1) ! Use Implement Ridge Spacing
            if(imprs.gt.0.001) then
              acxrow(sr) = imprs * mmtom
              ! check for implement seed placement and ridging
              if( (ac0rg(sr) .eq. 0) .and. (rdgflag .eq. 1) ) then
                ! seed placed in furrow bottom and ridge made unconditionally
                ! set transpiration depth parameters (meters)
                ahzfurcut(sr) = mmtom                                   &
     &                     * furrowcut(aszrgh(sr),asxrgw(sr),asxrgs(sr))
                ahztransprtmin(sr) = ahzfurcut(sr) + ac0growdepth(sr)
                ahztransprtmax(sr) = aczmrt(sr)
              end if
           else  ! no ridges, so this is a broadcast crop
              acxrow(sr) = 0.0
           endif
        case(2) ! Use Specified Row Spacing
!           convert incoming mm to meters used in acxrow
            acxrow(sr) = acxrow(sr)*mmtom
        case default
            write(*,*) 'Invalid row spacing flag value'
        end select

!       do process
!       do not initialize crop if no crop is present
        if( (acdpop(sr) .gt. 0.0) .and. (ac0idc(sr) .gt. 0) ) then
!         set flag for crop initialization - jt
          crop%growth%am0cif = .true.
!         set crop growth flag on - jt
          crop%growth%am0cgf = .true.
!         give crop the proper name
          crop%bname = cropname
          call stir_crop(sr, cropname, 1)
        endif
!       post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After planting process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        call set_calib(sr, crop)
        if( am0kilfl.eq.0 ) then
          ! not reported by the kill process in this
          call report_hydrobal( sr, bmrotation )
        end if
!-----END planting process (process code 51)

      case (61)
!-----START biomass remove process (process code 61)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &           sel_position, sel_pool,                                &
     &           storef, leaff, stemf, rootstoref, rootfiberf

        ! Set bioflg to look at all pools
        bioflg = 0

        ! do process
        call remove( sel_position, sel_pool, bioflg, &
     &    stemf, leaff, storef, rootstoref, rootfiberf, &
     &    crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &    crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &    crop%mass%rootstorez, crop%mass%rootfiberz, &
     &    crop%mass%stemz, &
     &    crop%geometry%zht, crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &    atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),        &
     &    atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),           &
     &    atmflatrootstore(sr), atmflatrootfiber(sr),                   &
     &    atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),        &
     &    atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),                 &
     &    atzht(sr), atdstm(sr), atgrainf(sr), residue,                 &
     &    nslay(sr), mass_rem, mass_left)

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
        mature_warn_flg = 1
!       no harvest report if nothing removed or no crop present
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_harvest( sr, bmrotation, mass_rem, mass_left, 0,&
     &           mandate, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
        end if
!-----END biomass remove process (process code 61)

      case (62)
!-----START biomass remove pool process (process code 62)
        ! pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before biomass remove pool process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &      harv_report_flg, harv_calib_flg, harv_unit_flg,             &
     &      mature_warn_flg, sel_position, sel_pool, bioflg,            &
     &      storef, leaff, stemf, rootstoref, rootfiberf

        ! do process
        call remove( sel_position, sel_pool, bioflg, &
     &    stemf, leaff, storef, rootstoref, rootfiberf, &
     &    crop%mass%standstem, crop%mass%standleaf, crop%mass%standstore, &
     &    crop%mass%flatstem, crop%mass%flatleaf, crop%mass%flatstore, &
     &    crop%mass%rootstorez, crop%mass%rootfiberz, &
     &    crop%mass%stemz, &
     &    crop%geometry%zht, crop%geometry%dstm, crop%geometry%grainf, crop%geometry%hyfg, &
     &    atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),        &
     &    atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),           &
     &    atmflatrootstore(sr), atmflatrootfiber(sr),                   &
     &    atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),        &
     &    atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),                 &
     &    atzht(sr), atdstm(sr), atgrainf(sr), residue,                 &
     &    nslay(sr), mass_rem, mass_left)

        ! post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After biomass remove pool process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
        ! crop pool state has been changed, force dependent variable update  
        am0cropupfl = 1
!       no harvest report if nothing removed
        if( (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
     &      .and. ((crop_present.gt.0) .or. (temp_present.gt.0)) ) then
          ! removed mass is used in calibration
          if( harv_calib_flg .gt. 0 ) then
            call get_calib_crops(sr, crop)
            call get_calib_yield(sr, bmrotation, mass_rem, mass_left, crop)
            call report_calib_harvest( sr, bmrotation, mass_rem, mass_left, crop )
          end if
          ! removed mass appears in crop report
          if( harv_report_flg .gt. 0 ) then
            call report_harvest( sr, bmrotation, mass_rem, mass_left,   &
     &                           harv_unit_flg, mandate, crop )
            if( am0kilfl.eq.0 ) then
              ! not reported by the kill process in this
              call report_hydrobal( sr, bmrotation )
              call crop_endseason( sr, bmrotation, crop%bname, am0cfl(sr), &
     &        nslay(sr), ac0idc(sr), crop%growth%dayam, &
     &        acthum(sr), crop%geometry%xstmrep, &
     &        prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr), &
     &        prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),    &
     &        prevbgstemz(1,sr),                                        &
     &        prevrootstorez(1,sr), prevrootfiberz(1,sr),               &
     &        prevht(sr), prevstm(sr), prevrtd(sr),                     &
     &        prevdayap(sr), prevhucum(sr), prevrthucum(sr),            &
     &        prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),      &
     &        prevdayspring(sr), mature_warn_flg )
            end if
          end if
        end if
!-----END biomass remove pool process (process code 62)

      case (65)
!-----START add residue process (process code 65)
        !New residue is assigned to residue pool 1.
        !Existing residue is transfered to other pools.
        !ADD RESIDUE was modeled after residue initialization (process 50)

        ! this is modified to avoid polluting the parameters of an
        ! existing crop, which could happen if residue is added while a
        ! crop is growing.

!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add residue process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    atdstm(sr), atzht(sr), atmstandstem(sr),                      &
     &    atmflatstem(sr), trbc
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read buried residue amounts
        read(line(2:len_trim(line)), *, err=901)                        &
     &    dmassres, zmassres, dmassrot, zmassrot
        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, nslay(sr),                     &
     &               atmbgrootfiberz(1,sr), aszlyt(1,sr))
        call resinit(dmassres,zmassres,nslay(sr),                       &
     &               atmbgstemz(1,sr), aszlyt(1,sr))
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read decomposition parameters
        read(line(2:len_trim(line)), *, err=901)                        &
     &    tdkrate(1), tdkrate(2), tdkrate(3), tdkrate(4), tdkrate(5),   &
     &    txstm, tddsthrsh, tcovfact
        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read parameters for residue suppression of evaporation
        read(line(2:len_trim(line)), *, err=901)                        &
     &    tresevapa, tresevapb

        !Set to 0
        !above ground
        atmstandleaf(sr) = 0.0
        atmstandstore(sr) = 0.0
        atmflatleaf(sr) = 0.0
        atmflatstore(sr) = 0.0
        atmflatrootstore(sr) = 0.0
        atmflatrootfiber(sr) = 0.0
        !below ground by layer
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        call resinit(dmassres, zmassres, nslay(sr),                     &
     &               atmbgleafz(1,sr), aszlyt(1,sr))
        call resinit(dmassres, zmassres, nslay(sr),                     &
     &               atmbgstorez(1,sr), aszlyt(1,sr))
        call resinit(dmassrot, zmassrot, nslay(sr),                     &
     &               atmbgrootstorez(1,sr), aszlyt(1,sr))

        atgrainf(sr) = 1.0
        atxstmrep(sr) = txstm
        thyfg = 0

        !I don't think it matters what values we put here.
        !We set leaf mass to 0 anyway.
        t0sla = 0.0
        t0ck = 0.0

        ! check for amount of added biomass
        if( poolmass( nslay(sr),                                        &
     &           atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr), &
     &           atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),    &
     &           atmflatrootstore(sr), atmflatrootfiber(sr),            &
     &           atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr), &
     &           atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr) )         &
     &    .gt. 0.0 ) then
          ! biomass was added, so do transfer
          call trans(                                                   &
     &      atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),      &
     &      atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),         &
     &      atmflatrootstore(sr), atmflatrootfiber(sr),                 &
     &      atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),      &
     &      atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),               &
     &      atzht(sr), atdstm(sr),atxstmrep(sr),atgrainf(sr),           &
     &      cropname, txstm, trbc, t0sla, t0ck,                         &
     &      tdkrate(1), tcovfact, tddsthrsh, thyfg,                     &
     &      tresevapa, tresevapb,                                       &
     &      nslay(sr), residue )
        end if

!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After add residue process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END add residue process (process code 65)

      case (66)
!-----START add manure process (process code 66)
        !New residue (manure) is assigned to residue pool 1.
        !Existing residue is transfered to other pools.
        !ADD MANURE was modeled after ADD RESIDUE (process 65)
        ! The only difference between process ADD MANURE and
        ! ADD RESIDUE is that NRCS wanted to be able to specify
        ! the "total" mass of manure applied and the fraction
        ! that is buried of that total.  So, ADD MANURE is a
        ! special case of ADD RESIDUE (just uses two additional
        ! input parameters)

        ! this is modified to avoid polluting the parameters of an
        ! existing crop, which could happen if residue is added while a
        ! crop is growing.

!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before add manure process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    atdstm(sr), atzht(sr), atmstandstem(sr),                      &
     &    atmflatstem(sr), trbc

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read buried residue amounts
        read(line(2:len_trim(line)), *, err=901)                        &
     &    dmassres, zmassres, dmassrot, zmassrot

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read total manure mass amount and buried fraction
        read(line(2:len_trim(line)), *, err=901)                        &
     &    manure_total_mass, manure_buried_fraction
       ! Now we add the "flat and buried" manure to the generic residue
       ! flat and buried quantities
        atmflatstem(sr) = atmflatstem(sr) +                             &      
     &          (1.0 - manure_buried_fraction) * manure_total_mass
        dmassres = dmassres +                                           &
     &          (manure_buried_fraction) * manure_total_mass

        ! place buried residue in pools by layer
        call resinit(dmassrot, zmassrot, nslay(sr),                     &
     &               atmbgrootfiberz(1,sr), aszlyt(1,sr))
        call resinit(dmassres,zmassres,nslay(sr),                       &
     &               atmbgstemz(1,sr), aszlyt(1,sr))

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read decomposition parameters
        read(line(2:len_trim(line)), *, err=901)                        &
     &    tdkrate(1), tdkrate(2), tdkrate(3), tdkrate(4), tdkrate(5),   &
     &    txstm, tddsthrsh, tcovfact

        ! get additional line of data
        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        ! read parameters for residue suppression of evaporation
        read(line(2:len_trim(line)), *, err=901)                        &
     &    tresevapa, tresevapb

        !Set to 0
        !above ground
        atmstandleaf(sr) = 0.0
        atmstandstore(sr) = 0.0
        atmflatleaf(sr) = 0.0
        atmflatstore(sr) = 0.0
        atmflatrootstore(sr) = 0.0
        atmflatrootfiber(sr) = 0.0
        !below ground by layer
        dmassres = 0.0
        zmassres = 0.0
        dmassrot = 0.0
        zmassrot = 0.0
        call resinit(dmassres, zmassres, nslay(sr),                     &
     &               atmbgleafz(1,sr), aszlyt(1,sr))
        call resinit(dmassres, zmassres, nslay(sr),                     &
     &               atmbgstorez(1,sr), aszlyt(1,sr))
        call resinit(dmassrot, zmassrot, nslay(sr),                     &
     &               atmbgrootstorez(1,sr), aszlyt(1,sr))

        atgrainf(sr) = 1.0
        atxstmrep(sr) = txstm
        thyfg = 0

        !I don't think it matters what values we put here.
        !We set leaf mass to 0 anyway.
        t0sla = 0.0
        t0ck = 0.0

        if( poolmass( nslay(sr),                                        &
     &           atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr), &
     &           atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),    &
     &           atmflatrootstore(sr), atmflatrootfiber(sr),            &
     &           atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr), &
     &           atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr) )         &
     &    .gt. 0.0 ) then
          ! biomass was added, so do transfer
          call trans(                                                   &
     &      atmstandstem(sr), atmstandleaf(sr), atmstandstore(sr),      &
     &      atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),         &
     &      atmflatrootstore(sr), atmflatrootfiber(sr),                 &
     &      atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),      &
     &      atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),               &
     &      atzht(sr), atdstm(sr),atxstmrep(sr),atgrainf(sr),           &
     &      cropname, txstm, trbc, t0sla, t0ck,                         &
     &      tdkrate(1), tcovfact, tddsthrsh, thyfg,                     &
     &      tresevapa, tresevapb,                                       &
     &      nslay(sr), residue )
        end if
 
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After add manure process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END add manure process (process code 66)

      case (71)
!-----START irrigate process (process code 71) (OBSOLETE)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901) roughflg, irrig

!     do process
        ! replaced am0irr (1 - sprinkler, 2 furrow) with ahlocirr
        ! using roughflg to read in old value and set some default values
        if( roughflg .eq. 1 ) then
            ahlocirr(sr) = 2000.0
        else
            ahlocirr(sr) = 0.0
        end if
        h1et%zirr = h1et%zirr + irrig
        ! make sure rate and duration are consistent
        ! these values are not set in this process but may have been set
        ! in process 72, if this is used in conjunction with it
        call ratedura(h1et%zirr, ahratirr(sr), ahdurirr(sr))
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After irrigate process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END irrigate process (process code 71)

      case (72)
!-----START irrigation monitoring process (process code 72)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before irrigation monitoring process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    am0monirr(sr), ahzdmaxirr(sr), ahratirr(sr), ahdurirr(sr),    &
     &    ahlocirr(sr), ahminirr(sr), ahmadirr(sr), ahmintirr(sr)
!     do process
        ! set next irrigation day to zero so irrigations will trigger
        ahndayirr(sr) = 0
        ! use inputs to set the irrigation rate, if 
        call ratedura(ahzdmaxirr(sr), ahratirr(sr), ahdurirr(sr))
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After irrigation monitoring process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END irrigation monitoring process (process code 72)

      case (73)
!-----START single event irrigation process (process code 73)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before single event irrigation process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if

        mcur(sr) = mcur(sr) + 1
        line = mtbl(mcur(sr))
        read(line(2:len_trim(line)),* , err=901)                        &
     &    irrig, ahratirr(sr), ahdurirr(sr), ahlocirr(sr)
!     do process
        ! add this irrigation event to any previous event on this same day
        h1et%zirr = h1et%zirr + irrig
        ! use inputs to set the irrigation rate, if 
        call ratedura(h1et%zirr, ahratirr(sr), ahdurirr(sr))
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          !call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After single event irrigation process//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END irrigation monitoring process (process code 73)

      case (74)
!-----START terminate irrigation monitoring terminate process (process code 74)
!     pre-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*)
          write (luotdb(sr),*) '//Before terminate irrigation monitoring//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!     do process
        am0monirr(sr) = 0
!     post-process stuff
        if (am0tdb(sr) .eq. 1) then
          write (luotdb(sr),*) '//After terminate irrigation monitoring//'
          call tdbug(sr, nslay(sr), prcode, crop, residue)
        end if
!-----END terminate irrigation monitoring process (process code 74)

      case default
        goto 902
      end select

      return

! Error stops

  901 write(0,*) 'Error reading parameter ', line
      call exit (1)
  902 write(0,*) 'Invalid process ', prname, ' ', prcode
      call exit (1)
      end
