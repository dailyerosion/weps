!$Author$
!$Date$
!$Revision$
!$HeadURL$

!
      SUBROUTINE init_wepp(isr, afterWarmup)
      
      use wepp_interface_defs
      use grid_mod, only: amxsim, sim_area
      use Points_Mod, only: slen

      implicit none
      
      integer, intent(in) :: isr, afterWarmup

!----------------------------------------------------------------------------
!
!     This is the entry point for setting up the WEPP routines.
!     This is code that only has to be done once and can safely
!     be done at the start of the simulation.
!
!     This does not read from any files but does rely on converting some
!     WEPS variables into equivalent WEPP variables.
!
!     During the continous simulation the routine getFromWeps is called to keep
!     the WEPP variables in-sync with the WEPS. Ideally both models would use the
!     same names but for now they are kept the different to allow easier comparison
!     and testing with the standalone versions of the models and OMS modules.
!  
!     Any variables that begin with wp_ belong to the WEPP routines, they are static but
!     passed as parameters to routines. See the file wepp_erosion.inc for the 
!     declarations.
!
!     Jim Frankenberger
!     November 6, 2008
!
!----------------------------------------------------------------------------
      include 'p1werm.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 'm1subr.inc'
      include 'hydro/htheta.inc'
      include 'wepp_erosion.inc'


!    WEPS similar variables used for WEPP initialzation:

!    asfsan - Soil layer sand content (Mg/Mg)
!    asfsil - Soil layer silt content (Mg/Mg)
!    asfcla - Soil layer clay content (Mg/Mg)
!    asfom - Soil layer organic matter content (Mg/Mg)
!    amxsim - This variable contains the coordinates of two
!              opposite points for a rectangular simulation region. (m)
!    sim_area - area of simulation region (m^2)
!    asvroc - Soil layer coarse fragments (m^3/m^3)
!    theta - soil layer water content (m^3/m^3)


!      real falvel
      REAL sandf,siltf,clayf,orgmatf
      real sand(mxnsl), silt(mxnsl), clay(mxnsl), orgmat(mxnsl)
      real eqom, vfsand, eqclay,totlen, thetfc(mxnsl)
      real rfg(mxnsl)

!      real SLPINP(MXSLP)
      real kconsd
      integer jdx, i

      wp_npart = 5

!      initialized in hydrinit.
!      wp_totalRunoff = 0
!      wp_totalPrecip = 0
!      wp_totalSnowrunoff = 0
!      wp_precipEvents = 0
!      wp_runoffEvents = 0
!      wp_snowmeltEvents = 0

      if (afterWarmup.gt.0) then      
         return
      endif

      sandf = asfsan(1,isr)
      siltf = asfsil(1,isr)
      clayf = asfcla(1,isr)
      orgmatf = asfom(1,isr)
    !  
!     compute slope length based on the length of the rectangular WEPS simulation area
      wp_efflen = slen(amxsim(1), amxsim(2))/2.0
      wp_slplen = wp_efflen

!
!     compute slope width based on the width of the rectangular WEPS simulation area      
	wp_fwidth = sim_area / wp_efflen
	

	sand(1) = asfsan(1,isr)
	silt(1) = asfsil(1,isr)
	clay(1) = asfcla(1,isr)
	orgmat(1) = asfom(1,isr)
	thetfc(1) = theta(1)
	rfg(1) = asvroc(1,isr)

!     default rill spacing to 1m
      wp_rspace = 1.0
      wp_avsolf = 0.0
      wp_avsolm = 0.0
      wp_avsoly = 0.0
      
      wp_irdgdx = 1.0;

!     default rill width to 0.15m      
      wp_width = 0.15
      
!     use temporary rill width       
      wp_rwflag = 1

      wp_froday = 0
      wp_cycle = 0
      wp_daydis = 0

      do i=1,100
         wp_dsavg(i) = 0.0
         wp_dsmon(i) = 0.0
         wp_dsyear(i) = 0.0
      end do

      call prtcmp(5,wp_spg,wp_dia,wp_frac,wp_frcly,wp_frslt,wp_frsnd,   &
     & wp_frorg, sandf,siltf,clayf,orgmatf)

      do i = 1, wp_npart
	   wp_frcff1(i) = 0.0
		 wp_frcff2(i) = 0.0
         wp_fall(i) = falvel(wp_spg(i),wp_dia(i))
      end do

      wp_enrff1 = 0.0
	wp_enrff2 = 0.0

      CALL PROFIL(wp_a,wp_b,wp_AVGSLP,wp_nslpts,wp_SLPLEN,wp_xinput,    &
     & wp_slpinp, wp_xu, wp_xl, wp_y, wp_x, TOTLEN)
    

!     set up intial ki, kr,tc values based on sand,silt,clay
!     this code is taken from scon.for


!     ---------------------------------------------
!     compute initial ki
!     ---------------------------------------------
!     for soils with less than 30 percent sand content

      if(sand(1).lt.0.30)then
         eqclay=clay(1)
         if(eqclay.lt.0.10)eqclay=0.10
         wp_ki = 6054000.0 - 5513000.0*eqclay
      else

!         for soils with 30 percent sand or greater
!
!           at present, assume very fine sand content is
!           25% of total sand content.  In the future, allow
!           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1)
            if(vfsand.gt.0.40)vfsand=0.40
            wp_ki = 2728000.0 + 19210000.0*vfsand
      end if

!     -------------------------------------------------   
!     compute initial kr
!     -------------------------------------------------
 
!         for soils with less than 30 percent sand content

          if(sand(1).lt.0.30)then
            eqclay=clay(1)
            if(eqclay.lt.0.10)eqclay=0.10
            wp_kr = 0.0069 + 0.134*exp(-20.0*eqclay)
          else

!         for soils with 30 percent sand or greater
!
!           at present, assume very fine sand content is
!           25% of total sand content.  In the future, allow
!           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1)
            if(vfsand.gt.0.40)vfsand=0.40
            
            eqom = orgmat(1)
            if(eqom.lt.0.0035)eqom=0.0035
            wp_kr = 0.00197 + 0.03*vfsand                               &
     &                   + 0.03863*exp(-184.0*eqom)
          end if

!     --------------------------------------------------
!     compute initial shcrit
!     --------------------------------------------------
!
!         for soils with less than 30 percent sand content

          if(sand(1).lt.0.30)then
            wp_shcrit = 3.5
          else

!         for soils with 30 percent sand or greater
!
!           at present, assume very fine sand content is
!           25% of total sand content.  In the future, allow
!           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1)
            if(vfsand.gt.0.40)vfsand=0.40
 
            eqclay=clay(1)
            if(eqclay.gt.0.40)eqclay=0.40
            wp_shcrit = 2.67 + 6.5*eqclay - 5.8*vfsand
          end if

!       -------------------------------------------------
!       Compute the critical shear stress consolidation parameters
!       -------------------------------------------------
!
        kconsd = 8.37 - 11.8 * thetfc(1) - 4.9 * sand(1)
!
        if (kconsd.lt.0.3) kconsd = 0.3
        if (kconsd.gt.7.0) kconsd = 7.0
!
        wp_tccrat = kconsd / wp_shcrit
!
      if (wp_tccrat.lt.1.0) wp_tccrat = 1.0
      if (wp_tccrat.gt.4.0) wp_tccrat = 4.0

!       -------------------------------------------------------
!       Compute variables needed for new consolidation calculations.
!       -------------------------------------------------------

        kconsd = 1000. * (3042.0-3166.0*sand(1)-8816.*                  &
     &      orgmat(1)-2477.*thetfc(1))
        if (kconsd.lt.10000.) kconsd = 10000.
        if (kconsd.gt.2000000.) kconsd = 2000000.
        wp_kicrat = kconsd / wp_ki
        if (wp_kicrat.gt.1.0) wp_kicrat = 1.0
        if (wp_kicrat.lt.0.1) wp_kicrat = 0.1

!       -------------------------------------------------------
!       Compute ratio of freshly tilled to fully consolidated
!                      rill erodibility (nondimensional)
!       -------------------------------------------------------
        kconsd = 0.00035 - 0.0014 * thetfc(1) + 0.00068 *               &
     &      silt(1) + 0.0049 * rfg(1)
!
        if (kconsd.lt.0.00001) kconsd = 0.00001
        if (kconsd.gt.0.004) kconsd = 0.004
!
        wp_krcrat = kconsd / wp_kr
!
!       limit the ratio values to between 0.05 and 1.0
!
        if (wp_krcrat.gt.1.0) wp_krcrat = 1.0
        if (wp_krcrat.lt.0.05) wp_krcrat = 0.05

      wp_bconsd = 0.02

      write(*,*) 'Initializing WEPP parameters from WEPS data'
	write(*,*) 'spg ', (wp_spg(jdx),jdx=1,5)
	write(*,*) 'dia ', (wp_dia(jdx),jdx=1,5)
      write(*,*) 'frac ', (wp_frac(jdx),jdx=1,5)
	write(*,*) 'wp_frcly ', (wp_frcly(jdx),jdx=1,5)
	write(*,*) 'wp_frslt ', (wp_frslt(jdx),jdx=1,5)
	write(*,*) 'wp_frsnd ', (wp_frsnd(jdx),jdx=1,5)
	write(*,*) 'wp_frorg ', (wp_frorg(jdx),jdx=1,5)
	write(*,*) 'sand ' ,sand(1)
	write(*,*) 'silt ', silt(1)
	write(*,*) 'clay ', clay(1)
	write(*,*) 'orgmat ', orgmat(1)
	write(*,*) 'efflen ', wp_efflen
	write(*,*) 'fwidth ', wp_fwidth
	write(*,*) 'slplen ', wp_slplen
	write(*,*) 'ki ', wp_ki
	write(*,*) 'kr ', wp_kr
	write(*,*) 'shcrit ', wp_shcrit

	write(*,*) '---- Done initializing WEPP parameters'

      END
