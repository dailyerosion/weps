!$Author$
!$Date$
!$Revision$
!$HeadURL$

!----------------------------------------------------------
!
! These are WEPP specific variables that are maintained across calls. 
! The names are from the orgiginal WEPP with the added prefix wp_
! They are generally not treated as global variables but a few 
! routines access them and pass to lower level routines. 
!
!  Jim Frankenberger
!  November 7, 2008
!
!------------------------------------------------------------
     
      include 'wepp_parameters.inc'

!---------------------------------------------------------
!     crop residue related
!     wp_daydis - days since disturbance
!     wp_inrcov - interrill cover (0-1, unitless)
!
      integer wp_daydis
	  real wp_inrcov

!---------------------------------------------------------
!     Soil related
!     wp_krcrat - ratio of freshly tilled to fully consolidated
!                     rill erodibility (nondimensional)
!     wp_tccrat - ratio of freshly tilled to fully consolidated
!                      critical shear stress (nondimensional)
!     wp_kicrat - ratio of freshly tilled to fully consolidated
!                     interrill erodibility (nondimensional)
!     wp_frorg - fraction of organic matter (0-1)
!     wp_frcly - fraction of clay (0-1)
!     wp_frslt - fraction of silt (0-1)
!     wp_frsnd - fraction of sand (0-1)
!     wp_cycle - ?
!     wp_froday - ?
!     wp_ki - initial interrill detachment parameter
!                            (baseline interrill erodibility, kg s m -4)
!     wp_kr - initial rill detachment rate parameter (s/m)
!     wp_shcrit - rill detachment threshold parameter,
!                     or critical shear stress (kg/m/s^2)
!     wp_dia - diameter of each particle class (m)
!     wp_spg - specific gravity of each particle class
!     wp_frac - fraction of each particle class (0-1)
!     wp_fall - fall velocity of each particle class (m/s)
!     wp_rspace - rill spacing (m)
!     wp_bconsd - consolidation decay coefficient (1/day)
!     wp_frcff1 - used to calculated total weighted
!                             particle size fractions exiting
!     wp_frcff2 - used to calculated total weighted
!                            particle size fractions exiting
!     wp_enrff1 - used to calculate total weighted enrichment ratio
!     wp_enrff2 - used to calculate total weighted enrichment ratio
!                    cumulative kinetic energy since last tillage (J/m2)
!
      real wp_krcrat, wp_tccrat, wp_kicrat, wp_frorg(mxpart)
	  real wp_frcly(mxpart), wp_frslt(mxpart), wp_frsnd(mxpart)
	  integer wp_cycle, wp_froday
	  real wp_ki,wp_kr,wp_shcrit
	  real wp_dia(mxpart), wp_spg(mxpart), wp_frac(mxpart)
	  real wp_fall(mxpart), wp_rspace, wp_bconsd
	  real  wp_frcff1(mxpart), wp_frcff2(mxpart)
	  real wp_enrff1,wp_enrff2
       real wp_rkecum, wp_prev_crust_frac

!--------------------------------------------------------------------------
!     Hillslope topography related
!     These are set during initialization and will not change during
!     the simulatation.
!
!     wp_nslpts - number of input slope points
!     wp_efflen - effective flow length of an overland flow element (m)
!               for continuous flow planes - efflen is the sum of
!               the plane lengths having flow - for a Case 4 plane
!               on which runoff ends - efflen is the length of the
!               portion of the Case 4 OFE on which runoff is present
!     wp_fwidth - width of field (m)
!     wp_avgslp - average slope gradient
!     wp_slplen - slope length (m)
!     wp_a - profile coefficient for curvature
!     wp_b - ?
!     wp_xu - dimensionless upper end of section
!     wp_xl - dimensionless lower end of section
!     wp_y - ?
!     wp_xinput - unitless distances (points) down the slope
!     wp_slpinp - ?
!     wp_x - ?
!
      integer wp_nslpts
	  real wp_efflen, wp_fwidth, wp_avgslp, wp_slplen
	  real wp_a(mxslp),wp_b(mxslp), wp_xu(mxslp), wp_xl(mxslp)
	  real wp_y(101),wp_xinput(mxslp),wp_slpinp(mxslp)
      real wp_x(101)

!---------------------------------------------------------------------
!     Hydrology related
!     These are set on a per storm basis. They are set by the hydrology
!     code and then used in the erosion code. These variables are
!     set to zero each time at the start of the wepp hydrology.
!
!     wp_peakro -  peak runoff rate (m^3/s)
!     wp_qout - flow discharge per unit width (m^3/m*s) at the bottom
!     wp_qin - flow discharge per unit width (m^3/m*s) at the top
!     wp_effdrr -  duration of rainfall excess (s)
!     wp_effdrn - effective duration (h)
!     wp_effint - effective rainfall intensity (m/s)
!     wp_qsout - sediment discharge at end (kg/m of width per second)
!     wp_runoff - daily runoff amount (m)
!     wp_precipEvents - total number of precip events during simulation
!     wp_runoffEvents - total number of runoff events during simulation
!     wp_totalPrecip - total precipitation all years
!     wp_totalRunoff - total runoff all years
!     wp_detailout - detailed event out(1) or only average annual(0)
!
      real wp_peakro,wp_qout,wp_qin
	  real wp_effdrr, wp_effdrn, wp_effint, wp_qsout, wp_runoff
	  real wp_totalRunoff,wp_totalPrecip,wp_totalSnowrunoff
	  integer wp_runoffEvents,wp_precipEvents,wp_detailout
	  integer wp_snowmeltEvents
	  
!-------------------------------------------------------------------
!	  Winter Hydrology related
!
!     These are used in adjusting the hydraulic conductivity for additions
!     that are in the WEPP portion. 
!     
!     Saxton K.E. and Rawls W.J., 2006. Soil water characteristics estimates 
!     by texture and organic matter for hydraologic solution.
!     Soil SCI. SOC. AM. J., 70, 1569--1578 These constants are computed once
!     at the beginning of the simulation (in saxpar) and the used in saxfun on a
!     daily basis.
!
!     wp_saxwp -  1500 kpa soil water content (wilting point)
!     wp_saxfc -  33 kpa soil water content (field capacity)
!     wp_saxpor - saturated water content
!     wp_saxenp - air entry pressure (kpa) 
!     wp_saxA, wp_saxB : moisture tension equition coefficients
!     wp_saxks: saturated hydraulic conductivity (m/s) 
!
      real wp_saxwp(mxnsl),wp_saxfc(mxnsl),wp_saxenp(mxnsl)
      real wp_saxpor(mxnsl)
      real wp_saxA(mxnsl), wp_saxB(mxnsl), wp_saxks(mxnsl)   

!----------------------------------------------------------------------
!     Erosion related
!     These are cumulative monthly, yearly, total. These are initialized
!     to zero at the start of the simulation and not updated until 
!     after the WEPS warmup period. The WEPP erosion code is not called
!     until after the WEPS warmup period.
!
!     wp_dsmon - monthly sediment loss at each point (kg/m^2)
!     wp_dsavg - total sediment loss at each point for each  (kg/m^2)
!     wp_dsyear - annual sediment loss at each point (kg/m^2)
!     wp_avsolm - monthly sediment loss (kg/m)
!     wp_avsoly - annual sediment loss (kg/m)
!     wp_avsolf - total sediment loss (kg/m)
!     wp_npart - number of particle classes
!     wp_irdgdx - interrill contribution to dG/dx
!    wp_width - rill width (m) computed using flow at end of OFE
!    wp_rwflag - flag to indicate if user desires WEPP to reset
!                    : rill width after each tillage operation and compute
!                    : new rill widths with each runoff event:
!                      rwflag = 1  have model compute varying rill widths
!                                  due to tillage and flow
!                             = 2  use input rill width always
!
      real wp_dsmon(101), wp_dsavg(101), wp_dsyear(101)
      real wp_avsolm,wp_avsoly,wp_avsolf
      integer wp_npart,wp_rwflag
      real wp_irdgdx,wp_width

!
! Put all the above inside a WEPP common block.
!
      common / wepp /                                                   &
     & wp_daydis, wp_cycle, wp_froday, wp_nslpts,                       &
     & wp_inrcov, wp_krcrat, wp_tccrat, wp_kicrat, wp_frorg,            &
     & wp_frcly, wp_frslt, wp_frsnd,                                    &
     & wp_ki,wp_kr,wp_shcrit,                                           &
     & wp_dia, wp_spg, wp_frac,                                         &
     & wp_fall, wp_rspace, wp_bconsd,                                   &
     & wp_efflen, wp_fwidth, wp_avgslp, wp_slplen,                      &
     & wp_a,wp_b, wp_xu, wp_xl,                                         &
     & wp_y, wp_x, wp_irdgdx, wp_npart,                                 &
     & wp_peakro,wp_qout,wp_qin,                                        &
     & wp_effdrr, wp_effdrn, wp_effint, wp_qsout, wp_runoff,            &
     & wp_dsmon, wp_dsavg, wp_dsyear,                                   &
     & wp_avsolm, wp_avsoly, wp_avsolf, wp_frcff1, wp_frcff2,           &
     & wp_enrff1, wp_enrff2, wp_rkecum, wp_prev_crust_frac, wp_xinput,  &
     & wp_slpinp, wp_width, wp_rwflag,                                  &
     & wp_precipEvents,wp_runoffEvents,wp_totalPrecip,wp_totalRunoff,   &
     & wp_totalSnowrunoff, wp_detailout,wp_snowmeltEvents,              &
     & wp_saxwp,wp_saxfc,wp_saxenp,wp_saxpor,wp_saxA,wp_saxB,wp_saxks







	   
      

   
