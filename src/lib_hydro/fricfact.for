!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function fricfact(ref_ranrough, ranrough,                    &
     &                  tot_stems, tot_flat_cov )

!     + + + PURPOSE + + +
!     returns the darcy weisbach friction factor based on random roughness,
!     standing and flat biomass adapted from WEPP (chapter 10)

!     + + + KEY WORDS + + +
!     hydrology, overland flow

!     + + + ARGUMENT DECLARATIONS + + +
      real ref_ranrough, ranrough
      real tot_stems, tot_flat_cov

!     + + +  ARGUMENT DEFINITIONS + + +
!     ref_ranrough   - random roughness of soil surface after last tillage (m)
!     cum_rain       - accumulated rainfall since last rainfall (m)
!     ranrough       - present random roughness (m)
!     tot_stems      - total number of standing stems (#/m^2)
!     tot_flat_cov   - fraction of soil surface covered by flat biomass

!     + + + LOCAL VARIABLES + + +
      real coef_a, coef_b, coef_c, coef_d
      real coef_e, coef_f, coef_g, coef_h
      real f_stem_max, f_stem, f_flat, f_soil
      real tot_stems_max, f_soil_ref
      parameter( coef_a = 14.5 )
      parameter( coef_b = 1.55 )
      parameter( coef_c = 3.02 )
      parameter( coef_d = -5.04 )
      parameter( coef_e = -161.0 )
      parameter( coef_f = 0.5 )
      parameter( coef_g = 1.13 )
      parameter( coef_h = -3.09 )

      parameter( f_stem_max = 12 )
      parameter( tot_stems_max=220 )

!     this relationship is unproven, but does give some variation as desired
      f_stem = f_stem_max * tot_stems / tot_stems_max

      f_flat = coef_a * tot_flat_cov ** coef_b

      f_soil_ref = exp( coef_c + coef_d * exp( coef_e * ref_ranrough ))
      if( ref_ranrough.le.ranrough ) then
           f_soil = coef_f * f_soil_ref ** coef_g
      else
           f_soil = coef_f * f_soil_ref ** coef_g                       &
     &            * exp( coef_h * (1.0-ranrough/ref_ranrough))
      endif

      fricfact = f_stem + f_flat + f_soil

      return
      end
