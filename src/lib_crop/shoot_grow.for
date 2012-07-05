!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine shoot_grow( bnslay, bszlyd, bcdpop,                    &
     &                 bczmxc, bczmrt, bcfleafstem,                     &
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


!     + + + PURPOSE + + +

!     + + + KEYWORDS + + +
!     shoot growth

!     + + + ARGUMENT DECLARATIONS + + +
      integer bnslay
      real bszlyd(*), bcdpop
      real bczmxc, bczmrt, bcfleafstem
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

!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to botom of layer, m
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     bczmxc - maximum potential plant height (m)
!     bczmrt - maximum root depth
!     bcfleafstem - crop leaf to stem mass ratio for shoots
!     bcfshoot - crop ratio of shoot diameter to length
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bc0diammax - crop maximum plant diameter (m)
!     hui - heat unit index for today
!     huiy - heat unit index for yesterday
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of shoot grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot grow from root storage period
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is destributed into below ground stem and
!                standing stem as each increment of the shoot is added
!     bcmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emegence heat units
!     bcmbgstemz - crop stem mass below soil surface by layer (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bczht  - Crop height (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!     bczrtd - root depth (m)
!     bczgrowpt - depth in the soil of the growing point (m)
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bc0nam - crop name
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcgrf  - fraction of reproductive biomass that is yield
!     daysim   - day of the simulation
!     bcdayap - number of days of growth completed since crop planted

!     + + + COMMON BLOCKS + + +
      include 'file.inc'
      include 'm1flag.inc'
      include 'p1unconv.inc'
      include 'command.inc'

!     + + + LOCAL VARIABLES + + +
      integer day, mo, yr, doy
      integer lay
      real shoot_hui, shoot_huiy
      real fexp_hui, fexp_huiy
      real d_shoot_mass, d_stem_mass, d_leaf_mass, d_root_mass
      real d_s_root_mass, tot_mass_req, red_mass_rat, diff_mass
      real end_root_mass, end_shoot_mass, end_stem_mass
      real end_stem_area, end_shoot_len, yesterday_len
      real stem_propor
      real ag_stem, bg_stem, flat_stem, stand_stem
      real f_root_sum, s_root_sum
      real lost_mass, dlfwt, dstwt, drpwt, drswt

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     day - day of month
!     mo - month of year
!     yr - year
!     doy - day of year
!     lay - index into soil layers for looping
!     shoot_hui - today fraction of heat unit shoot growth index accumulation
!     shoot_huiy - previous day fraction of heat unit shoot growth index accumulation
!     fexp_hui - exponential function evaluated at todays shoot heat unit index
!     fexp_huiy - exponential function evaluated at yesterdays shoot heat unit index
!     d_shoot_mass - mass increment added to shoot for the present day (mg/shoot)
!     d_stem_mass - mass increment added to stem for the present day (mg/shoot)
!     d_leaf_mass - mass increment added to leaf for the present day (mg/shoot)
!     d_root_mass - mass increment added to roots for the present day (mg/shoot)
!     d_s_root_mass - mass increment removed from storage roots for the present day (mg/shoot)
!     tot_mass_req - mass required from root mass for one shoot (mg/shoot)
!     red_mass_rat - ratio of reduced mass available for stem growth to expected mass available
!     diff_mass - mass difference for adjustment
!     end_root_mass - total root mass at end of shoot growth period (mg/shoot)
!     end_shoot_mass - total shoot mass at end of shoot growth period (mg/shoot)
!     end_stem_mass - total stem mass at end of shoot growth period (mg/shoot)
!     end_stem_area - total stem area at end of shoot growth period (m^2/shoot)
!     end_shoot_len - total shoot length at end of shoot growth period (m)
!     yesterday_len - length of shoot yesterday (m)
!     stem_propor - ratio of standing stems mass to flat stem mass
!     ag_stem - above ground stem mass (mg/shoot)
!     bg_stem - below ground stem mass (mg/shoot)
!     flat_stem - flat stem mass (mg/shoot)
!     stand_stem - standing stem mass (mg/shoot)
!     f_root_sum - fibrous root mass sum (total in all layers) (kg/m^2)
!     s_root_sum - storage root mass sum (total in all layers) (kg/m^2)
!     lost_mass - passed into cook yield, is simply set to zero
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage

!     + + + LOCAL PARAMETERS + + +
      real shoot_exp, be_stor, rootf
      parameter( shoot_exp = 2.0 )
      parameter( be_stor = 0.7 )
      parameter( rootf = 0.4 )

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     shoot_exp - exponent for shape of exponential function
!                 small numbers  go toward straight line
!                 large numbers delay development to end of period
!     be_stor - conversion efficiency of biomass from storage to growth
!     rootf - fraction of biomass allocated to roots when growing from seed

!     + + + FUNCTIONS CALLED + + +
      integer dayear
      real frac_lay

!     + + + END OF SPECIFICATIONS + + +

      call caldatw(day, mo, yr)
      doy = dayear (day, mo, yr)

      ! fraction of shoot growth from stored reserves (today and yesterday)
      shoot_hui = min( 1.0, (hui - bcthu_shoot_beg)                     &
     &          / (bcthu_shoot_end - bcthu_shoot_beg) )
      shoot_huiy = max( 0.0, (huiy - bcthu_shoot_beg)                   &
     &           / (bcthu_shoot_end - bcthu_shoot_beg) )

      ! total shoot mass is grown at an exponential rate
      fexp_hui = (exp(shoot_exp*shoot_hui)-1.0) / (exp(shoot_exp)-1)
      fexp_huiy = (exp(shoot_exp*shoot_huiy)-1.0) / (exp(shoot_exp)-1)

      ! sum present storage and fibrous root mass (kg/m^2)
      s_root_sum = 0.0
      f_root_sum = 0.0
      do lay = 1, bnslay
          s_root_sum = s_root_sum + bcmrootstorez(lay)
          f_root_sum = f_root_sum + bcmrootfiberz(lay)
      end do

      ! calculate storage mass required to grow a single shoot
      ! units: kg/m^2 / ( shoots/m^2 * kg/mg ) = mg/shoot
      tot_mass_req = bcmtotshoot / (bcdstm * mgtokg)

      ! divide ending mass between shoot and root
      if( f_root_sum .le. bcmshoot ) then   ! this works as long as rootf <= 0.5
          !roots develop along with shoot from same mass
          end_shoot_mass = tot_mass_req * be_stor * (1.0-rootf)
          end_root_mass = tot_mass_req * be_stor * rootf
      else
          !roots remain static, while shoot uses all mass from storage
          end_shoot_mass = tot_mass_req * be_stor
          end_root_mass = 0.0
      end if

      ! this days incremental shoot mass for a single shoot (mg/shoot)
      d_shoot_mass = end_shoot_mass * (fexp_hui - fexp_huiy)
      d_root_mass = end_root_mass * (fexp_hui - fexp_huiy)

      ! this days mass removed from the storage root (mg/shoot)
      d_s_root_mass = (d_shoot_mass + d_root_mass) / be_stor

      ! check that sufficient storage root mass is available
      ! units: mg/shoot = kg/m^2 / (kg/mg * shoot/m^2)
      diff_mass = d_s_root_mass - s_root_sum  / (bcdstm * mgtokg)
      if( diff_mass .gt. 0.0 ) then
          ! reduce removal to match available storage
          red_mass_rat = d_s_root_mass / (diff_mass + d_s_root_mass)
          ! adjust root increment to match
          d_root_mass = d_root_mass * red_mass_rat
          ! adjust shoot increment to match
          d_shoot_mass = d_shoot_mass * red_mass_rat
          ! adjust removal amount to match exactly
          d_s_root_mass =  d_s_root_mass * red_mass_rat
      end if

      ! if no additional mass, no need to go further
      if( d_shoot_mass .le. 0.0) return
!! +++++++++++++ RETURN FROM HERE IF ZERO +++++++++++++++++

      ! find stem mass when shoot completely developed
      ! (mg tot/shoot) / ((kg leaf/kg stem)+1) = mg stem/shoot
      end_stem_mass = end_shoot_mass / (bcfleafstem+1.0)

      ! length of shoot when completely developed, use the mass of stem per plant
      ! (mg stem/shoot)*(kg/mg)*(#stem/m^2)/(#plants/m^2) = kg stem/plant
      ! inserted into stem area index equation to get stem area in m^2 per plant
      ! and then conversted back to m^2 per stem
      end_stem_area = bc0ssa                                            &
     &              * (end_stem_mass*mgtokg*bcdstm/bcdpop)**bc0ssb      &
     &              * bcdpop / bcdstm
      ! use silhouette area and stem diameter to length ratio to find length
      ! since silhouette area = length * diameter
      ! *** the square root is included since straight ratios do not really
      ! fit, but grossly underestimate the shoot length. This is possibly
      ! due to the difference between mature stem density vs. new growth
      ! with new stems being much higher in water content ***
      ! note: diameter to length ratio is when shoot has fully grown from root reserves
      ! during it's extension, it is assumed to grow at full diameter
      end_shoot_len = sqrt( end_stem_area / bcfshoot )

      ! screen shoot emergence parameters for validity
      if( end_shoot_len .le. bczgrowpt ) then
             write(UNIT=6,FMT="(1x,3(a),f7.4,a,f7.4,a)")                &
     &           'Warning: ',                                           &
     &           bc0nam(1:len_trim(bc0nam)),                            &
     &           ' growth halted. Shoot extension: ', end_shoot_len,    &
     &           ' Depth in soil: ', bczgrowpt, ' meters.'
      end if

      ! today and yesterday shoot length and stem and leaf mass increments
      ! length increase scaled by mass increase
      ! stem and leaf mass allocated proportionally (prevents premature emergence)
      bczshoot = end_shoot_len                                          &
     &         * ((bcmshoot /(mgtokg * bcdstm))+d_shoot_mass)           &
     &         / end_shoot_mass
      yesterday_len = end_shoot_len * (bcmshoot /(mgtokg * bcdstm))     &
     &              / end_shoot_mass
      d_stem_mass = d_shoot_mass  / (bcfleafstem+1.0)
      d_leaf_mass = d_shoot_mass * bcfleafstem / (bcfleafstem+1.0)

      ! divide above ground and below ground mass
      if( bczshoot .le. bczgrowpt ) then
          ! all shoot growth for today below ground
          ag_stem = 0.0
          bg_stem = d_stem_mass
      else if( yesterday_len .ge. bczgrowpt ) then
          ! all shoot growth for today above ground
          ag_stem = d_stem_mass
          bg_stem = 0.0
      else
          ! shoot breaks ground surface today
          ag_stem = d_stem_mass                                         &
     &            * (bczshoot-bczgrowpt) / (bczshoot-yesterday_len)
          bg_stem = d_stem_mass * (bczgrowpt - yesterday_len)           &
     &            / (bczshoot - yesterday_len)
      end if

      !convert to from mg/shoot to kg/m^2
      dlfwt = d_leaf_mass * mgtokg * bcdstm
      dstwt = ag_stem * mgtokg * bcdstm
      drpwt = 0.0
      drswt = 0.0
      lost_mass = 0.0

      ! yield residue relationship adjustment
      ! since this is in shoot_grow, do not allow this with bchyfg=5 since
      ! it is illogical to store yield into the storage root while at the 
      ! same time using the storage root to grow the shoot
      if(     (cook_yield .eq. 1)                                       &
     &  .and. (bcyld_coef .gt. 1.0) .and. (bcresid_int .ge. 0.0)        &
     &  .and. ( (bchyfg.eq.0).or.(bchyfg.eq.1) ) ) then

          call cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,    &
     &                   bcmstandstem, bcmstandleaf, bcmstandstore,     &
     &                   bcmflatstem, bcmflatleaf, bcmflatstore,        &
     &                   bcmrootstorez, lost_mass,                      &
     &                   bcyld_coef, bcresid_int, bcgrf )

      end if

      ! divide above ground stem between standing and flat
      stem_propor = min(1.0, bczmxc/bc0diammax)
      stand_stem = dstwt * stem_propor
      flat_stem = dstwt * (1.0 - stem_propor)

      ! distribute mass into mass pools
      ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
      ! shoot mass pool (breakout pool, not true accumulator)
      bcmshoot = bcmshoot + d_shoot_mass * mgtokg * bcdstm

      ! reproductive mass is added to above ground pools
      bcmstandstore = bcmstandstore + drpwt * stem_propor
      bcmflatstore = bcmflatstore + drpwt * (1.0 - stem_propor)

      ! leaf mass is added even if below ground
      ! leaf has very low mass (small effect) and some light interaction
      ! does occur as emergence apporaches (if problem can be changed easily)
      if( (bcmstandleaf + dlfwt) .gt. 0.0 ) then
          ! added leaf mass adjusts live leaf fraction, otherwise no change
          bcfliveleaf = (bcfliveleaf*bcmstandleaf+dlfwt)                &
     &            / (bcmstandleaf + dlfwt)
      end if
      bcmstandleaf = bcmstandleaf + dlfwt

      ! above ground stems
      bcmstandstem = bcmstandstem + stand_stem
      bcmflatstem = bcmflatstem + flat_stem

      ! below ground stems
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmbgstemz(lay) = bcmbgstemz(lay) + bg_stem               & 
     &        * mgtokg * bcdstm * frac_lay( bczgrowpt-bczshoot,         &
     &        bczgrowpt-yesterday_len, 0.0, bszlyd(lay) * mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmbgstemz(lay) = bcmbgstemz(lay) + bg_stem               &
     &        * mgtokg * bcdstm * frac_lay( bczgrowpt-bczshoot,         &
     &        bczgrowpt-yesterday_len, bszlyd(lay-1) * mmtom,           &
     &        bszlyd(lay) * mmtom )
          end if
      end do

      ! check plant height, the the case of regrowth from stem
      ! do not allow reaching max height in single day
      ! use stem proportion to account for flat stems
      bczht = min( 0.5 * (bczmxc + bczht), max( bczht, max( 0.0,        &
     &            (bczshoot-bczgrowpt)*stem_propor ) ) )

      ! check root depth
      bczrtd = max( bczrtd, (bczgrowpt + bczshoot) )

      ! add to fibrous root mass, remove from storage root mass
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootfiberz(lay) = bcmrootfiberz(lay) + d_root_mass     &
     &        * mgtokg * bcdstm * frac_lay( bczgrowpt, bczrtd,          &
     &        0.0, bszlyd(lay) * mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootfiberz(lay) = bcmrootfiberz(lay) + d_root_mass     &
     &        * mgtokg * bcdstm * frac_lay( bczgrowpt, bczrtd,          &
     &        bszlyd(lay-1) * mmtom, bszlyd(lay) * mmtom )
          end if
          ! check for sufficient storage in layer to meet demand
          if(       (bcmrootstorez(lay) .gt. 0.0)                       &
     &        .and. (d_s_root_mass .gt. 0.0) ) then
              ! demand and storage to meet it
              ! units: mg/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootstorez(lay) = bcmrootstorez(lay) - d_s_root_mass   &
     &                           * mgtokg * bcdstm
              if( bcmrootstorez(lay) .lt. 0.0 ) then
                  ! not enough mass in this layer to meet need. Carry over
                  ! to next layer in d_s_root_mass
                  d_s_root_mass = - bcmrootstorez(lay) / (mgtokg*bcdstm)
                  bcmrootstorez(lay) = 0.0
              else
                  ! no more mass needed
                  d_s_root_mass = 0.0
             end if
          end if
      end do

!     the following write statements are for 'shoot.out'
!     am0cfl is flag to print crop submodel output
      if (am0cfl .ge. 1) then
          write(luoshoot, 1000) daysim, doy, yr, bcdayap, shoot_hui,    &
     &        s_root_sum, f_root_sum, tot_mass_req, end_shoot_mass,     &
     &        end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass,  &
     &        end_stem_mass, end_stem_area, end_shoot_len, bczshoot,    &
     &        bcmshoot, bcdstm, bc0nam
      end if

 1000 format(1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,                           &
     &       2(1x,f10.4), 2(1x,f12.4),                                  &
     &       4(1x,f10.4),                                               &
     &       4(1x,f10.4),                                               &
     &       (1x,f8.4),(1x,f8.3),1x,a20)

      ! check if shoot sucessfully reached above ground
      if( (d_s_root_mass .gt. 0.0) .and. (bczht .le. 0.0) ) then
          write(0,*) "shoot_grow: not enough root storage to grow shoot"
          call exit(1)
      end if

      return
      end


      real function frac_lay( top_loc, bot_loc, top_lay, bot_lay )

      ! this function determines the fraction of a location which
      ! is contained in a layer. It could also be viewed as the
      ! fraction of "overlap" of the linear location with a layer
      ! depth slice. It was written assuming that top values are 
      ! less than bottom values

      real top_loc, bot_loc, top_lay, bot_lay

      if( top_lay .le. top_loc .and. bot_lay .gt. top_loc ) then
          ! top location is in layer
          if( bot_lay .ge. bot_loc ) then
              ! bottom location is also in layer
              frac_lay = 1.0
          else
              ! bottom location is below layer, proportion
              frac_lay = (bot_lay - top_loc)/(bot_loc - top_loc)
          end if
      else if( top_lay .lt. bot_loc .and. bot_lay .ge. bot_loc ) then
          ! bottom location is in layer
          ! if we are here, top location is not in layer so proportion
          frac_lay = (bot_loc - top_lay)/(bot_loc - top_loc)
      else if( top_lay .gt. top_loc .and. bot_lay .lt. bot_loc ) then
          ! location completely spans layer
          frac_lay = (bot_lay - top_lay)/(bot_loc - top_loc)
      else
          ! location is not in the layer at all
          frac_lay = 0.0
      end if

      return
      end
