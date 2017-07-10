!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! print out many of the biomass pool components (used for debugging purposes)

! These files use the following columnar format.  Some are filled with zeros
! to make it easier to select specific columns for comparisons between the
! crop and individual biomass pools (not all pools have the same variables)


      subroutine bpools( isr, residue, restot, biotot, decompfac )

      use weps_main_mod, only: daysim, am0ifl
      use datetime_mod, only: get_simdate_doy, get_simdate_year
      use biomaterial, only: biomatter, biototal, decomp_factors
      use file_io_mod, only: luocrp1, luobio1
      use decomp_data_struct_defs, only: am0dfl
      use climate_input_mod, only: cli_today

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(decomp_factors), intent(in) :: decompfac

! statements below added by Simon

!     + + + LOCAL VARIABLES + + +
      integer doy, cy, idx
      real total   !, saitotal !added by Simon
      integer :: npools

!     + + + END OF SPECIFICATIONS + + +

      npools = size(residue)

      if( .not. am0ifl ) then
        cy = get_simdate_year()
        doy = get_simdate_doy()
      end if

      if ((am0dfl(isr) .eq. 1).or.(am0dfl(isr).eq.3)) then

        ! day, month, year
        ! flat crop cover, standing crop cover, total crop cover
        ! crop cover fract, crop SAI, crop LAI
        ! total crop biomass, 0.0, standing crop mass
        ! (no "flat crop biomass")
        ! crop root mass, 0.0, crop yield mass
        ! (no "buried crop biomass")
        ! qty crop stems per area, crop height, crop root depth, repr stem dia

        ! Dead Crop Biomass Pool
        ! write file header if still initializing
        if (am0ifl .eqv. .true.) then
          write(luocrp1(isr),*) '#daysim doy yy Tmin Tmax Tavg',        &
     &        ' Tfacabove',                                             &
     &        ' Water Wfacstand Wfacflat Ddaystand Ddayflat Mstand1',   &
     &        ' Mstand2 Mstand3 MstandAll Mflat1 Mflat2 Mflat3',        &
     &        ' MflatAll MaboveAll Mburied1 Mburied2 Mburied3',         &
     &        ' MburiedAll Mroot1 Mroot2 Mroot3 MrootAll Cstand1',      &
     &        ' Cstand2 Cstand3 CstandAll Cflat1 Cflat2 Cflat3',        &
     &        ' CflatAll Cstand+flat1 Cstand+flat2 Cstand+flat3',       &
     &        ' Cstand+flatAll SAI1 SAI2 SAI3 SAIAll LAI1 LAI2 LAI3',   &
     &        ' LAIAll Biodrag #stem1 #stem2 #stem3 #stemAll Hstem1',   &
     &        ' Hstem2 Hstem3 HstemAll Mrt4all'

        else

          total = restot%msttot + restot%mftot   !sum of standing and flat residue mass, all pools

          ! insert double blank lines to demarcate years
          if( doy .eq. 1 ) then
              write (luocrp1(isr),*)
              write (luocrp1(isr),*)
          end if

          write(luocrp1(isr),2222) daysim, doy, cy,                     & !simulation day, day of year, year
     &    cli_today%tdmn, cli_today%tdmx, cli_today%tdav, decompfac%itcs,                       & !tmin, tmax, tavg, tf  
     &    decompfac%aqua, decompfac%iwcs, decompfac%iwcf, decompfac%idds, decompfac%iddf,   & !precip, wf standing, wf flat, dd standing, dd flat
     &    residue(1)%deriv%mst, residue(2)%deriv%mst, residue(3)%deriv%mst, restot%msttot,              & !mass, standing
     &    residue(1)%deriv%mf, residue(2)%deriv%mf, residue(3)%deriv%mf, restot%mftot,                  & !mass, flat
     &    total,                                                        & !sum of standing and flat residue mass, all pools
     &    residue(1)%deriv%mbg, residue(2)%deriv%mbg, residue(3)%deriv%mbg, restot%mbgtot,              & !mass, below ground
     &    residue(1)%deriv%mrt, residue(2)%deriv%mrt, residue(3)%deriv%mrt, restot%mrttot,              & !mass, roots
     &    residue(1)%deriv%fscv, residue(2)%deriv%fscv, residue(3)%deriv%fscv, restot%fscvtot,          & !cover provided by standing residue (fraction)
     &    residue(1)%deriv%ffcv, residue(2)%deriv%ffcv, residue(3)%deriv%ffcv, restot%ffcvtot,          & !cover provided by flat residue (fraction)
     &    residue(1)%deriv%ftcv, residue(2)%deriv%ftcv, residue(3)%deriv%ftcv, restot%ftcvtot,          & !cover provided by standing+flat residue (fraction)
     &    residue(1)%deriv%rsai, residue(2)%deriv%rsai, residue(3)%deriv%rsai, restot%rsaitot,          & !stem area index 
     &    residue(1)%deriv%rlai, residue(2)%deriv%rlai, residue(3)%deriv%rlai, restot%rlaitot,          & !leaf area index
     &    restot%rcdtot,                                                  & !biodrag
     &    residue(1)%geometry%dstm, residue(2)%geometry%dstm, residue(3)%geometry%dstm, restot%dstmtot,          & !stems (no/m2) 
     &    residue(1)%geometry%zht, residue(2)%geometry%zht, residue(3)%geometry%zht, restot%zht_ave,             & !stem height for each residue pool
     &    restot%mrttotto4                                                  !root mass to 4 inches

! tf=temperature factor, wf=water factor, dd=decomposition day
   
2222     format (' ',i6,' ',i3,' ',i4,' ', 3f7.1, f7.3, f7.2, 4f7.3, 17(1x,f8.4), 21f7.4, 4(1x,f7.2), 4(1x,f7.3), f8.4)  !added by Simon

          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass
          ! qty residue stems per area, "ave" residue height, 0.0, 0.0
          ! (no "ave" root depth or stem dia computed across residue pools)

2345     format (i6,i4,i5,3f10.5,13f10.3)

          ! All Residue Pools Combined
          write(luobio1(isr),2345) daysim, doy, cy,                     &
          biotot%ffcvtot, biotot%fscvtot, biotot%ftcvtot, &
          0.0, biotot%rsaitot, biotot%rlaitot, &
          biotot%mtot, biotot%mftot, biotot%msttot, &
          biotot%mrttot, biotot%mbgtot, &
          biotot%dstmtot, biotot%zht_ave, 0.0, 0.0
        endif
      endif

      if ((am0dfl(isr) .eq. 2).or.(am0dfl(isr).eq.3)) then

          ! day, month, year
          ! flat residue cover, standing residue cover, total residue cover
          ! residue cover fract, residue SAI, residue LAI
          ! total residue biomass, flat residue mass, standing residue mass
          ! residue root mass, below gnd residue mass, 0.0
          ! (no residue yield mass)
          ! qty residue stems per area, residue height, 0.0, rep stem dia
          ! (no root depth for residue pools)

        do idx=1,npools

          ! write file header if still initializing
         if (am0ifl .eqv. .true.) then
           write(residue(idx)%luo%dec,*) '#daysim resday resyear doy yy pool#',  &
     &          ' cumddysta cumddyflat cumddybg10 flatcov standcov',    &
     &          ' totalcov covfact silhoutte leafarea totalmass',       &
     &          ' flatmass standmass bgrootmass bgshootmass stemnumb',  &
     &          ' height repstemdia stemstandm leafstandm storstandm',  &
     &          ' stemflatm leafflatm storflatm rstorflatm rfiberflatm',&
     &          ' stembgm leafbgm storbgm rstorgbm rfibergbm name'

         else

2355       format (i6,1x,i5,1x,i4,1x,i3,1x,i4,1x,i2,30(1x,f10.5),1x,a30)

           ! Residue Pool #idx
           write(residue(idx)%luo%dec,2355) daysim, &
     &     residue(idx)%decomp%resday, residue(idx)%decomp%resyear, doy, cy, idx, &
     &     residue(idx)%decomp%cumdds, residue(idx)%decomp%cumddf, residue(idx)%decomp%cumddg(10), &
     &     residue(idx)%deriv%ffcv, residue(idx)%deriv%fscv, residue(idx)%deriv%ftcv, &
     &     residue(idx)%database%covfact, residue(idx)%deriv%rsai, residue(idx)%deriv%rlai, &
     &     residue(idx)%deriv%m, residue(idx)%deriv%mf, residue(idx)%deriv%mst, &
     &     residue(idx)%deriv%mrt, residue(idx)%deriv%mbg, &
     &     residue(idx)%geometry%dstm, residue(idx)%geometry%zht, residue(idx)%geometry%xstmrep, &
     &     residue(idx)%mass%standstem, residue(idx)%mass%standleaf, &
     &     residue(idx)%mass%standstore, residue(idx)%mass%flatstem, &
     &     residue(idx)%mass%flatleaf, residue(idx)%mass%flatstore, &
     &     residue(idx)%mass%flatrootstore, residue(idx)%mass%flatrootfiber, &
     &     residue(idx)%deriv%mbgstem, residue(idx)%deriv%mbgleaf, &
     &     residue(idx)%deriv%mbgstore, residue(idx)%deriv%mbgrootstore, &
     &     residue(idx)%deriv%mbgrootfiber, &
     &     residue(idx)%bname

         endif
        end do
      endif

      end
