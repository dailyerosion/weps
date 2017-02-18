!$Author$
!$Date$
!$Revision$
!$HeadURL$

      Program tstasd

      use soil_data_struct_defs, only: soil_def, allocate_soil

      include 'p1werm.inc'
      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1agg.inc'
      include 'manage/asd.inc'

      type(soil_def), dimension(:), allocatable :: soil, soil2             ! structure with soil state and parameters as updated suring simulation

      integer :: alloc_stat, sum_stat
      integer :: nsubr

      integer :: sr,lay,l,i,iter
      real    :: massf(msieve+1,mnsz)
      real    :: initgmd, initgsd

      nsubr = 1
      sum_stat = 0
      allocate(soil(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(soil2(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
!        write(0,*) "ERROR: unable to allocate enough memory for weps main data arrays."
      end if



      do sr=1, nsubr
        soil(sr)%nslay = 29
        soil2(sr)%nslay = 29
        call allocate_soil(soil(sr))  ! allocate layer arrays
        call allocate_soil(soil2(sr)) ! allocate layer arrays
      end do

      write (0,*) 'soil(1)%nslay: ', soil(1)%nslay

      logcas = 3

      call asdini()

!      do sr=1, nsubr
!         do lay=1, soil(sr)%nslay
!            soil(sr)%aslagm(lay) = .84*sr
!            soil(sr)%as0ags(lay) = 1.0+1.0/(0.0124484131375676
!     &                     + 0.00246316704248082*soil(sr)%aslagm(lay)
!     &                     + 0.0934666574302924/sqrt(soil(sr)%aslagm(lay)))
!            soil(sr)%aslagn(lay) = 0.01
!            soil(sr)%aslagx(lay) =  soil(sr)%aslagm(lay)
!     &         * (soil(sr)%as0ags(lay)**(1.52 * soil(sr)%aslagm(lay)**(-0.449)))
!         end do
!      end do

!      do sr=1, nsubr

! conversions from distribution to bin and back are tested and compared.
! conversions are perfect if gsd is set equal to e
! error is decreased if the number of bins is increased (msieve in asd.inc)
! experimentation with adaptive binning yielded moderate improvements
! but would require maintaining separate sieve arrays for every soil layer.
! A middle approach of keeping gsd above 2.0 (mingsd in asd.inc)
! keeps errors on the large end within reason. High values of GSD
! result in reduction of GMD by the conversion in all ranges, but more so
! with larger GMD. The cure is to go to bins in all phases.

!         lay=1

!         do iter=1,110

!         soil(sr)%aslagn(lay) = 0.01
!         soil(sr)%aslagm(lay) = exp(log(soil(sr)%aslagn(lay))                       &
!     &            + iter*(log(10000.)-log(soil(sr)%aslagn(lay)))/(110.))

!         initgmd = soil(sr)%aslagm(lay)
!         soil(sr)%as0ags(lay) = 1.0+1.0/(0.0124484131375676                   &
!     &                  + 0.00246316704248082*soil(sr)%aslagm(lay)            &
!     &                  + 0.0934666574302924/sqrt(soil(sr)%aslagm(lay)))

!         this is a test function for gsd that keeps it high for 
!         high values of gmd.
!         soil(sr)%as0ags(lay) = -41.46857 + 57.4838                           &
!     &                  / (1+exp(-(soil(sr)%aslagm(lay)+0.953397)/0.836953))

!         soil(sr)%as0ags(lay) = min(5.0,soil(sr)%as0ags(lay))

!         initgsd = soil(sr)%as0ags(lay)
!         soil(sr)%aslagx(lay) = soil(sr)%aslagm(lay)                                &
!     &       * (soil(sr)%as0ags(lay)**(1.52 * soil(sr)%aslagm(lay)**(-0.449)))

!         print*,'lognormal case:', logcas, 'subregion',sr
!         print*,'inter, gmd, gsd, mnot, minf'
!         do lay=1, soil(sr)%nslay
!            print*, 0, soil(sr)%aslagm(lay), soil(sr)%as0ags(lay),                  &
!     &              soil(sr)%aslagn(lay), soil(sr)%aslagx(lay)
!            print*, (massf(i,lay),i=1,nsieve+1)
!         end do

!         call asd2m(soil(sr)%aslagn(1,sr), soil(sr)%aslagx(1,sr), soil(sr)%aslagm(1,sr),
!     &              soil(sr)%as0ags(1,sr), soil(sr)%nslay, massf)

!         print*,'----1------subregion',sr,' after asd2m --------------'
!         do lay=1, soil(sr)%nslay
!            print*, soil(sr)%aslagm(lay), soil(sr)%as0ags(lay),
!     &              soil(sr)%aslagn(lay), soil(sr)%aslagx(lay)
!            print*, (massf(i,lay),i=1,nsieve+1)
!         end do

      sr = 1
! massf(msieve+1, lay)
       soil(1)%aslagn( 1 ) =  9.99999978E-03
       soil(1)%aslagx( 1 ) =  34.1721382
       massf( 1 , 1 ) = 3.26654903E-04
       massf( 2 , 1 ) = 7.21673341E-03
       massf( 3 , 1 ) = 2.01139003E-02
       massf( 4 , 1 ) = 2.78498773E-02
       massf( 5 , 1 ) = 3.63771319E-02
       massf( 6 , 1 ) = 4.57650907E-02
       massf( 7 , 1 ) = 5.55659123E-02
       massf( 8 , 1 ) = 6.50868788E-02
       massf( 9 , 1 ) = 7.35133067E-02
       massf( 10 , 1 ) = 8.00503865E-02
       massf( 11 , 1 ) = 8.40748474E-02
       massf( 12 , 1 ) = 8.52634832E-02
       massf( 13 , 1 ) = 8.36649835E-02
       massf( 14 , 1 ) = 7.96904415E-02
       massf( 15 , 1 ) = 7.39659220E-02
       massf( 16 , 1 ) = 6.68115392E-02
       massf( 17 , 1 ) = 5.69429249E-02
       massf( 18 , 1 ) = 4.05939072E-02
       massf( 19 , 1 ) = 1.71260759E-02
       massf( 20 , 1 ) = 0.00000000E+00
       massf( 21 , 1 ) = 0.00000000E+00
       massf( 22 , 1 ) = 0.00000000E+00
       massf( 23 , 1 ) = 0.00000000E+00
       massf( 24 , 1 ) = 0.00000000E+00
       massf( 25 , 1 ) = 0.00000000E+00
       massf( 26 , 1 ) = 0.00000000E+00
       massf( 27 , 1 ) = 0.00000000E+00
       soil(1)%aslagn( 2 ) =  9.99999978E-03
       soil(1)%aslagx( 2 ) =  34.7214241
       massf( 1 , 2 ) = 2.96762533E-04
       massf( 2 , 2 ) = 6.48128474E-03
       massf( 3 , 2 ) = 1.67055521E-02
       massf( 4 , 2 ) = 2.25283150E-02
       massf( 5 , 2 ) = 2.92506050E-02
       massf( 6 , 2 ) = 3.69319133E-02
       massf( 7 , 2 ) = 4.53120396E-02
       massf( 8 , 2 ) = 5.39501607E-02
       massf( 9 , 2 ) = 6.22820817E-02
       massf( 10 , 2 ) = 6.96998015E-02
       massf( 11 , 2 ) = 7.56558180E-02
       massf( 12 , 2 ) = 7.97739998E-02
       massf( 13 , 2 ) = 8.19518119E-02
       massf( 14 , 2 ) = 8.24420527E-02
       massf( 15 , 2 ) = 8.18588436E-02
       massf( 16 , 2 ) = 8.07231665E-02
       massf( 17 , 2 ) = 7.73597583E-02
       massf( 18 , 2 ) = 6.41614199E-02
       massf( 19 , 2 ) = 3.26346084E-02
       massf( 20 , 2 ) = 0.00000000E+00
       massf( 21 , 2 ) = 0.00000000E+00
       massf( 22 , 2 ) = 0.00000000E+00
       massf( 23 , 2 ) = 0.00000000E+00
       massf( 24 , 2 ) = 0.00000000E+00
       massf( 25 , 2 ) = 0.00000000E+00
       massf( 26 , 2 ) = 0.00000000E+00
       massf( 27 , 2 ) = 0.00000000E+00
       soil(1)%aslagn( 3 ) =  9.99999978E-03
       soil(1)%aslagx( 3 ) =  34.8627777
       massf( 1 , 3 ) = 2.92250421E-04
       massf( 2 , 3 ) = 6.37632981E-03
       massf( 3 , 3 ) = 1.63181107E-02
       massf( 4 , 3 ) = 2.19592806E-02
       massf( 5 , 3 ) = 2.85046343E-02
       massf( 6 , 3 ) = 3.60126682E-02
       massf( 7 , 3 ) = 4.42402437E-02
       massf( 8 , 3 ) = 5.27702123E-02
       massf( 9 , 3 ) = 6.10630475E-02
       massf( 10 , 3 ) = 6.85318932E-02
       massf( 11 , 3 ) = 7.46416375E-02
       massf( 12 , 3 ) = 7.90179223E-02
       massf( 13 , 3 ) = 8.15516263E-02
       massf( 14 , 3 ) = 8.24898407E-02
       massf( 15 , 3 ) = 8.24633613E-02
       massf( 16 , 3 ) = 8.20585713E-02
       massf( 17 , 3 ) = 7.96417817E-02
       massf( 18 , 3 ) = 6.71819374E-02
       massf( 19 , 3 ) = 3.48846614E-02
       massf( 20 , 3 ) = 0.00000000E+00
       massf( 21 , 3 ) = 0.00000000E+00
       massf( 22 , 3 ) = 0.00000000E+00
       massf( 23 , 3 ) = 0.00000000E+00
       massf( 24 , 3 ) = 0.00000000E+00
       massf( 25 , 3 ) = 0.00000000E+00
       massf( 26 , 3 ) = 0.00000000E+00
       massf( 27 , 3 ) = 0.00000000E+00
       soil(1)%aslagn( 4 ) =  9.99999978E-03
       soil(1)%aslagx( 4 ) =  40.2471161
       massf( 1 , 4 ) = 1.66178812E-04
       massf( 2 , 4 ) = 3.61970882E-03
       massf( 3 , 4 ) = 9.16925538E-03
       massf( 4 , 4 ) = 1.26060527E-02
       massf( 5 , 4 ) = 1.68409832E-02
       massf( 6 , 4 ) = 2.19988301E-02
       massf( 7 , 4 ) = 2.80637369E-02
       massf( 8 , 4 ) = 3.49157453E-02
       massf( 9 , 4 ) = 4.23346274E-02
       massf( 10 , 4 ) = 5.00199050E-02
       massf( 11 , 4 ) = 5.76375388E-02
       massf( 12 , 4 ) = 6.48960248E-02
       massf( 13 , 4 ) = 7.16627762E-02
       massf( 14 , 4 ) = 7.81575441E-02
       massf( 15 , 4 ) = 8.52868482E-02
       massf( 16 , 4 ) = 9.48690921E-02
       massf( 17 , 4 ) = 0.107569844
       massf( 18 , 4 ) = 0.113597333
       massf( 19 , 4 ) = 8.52066725E-02
       massf( 20 , 4 ) = 2.13812850E-02
       massf( 21 , 4 ) = 0.00000000E+00
       massf( 22 , 4 ) = 0.00000000E+00
       massf( 23 , 4 ) = 0.00000000E+00
       massf( 24 , 4 ) = 0.00000000E+00
       massf( 25 , 4 ) = 0.00000000E+00
       massf( 26 , 4 ) = 0.00000000E+00
       massf( 27 , 4 ) = 0.00000000E+00
       soil(1)%aslagn( 5 ) =  9.99999978E-03
       soil(1)%aslagx( 5 ) =  39.3305817
       massf( 1 , 5 ) = 1.84754608E-04
       massf( 2 , 5 ) = 4.01705550E-03
       massf( 3 , 5 ) = 1.00384625E-02
       massf( 4 , 5 ) = 1.36616677E-02
       massf( 5 , 5 ) = 1.81073137E-02
       massf( 6 , 5 ) = 2.34846584E-02
       massf( 7 , 5 ) = 2.97546871E-02
       massf( 8 , 5 ) = 3.67703065E-02
       massf( 9 , 5 ) = 4.42825630E-02
       massf( 10 , 5 ) = 5.19651808E-02
       massf( 11 , 5 ) = 5.94654828E-02
       massf( 12 , 5 ) = 6.64841607E-02
       massf( 13 , 5 ) = 7.28923157E-02
       massf( 14 , 5 ) = 7.89224505E-02
       massf( 15 , 5 ) = 8.54955167E-02
       massf( 16 , 5 ) = 9.44151357E-02
       massf( 17 , 5 ) = 0.106165051
       massf( 18 , 5 ) = 0.110337973
       massf( 19 , 5 ) = 7.89033994E-02
       massf( 20 , 5 ) = 1.46518592E-02
       massf( 21 , 5 ) = 0.00000000E+00
       massf( 22 , 5 ) = 0.00000000E+00
       massf( 23 , 5 ) = 0.00000000E+00
       massf( 24 , 5 ) = 0.00000000E+00
       massf( 25 , 5 ) = 0.00000000E+00
       massf( 26 , 5 ) = 0.00000000E+00
       massf( 27 , 5 ) = 0.00000000E+00
       soil(1)%aslagn( 6 ) =  9.99999978E-03
       soil(1)%aslagx( 6 ) =  42.5451279
       massf( 1 , 6 ) = 1.24782047E-04
       massf( 2 , 6 ) = 2.73581850E-03
       massf( 3 , 6 ) = 7.26578943E-03
       massf( 4 , 6 ) = 1.03129819E-02
       massf( 5 , 6 ) = 1.41034862E-02
       massf( 6 , 6 ) = 1.87990516E-02
       massf( 7 , 6 ) = 2.44329963E-02
       massf( 8 , 6 ) = 3.09435278E-02
       massf( 9 , 6 ) = 3.81705761E-02
       massf( 10 , 6 ) = 4.58674878E-02
       massf( 11 , 6 ) = 5.37377484E-02
       massf( 12 , 6 ) = 6.15035370E-02
       massf( 13 , 6 ) = 6.90189749E-02
       massf( 14 , 6 ) = 7.64653385E-02
       massf( 15 , 6 ) = 8.46966952E-02
       massf( 16 , 6 ) = 9.55372751E-02
       massf( 17 , 6 ) = 0.110091493
       massf( 18 , 6 ) = 0.120202422
       massf( 19 , 6 ) = 9.91356596E-02
       massf( 20 , 6 ) = 3.68543491E-02
       massf( 21 , 6 ) = 0.00000000E+00
       massf( 22 , 6 ) = 0.00000000E+00
       massf( 23 , 6 ) = 0.00000000E+00
       massf( 24 , 6 ) = 0.00000000E+00
       massf( 25 , 6 ) = 0.00000000E+00
       massf( 26 , 6 ) = 0.00000000E+00
       massf( 27 , 6 ) = 0.00000000E+00
       soil(1)%aslagn( 7 ) =  9.99999978E-03
       soil(1)%aslagx( 7 ) =  42.5451279
       massf( 1 , 7 ) = 1.24782047E-04
       massf( 2 , 7 ) = 2.73581850E-03
       massf( 3 , 7 ) = 7.26578943E-03
       massf( 4 , 7 ) = 1.03129819E-02
       massf( 5 , 7 ) = 1.41034862E-02
       massf( 6 , 7 ) = 1.87990516E-02
       massf( 7 , 7 ) = 2.44329963E-02
       massf( 8 , 7 ) = 3.09435278E-02
       massf( 9 , 7 ) = 3.81705761E-02
       massf( 10 , 7 ) = 4.58674878E-02
       massf( 11 , 7 ) = 5.37377484E-02
       massf( 12 , 7 ) = 6.15035370E-02
       massf( 13 , 7 ) = 6.90189749E-02
       massf( 14 , 7 ) = 7.64653385E-02
       massf( 15 , 7 ) = 8.46966952E-02
       massf( 16 , 7 ) = 9.55372751E-02
       massf( 17 , 7 ) = 0.110091493
       massf( 18 , 7 ) = 0.120202422
       massf( 19 , 7 ) = 9.91356596E-02
       massf( 20 , 7 ) = 3.68543491E-02
       massf( 21 , 7 ) = 0.00000000E+00
       massf( 22 , 7 ) = 0.00000000E+00
       massf( 23 , 7 ) = 0.00000000E+00
       massf( 24 , 7 ) = 0.00000000E+00
       massf( 25 , 7 ) = 0.00000000E+00
       massf( 26 , 7 ) = 0.00000000E+00
       massf( 27 , 7 ) = 0.00000000E+00
       soil(1)%aslagn( 8 ) =  9.99999978E-03
       soil(1)%aslagx( 8 ) =  39.8283195
       massf( 1 , 8 ) = 1.74528570E-04
       massf( 2 , 8 ) = 3.79820727E-03
       massf( 3 , 8 ) = 9.55770351E-03
       massf( 4 , 8 ) = 1.30765252E-02
       massf( 5 , 8 ) = 1.74044464E-02
       massf( 6 , 8 ) = 2.26592366E-02
       massf( 7 , 8 ) = 2.88147125E-02
       massf( 8 , 8 ) = 3.57388332E-02
       massf( 9 , 8 ) = 4.31987531E-02
       massf( 10 , 8 ) = 5.08827083E-02
       massf( 11 , 8 ) = 5.84483854E-02
       massf( 12 , 8 ) = 6.56009540E-02
       massf( 13 , 8 ) = 7.22097009E-02
       massf( 14 , 8 ) = 7.85004050E-02
       massf( 15 , 8 ) = 8.53867680E-02
       massf( 16 , 8 ) = 9.46817622E-02
       massf( 17 , 8 ) = 0.106968105
       massf( 18 , 8 ) = 0.112166926
       massf( 19 , 8 ) = 8.23879689E-02
       massf( 20 , 8 ) = 1.83433779E-02
       massf( 21 , 8 ) = 0.00000000E+00
       massf( 22 , 8 ) = 0.00000000E+00
       massf( 23 , 8 ) = 0.00000000E+00
       massf( 24 , 8 ) = 0.00000000E+00
       massf( 25 , 8 ) = 0.00000000E+00
       massf( 26 , 8 ) = 0.00000000E+00
       massf( 27 , 8 ) = 0.00000000E+00
       soil(1)%aslagn( 9 ) =  9.99999978E-03
       soil(1)%aslagx( 9 ) =  39.0524673
       massf( 1 , 9 ) = 1.90604856E-04
       massf( 2 , 9 ) = 4.14241804E-03
       massf( 3 , 9 ) = 1.03168627E-02
       massf( 4 , 9 ) = 1.40022794E-02
       massf( 5 , 9 ) = 1.85176525E-02
       massf( 6 , 9 ) = 2.39674505E-02
       massf( 7 , 9 ) = 3.03053986E-02
       massf( 8 , 9 ) = 3.73752452E-02
       massf( 9 , 9 ) = 4.49186005E-02
       massf( 10 , 9 ) = 5.26005998E-02
       massf( 11 , 9 ) = 6.00623563E-02
       massf( 12 , 9 ) = 6.70015663E-02
       massf( 13 , 9 ) = 7.32904971E-02
       massf( 14 , 9 ) = 7.91650638E-02
       massf( 15 , 9 ) = 8.55494440E-02
       massf( 16 , 9 ) = 9.42405164E-02
       massf( 17 , 9 ) = 0.105666630
       massf( 18 , 9 ) = 0.109245270
       massf( 19 , 9 ) = 7.68877193E-02
       massf( 20 , 9 ) = 1.25538241E-02
       massf( 21 , 9 ) = 0.00000000E+00
       massf( 22 , 9 ) = 0.00000000E+00
       massf( 23 , 9 ) = 0.00000000E+00
       massf( 24 , 9 ) = 0.00000000E+00
       massf( 25 , 9 ) = 0.00000000E+00
       massf( 26 , 9 ) = 0.00000000E+00
       massf( 27 , 9 ) = 0.00000000E+00
       soil(1)%aslagn( 10 ) =  9.99999978E-03
       soil(1)%aslagx( 10 ) =  36.7751274
       massf( 1 , 10 ) = 2.42009861E-04
       massf( 2 , 10 ) = 5.25301788E-03
       massf( 3 , 10 ) = 1.29520195E-02
       massf( 4 , 10 ) = 1.73229314E-02
       massf( 5 , 10 ) = 2.25832462E-02
       massf( 6 , 10 ) = 2.88021024E-02
       massf( 7 , 10 ) = 3.58582847E-02
       massf( 8 , 10 ) = 4.35005352E-02
       massf( 9 , 10 ) = 5.13685644E-02
       massf( 10 , 10 ) = 5.90341799E-02
       massf( 11 , 10 ) = 6.60703182E-02
       massf( 12 , 10 ) = 7.21424147E-02
       massf( 13 , 10 ) = 7.71251172E-02
       massf( 14 , 10 ) = 8.12657028E-02
       massf( 15 , 10 ) = 8.54246840E-02
       massf( 16 , 10 ) = 9.10542980E-02
       massf( 17 , 10 ) = 9.77471471E-02
       massf( 18 , 10 ) = 9.45795104E-02
       massf( 19 , 10 ) = 5.76739237E-02
       massf( 20 , 10 ) = 0.00000000E+00
       massf( 21 , 10 ) = 0.00000000E+00
       massf( 22 , 10 ) = 0.00000000E+00
       massf( 23 , 10 ) = 0.00000000E+00
       massf( 24 , 10 ) = 0.00000000E+00
       massf( 25 , 10 ) = 0.00000000E+00
       massf( 26 , 10 ) = 0.00000000E+00
       massf( 27 , 10 ) = 0.00000000E+00
       soil(1)%aslagn( 11 ) =  9.99999978E-03
       soil(1)%aslagx( 11 ) =  36.8878746
       massf( 1 , 11 ) = 0.00000000E+00
       massf( 2 , 11 ) = 4.19050455E-03
       massf( 3 , 11 ) = 1.15625858E-02
       massf( 4 , 11 ) = 1.53107643E-02
       massf( 5 , 11 ) = 1.97986364E-02
       massf( 6 , 11 ) = 2.51320601E-02
       massf( 7 , 11 ) = 3.12533379E-02
       massf( 8 , 11 ) = 3.80007625E-02
       massf( 9 , 11 ) = 4.51166630E-02
       massf( 10 , 11 ) = 5.22673130E-02
       massf( 11 , 11 ) = 5.90823889E-02
       massf( 12 , 11 ) = 6.52136803E-02
       massf( 13 , 11 ) = 7.04066157E-02
       massf( 14 , 11 ) = 7.45829344E-02
       massf( 15 , 11 ) = 7.79446065E-02
       massf( 16 , 11 ) = 8.11526179E-02
       massf( 17 , 11 ) = 8.58341604E-02
       massf( 18 , 11 ) = 9.68726873E-02
       massf( 19 , 11 ) = 0.146277681
       massf( 20 , 11 ) = 0.00000000E+00
       massf( 21 , 11 ) = 0.00000000E+00
       massf( 22 , 11 ) = 0.00000000E+00
       massf( 23 , 11 ) = 0.00000000E+00
       massf( 24 , 11 ) = 0.00000000E+00
       massf( 25 , 11 ) = 0.00000000E+00
       massf( 26 , 11 ) = 0.00000000E+00
       massf( 27 , 11 ) = 0.00000000E+00
       soil(1)%aslagn( 12 ) =  9.99999978E-03
       soil(1)%aslagx( 12 ) =  36.8483849
       massf( 1 , 12 ) = 0.00000000E+00
       massf( 2 , 12 ) = 4.20689583E-03
       massf( 3 , 12 ) = 1.16102099E-02
       massf( 4 , 12 ) = 1.53723359E-02
       massf( 5 , 12 ) = 1.98749900E-02
       massf( 6 , 12 ) = 2.52240300E-02
       massf( 7 , 12 ) = 3.13603282E-02
       massf( 8 , 12 ) = 3.81208062E-02
       massf( 9 , 12 ) = 4.52459455E-02
       massf( 10 , 12 ) = 5.23998737E-02
       massf( 11 , 12 ) = 5.92108369E-02
       massf( 12 , 12 ) = 6.53296113E-02
       massf( 13 , 12 ) = 7.05012679E-02
       massf( 14 , 12 ) = 7.46481121E-02
       massf( 15 , 12 ) = 7.79724717E-02
       massf( 16 , 12 ) = 8.11353922E-02
       massf( 17 , 12 ) = 8.57608616E-02
       massf( 18 , 12 ) = 9.67171341E-02
       massf( 19 , 12 ) = 0.145308897
       massf( 20 , 12 ) = 0.00000000E+00
       massf( 21 , 12 ) = 0.00000000E+00
       massf( 22 , 12 ) = 0.00000000E+00
       massf( 23 , 12 ) = 0.00000000E+00
       massf( 24 , 12 ) = 0.00000000E+00
       massf( 25 , 12 ) = 0.00000000E+00
       massf( 26 , 12 ) = 0.00000000E+00
       massf( 27 , 12 ) = 0.00000000E+00
       soil(1)%aslagn( 13 ) =  9.99999978E-03
       soil(1)%aslagx( 13 ) =  39.5941772
       massf( 1 , 13 ) = 0.00000000E+00
       massf( 2 , 13 ) = 3.13174725E-03
       massf( 3 , 13 ) = 8.79931450E-03
       massf( 4 , 13 ) = 1.18762255E-02
       massf( 5 , 13 ) = 1.56235099E-02
       massf( 6 , 13 ) = 2.01714039E-02
       massf( 7 , 13 ) = 2.55243182E-02
       massf( 8 , 13 ) = 3.16033363E-02
       massf( 9 , 13 ) = 3.82452011E-02
       massf( 10 , 13 ) = 4.52102423E-02
       massf( 11 , 13 ) = 5.22075891E-02
       massf( 12 , 13 ) = 5.89397550E-02
       massf( 13 , 13 ) = 6.51661158E-02
       massf( 14 , 13 ) = 7.07880259E-02
       massf( 15 , 13 ) = 7.59680271E-02
       massf( 16 , 13 ) = 8.13485682E-02
       massf( 17 , 13 ) = 8.86406302E-02
       massf( 18 , 13 ) = 0.103109479
       massf( 19 , 13 ) = 0.160363585
       massf( 20 , 13 ) = 4.32829335E-02
       massf( 21 , 13 ) = 0.00000000E+00
       massf( 22 , 13 ) = 0.00000000E+00
       massf( 23 , 13 ) = 0.00000000E+00
       massf( 24 , 13 ) = 0.00000000E+00
       massf( 25 , 13 ) = 0.00000000E+00
       massf( 26 , 13 ) = 0.00000000E+00
       massf( 27 , 13 ) = 0.00000000E+00
       soil(1)%aslagn( 14 ) =  9.99999978E-03
       soil(1)%aslagx( 14 ) =  38.4066277
       massf( 1 , 14 ) = 0.00000000E+00
       massf( 2 , 14 ) = 3.58003378E-03
       massf( 3 , 14 ) = 9.91261005E-03
       massf( 4 , 14 ) = 1.32325888E-02
       massf( 5 , 14 ) = 1.72546506E-02
       massf( 6 , 14 ) = 2.20955610E-02
       massf( 7 , 14 ) = 2.77356505E-02
       massf( 8 , 14 ) = 3.40646505E-02
       massf( 9 , 14 ) = 4.08844948E-02
       massf( 10 , 14 ) = 4.79207635E-02
       massf( 11 , 14 ) = 5.48532605E-02
       massf( 12 , 14 ) = 6.13659620E-02
       massf( 13 , 14 ) = 6.72134757E-02
       massf( 14 , 14 ) = 7.23056793E-02
       massf( 15 , 14 ) = 7.68220723E-02
       massf( 16 , 14 ) = 8.14190805E-02
       massf( 17 , 14 ) = 8.77979398E-02
       massf( 18 , 14 ) = 0.101176649
       massf( 19 , 14 ) = 0.160571322
       massf( 20 , 14 ) = 1.97935533E-02
       massf( 21 , 14 ) = 0.00000000E+00
       massf( 22 , 14 ) = 0.00000000E+00
       massf( 23 , 14 ) = 0.00000000E+00
       massf( 24 , 14 ) = 0.00000000E+00
       massf( 25 , 14 ) = 0.00000000E+00
       massf( 26 , 14 ) = 0.00000000E+00
       massf( 27 , 14 ) = 0.00000000E+00
       soil(1)%aslagn( 15 ) =  9.99999978E-03
       soil(1)%aslagx( 15 ) =  38.1134644
       massf( 1 , 15 ) = 0.00000000E+00
       massf( 2 , 15 ) = 3.69483232E-03
       massf( 3 , 15 ) = 1.02081299E-02
       massf( 4 , 15 ) = 1.35979056E-02
       massf( 5 , 15 ) = 1.76974535E-02
       massf( 6 , 15 ) = 2.26207972E-02
       massf( 7 , 15 ) = 2.83414721E-02
       massf( 8 , 15 ) = 3.47408056E-02
       massf( 9 , 15 ) = 4.16107774E-02
       massf( 10 , 15 ) = 4.86669540E-02
       massf( 11 , 15 ) = 5.55810332E-02
       massf( 12 , 15 ) = 6.20315671E-02
       massf( 13 , 15 ) = 6.77716136E-02
       massf( 14 , 15 ) = 7.27131963E-02
       massf( 15 , 15 ) = 7.70399868E-02
       massf( 16 , 15 ) = 8.14105868E-02
       massf( 17 , 15 ) = 8.75184238E-02
       massf( 18 , 15 ) = 0.100546598
       massf( 19 , 15 ) = 0.160344720
       massf( 20 , 15 ) = 1.38631454E-02
       massf( 21 , 15 ) = 0.00000000E+00
       massf( 22 , 15 ) = 0.00000000E+00
       massf( 23 , 15 ) = 0.00000000E+00
       massf( 24 , 15 ) = 0.00000000E+00
       massf( 25 , 15 ) = 0.00000000E+00
       massf( 26 , 15 ) = 0.00000000E+00
       massf( 27 , 15 ) = 0.00000000E+00
       soil(1)%aslagn( 16 ) =  9.99999978E-03
       soil(1)%aslagx( 16 ) =  39.1325493
       massf( 1 , 16 ) = 0.00000000E+00
       massf( 2 , 16 ) = 3.30263376E-03
       massf( 3 , 16 ) = 9.21744108E-03
       massf( 4 , 16 ) = 1.23823881E-02
       massf( 5 , 16 ) = 1.62301064E-02
       massf( 6 , 16 ) = 2.08852887E-02
       massf( 7 , 16 ) = 2.63432264E-02
       massf( 8 , 16 ) = 3.25136185E-02
       massf( 9 , 16 ) = 3.92205119E-02
       massf( 10 , 16 ) = 4.62114215E-02
       massf( 11 , 16 ) = 5.31851053E-02
       massf( 12 , 16 ) = 5.98369837E-02
       massf( 13 , 16 ) = 6.59251809E-02
       massf( 14 , 16 ) = 7.13541508E-02
       massf( 15 , 16 ) = 7.62932599E-02
       massf( 16 , 16 ) = 8.13911557E-02
       massf( 17 , 16 ) = 8.83587003E-02
       massf( 18 , 16 ) = 0.102458090
       massf( 19 , 16 ) = 0.160612166
       massf( 20 , 16 ) = 3.42785642E-02
       massf( 21 , 16 ) = 0.00000000E+00
       massf( 22 , 16 ) = 0.00000000E+00
       massf( 23 , 16 ) = 0.00000000E+00
       massf( 24 , 16 ) = 0.00000000E+00
       massf( 25 , 16 ) = 0.00000000E+00
       massf( 26 , 16 ) = 0.00000000E+00
       massf( 27 , 16 ) = 0.00000000E+00
       soil(1)%aslagn( 17 ) =  9.99999978E-03
       soil(1)%aslagx( 17 ) =  38.8608475
       massf( 1 , 17 ) = 0.00000000E+00
       massf( 2 , 17 ) = 3.40527296E-03
       massf( 3 , 17 ) = 9.47195292E-03
       massf( 4 , 17 ) = 1.26922727E-02
       massf( 5 , 17 ) = 1.66026354E-02
       massf( 6 , 17 ) = 2.13246346E-02
       massf( 7 , 17 ) = 2.68480778E-02
       massf( 8 , 17 ) = 3.30755115E-02
       massf( 9 , 17 ) = 3.98229957E-02
       massf( 10 , 17 ) = 4.68302965E-02
       massf( 11 , 17 ) = 5.37890792E-02
       massf( 12 , 17 ) = 6.03910685E-02
       massf( 13 , 17 ) = 6.63928390E-02
       massf( 14 , 17 ) = 7.17011094E-02
       massf( 15 , 17 ) = 7.64890313E-02
       massf( 16 , 17 ) = 8.14082921E-02
       massf( 17 , 17 ) = 8.81678462E-02
       massf( 18 , 17 ) = 0.102019534
       massf( 19 , 17 ) = 0.160668939
       massf( 20 , 17 ) = 2.88986098E-02
       massf( 21 , 17 ) = 0.00000000E+00
       massf( 22 , 17 ) = 0.00000000E+00
       massf( 23 , 17 ) = 0.00000000E+00
       massf( 24 , 17 ) = 0.00000000E+00
       massf( 25 , 17 ) = 0.00000000E+00
       massf( 26 , 17 ) = 0.00000000E+00
       massf( 27 , 17 ) = 0.00000000E+00
       soil(1)%aslagn( 18 ) =  9.99999978E-03
       soil(1)%aslagx( 18 ) =  37.3938179
       massf( 1 , 18 ) = 0.00000000E+00
       massf( 2 , 18 ) = 3.98290157E-03
       massf( 3 , 18 ) = 1.09768510E-02
       massf( 4 , 18 ) = 1.45614743E-02
       massf( 5 , 18 ) = 1.88741088E-02
       massf( 6 , 18 ) = 2.40231156E-02
       massf( 7 , 18 ) = 2.99643874E-02
       massf( 8 , 18 ) = 3.65558863E-02
       massf( 9 , 18 ) = 4.35619950E-02
       massf( 10 , 18 ) = 5.06715775E-02
       massf( 11 , 18 ) = 5.75331450E-02
       massf( 12 , 18 ) = 6.38105273E-02
       massf( 13 , 18 ) = 6.92525506E-02
       massf( 14 , 18 ) = 7.37766027E-02
       massf( 15 , 18 ) = 7.75768757E-02
       massf( 16 , 18 ) = 8.13146532E-02
       massf( 17 , 18 ) = 8.66523385E-02
       massf( 18 , 18 ) = 9.86321121E-02
       massf( 19 , 18 ) = 0.158278897
       massf( 20 , 18 ) = 0.00000000E+00
       massf( 21 , 18 ) = 0.00000000E+00
       massf( 22 , 18 ) = 0.00000000E+00
       massf( 23 , 18 ) = 0.00000000E+00
       massf( 24 , 18 ) = 0.00000000E+00
       massf( 25 , 18 ) = 0.00000000E+00
       massf( 26 , 18 ) = 0.00000000E+00
       massf( 27 , 18 ) = 0.00000000E+00
       soil(1)%aslagn( 19 ) =  9.99999978E-03
       soil(1)%aslagx( 19 ) =  36.7600632
       massf( 1 , 19 ) = 0.00000000E+00
       massf( 2 , 19 ) = 4.24367189E-03
       massf( 3 , 19 ) = 1.17177963E-02
       massf( 4 , 19 ) = 1.55118704E-02
       massf( 5 , 19 ) = 2.00482607E-02
       massf( 6 , 19 ) = 2.54326463E-02
       massf( 7 , 19 ) = 3.16034555E-02
       massf( 8 , 19 ) = 3.83937359E-02
       massf( 9 , 19 ) = 4.55395579E-02
       massf( 10 , 19 ) = 5.27009368E-02
       massf( 11 , 19 ) = 5.95025420E-02
       massf( 12 , 19 ) = 6.55925274E-02
       massf( 13 , 19 ) = 7.07156062E-02
       massf( 14 , 19 ) = 7.47949481E-02
       massf( 15 , 19 ) = 7.80341923E-02
       massf( 16 , 19 ) = 8.10942054E-02
       massf( 17 , 19 ) = 8.55911821E-02
       massf( 18 , 19 ) = 9.63584781E-02
       massf( 19 , 19 ) = 0.143124387
       massf( 20 , 19 ) = 0.00000000E+00
       massf( 21 , 19 ) = 0.00000000E+00
       massf( 22 , 19 ) = 0.00000000E+00
       massf( 23 , 19 ) = 0.00000000E+00
       massf( 24 , 19 ) = 0.00000000E+00
       massf( 25 , 19 ) = 0.00000000E+00
       massf( 26 , 19 ) = 0.00000000E+00
       massf( 27 , 19 ) = 0.00000000E+00
       soil(1)%aslagn( 20 ) =  9.99999978E-03
       soil(1)%aslagx( 20 ) =  36.6222725
       massf( 1 , 20 ) = 0.00000000E+00
       massf( 2 , 20 ) = 4.30124998E-03
       massf( 3 , 20 ) = 1.18889809E-02
       massf( 4 , 20 ) = 1.57349110E-02
       massf( 5 , 20 ) = 2.03258991E-02
       massf( 6 , 20 ) = 2.57674456E-02
       massf( 7 , 20 ) = 3.19938660E-02
       massf( 8 , 20 ) = 3.88321280E-02
       massf( 9 , 20 ) = 4.60113287E-02
       massf( 10 , 20 ) = 5.31845093E-02
       massf( 11 , 20 ) = 5.99703193E-02
       massf( 12 , 20 ) = 6.60135746E-02
       massf( 13 , 20 ) = 7.10576773E-02
       massf( 14 , 20 ) = 7.50273466E-02
       massf( 15 , 20 ) = 7.81285465E-02
       massf( 16 , 20 ) = 8.10215175E-02
       massf( 17 , 20 ) = 8.53095949E-02
       massf( 18 , 20 ) = 9.57670957E-02
       massf( 19 , 20 ) = 0.139664009
       massf( 20 , 20 ) = 0.00000000E+00
       massf( 21 , 20 ) = 0.00000000E+00
       massf( 22 , 20 ) = 0.00000000E+00
       massf( 23 , 20 ) = 0.00000000E+00
       massf( 24 , 20 ) = 0.00000000E+00
       massf( 25 , 20 ) = 0.00000000E+00
       massf( 26 , 20 ) = 0.00000000E+00
       massf( 27 , 20 ) = 0.00000000E+00
       soil(1)%aslagn( 21 ) =  9.99999978E-03
       soil(1)%aslagx( 21 ) =  36.3896484
       massf( 1 , 21 ) = 0.00000000E+00
       massf( 2 , 21 ) = 4.39929962E-03
       massf( 3 , 21 ) = 1.21878982E-02
       massf( 4 , 21 ) = 1.61277056E-02
       massf( 5 , 21 ) = 2.08169222E-02
       massf( 6 , 21 ) = 2.63610482E-02
       massf( 7 , 21 ) = 3.26868892E-02
       massf( 8 , 21 ) = 3.96108627E-02
       massf( 9 , 21 ) = 4.68493104E-02
       massf( 10 , 21 ) = 5.40425777E-02
       massf( 11 , 21 ) = 6.07990623E-02
       massf( 12 , 21 ) = 6.67567849E-02
       massf( 13 , 21 ) = 7.16577172E-02
       massf( 14 , 21 ) = 7.54289925E-02
       massf( 15 , 21 ) = 7.82810152E-02
       massf( 16 , 21 ) = 8.08723271E-02
       massf( 17 , 21 ) = 8.47820044E-02
       massf( 18 , 21 ) = 9.46713537E-02
       massf( 19 , 21 ) = 0.133668229
       massf( 20 , 21 ) = 0.00000000E+00
       massf( 21 , 21 ) = 0.00000000E+00
       massf( 22 , 21 ) = 0.00000000E+00
       massf( 23 , 21 ) = 0.00000000E+00
       massf( 24 , 21 ) = 0.00000000E+00
       massf( 25 , 21 ) = 0.00000000E+00
       massf( 26 , 21 ) = 0.00000000E+00
       massf( 27 , 21 ) = 0.00000000E+00
       soil(1)%aslagn( 22 ) =  9.99999978E-03
       soil(1)%aslagx( 22 ) =  35.0875130
       massf( 1 , 22 ) = 0.00000000E+00
       massf( 2 , 22 ) = 4.97603416E-03
       massf( 3 , 22 ) = 1.42389536E-02
       massf( 4 , 22 ) = 1.89542174E-02
       massf( 5 , 22 ) = 2.44268775E-02
       massf( 6 , 22 ) = 3.07773352E-02
       massf( 7 , 22 ) = 3.78750563E-02
       massf( 8 , 22 ) = 4.54491973E-02
       massf( 9 , 22 ) = 5.31144142E-02
       massf( 10 , 22 ) = 6.04107380E-02
       massf( 11 , 22 ) = 6.68656826E-02
       massf( 12 , 22 ) = 7.20706582E-02
       massf( 13 , 22 ) = 7.57628977E-02
       massf( 14 , 22 ) = 7.78993070E-02
       massf( 15 , 22 ) = 7.87275732E-02
       massf( 16 , 22 ) = 7.88900852E-02
       massf( 17 , 22 ) = 7.97383785E-02
       massf( 18 , 22 ) = 8.48482624E-02
       massf( 19 , 22 ) = 9.49743316E-02
       massf( 20 , 22 ) = 0.00000000E+00
       massf( 21 , 22 ) = 0.00000000E+00
       massf( 22 , 22 ) = 0.00000000E+00
       massf( 23 , 22 ) = 0.00000000E+00
       massf( 24 , 22 ) = 0.00000000E+00
       massf( 25 , 22 ) = 0.00000000E+00
       massf( 26 , 22 ) = 0.00000000E+00
       massf( 27 , 22 ) = 0.00000000E+00
       soil(1)%aslagn( 23 ) =  9.99999978E-03
       soil(1)%aslagx( 23 ) =  43.1936340
       massf( 1 , 23 ) = 0.00000000E+00
       massf( 2 , 23 ) = 1.97649002E-03
       massf( 3 , 23 ) = 6.04480505E-03
       massf( 4 , 23 ) = 8.58020782E-03
       massf( 5 , 23 ) = 1.17012262E-02
       massf( 6 , 23 ) = 1.55790448E-02
       massf( 7 , 23 ) = 2.02774405E-02
       massf( 8 , 23 ) = 2.57897377E-02
       massf( 9 , 23 ) = 3.20329666E-02
       massf( 10 , 23 ) = 3.88457775E-02
       massf( 11 , 23 ) = 4.60018516E-02
       massf( 12 , 23 ) = 5.32427430E-02
       massf( 13 , 23 ) = 6.03334904E-02
       massf( 14 , 23 ) = 6.71461821E-02
       massf( 15 , 23 ) = 7.37860203E-02
       massf( 16 , 23 ) = 8.08227956E-02
       massf( 17 , 23 ) = 8.98782611E-02
       massf( 18 , 23 ) = 0.105895251
       massf( 19 , 23 ) = 0.155650258
       massf( 20 , 23 ) = 0.106415451
       massf( 21 , 23 ) = 0.00000000E+00
       massf( 22 , 23 ) = 0.00000000E+00
       massf( 23 , 23 ) = 0.00000000E+00
       massf( 24 , 23 ) = 0.00000000E+00
       massf( 25 , 23 ) = 0.00000000E+00
       massf( 26 , 23 ) = 0.00000000E+00
       massf( 27 , 23 ) = 0.00000000E+00
       soil(1)%aslagn( 24 ) =  9.99999978E-03
       soil(1)%aslagx( 24 ) =  41.9258575
       massf( 1 , 24 ) = 0.00000000E+00
       massf( 2 , 24 ) = 2.34484673E-03
       massf( 3 , 24 ) = 6.92212582E-03
       massf( 4 , 24 ) = 9.62918997E-03
       massf( 5 , 24 ) = 1.29488111E-02
       massf( 6 , 24 ) = 1.70390606E-02
       massf( 7 , 24 ) = 2.19444633E-02
       massf( 8 , 24 ) = 2.76356339E-02
       massf( 9 , 24 ) = 3.40040326E-02
       massf( 10 , 24 ) = 4.08635736E-02
       massf( 11 , 24 ) = 4.79676127E-02
       massf( 12 , 24 ) = 5.50457239E-02
       massf( 13 , 24 ) = 6.18622899E-02
       massf( 14 , 24 ) = 6.82998896E-02
       massf( 15 , 24 ) = 7.44852424E-02
       massf( 16 , 24 ) = 8.10171068E-02
       massf( 17 , 24 ) = 8.95617306E-02
       massf( 18 , 24 ) = 0.105219603
       massf( 19 , 24 ) = 0.157551974
       massf( 20 , 24 ) = 8.56570899E-02
       massf( 21 , 24 ) = 0.00000000E+00
       massf( 22 , 24 ) = 0.00000000E+00
       massf( 23 , 24 ) = 0.00000000E+00
       massf( 24 , 24 ) = 0.00000000E+00
       massf( 25 , 24 ) = 0.00000000E+00
       massf( 26 , 24 ) = 0.00000000E+00
       massf( 27 , 24 ) = 0.00000000E+00
       soil(1)%aslagn( 25 ) =  9.99999978E-03
       soil(1)%aslagx( 25 ) =  35.7278252
       massf( 1 , 25 ) = 0.00000000E+00
       massf( 2 , 25 ) = 4.68474627E-03
       massf( 3 , 25 ) = 1.31267309E-02
       massf( 4 , 25 ) = 1.73914433E-02
       massf( 5 , 25 ) = 2.24142075E-02
       massf( 6 , 25 ) = 2.83042192E-02
       massf( 7 , 25 ) = 3.49637866E-02
       massf( 8 , 25 ) = 4.21721935E-02
       massf( 9 , 25 ) = 4.96029854E-02
       massf( 10 , 25 ) = 5.68536520E-02
       massf( 11 , 25 ) = 6.34973645E-02
       massf( 12 , 25 ) = 6.91508055E-02
       massf( 13 , 25 ) = 7.35518932E-02
       massf( 14 , 25 ) = 7.66383708E-02
       massf( 15 , 25 ) = 7.86348581E-02
       massf( 16 , 25 ) = 8.01932812E-02
       massf( 17 , 25 ) = 8.28001946E-02
       massf( 18 , 25 ) = 9.06831175E-02
       massf( 19 , 25 ) = 0.115336150
       massf( 20 , 25 ) = 0.00000000E+00
       massf( 21 , 25 ) = 0.00000000E+00
       massf( 22 , 25 ) = 0.00000000E+00
       massf( 23 , 25 ) = 0.00000000E+00
       massf( 24 , 25 ) = 0.00000000E+00
       massf( 25 , 25 ) = 0.00000000E+00
       massf( 26 , 25 ) = 0.00000000E+00
       massf( 27 , 25 ) = 0.00000000E+00
       soil(1)%aslagn( 26 ) =  9.99999978E-03
       soil(1)%aslagx( 26 ) =  35.4029770
       massf( 1 , 26 ) = 0.00000000E+00
       massf( 2 , 26 ) = 4.82988358E-03
       massf( 3 , 26 ) = 1.36559606E-02
       massf( 4 , 26 ) = 1.81256533E-02
       massf( 5 , 26 ) = 2.33548284E-02
       massf( 6 , 26 ) = 2.94567943E-02
       massf( 7 , 26 ) = 3.63189578E-02
       massf( 8 , 26 ) = 4.36978340E-02
       massf( 9 , 26 ) = 5.12399077E-02
       massf( 10 , 26 ) = 5.85163832E-02
       massf( 11 , 26 ) = 6.50789738E-02
       massf( 12 , 26 ) = 7.05325603E-02
       massf( 13 , 26 ) = 7.46135712E-02
       massf( 14 , 26 ) = 7.72680044E-02
       massf( 15 , 26 ) = 7.87301958E-02
       massf( 16 , 26 ) = 7.96474814E-02
       massf( 17 , 26 ) = 8.14450383E-02
       massf( 18 , 26 ) = 8.80564600E-02
       massf( 19 , 26 ) = 0.105431512
       massf( 20 , 26 ) = 0.00000000E+00
       massf( 21 , 26 ) = 0.00000000E+00
       massf( 22 , 26 ) = 0.00000000E+00
       massf( 23 , 26 ) = 0.00000000E+00
       massf( 24 , 26 ) = 0.00000000E+00
       massf( 25 , 26 ) = 0.00000000E+00
       massf( 26 , 26 ) = 0.00000000E+00
       massf( 27 , 26 ) = 0.00000000E+00
       soil(1)%aslagn( 27 ) =  9.99999978E-03
       soil(1)%aslagx( 27 ) =  34.4231033
       massf( 1 , 27 ) = 0.00000000E+00
       massf( 2 , 27 ) = 5.32758236E-03
       massf( 3 , 27 ) = 1.59782767E-02
       massf( 4 , 27 ) = 2.15560198E-02
       massf( 5 , 27 ) = 2.78626680E-02
       massf( 6 , 27 ) = 3.50480080E-02
       massf( 7 , 27 ) = 4.29201722E-02
       massf( 8 , 27 ) = 5.11114001E-02
       massf( 9 , 27 ) = 5.91250062E-02
       massf( 10 , 27 ) = 6.63970709E-02
       massf( 11 , 27 ) = 7.23783970E-02
       massf( 12 , 27 ) = 7.66293406E-02
       massf( 13 , 27 ) = 7.89050460E-02
       massf( 14 , 27 ) = 7.92190135E-02
       massf( 15 , 27 ) = 7.78803527E-02
       massf( 16 , 27 ) = 7.55292773E-02
       massf( 17 , 27 ) = 7.32834190E-02
       massf( 18 , 27 ) = 7.35431090E-02
       massf( 19 , 27 ) = 6.73058406E-02
       massf( 20 , 27 ) = 0.00000000E+00
       massf( 21 , 27 ) = 0.00000000E+00
       massf( 22 , 27 ) = 0.00000000E+00
       massf( 23 , 27 ) = 0.00000000E+00
       massf( 24 , 27 ) = 0.00000000E+00
       massf( 25 , 27 ) = 0.00000000E+00
       massf( 26 , 27 ) = 0.00000000E+00
       massf( 27 , 27 ) = 0.00000000E+00
       soil(1)%aslagn( 28 ) =  9.99999978E-03
       soil(1)%aslagx( 28 ) =  36.5747108
       massf( 1 , 28 ) = 0.00000000E+00
       massf( 2 , 28 ) = 2.17258930E-04
       massf( 3 , 28 ) = 2.15524435E-03
       massf( 4 , 28 ) = 5.47182560E-03
       massf( 5 , 28 ) = 1.05124116E-02
       massf( 6 , 28 ) = 1.77884698E-02
       massf( 7 , 28 ) = 2.76243091E-02
       massf( 8 , 28 ) = 3.99852991E-02
       massf( 9 , 28 ) = 5.43172359E-02
       massf( 10 , 28 ) = 6.94804788E-02
       massf( 11 , 28 ) = 8.38524699E-02
       massf( 12 , 28 ) = 9.56113935E-02
       massf( 13 , 28 ) = 0.103138983
       massf( 14 , 28 ) = 0.105409920
       massf( 15 , 28 ) = 0.102219850
       massf( 16 , 28 ) = 9.41496938E-02
       massf( 17 , 28 ) = 8.22157785E-02
       massf( 18 , 28 ) = 6.69156760E-02
       massf( 19 , 28 ) = 3.89336981E-02
       massf( 20 , 28 ) = 0.00000000E+00
       massf( 21 , 28 ) = 0.00000000E+00
       massf( 22 , 28 ) = 0.00000000E+00
       massf( 23 , 28 ) = 0.00000000E+00
       massf( 24 , 28 ) = 0.00000000E+00
       massf( 25 , 28 ) = 0.00000000E+00
       massf( 26 , 28 ) = 0.00000000E+00
       massf( 27 , 28 ) = 0.00000000E+00
       soil(1)%aslagn( 29 ) =  9.99999978E-03
       soil(1)%aslagx( 29 ) =  34.4231033
       massf( 1 , 29 ) = 0.00000000E+00
       massf( 2 , 29 ) = 5.32758236E-03
       massf( 3 , 29 ) = 1.59782767E-02
       massf( 4 , 29 ) = 2.15560198E-02
       massf( 5 , 29 ) = 2.78626680E-02
       massf( 6 , 29 ) = 3.50480080E-02
       massf( 7 , 29 ) = 4.29201722E-02
       massf( 8 , 29 ) = 5.11114001E-02
       massf( 9 , 29 ) = 5.91250062E-02
       massf( 10 , 29 ) = 6.63970709E-02
       massf( 11 , 29 ) = 7.23783970E-02
       massf( 12 , 29 ) = 7.66293406E-02
       massf( 13 , 29 ) = 7.89050460E-02
       massf( 14 , 29 ) = 7.92190135E-02
       massf( 15 , 29 ) = 7.78803527E-02
       massf( 16 , 29 ) = 7.55292773E-02
       massf( 17 , 29 ) = 7.32834190E-02
       massf( 18 , 29 ) = 7.35431090E-02
       massf( 19 , 29 ) = 6.73058406E-02
       massf( 20 , 29 ) = 0.00000000E+00
       massf( 21 , 29 ) = 0.00000000E+00
       massf( 22 , 29 ) = 0.00000000E+00
       massf( 23 , 29 ) = 0.00000000E+00
       massf( 24 , 29 ) = 0.00000000E+00
       massf( 25 , 29 ) = 0.00000000E+00
       massf( 26 , 29 ) = 0.00000000E+00
       massf( 27 , 29 ) = 0.00000000E+00

         call m2asd(massf, soil(1)%nslay,                                   &
     &   soil(1)%aslagn(1), soil(1)%aslagx(1),                              &
     &   soil(1)%aslagm(1), soil(1)%as0ags(1))

!         print*,'subregion',sr,' after m2asd, iteration',iter
!         do lay=1, soil(1)%nslay
!            print*, iter, initgmd, soil(1)%aslagm(lay), initgsd,
!     &              soil(1)%as0ags(lay),
!     &              soil(1)%aslagn(lay), soil(1)%aslagx(lay)
!            do i=1,nsieve+1
!                print*, mdia(i),'massf(',i,',',lay,')',massf(i,lay)
!            end do
!         end do

         initgmd = 0.0
         initgsd = 0.0
         do lay=1, soil(1)%nslay
           write(*,*) lay, initgmd, soil(1)%aslagm(lay),                        &
     &                initgsd, soil(1)%as0ags(lay)
         end do
!      end do
      write(0,*)

! New data test code here - 2/14/2017 - LEW

!     Initialize "soil2" data to "soil" data
      do sr=1, nsubr
         soil2(sr)%aslagn = soil(sr)%aslagn
         soil2(sr)%aslagm = soil(sr)%aslagm
         soil2(sr)%as0ags = soil(sr)%as0ags
         soil2(sr)%aslagx = soil(sr)%aslagx
      end do

      write(UNIT=6,FMT="(2(A), 4(A))",ADVANCE="YES") '  sr', ' lay',            &
     &        '     m_not', '       gsd', '       gsd', '     m_inf'

      do sr=1, nsubr
        do lay=1, soil(sr)%nslay
          write(UNIT=6,FMT="(2(i4), 4(f10.4))",ADVANCE="YES") sr, lay,          &
     &              soil(sr)%aslagn(lay), soil(sr)%aslagm(lay),                 &
     &              soil(sr)%as0ags(lay), soil(sr)%aslagx(lay)
        end do
      end do
      write(0,*)

! Convert to massf and back again, then print
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &         soil(sr)%aslagm(l), soil(sr)%as0ags(l),                      &
     &         soil(sr)%nslay, massf)
          call m2asd(massf, soil2(sr)%nslay,                                    &
     &         soil2(sr)%aslagn(l), soil2(sr)%aslagx(l),                    &
     &         soil2(sr)%aslagm(l), soil2(sr)%as0ags(l))
 
         write(UNIT=6,FMT="(2(i4), 8(f10.4))",ADVANCE="YES") sr, lay,           &
     &              soil2(sr)%aslagn(l), soil2(sr)%aslagm(l),               &
     &              soil2(sr)%as0ags(l), soil2(sr)%aslagx(l),               &
     &  100*(soil(sr)%aslagn(l)-soil2(sr)%aslagn(l))/soil(sr)%aslagn(l),    &
     &  100*(soil(sr)%aslagm(l)-soil2(sr)%aslagm(l))/soil(sr)%aslagm(l),    &
     &  100*(soil(sr)%as0ags(l)-soil2(sr)%as0ags(l))/soil(sr)%as0ags(l),    &
     &  100*(soil(sr)%aslagx(l)-soil2(sr)%aslagx(l))/soil(sr)%aslagx(l)
        end do
      end do
      write(0,*)

! Convert to massf and back again, then print
      do sr=1, nsubr
        do l=1, soil(sr)%nslay
          call asd2m(soil(sr)%aslagn(l), soil(sr)%aslagx(l),                &
     &         (soil(sr)%aslagm(l)-soil(sr)%aslagn(l))*                     &
     &         (soil(sr)%aslagx(l)-soil(sr)%aslagn(l))/                     &
     &         (soil(sr)%aslagx(l)-soil(sr)%aslagm(l)),                     &
     &         (soil(sr)%as0ags(l)-soil(sr)%aslagn(l))*                     &
     &         (soil(sr)%aslagx(l)-soil(sr)%aslagn(l))/                     &
     &         (soil(sr)%aslagx(l)-soil(sr)%as0ags(l)),                     &
     &         soil(sr)%nslay, massf)

          call m2asd(massf, soil2(sr)%nslay,                                    &
     &         soil2(sr)%aslagn(l), soil2(sr)%aslagx(l),                    &
     &         soil2(sr)%aslagm(l), soil2(sr)%as0ags(l))
 
         write(UNIT=6,FMT="(2(i4), 8(f10.4))",ADVANCE="YES") sr, lay,           &
     &              soil2(sr)%aslagn(l), soil2(sr)%aslagm(l),               &
     &              soil2(sr)%as0ags(l), soil2(sr)%aslagx(l),               &
     &  100*(soil(sr)%aslagn(l)-soil2(sr)%aslagn(l))/soil(sr)%aslagn(l),    &
     &  100*(soil(sr)%aslagm(l)-soil2(sr)%aslagm(l))/soil(sr)%aslagm(l),    &
     &  100*(soil(sr)%as0ags(l)-soil2(sr)%as0ags(l))/soil(sr)%as0ags(l),    &
     &  100*(soil(sr)%aslagx(l)-soil2(sr)%aslagx(l))/soil(sr)%aslagx(l)
        end do
      end do

      stop
      end
