!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cpout
!     Author : A. Retta - 11/19/96
!     + + + PURPOSE + + +
!     Prints headers for the CROP submodel output files

      include 'file.inc'
      include 'm1flag.inc'

!     + + + OUTPUT FORMATS + + +
 2131 format ('#                           stand   stand   stand   flat &
     &   flat    flat    root    root  bel.grnd  total   total')
 2132 format ('#daysim doy year dap heatui stem    leaf    store   stem &
     &   leaf    store   store   fiber   stem    leaf    stem    height &
     & #stem   lai     eff_lai rootd  grainf tempst watstf  frost  ffa  &
     &  ffw   par     apar     massinc    p_rw   p_st   p_lf   p_rp  std&
     &flt pdiam  parea  fpdiam fparea hu_del crop')
 2133 format ('#                           kg/m^2  kg/m^2  kg/m^2  kg/m^&
     &2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  meters &
     &         m^2/m^2 m^2/m^2 meters                                   &
     &        MJ/m^2  MJ/m^2   kg/plnt                                  &
     &    meters m^2')

 2231 format ('#daysim doy year dap heatui ',                           &
     &        's_root_sum f_root_sum tot_mass_req end_shoot_mass ',     &
     &        'end_root_mass d_root_mass d_shoot_mass d_s_root_mass ',  &
     &        'end_stem_mass end_stem_area end_shoot_len bczshoot ',    &
     &        'bcmshoot bcdstm bc0nam')
 2232 format ('#(dy) (dy) (yr) (dy) (C)    ',                           &
     &        '(kg/m^2)   (kg/m^2)   (mg/shoot)   (mg/shoot)     ',     &
     &        '(mg/shoot)    (mg/shoot)  (mg/shoot)   (mg/shoot)    ',  &
     &        '(mg/shoot)    (m^2/shoot)   (m)           (m)      ',    &
     &        '(kg/m^2) (#/m^2)')

 2033 format('#     standing                flat                    root&
     &                                     root')
 2034 format('#year stem    leaf    store   stem    leaf    store   stem&
     &    store   fiber   height stemcount depth   grainf  stmrepd dapl &
     &chill  hucum   mxhu huind dafm spring crop_name')
 2035 format('#     kg/m^2  --------------------------------------------&
     &-----------------   meter  #/m^2     meter   ------  meter  ----  &
     &deg_C  deg_C  deg_C ----- ---- ------')

 6000 format('#plant harvest 0=days_mat calc_d_mat db_d_mat calc_heatu d&
     &b_heatu')
 6001 format('# doy    doy   1=heatunit    days      days    degree_C  d&
     &egree_C')



      ! season.out headers
      write(luoseason, 2033)
      write(luoseason, 2034)
      write(luoseason, 2035)

      if (am0cfl.gt.0) then

         ! crop.out headers
         write(luocrop, 2131)
         write(luocrop, 2132)
         write(luocrop, 2133)

         ! shoot.out headers
         write(luoshoot, 2231)
         write(luoshoot, 2232)

         ! inpt.out headers
         write(luoinpt, 6000)
         write(luoinpt, 6001)

      endif
      return
      end

