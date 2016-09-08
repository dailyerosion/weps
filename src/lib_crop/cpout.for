!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cpout( isr )
!     Author : A. Retta - 11/19/96
!     + + + PURPOSE + + +
!     Prints headers for the CROP submodel output files

      use file_io_mod, only: luoseason, luocrop, luoshoot, luoinpt
      use crop_data_struct_defs, only: am0cfl

      integer, intent(in) :: isr   ! subregion number

!     + + + OUTPUT FORMATS + + +
 2131 format ('#                           stand   stand   stand   flat &
     &   flat    flat    root    root  bel.grnd  total   total')
 2132 format ('#daysim doy year dap heatui stem    leaf    store   stem &
     &   leaf    store   store   fiber   stem    leaf    stem    height &
     & #stem   lai     eff_lai rootd  grainf tempst watstf  frost  ffa  &
     &  ffw   par     apar     massinc    p_rw   p_st   p_lf   p_rp  std&
     &flt pdiam  parea  fpdiam fparea hu_del frzhrd sai repstmd crop')
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

 2043 format('# Planting |Harv/Term  |')
 2044 format('#dy mo year|dy mo year |')
 2045 format('#          |           |')                                 &

 2053 format('                                        |')
 2054 format('crop_name                               |')
 2055 format('                                        |')

 2063 format('standing|      |       |flat   |       |       |root  |')
 2064 format('stem    |leaf  |store  |stem   |leaf   |store  |stem  |')
 2065 format('kg/m^2  |------|-------|-------|-------|-------|------|')

 2073 format('       |       |      |         |root  |')
 2074 format('store  |fiber  |height|stemcount|depth |')
 2075 format('-------|-------|meters|#/m^2    |meters|')

 2084 format('grainf |stmrepd|cancov|dapl    |chill |hucum  |mxhu |')
 2085 format('-------|meters |----- |days    |deg_C |deg_C  |deg_C|')

 2094 format('huind|dafm|spring')
 2095 format('-----|days|------')

 6000 format('#plant harvest 0=days_mat calc_d_mat db_d_mat calc_heatu d&
     &b_heatu')
 6001 format('# doy    doy   1=heatunit    days      days    degree_C  d&
     &egree_C')



      ! season.out headers

      write(luoseason(isr),2043,ADVANCE="NO")
      write(luoseason(isr),2053,ADVANCE="NO")
      write(luoseason(isr),2063,ADVANCE="NO")
      write(luoseason(isr),2073,ADVANCE="YES")

      write(luoseason(isr),2044,ADVANCE="NO")
      write(luoseason(isr),2054,ADVANCE="NO")
      write(luoseason(isr),2064,ADVANCE="NO")
      write(luoseason(isr),2074,ADVANCE="NO")
      write(luoseason(isr),2084,ADVANCE="NO")
      write(luoseason(isr),2094,ADVANCE="YES")

      write(luoseason(isr),2045,ADVANCE="NO")
      write(luoseason(isr),2055,ADVANCE="NO")
      write(luoseason(isr),2065,ADVANCE="NO")
      write(luoseason(isr),2075,ADVANCE="NO")
      write(luoseason(isr),2085,ADVANCE="NO")
      write(luoseason(isr),2095,ADVANCE="YES")

      if (am0cfl(isr).gt.0) then

         ! crop.out headers
         write(luocrop(isr), 2131)
         write(luocrop(isr), 2132)
         write(luocrop(isr), 2133)

         ! shoot.out headers
         write(luoshoot(isr), 2231)
         write(luoshoot(isr), 2232)

         ! inpt.out headers
         write(luoinpt(isr), 6000)
         write(luoinpt(isr), 6001)

      endif
      return
      end

