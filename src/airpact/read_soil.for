! This is a function to read soil spatial data from file
! into soil array called 'soilDIM'
! @ read from adm_soil.dat
 
      subroutine read_soil(a,b,s,t,isr)

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'airpact/spatialGIS.inc'

      include 'm1subr.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'command.inc'   
 
      include 'file.inc'
! define the spatial variable. m as rows and n as columns         
       integer a,b,s,t,isr
       character *125 soilatt
       character *125 soilcomp
       character *512 line         
       integer i, j, lineNum 


   ! define local variables for soil_comp
      integer grid_code1
      character *14 cokey1
      character *6 mukey,musym
      character *24 compname,taxorder
      integer comppct_r,tfact,slope_r,layernum
      real albedodry
       
! define local variables for soil_att variables
      integer grid_code,depth,fragvol_r  
      character *14 cokey
      character *14 chkey
      real claytotal_r,sandtotal_r,silttotal_r,sandvc_r,sandco_r
      real sandmed_r,sandfine_r,sandvf_r,dbthirdbar_r,om_r
      real ph1to1h2o_r,caco3_r,cec7_r,lep_r,init_swc,wsatiated_r
      real wthirdbar_r,wfifteenbar_r,ksat_r
      real sax_a,sax_e, sax_f,sax_g,sax_m,sax_n,sax_x,sax_y,sax_z
      integer lay

!     + + + FUNCTION DECLARATIONS + + +
      real   plant_wat_g

        soilatt = 'wa_merge_mat.dat'
        soilcomp = 'wa_merge_mco.dat'
        lineNum = 1
        lay = 1

         
        aszcr(isr) = 0.01  ! crust thickness (mm)
 !       asdcr(isr) = asdagd(1,isr)   ! crust density (mg/m3)=agg density
        asfcr(isr) = 0.0   !crust surface fraction
        asmlos(isr) = 0.0 ! mass of loose material on crust
        asflos(isr) = 0.0 ! fraction of loose material on crust
! roughness
        aslrr(isr) = 4.0          ! Random roughness (mm)
        aslrro(isr) = aslrr(isr)   ! init after-tillage RR
        asargo(isr)= 0.0  ! ridge orientation    
        aszrgh(isr) = 0.0 ! ridge height (mm)
        asxrgs(isr) = 10.0 !riage space(mm)
        asxrgw(isr) = 10.0 !ridge width (mm)
    
! calculate soil saturated content and soil CB
       sax_a = 0.0 
       sax_e = -3.140
       sax_f = -2.22e-3
       sax_g = -3.484e-5	  ! coef used to calc CB
       sax_m = -0.108
       sax_n = 0.341   ! coef used to calc POTE  (not used in this code)
       sax_x = 0.332
       sax_y = -7.251e-4
       sax_z = 0.1276 

!copied from insub to assign soil property
         bedrock_depth = 99990.0
         restrict_depth = 99990.0
   
       call fopenk(49,rootp(1:len_trim(rootp))//soilcomp,'old')
        write(*,*) 'File1 is openned!'
       call fopenk(51,rootp(1:len_trim(rootp))//soilatt,'old')
        write(*,*) 'File 2 is openned!'

  100   read (49,'(a)',err=80) line    
            
          if (line(1:1) .eq. '#') then
             go to 100
          else if ((line(1:) .eq. 'EOF') .or. (line(1:).eq.'END')) then
              write(*,*) 'Soil id is not FOUND!!!',soilDim(a,b,s,t)
             goto 300
          else 
             go to 150
          end if

  150     read(line,*)grid_code1,cokey1,mukey,musym,                     &
     &  comppct_r,taxorder,tfact,albedodry,slope_r,layernum   
! test of layer as 0
       
        if (grid_code1 .eq. soilDim(a,b,s,t))  then
 !        am0sid(isr) = grid_code1+'_'+cokey1+musym+compname  ! soil name
           am0tax(isr) = taxorder                     ! taxorder
!           SoilLossTol(isr) = tfact		!NRCS soil loass tolerance
           asfald(isr) = albedodry  		!Dry soil albedo(fraction)
            if( layernum .eq. 0)   then
                nslay = 1
            else
               nslay(isr) = layernum 
            end if    
           
          ! set slope gradient
           amrslp(isr) = slope_r/100.   
             ! check subregion slope value for validity
                   if( amrslp(isr) .lt. 0.0 ) then
                ! no valid value found in IFC file either, set default value of 1%
                     amrslp(isr) = 0.01
                    end if 
           rewind 49 
           go to 200       
         else     
             go to 100    
         end if 
           
  200   read (51,'(a)',end=80) line
         if (line(1:1) .eq. '#') then
             go to 200
          else if ((line(1:) .eq. 'EOF') .or. (line(1:).eq.'END')) then
           write(*,*) 'Soil id is not FOUND!!!',soilDim(a,b,s,t)
             goto 301
          else 
             go to 250
          end if

! min agg size (mm)         
  250  aslagn(lay,isr) = 0.01 
! agg density (mg/m3)
       asdagd(lay,isr) = 1.8         
   
!        write(*,*)'Line:',line
       read(line,*) grid_code,cokey,chkey,depth,claytotal_r,             &
     & sandtotal_r,silttotal_r,sandvc_r,sandco_r,sandmed_r,sandfine_r,   &
     & sandvf_r,dbthirdbar_r,om_r,ph1to1h2o_r,caco3_r,cec7_r,lep_r,      &
     & init_swc,wsatiated_r,wthirdbar_r,wfifteenbar_r,ksat_r,fragvol_r
 ! find the soil type with grid-code and assign the values into global soil 
 ! variables  
!         write(*,*)'Line:',line
        if (grid_code .eq. soilDim(a,b,s,t)) then
!         write(*,*)'Same ID:',line
! assign the variables to globle variables,
       
         aszlyt(lay,isr) = depth *10   ! layer thickness (mm)
           
         asfsan(lay,isr) = sandtotal_r/100.     ! sand fraction
         asfsil(lay,isr) = silttotal_r/100.      ! silt fraction
         asfcla(lay,isr) = claytotal_r/100.     ! clay fraction 
         asfvcs(lay,isr) = sandvc_r/100.        ! very coarse sand
         asvroc(lay,isr) = fragvol_r/100.       ! rock fragment
         asfcs(lay,isr) =  sandco_r/100.        ! coarse sand 
         asfms(lay,isr) = sandmed_r/100.        ! medial sand             
         asffs(lay,isr) = sandfine_r/100.       ! fine sand
         asfvfs(lay,isr) = sandvf_r/100.        ! very fine sand
         asdwblk(lay,isr) = dbthirdbar_r       ! bulk density 

         asfom(lay,isr) = om_r/100.             ! organic matter (kg/kg)
         as0ph(lay,isr) = ph1to1h2o_r           ! soil PH
         asfcce(lay,isr) = caco3_r/100.         ! soil Calcium
         asfcec(lay,isr) = cec7_r              ! cation exchange capacity
         asfcle(lay,isr) = lep_r               ! linear extensibility
         
         asdblk0(lay,isr) = dbthirdbar_r       ! initial previous day BD
         ahrwc(lay,isr) = init_swc/dbthirdbar_r/100. !convert to mass basis
         ahrwcf(lay,isr)=wthirdbar_r/100./dbthirdbar_r !Field capacity
         ahrwcw(lay,isr)=wfifteenbar_r/100./dbthirdbar_r !wilting point     
         
         ahrsk(lay,isr)= ksat_r/1000000.   ! saturated Hydraulic conductivity
         
!        write(*,*) 'Layer info:',lay, nslay(isr)
! calculate soil aggregate information
! aggregate geometric mean diameter(mm)
       
        if (asfcla(lay,isr) .eq. 0) then 
         asfcla(lay,isr) = 1.0
         write(*,*) 'There is a float error at:',lay, isr 
         end if 

        aslagm(lay,isr)= exp(1.343 - 2.235*asfsan(lay,isr)-              &  
     &   1.226*asfsil(lay,isr)- 0.0238*asfsan(lay,isr)/asfcla(lay,isr)+  &
     &   33.6*asfom(lay,isr)+6.85*asfcce(lay,isr))*(1. + 0.006*          &
     &   aszlyt(lay,isr))

      
! aggregate standard derivation
     
       as0ags(lay,isr)=1./(0.012448+0.002463*aslagm(lay,isr)+(0.093467/  &
     &  aslagm(lay,isr)**0.5))
       
!        if (as0ags(lay,isr) .eq. 1.0) then 
!       write(*,*)'as0ags is 1 at:',aslagm(1,isr),as0ags(1,isr)                
!       end if
! max agg size(mm)
        aslagx(lay,isr)= as0ags(lay,isr)**(1.52*aslagm(lay,isr)**        &
     &   (-0.449))* aslagm(lay,isr) 

      
       sax_a = 100. * exp(-4.396 - 0.0715*asfcla(lay,isr)*100.          &				
     &        -(4.880e-4)*(asfsan(lay,isr)*100.)**2. -                  &	
     &      (4.285e-5)*(asfsan(lay,isr)*100.)**2.*asfcla(lay,isr)*100.)

! saturated soil content
       if(asfcla(lay,isr).le. 0) then
          ahrwcs(lay,isr)=sax_x + sax_y*asfsan(lay,isr)*100.

       ahrwcs(lay,isr)=sax_x + sax_y*asfsan(lay,isr)*100. +             &	! saturated soil water content
     &     sax_z * log(asfcla(lay,isr)*100.)/log(10.0)
       end if

       ahrwcs(lay,isr) = ahrwcs(lay,isr)/dbthirdbar_r/100.          ! convert to mass basis
       
       ah0cb(lay,isr) = -(sax_e+ sax_f * (asfcla(lay,isr)*100.)**2 +    &
     &  (sax_g * (asfsan(lay,isr)*100.)**2.0)* asfcla(lay,isr)*100.)
! Air entry potential (J/kg)
       aheaep(lay,isr)=-sax_a*(ahrwcs(lay,isr)**(-ah0cb(lay,isr)))

! calculate the dry aggregated stability 
           if (asfcla(lay,isr) .gt. 0.5) then 
               aseags(lay,isr) = 2.73
           else
               aseags(lay,isr) = 0.83 + 15.7*asfcla(lay,isr)-            &
     &                     23.8*asfcla(lay,isr)**2.
           end if
    
         lay = lay + 1
            if (lay .le. nslay(isr)) then 
                go to 200                
            else 
               go to 70  
            end if       
        else 
            go to 200            
        end if
! assign soil depth to rock
             

 70      aszlyd(1, isr) = aszlyt(1, isr)
       
 !       do i=2,nslay(isr)    
 !        aszlyd(i,isr) = aszlyt(i,isr) + aszlyd(i-1,isr)
 !       end do   

  

! initialize new variables not read in from ifc file 
       do lay = 1, nslay(isr)
             ahfredsat(lay,isr) = 0.0
       end do


! Crust stability
       asecr(isr) = aseags(1,isr)
      ! set layer thickness of the soils as is appropriate for the simulation
      call spllay_ifc(isr)
      
      ! calculate wet albedo from dry
      asfalw(isr) = asfald(isr)/((1.33**2.)*(1-asfald(isr))+asfald(isr))

      ! texture based calculation of settled bulk density and particle density
      call proptext( nslay(isr), asfcla(1,isr), asfsan(1,isr),          &
     &               asfom(1,isr), asdsblk(1,isr), asdpart(1,isr) )

      ! calculate (or recalculate) additional values from soil basic properties
    
      do lay=1,nslay(isr)

!       command line switch, changes to IFC values
        if( wc_type.eq.0 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(vol), convert both to (grav)
            continue
        else if( wc_type.eq.1 ) then
!           Ifc inputs are 1/3bar(vol), 15bar(grav), convert 1/3bar(vol) to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
        else if( wc_type.eq.2 ) then
!           Ifc inputs are 1/3bar(grav), 15bar(grav), no conversion necessary
        else if( wc_type.eq.3 ) then
!!          Use texture based calculation of 1/3bar(vol), 15bar(vol) and bulk
!           density and convert to (grav). Using Saxton Method
            call propsaxt(asfsan(lay,isr), asfcla(lay,isr),             &
     &                    ahrwcs(lay,isr),                              &
     &                    ahrwcf(lay,isr), ahrwcw(lay,isr) )
!!          use volumetric saturation to calculate bulk density
            asdwblk(lay,isr) = (1.0-ahrwcs(lay,isr)) * asdpart(lay,isr)
!           Returned values are 1/3bar(vol), 15bar(vol), convert both to (grav)
            ahrwcf(lay,isr) = ahrwcf(lay,isr) / asdwblk(lay,isr)
            ahrwcw(lay,isr) = ahrwcw(lay,isr) / asdwblk(lay,isr)
        end if
   
        
!       set soil to field capacity not wilting point
!        ahrwc(lay,isr) = ahrwcf(lay,isr)
       
!       make sure settled bd is greater than or equal to wet bulk density
        if( asdsblk(lay,isr).lt.asdwblk(lay,isr) ) then
            asdsblk(lay,isr) = asdwblk(lay,isr)
        endif

!       set initial condition to wet bulk density, not dry
        asdblk(lay,isr) = asdwblk(lay,isr)
!       set previous day bulk density
        asdblk0(lay,isr) = asdblk(lay,isr)


!       set saturation based on definition
        ahrwcs(lay,isr) = 1.0/asdblk(lay,isr)-1.0/asdpart(lay,isr)
        if(ahrwcs(lay,isr).lt.ahrwcf(lay,isr)) then
!            ahrwcf(lay,isr) = ahrwcs(lay,isr)
            write(*,*) 'Layer, Field Capacity > Saturation',            &
     &                 lay, ahrwcf(lay,isr), ahrwcs(lay,isr)

        endif

!      write(luo_erod,*)'ahrwcf:',ahrwcf(1:3,isr) 
!      write(luo_erod,*)'asdblk:',asdblk(1:3,isr)
!      write(luo_erod,*)'ahrwc:',ahrwc(1:3,isr)
      end do
        
       
      if( wc_type.eq.4 ) then
          ! use texture based calculations from Rawls to set all soil
          ! water properties.
          call param_prop_bc(                                           &
     &        nslay(isr), aszlyd(1,isr), asdblk(1,isr), asdpart(1,isr), &
     &        asfcla(1,isr), asfsan(1,isr), asfom(1,isr), asfcec(1,isr),&
     &        ahrwcs(1,isr), ahrwcf(1,isr), ahrwcw(1,isr),ahrwcr(1,isr),&
     &        ahrwca(1,isr), ah0cb(1,isr), aheaep(1,isr), ahrsk(1,isr), &
     &        ahfredsat(1,isr) )

      else
 
          ! set matrix potential parameters to match 1/3 bar and 15 bar water contents
          call param_pot_bc( nslay(isr), asdblk(1,isr), asdpart(1,isr), &
     &                     ahrwcf(1,isr), ahrwcw(1,isr),                &
     &                     asfcla(1,isr), asfom(1,isr),                 &
     &                     ah0cb(1,isr), aheaep(1,isr) )
      end if
!      write(luo_erod,*)'pot_b',aheaep(1,isr),ahrwc(1,isr),ah0cb(1,isr)
  
      
!       write(luo_erod,*)'ahrwcf:',ahrwcf(1:3,isr)
!       write(luo_erod,*)'ahrwcw:',ahrwcw(1:3,isr)
!       write(luo_erod,*)'ahrwc:',ahrwc(1:3,isr) 

 !         write(*,*) 'inpsub:total 500mm',                              &
!     &    plant_wat_g( 0.0, 500.0, ahrwcf(1,isr), ahrwcw(1,isr),        &
!     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) ),      &
!     &    plant_wat_g( 500.0, 1000.0, ahrwcf(1,isr), ahrwcw(1,isr),     &
!     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) ),      &
!     &    plant_wat_g( 1000.0, 1500.0, ahrwcf(1,isr), ahrwcw(1,isr),    &
!     &                 asdblk(1,isr), aszlyt(1,isr), nslay(isr) )

        
!       some soil characteristic values for crop nutrient effects 
!       were originally planned and then dropped and are not included in 
!       layer splitting. A Debug full debug compile complains
!       that these values are not initialized when they are mixed as
!       part of  management process. they are initialized here to avoid
!       removing them from mix and invert
        do lay = 1, nslay(isr)
            ascmg(lay,isr) = 0.0
            ascna(lay,isr) = 0.0
            asfesp(lay,isr) = 0.0
            asfnoh(lay,isr) = 0.0
            asfpoh(lay,isr) = 0.0
            asfpsp(lay,isr) = 0.0
        end do
        asdcr(isr) = asdagd(1,isr)   ! crust density (mg/m3)=agg density 
! end copied
!      write(luo_erod,*)'ahrwcf3:',ahrwcf(1:3,isr)
!       write(luo_erod,*)'ahrwcw3:',ahrwcw(1:3,isr)
!       write(luo_erod,*)'ahrwc3:',ahrwc(1:3,isr) 
          
 80    write (0,9001) soilDim(a,b,s,t),s,t
 9001  format('Soil ID are not found at:',I4,I3,I3)
 81    write (0,9002) soilDim(a,b,s,t),s,t
 9002  format('Soil comp are not found at:',I4,I3,I3)
! 1001  format (3i6)
! 1002  format (2f16.8)           
 300   close (49)
 301   close (51)
       end                
