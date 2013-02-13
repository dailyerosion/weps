!$Author$
!$Date$
!$Revision$
!$HeadURL$

!
      subroutine prtcmp(npart,spg,dia,frac,frcly,frslt,frsnd,frorg,     &
     & sand1,silt1,clay1,orgmat1)
     
      use wepp_interface_defs
      
      implicit none
!
!**********************************************************************
!                                                                     *
!  Generates a default set of particle for the program using the soil *
!  characteristics read in with the initial parameters.               *
!  Calls functions SEDIA and FALVEL                                   *
!                                                                     *
!  Authors: Foster, Flanagan                                          *
!                                                                     *
!  Reference:                                                         *
!                                                                     *
!    Foster, Young and Neibling. 1985. Sediment composition for       *
!    nonpoint source pollution analyses. Trans. ASAE 28(1):133-139.   *
!                                                                     *
!  In subroutine PRTCMP as well as through the rest of the WEPP       *
!  model, the 5 particle size classes used are:                       *
!                                                                     *
!               1   primary clay                                      *
!               2   primary silt                                      *
!               3   small aggregate                                   *
!               4   large aggregate                                   *
!               5   primary sand                                      *
!                                                                     *
!  Note:                                                              *
!                                                                     *
!    The prediction of particle sizes and detached distributions      *
!    needs to be improved since cover conditions on the plots as well *
!    as whether the sediment is coming from rill or interrill areas   *
!    may greatly affect the sizes and fractions.  The following code  *
!    is the best available predictive science as of now.  dcf 3/5/91  *
!                                                                     *
!**********************************************************************
!
!  
!     spg(10) - specific gravity of each particle class
!     dia(10) - diameter of each particle class (m)
!     frac(10) - fraction of each particle class (0-1)
!     frcly(10) - fraction of clay (0-1) in each particle class
!     frslt(10) - fraction of silt (0-1) in each particle class
!     frsnd(10) - fraction of sand (0-1) in each particle class
!     frorg(10) - fraction of organic matter (0-1) in each part class
!     clay1 - fraction of clay in layer 1
!     sand1- fraction of sand in layer 1
!     silt1 - fraction of silt in layer 1
!     orgmat1 - fraction of organic matter in layer 1
!
      real, intent(out) :: spg(10), dia(10), frcly(10),frslt(10),       &
     & frsnd(10),frorg(10), frac(10)
      real, intent(in) :: clay1, sand1, silt1, orgmat1
      integer, intent(in) :: npart
      
!      real falvel, sedia
  

!  
!******************************************************************
!                                                                 *
!   Local Variables                                               *
!     ratiom : ratio of organic matter to clay in the insitu soil *
!     crct   : correction factor to adjust size fractions when    *
!              fraction of large aggregate is too low             *
!     frclyt : used to calculate fraction of clay in large        *
!              aggregates                                         *
!     frcly1 : used to calculate fraction of clay in large        *
!              aggregates                                         *
!     frcly5 : used to calculate fraction of clay in large        *
!              aggregates                                         *
!                                                                 *
!******************************************************************
!
!     save
      real ratiom, crct, frclyt, frcly1, f1f2f5
      integer k, i, jflag
!
!     all the arrays within this subroutine have been redimensionalized
!     from mxplan to mxelem.  this is required for the channel erosion
!     component for the watershed version.  since all the planar
!     computations are completed prior to the other elements during
!     the watershed simulation, the index iplane is identical to IELMT
!     up until IELMT exceeds iplane.  therefore, the extension of these
!     arrays should not affect hillslope computations. the common blocks
!     /part/ and /solvar/ have been redimensionalized.  **rv** 25-aug-89
!
      jflag = 0
!
!
!     Set the value to proportion the organic matter -
!     this should be done using the clay of the in-situ soil
!     however, if there is no clay, then use the silt.  If
!     there is no silt, then use the sand  - dcf 3/8/91
!
      if (clay1.gt.0.0) then
        ratiom = orgmat1 / clay1
      else if (silt1.gt.0.0) then
        ratiom = orgmat1 / silt1
      else
        ratiom = orgmat1 / sand1
      end if
!
!     Set or calculate the particle diameters
!
      dia(1) = 0.002
      dia(2) = 0.010
      dia(4) = 0.300
      if (clay1.gt.0.15) dia(4) = 2.0 * clay1
      dia(5) = 0.200
!
!     Set the particle specific gravities
!
      spg(1) = 2.60
      spg(2) = 2.65
      spg(3) = 1.80
      spg(4) = 1.60
      spg(5) = 2.65
!
!     Determine the fraction of each detached particle type based
!     upon the original soil texture (improved CREAMS approach)
!
!     Calculate the fraction of particle type 1 - primary clay
!
      if (clay1.gt.0.0.and.clay1.lt.1.0) then
        frac(1) = 0.26 * clay1
      else if (clay1.le.0.0) then
        frac(1) = 0.0001
      else
        frac(1) = 0.9996
      end if
!
!     Calculate the fraction of particle type 5 - primary sand
!
      frac(5) = sand1 * (1.0-clay1) ** 5.0
      if (frac(5).le.0.0) frac(5) = 0.0001
!
!     Calculate the diameter and fraction of class 3 - small agg.
!     and the fraction of class 2 - primary silt.
!
!     If the surface soil is 100% clay - no small aggregates can
!     exist - set a mimimal fraction and a diameter.
!
      if (clay1.ge.1.0) then
        dia(3) = 0.180
        frac(3) = 0.0001
        go to 20
      end if
!
      if (clay1.le.0.25) then
        dia(3) = 0.030
        frac(3) = 1.8 * clay1
!
!       If the surface soil has no clay in it, then no small aggregates
!       can exist - set a mimimal fraction
!
        if (frac(3).le.0.0) frac(3) = 0.0001
        go to 20
      end if
!
      if (clay1.lt.0.60) then
        dia(3) = 0.20 * (clay1-0.25) + 0.030
        if (clay1.ge.0.50) go to 10
        frac(3) = 0.45 - 0.6 * (clay1-0.25)
        go to 20
      end if
!
      dia(3) = 0.1
   10 frac(3) = 0.6 * clay1
   20 frac(2) = silt1 - frac(3)
!
      if (frac(2).le.0.0) then
        frac(2) = 0.0001
        frac(3) = silt1 - frac(2)
        if (frac(3).le.0.0) frac(3) = 0.0001
      end if
!
!     Calculate the diameter and fraction of particle class 4
!     large aggregates and make sure that all size classes have
!     at least a small amount of sediment and that no size
!     classes have negative fractions.
!
      frac(4) = 1.0 - frac(1) - frac(2) -                               &
     &    frac(3) - frac(5)
!
      if (frac(4).le.0.0) then
        crct = 1.0 / (1.0+abs(frac(4))+0.0001)
        frac(4) = 0.0001
        do 30 k = 1, npart
          frac(k) = frac(k) * crct
   30   continue
      end if
!
!
!     Determine the composition of each of the particle classes
!     in terms of primary clay, silt, sand, and organic matter.
!
!     Particle class 1 - primary clay
!
      frcly(1) = 1.0
      frslt(1) = 0.0
      frsnd(1) = 0.0
!
!     If there is no clay in the soil surface layer,
!     then there can be no organic matter associated with the
!     primary clay fraction since it does not exist.
!
      if (clay1.gt.0.0) then
        frorg(1) = frcly(1) * ratiom
      else
        frorg(1) = 0.0
      end if
!
!     Particle class 2 - primary silt
!
      frcly(2) = 0.0
      frslt(2) = 1.0
      frsnd(2) = 0.0
!
!     If there is no clay in the soil surface layer, but there
!     is silt, use the silt to proportion the organic matter to
!     the primary silt class.  If no clay or silt, then no organic
!     matter can be assigned to this class
!
      if (clay1.gt.0.0) then
        frorg(2) = 0.0
      else if (silt1.gt.0.0) then
        frorg(2) = ratiom
      else
        frorg(2) = 0.0
      end if
!
!     Particle class 3 - small aggregate
!
!
!     If no clay or silt in the surface profile then there can
!     be no clay or silt or associated organic matter in the
!     small aggregate class.
!
      if (clay1.gt.0.0.and.silt1.gt.0.0) then
!
        frcly(3) = clay1 / (clay1+silt1)
        frslt(3) = silt1 / (clay1+silt1)
        frorg(3) = frcly(3) * ratiom
      else
        frcly(3) = 0.0
        frslt(3) = 0.0
        frorg(3) = 0.0
      end if
!
      frsnd(3) = 0.0
!
!     Particle class 4 - large aggregate
!
      if (frac(4).gt.0.0001) then
!
        frcly(4) = (clay1-frac(1)-(frcly(3)*                            &
     &      frac(3))) / frac(4)
        if (frcly(4).lt.0.0.or.frcly(4).gt.1.0)                         &
     &      frcly(4) = 0.0
!
        frslt(4) = (silt1-frac(2)-(frslt(3)*                            &
     &      frac(3))) / frac(4)
        if (frslt(4).lt.0.0.or.frslt(4).gt.1.0)                         &
     &      frslt(4) = 0.0
!
        frsnd(4) = (sand1-frac(5)) / frac(4)
!
        if (frsnd(4).lt.0.0.or.frsnd(4).gt.1.0)                         &
     &      frsnd(4) = 0.0
!
        frorg(4) = frcly(4) * ratiom
!
      else
        frcly(4) = 0.0
        frslt(4) = 0.0
        frsnd(4) = 0.0
        frorg(4) = 0.0
      end if
!
!     Particle class 5 - primary sand
!
      frcly(5) = 0.0
      frslt(5) = 0.0
      frsnd(5) = 1.0
!
!     Check to be sure that there is some clay in soil surface
!     layer, or some silt in this layer.  If not, all organic matter
!     must then be associated with the primary sand
!
      if (clay1.gt.0.0.or.silt1.gt.0.0) then
        frorg(5) = 0.0
      else
        frorg(5) = ratiom
      end if
!
!
      frclyt = 0.5 * clay1
      frcly1 = 0.95 * frclyt
!
!     Check to make sure that large aggregate class does
!     not have too little of a fraction of primary clay within
!     it.  If it does - proportion more of the clay into size
!     class 4 (large aggregate) then go back to statement 40
!     and reproportion all of the size classes.
!
      if (clay1.lt.1.0.and.jflag.eq.0) then
!
        if (frcly(4).lt.frcly1) then
          f1f2f5 = frac(1) + frac(2) + frac(5)
          frcly(4) = frclyt
!
!
          frac(3) = (clay1-frcly(4)-frac(1)+                            &
     &        frcly(4)*f1f2f5) / (frcly(3)-frcly(4))
          if (frac(3).le.0.0) frac(3) = 0.0001
          jflag = 1
          go to 20
        end if
      end if
!
!     convert diameter from millimeters to meters
!
      do 40 k = 1, npart
        dia(k) = dia(k) / 1000.0
   40 continue
!
!     Calculate the fall velocities and equivalent sand diameters
!     for each particle class.
!
!      do 50 i = 1, npart
!        fall(i) = falvel(spg(i),dia(i))
!        eqsand(i) = sedia(2.65,fall(i))
!   50 continue
      return
      end
