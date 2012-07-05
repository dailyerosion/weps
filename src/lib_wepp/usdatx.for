!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine usdatx( sand, clay, class)
      integer class
      real sand, clay

! Determines the usda textural class from the sand and clay fractions.
! Original code included for reference below, was modified to use
! fractions instead of percent and modified to return a class number,
! also defined below, instead of returning the string shown in the comment
! after the line where class number is set.

      if (clay .gt. 0.40) then
         class = 12 !'c   '
         if (sand .gt. 0.45) class = 10 !'sc  '
         if ((sand+clay) .lt. 0.60) class = 11 !'sic '
      else
         if (clay .gt. 0.27) then
            class = 9 !'sicl'
            if (sand .gt. 0.20) class = 8 !'cl  '
            if (sand .gt. 0.45) then
               class = 7 !'scl '
               if (clay .gt. 0.35) class = 10 !'sc  '
            end if
         else
            if ((sand+clay) .lt. 0.50) class = 5 !'sil '
            if ((sand+clay) .lt. 0.20 .and.clay .lt. 0.12) class = 6 !'si  '
            if ((sand+clay) .ge. 0.50) class = 3 !'sl  '
            if (((sand+clay) .ge. 0.50) .and. ((sand+clay) .lt. 0.72)   &
     &         .and. (clay .gt. 0.7) .and. (sand .lt. 0.52)) class = 4 !'l   '
            if (((sand+clay).ge. 0.72).and.(clay .gt. 0.20)) class = 7 !'scl '
            if ((sand-clay) .gt. 0.70) class = 2 !'ls  '
            if ((sand-0.5*clay) .gt. 0.85) class = 1 !'s   '
         end if
      end if
      return
      end


!      PARTSIZE 4 DETERMINES THE USDA TEXTURAL CLASS FROM THE SAND AND
!      CLAY FRACTIONS
!
!	 Written by J. E. Hook, Univ. of Georgia, March, 1981.
!        Coastal Plain Exp Stn P.O. Box 748  Tifton, GA 31793-0748
!        Internet: jimhook@tifton.cpes.peachnet.edu
!        Voice: (912) 386-3182 Fax: (912) 386-7293     

!      SUBROUTINE USDATX(SAND,CLAY,CLASS)
!      CHARACTER*4 CLASS
!
!      IF (CLAY.GT.40) THEN
!         CLASS='C   '
!         IF (SAND.GT.45) CLASS='SC  '
!         IF ((SAND+CLAY).LT.60) CLASS='SIC '
!      ELSE
!         IF (CLAY.GT.27) THEN
!            CLASS='SICL'
!            IF (SAND.GT.20) CLASS='CL  '
!            IF (SAND.GT.45) THEN
!               CLASS='SCL '
!               IF (CLAY.GT.35) CLASS='SC  '
!            END IF
!         ELSE
!            IF ((SAND+CLAY).LT.50) CLASS='SIL '
!            IF ((SAND+CLAY).LT.20 .AND.CLAY.LT.12)CLASS='SI  '
!            IF ((SAND+CLAY).GE.50) CLASS='SL  '
!            IF (((SAND+CLAY).GE.50) .AND. ((SAND+CLAY).LT.72)
!     +          .AND. (CLAY.GT.7) .AND. (SAND.LT.52)) CLASS='L   '
!            IF (((SAND+CLAY).GE.72) .AND. (CLAY.GT.20)) CLASS='SCL '
!            IF ((SAND-CLAY).GT.70) CLASS='LS  '
!            IF ((SAND-0.5*CLAY).GT.85) CLASS='S   '
!         END IF
!      END IF
!      RETURN
!      END

!     TEXTURE       CODE
!     __________________
!     SAND           1    
!     LOAMY SAND     2    
!     SANDY LOAM     3    
!     LOAM           4    
!     SILT LOAM      5    
!     SILT           6    
!     S. CLAY LOAM   7    
!     CLAY LOAM      8    
!     SL. CLAY LOAM  9    
!     SANDY CLAY     10    
!     SILTY CLAY     11    
!     CLAY           12    
