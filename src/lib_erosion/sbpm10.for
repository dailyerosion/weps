!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbpm10         ver 1  3-4-2005
!**********************************************************************
        subroutine sbpm10                                               &
     &  (seags, secr, sfcla, sfsan, awzypt,                             &
     &  canag, cancr, sf10an, sf10en, sf10bk)
!
!     + + + PURPOSE + + +
!     Calculates abrasion coefficients and PM10 fractions in
!       sources of suspended soil
!
!     + + + ARGUMENT DECLARTAIONS + + +   
        real seags, secr, sfcla, sfsan, awzypt
        real canag, cancr,sf10an, sf10en, sf10bk
!
!     + + + ARGUMENT DEFINITIONS + + +
!     seags  = aggreg. stability [Ln(J/Kg)]
!     secr   = crust stabitlity [Ln(J/Kg)]
!     sfcla  = soil surface fraction clay
!     sfsan  = soil surface faction sand
!     awzypt = annual average precipitation (mm)
!     canag  = coefficent of abrasion of aggregates (1/m)
!     cancr  = coefficient of abrasion of crust (1/m)
!     sf10an = fraction pm10 in abraded supension size soil
!     sf10en = fraction pm10 in emitted suspension size soil
!     sf10bk = fraction pm10 in breakage from saltion size soil
!
!     + + + LOCAL VARIABLES + + +
        real ratio, cla, lnc, sfsil, ppt
!
!     + + + LOCAL VARIABLE DEFIINTIONS + + +
!       ratio = silt/clay^2
!       cla   = fraction clay with restricted range
!       lnc   = alog (cla)
!       sfsil = soil fraction silt
!       ppt   = annual average precip restricted to 100-800 mm
!     + + +  END SPECIFICATIONS + + +
!
!       calc. abrasion coefficients
        canag = exp(-2.07-0.077*seags**2.5-0.119*alog(seags))
        if (secr .eq. 0.0) then  ! No crust stability value specified
          cancr = 0.0
          write(0,*) "Warning:  Crust stability value is set to zero"
        else
          cancr = exp(-2.07-0.077*secr**2.5-0.119*alog(secr))
        end if
!
!       calc. pm10 fractions in suspended soil
        sf10an = 0.0116 + 0.00025/(canag+0.001)
!
        sfsil = 1 - sfsan - sfcla
        ratio = sfsil/(sfcla + 0.0001)**2
        ratio = min(300.0, ratio)
        sf10en = 0.0067 + 0.0000487*ratio - 0.0000044*awzypt
!
        cla = min(0.42,max(0.017,sfcla))  !restrict clay range
        lnc = alog(cla)
        ppt = min(800.0,max(100.0,awzypt))    !restrict precip range
        sf10bk = -0.201-(0.52+(0.422+(0.1395+0.0156*lnc)*lnc)*lnc)*lnc  &
     &          + 0.131*exp(-ppt/175.6)
!
        return
        end




