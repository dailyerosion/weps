!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
! ***************************************************************** LEW
! Lists storage variables used in "save_soil" and "restore_soil"
! subroutines which are used for re-initialization during
! WEPS "calibration" Runs.
!
!     Edit History
!  Aug 21, 2005 - LEW
!

      real             Zszcr(mnsub)
      real             Zsfcr(mnsub)
      real             Zsmlos(mnsub)
      real             Zsflos(mnsub)
      real             Zsdcr(mnsub)
      real             Zsecr(mnsub)
      real             Zsfald(mnsub)
      real             Zsfalw(mnsub)

      real             Zmrslp(mnsub)
      real             ZSFCov(mnsub)

      real             Zrestrict_depth(mnsub)
      real             Zbedrock_depth(mnsub)

      real             Zsdblk(0:mnsz, mnsub)
      real             Zsdsblk(mnsz, mnsub)
      real             Zsdwblk(mnsz, mnsub)
      real             Zsdblk0(mnsz, mnsub)
      real             Zsdpart(mnsz, mnsub)

      real             Zslrr(mnsub)
      real             Zslrro(mnsub)
      real             Zsargo(mnsub)
      real             Zszrgh(mnsub)
      real             Zsxrgs(mnsub)
      real             Zsxrgw(mnsub)
        real             Zszrho(mnsub)
        real             Zsxdks(mnsub)
        real             Zsxdkh(mnsub)
        real             Zs0rrk(mnsub)
        real             Zslrrc(mnsub)

      real             Zsfsan(0:mnsz, mnsub)
      real             Zsfsil(0:mnsz, mnsub)
      real             Zsfcla(0:mnsz, mnsub)
      real             Zsvroc(0:mnsz, mnsub)
      real             Zsfvcs(mnsz, mnsub)
      real             Zsfcs(mnsz, mnsub)
      real             Zsfms(mnsz, mnsub)
      real             Zsffs(mnsz, mnsub)
      real             Zsfvfs(mnsz, mnsub)

      real             Zsfom(0:mnsz, mnsub)
      real             Zs0ph(mnsz, mnsub)
      real             Zsfcce(mnsz, mnsub)
      real             Zsfcec(mnsz, mnsub)
      real             Zsfcle(mnsz, mnsub)

      real             Zsdagd(0:mnsz, mnsub)
      real             Zseags(0:mnsz, mnsub)
      real             Zslagm(0:mnsz, mnsub)
      real             Zslagn(0:mnsz, mnsub)
      real             Zslagx(0:mnsz, mnsub)
      real             Zs0ags(0:mnsz, mnsub)

      real             Zhrwc(mnsz, mnsub)

      real             Zhrwcw(mnsz, mnsub)
      real             Zhrwcf(mnsz, mnsub)
      real             Zhrwcs(mnsz, mnsub)

      real             Zhrwcr(mnsz, mnsub)
      real             Zhrwca(mnsz, mnsub)
      real             Zhrwc1(mnsz,mnsub)
      real             Zhfredsat(mnsz, mnsub)

      real             Zh0cb(mnsz, mnsub)
      real             Zheaep(mnsz, mnsub)
      real             Zhrsk(mnsz, mnsub)

      real             Zszlyt(mnsz+1, mnsub)

      common / soil_save /                                              &
     & Zszcr, Zsfcr, Zsmlos, Zsflos, Zsdcr, Zsecr, Zsfald, Zsfalw,      &
     & Zmrslp, ZSFCov, Zrestrict_depth, Zbedrock_depth,                 &
     & Zsdblk, Zsdsblk, Zsdwblk, Zsdblk0, Zsdpart,                      &
     &                 Zszrgh, Zszrho, Zsxrgw, Zsxrgs, Zsargo, Zsxdks,  &
     &                 Zsxdkh, Zs0rrk, Zslrrc, Zslrr, Zslrro,           &
     &                 Zsfsan, Zsfsil, Zsfcla, Zsvroc,                  &
     &                 Zsfvcs, Zsfcs, Zsfms, Zsffs, Zsfvfs,             &
     &                 Zs0ph, Zsfom, Zsfcce, Zsfcec, Zsfcle,            &
     & Zsdagd, Zseags, Zslagm, Zslagn, Zslagx, Zs0ags,                  &
     & Zhrwc,                                                           &
     &         Zhrwcw, Zhrwcf, Zhrwcs,                                  &
     &         Zhrwcr, Zhrwca, Zhrwc1, Zhfredsat,                       &
     &         Zh0cb, Zheaep, Zhrsk, Zszlyt

      save :: /soil_save/


