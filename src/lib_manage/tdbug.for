!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine tdbug(sr, slay, output)

!     + + + PURPOSE + + +
!    This program prints out many of the global variables before
!    and after the call to various MANAGEMENT practices

!     + + + KEY WORDS + + +
!     wind, erosion, tillage, soil, crop, decomposition
!     management

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr, slay, output

!     + + + ARGUMENT DEFINITIONS + + +
!     sr      - subregion number
!     slay    - number of soil layers
!     output  - process number for debugging output

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'c1glob.inc'
      include 'd1glob.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'decomp/decomp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'manage/tcrop.inc'

!     + + + LOCAL VARIABLES + + +
      integer idx
      real total

!     + + + LOCAL DEFINITIONS + + +

!     idx     - loop indexing variable
!     total   - summation variable

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTIONS CALLED + + +

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!    29 = debug MANAGement

!     + + + DATA INITIALIZATIONS + + +

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +

!     + + + END SPECIFICATIONS + + +

      select case (output)
      case (1) ! crust breakdown process (process code 01)

      case (2) ! random roughness process (process code 02)
 2067     format('aslrr') 
 2062     format (f7.2)
          write(29,2067)
          write(29,2062) aslrr(sr)

      case (3) ! oriented roughness ridge only process (process code 03)
 2070     format(3x,'aszrgh asxrgw asxrgs asargo asxdks asxdkh')
 2071     format (4x,6(2x,f7.3))
          write(29,2070)
          write(29,2071) aszrgh(sr), asxrgw(sr), asxrgs(sr),            &
     &      asargo(sr), asxdks(sr), asxdkh(sr)

      case (4) ! oriented roughness process dike only (process code 04)
          write(29,2070)
          write(29,2071) aszrgh(sr), asxrgw(sr), asxrgs(sr),            &
     &      asargo(sr), asxdks(sr), asxdkh(sr)

      case (5) ! oriented roughness process (process code 05)
 2072     format(3x,'asfcr  asflos')
 2073     format (1x,2f7.3)
          write(29,2072)
          write(29,2073) asfcr(sr), asflos(sr)
          write(29,2070)
          write(29,2071) aszrgh(sr), asxrgw(sr), asxrgs(sr),            &
     &      asargo(sr), asxdks(sr), asxdkh(sr)

      case (11) ! crushing process (process code 11)
 2040     format(3x,'aslagn aslagx aslagm as0ags') 
 2050     format (1x,4f7.2)
          write(29,2040) 
          do idx = 1,slay
            write(29,2050) aslagn(idx,sr), aslagx(idx,sr),              &
     &        aslagm(idx,sr), as0ags(idx,sr) 
          end do

      case (12) ! loosening process (process code 12)
 2041     format(3x,'asdblk  asdsblk   aszlyt') 
 2051     format (1x,f7.2,2x,f7.2,2x,f7.2)
          write(29,2041) 
          do idx = 1,slay
            write(29,2051) asdblk(idx,sr),asdsblk(idx,sr),aszlyt(idx,sr)
          end do 

      case (13) ! mixing process (process code 13)
 2060     format (1x,i4,1x,f7.2,1x,f7.2,f6.2,4f7.2,f6.2,3f7.2)
 2061     format (4f7.3,f6.2,4f7.2,f6.2,3f7.2)
 2063     format (4x,i1,6(1x,f8.4))
 2065     format (3x,'layer asdblk aszlyt sfsan asfsil asfcla ',        &
     &       'as0ph  ascmg ascna asfcce asfcec asfesp')
 2066     format(3x,'asfom asfnoh asfpoh asfpsp asfsmb asdagd aseags ', &
     &       'ahrwc aheaep ahrwcw ahrwcf ahrwca ahrwcs')
 2068     format(3x,'layer admrtz(1)  admrtz(2)  admrtz(3) ',           &
     &               ' admbgz(1)  admbgz(2)  admbgz(3)') 
          write(29,2065)
          do idx = 1,slay
            write(29,2060) idx, asdblk(idx,sr), aszlyt(idx,sr),         &
     &        asfsan(idx,sr), asfsil(idx,sr), asfcla(idx,sr),           &
     &        as0ph(idx,sr), ascmg(idx,sr), ascna(idx,sr),              &
     &        asfcce(idx,sr), asfcec(idx,sr), asfesp(idx,sr)
          end do 
          write(29,2066)
          do idx = 1,slay
            write(29,2061) asfom(idx,sr), asfnoh(idx,sr),               &
     &        asfpoh(idx,sr), asfpsp(idx,sr), asfsmb(idx,sr),           &
     &        asdagd(idx,sr), aseags(idx,sr), ahrwc(idx,sr),            &
     &        aheaep(idx,sr), ahrwcw(idx,sr), ahrwcf(idx,sr),           &
     &        ahrwca(idx,sr), ahrwcs(idx,sr)
          end do 
          write(29,2068)
          do idx = 1,slay
            write(29,2063)                                              &
     &        idx, admrtz(idx,1,sr), admrtz(idx,2,sr), admrtz(idx,3,sr),&
     &        admbgz(idx,1,sr), admbgz(idx,2,sr), admbgz(idx,3,sr)
          end do 

      case (14) ! inversion process (process code 14)
          do idx = 1,slay
            write(29,2060) idx, asdblk(idx,sr), aszlyt(idx,sr),         &
     &        asfsan(idx,sr), asfsil(idx,sr), asfcla(idx,sr),           &
     &        as0ph(idx,sr), ascmg(idx,sr), ascna(idx,sr),              &
     &        asfcce(idx,sr), asfcec(idx,sr), asfesp(idx,sr)
          end do 
          write(29,2066)
          do idx = 1,slay
            write(29,2061) asfom(idx,sr), asfnoh(idx,sr),               &
     &        asfpoh(idx,sr), asfpsp(idx,sr), asfsmb(idx,sr),           &
     &        asdagd(idx,sr), aseags(idx,sr), ahrwc(idx,sr),            &
     &        aheaep(idx,sr), ahrwcw(idx,sr), ahrwcf(idx,sr),           &
     &        ahrwca(idx,sr),ahrwcs(idx,sr)
          end do 

      case (21) ! below layer compaction (process code 21)

      case (24) ! flatten process variable toughness (process code 24)

      case (25) ! mass bury process variable toughness (process code 25)
 2500     format ('pool stem leaf store rootstore rootfiber (all flat)')
 2501     format ( i2, 5(1x, f7.4) )
          ! sum pools to get total flat mass
          total = atmflatstem(sr) + atmflatleaf(sr) + atmflatstore(sr)  &
     &          + atmflatrootstore(sr) + atmflatrootfiber(sr)
          do idx = 1, mnbpls
            total = total + admflatstem(idx,sr) + admflatleaf(idx,sr)   &
     &            + admflatstore(idx,sr) + admflatrootstore(idx,sr)     &
     &            + admflatrootfiber(idx,sr)
          end do 
          write(29,*) total, ' total flat mass'
          write(29,2500)
          write(29,2501) 0, atmflatstem(sr), atmflatleaf(sr),           &
     &      atmflatstore(sr), atmflatrootstore(sr), atmflatrootfiber(sr)
          do idx = 1, mnbpls
            write(29,2501) idx, admflatstem(idx,sr),                    &
     &        admflatleaf(idx,sr), admflatstore(idx,sr),                &
     &        admflatrootstore(idx,sr), admflatrootfiber(idx,sr)
          end do 

      case (26) ! re-surface process variable toughness (process code 26)
          ! sum pools to get total flat mass
          total = atmflatstem(sr) + atmflatleaf(sr) + atmflatstore(sr)  &
     &          + atmflatrootstore(sr) + atmflatrootfiber(sr)
          do idx = 1, mnbpls
            total = total + admflatstem(idx,sr) + admflatleaf(idx,sr)   &
     &            + admflatstore(idx,sr) + admflatrootstore(idx,sr)     &
     &            + admflatrootfiber(idx,sr)
          end do 
          write(29,*) total, ' total flat mass'
          write(29,2500)
          write(29,2501) 0, atmflatstem(sr), atmflatleaf(sr),           &
     &      atmflatstore(sr), atmflatrootstore(sr), atmflatrootfiber(sr)
          do idx = 1, mnbpls
            write(29,2501) idx, admflatstem(idx,sr),                    &
     &        admflatleaf(idx,sr), admflatstore(idx,sr),                &
     &        admflatrootstore(idx,sr), admflatrootfiber(idx,sr)
          end do 
!     &       atmflatstem(sr), atmflatleaf(sr), atmflatstore(sr),        &
!     &       atmflatrootstore(sr), atmflatrootfiber(sr),                &
!     &       atmbgstemz(1,sr), atmbgleafz(1,sr), atmbgstorez(1,sr),     &
!     &       atmbgrootstorez(1,sr), atmbgrootfiberz(1,sr),              &
!     &       admflatstem(1,sr), admflatleaf(1,sr), admflatstore(1,sr),  &
!     &       admflatrootstore(1,sr), admflatrootfiber(1,sr),            &
!     &       admbgstemz(1,1,sr), admbgleafz, admbgstorez(1,1,sr),       &
!     &       admbgrootstorez(1,1,sr), admbgrootfiberz(1,1,sr),          &

      case (31) ! killing process (process code 31)

      case (32) ! cutting to height process (process code 32)

      case (33) ! cutting by fraction process (process code 33)

      case (34) ! modify standing fall rate process variable toughness (process code 34)
 2074     format(3x,'admf(1) admf(2) admf(3) admst(1)',                 &
     &      ' admst(2) admst(3)')
 2075     format (6(2x,f7.3))
          write(29,2068)
          do idx = 1,slay
            write(29,2063) idx, admrtz(idx,1,sr), admrtz(idx,2,sr),     &
     &        admrtz(idx,3,sr), admbgz(idx,1,sr), admbgz(idx,2,sr),     &
     &        admbgz(idx,3,sr)
          end do 
          write(29,2074)
          write(29,2075) admf(1,sr), admf(2,sr), admf(3,sr),            &
     &      admst(1,sr), admst(2,sr), admst(3,sr)

      case (37) ! thinning to population process (process code 37)

      case (38) ! thinning by fraction process (process code 38)

      case (40) ! crop to biomass transfer process (process code 40)

      case (50) ! residue initialization process (process code 50)

      case (51) ! planting process (process code 51)

      case (61) ! biomass remove process (process code 61)
 2164     format (3x,3f7.3)
 2169     format(4x,'acmyld  aczht  aczrtd')
 2269     format(4x,'adfscv  adffcv ')
          write(29,2169)
          write(29,2164) acmstandstore(sr), aczht(sr), aczrtd(sr)
          write(29,2269)
          do idx = 1, mnbpls
            write(29,2073) adfscv(idx,sr), adffcv(idx,sr)
          end do 

      case (62) ! biomass remove pool process (process code 62)
 6200     format ( a2, 9(1x, f7.4) )
 6201     format ( i2, 9(1x, f7.4) )
          write(29,*) 'pool stand(height stem leaf store)',             &
     &                'flat(stem leaf store rootstore rootfiber)' 
          write(29,6200) 'T', atzht(sr), atmstandstem(sr),              &
     &        atmstandleaf(sr), atmstandstore(sr),                      &
     &        atmflatstem(sr), atmflatleaf(sr),                         &
     &      atmflatstore(sr), atmflatrootstore(sr), atmflatrootfiber(sr)
          do idx = 1, mnbpls
            write(29,6201) idx, adzht(idx,sr), admstandstem(idx,sr),    &
     &        admstandleaf(idx,sr), admstandstore(idx,sr),              &
     &        admflatstem(idx,sr), admflatleaf(idx,sr),                 &
     &        admflatstore(idx,sr), admflatrootstore(idx,sr),           &
     &        admflatrootfiber(idx,sr)
          end do 

      case (65) ! add residue process (process code 65)

      case (71) ! irrigate process (process code 71) (OBSOLETE)

      case (72) ! irrigation monitoring process (process code 72)

      case (73) ! single event irrigation process (process code 73)

      case default
      end select

      return

      end

