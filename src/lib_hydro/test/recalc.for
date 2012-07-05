!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine recalc
C ***************************************************************** wjr
C reads management file into common blocks
C
C     Edit History
C     06-Feb-99   wjr   created
C
      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1gen.inc'
      include 'd1gen.inc'
      include 'd1glob.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'file.inc'

c     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
c     + + + LOCAL VARIABLES + + +
      integer       ldx
      character     line*256
      integer       isr
      integer		newlay
      real			newthk(mnsz)
      real          xfmarr(mnsz, mnsz)

c     + + + FUNCTION DECLARATIONS + + +
       real   setbds
C
C
      newthk = 0

c     read subregion information
      do isr = 1,nsubr

        write(*,*) 'Enter number of layers '
        read(*,*, err=81) newlay

C if zero there is no transformation
        if (newlay.eq.0) cycle

        write(*,*) 'Enter layer thicknesses '
        read(*,*, err=81) (newthk(ldx), ldx=1,newlay)

        write(*,*) 'Layers & thicknesses ', newlay, newthk

        call makmat(newthk, newlay, aszlyt(1:mnsz,isr), 
     *    nslay(isr), xfmarr)

        write(*,9001) xfmarr
 9001   format('xfmarr ',/,10(10f6.3, ' ',/))

c     read soil physical properties
          write(*,*) ' asfsan ', asfsan(1:mnsz,isr) 
          asfsan(1:mnsz,isr) = matmul(xfmarr,asfsan(1:mnsz,isr))
          write(*,*) ' asfsan ', asfsan(1:mnsz,isr) 
          asfsil(1:mnsz,isr) = matmul(xfmarr,asfsil(1:mnsz,isr))
          asfcla(1:mnsz,isr) = matmul(xfmarr,asfcla(1:mnsz,isr))
          asvroc(1:mnsz,isr) = matmul(xfmarr,asvroc(1:mnsz,isr))
          asfcs(1:mnsz,isr) = matmul(xfmarr,asfcs(1:mnsz,isr))
          asfms(1:mnsz,isr) = matmul(xfmarr,asfms(1:mnsz,isr))
          asffs(1:mnsz,isr) = matmul(xfmarr,asffs(1:mnsz,isr))
          asfvfs(1:mnsz,isr) = matmul(xfmarr,asfvfs(1:mnsz,isr))
          asfwdc(1:mnsz,isr) = matmul(xfmarr,asfwdc(1:mnsz,isr))
          asdblk(1:mnsz,isr) = matmul(xfmarr,asdblk(1:mnsz,isr))
          asdwbd(1:mnsz,isr) = matmul(xfmarr,asdwbd(1:mnsz,isr))
c     aggregate properties
          aslagm(1:mnsz,isr) = matmul(xfmarr,aslagm(1:mnsz,isr))
          as0ags(1:mnsz,isr) = matmul(xfmarr,as0ags(1:mnsz,isr))
          aslagx(1:mnsz,isr) = matmul(xfmarr,aslagx(1:mnsz,isr))
          aslagn(1:mnsz,isr) = matmul(xfmarr,aslagn(1:mnsz,isr))
          asdagd(1:mnsz,isr) = matmul(xfmarr,asdagd(1:mnsz,isr))
          aseags(1:mnsz,isr) = matmul(xfmarr,aseags(1:mnsz,isr))
c     read soil hydrologic properties
          ahrwc(1:mnsz,isr) = matmul(xfmarr,ahrwc(1:mnsz,isr))
          ahrwcs(1:mnsz,isr) = matmul(xfmarr,ahrwcs(1:mnsz,isr))
          ahrwcf(1:mnsz,isr) = matmul(xfmarr,ahrwcf(1:mnsz,isr))
          ahrwcw(1:mnsz,isr) = matmul(xfmarr,ahrwcw(1:mnsz,isr))
          ahrwc1(1:mnsz,isr) = matmul(xfmarr,ahrwc1(1:mnsz,isr))
          ah0cb(1:mnsz,isr) = matmul(xfmarr,ah0cb(1:mnsz,isr))
          aheaep(1:mnsz,isr) = matmul(xfmarr,aheaep(1:mnsz,isr))
          ahrsk(1:mnsz,isr) = matmul(xfmarr,ahrsk(1:mnsz,isr))
c     read soil chemical properties
          asfom(1:mnsz,isr) = matmul(xfmarr,asfom(1:mnsz,isr))
C
C the three vars are set so set settled bulk density
          do l=1,nslay(isr)
            asdsbk(l,isr)=setbds(asfcla(l,isr), asfsan(l,isr),
     *        asfom(l,isr))
          end do

          as0ph(1:mnsz,isr) = matmul(xfmarr,as0ph(1:mnsz,isr))
          asfcce(1:mnsz,isr) = matmul(xfmarr,asfcce(1:mnsz,isr))
c     read other soil chemical properties needed by the CROP
          asfcec(1:mnsz,isr) = matmul(xfmarr,asfcec(1:mnsz,isr))
          asfsmb(1:mnsz,isr) = matmul(xfmarr,asfsmb(1:mnsz,isr))
          as0ec(1:mnsz,isr) = matmul(xfmarr,as0ec(1:mnsz,isr))
          asrsar(1:mnsz,isr) = matmul(xfmarr,asrsar(1:mnsz,isr))
          asftan(1:mnsz,isr) = matmul(xfmarr,asftan(1:mnsz,isr))
          asftap(1:mnsz,isr) = matmul(xfmarr,asftap(1:mnsz,isr))
          admbgz(1:mnsz,1,isr) = matmul(xfmarr,admbgz(1:mnsz,1,isr))
          admrtz(1:mnsz,1,isr) = matmul(xfmarr,admrtz(1:mnsz,1,isr))

C change layer thicknesses
          nslay(isr) = newlay
          aszlyt(1:mnsz,isr) = newthk(1:mnsz)

      end do
C
      return
   81 write(*,9002)
 9002 format(' error reading new layer depths ')
      stop ' stop in recalc '
      end

C *************************************************************************

      subroutine makmat(newthk, newlay, oldthk, oldlay, xfmarr)

      include 'p1werm.inc'

      real     newthk(mnsz)
      integer  newlay
      real     oldthk(mnsz)
      integer  oldlay
      real     xfmarr(mnsz,mnsz)

      integer  idx,jdx
      real     boldthk(mnsz)
      real     bnewthk(mnsz)

	  integer odx, ndx

C initialize matrix
      xfmarr = 0.0
      boldthk = oldthk
      bnewthk = newthk

      odx = 1
      ndx = 1

C create array (matrix) that translates old layers into new


      do while (odx.le.oldlay.and.ndx.le.newlay)
        xfmarr(ndx,odx) = min (bnewthk(ndx), boldthk(odx))
        boldthk(odx) = boldthk(odx) - xfmarr(ndx,odx)
        bnewthk(ndx) = bnewthk(ndx) - xfmarr(ndx,odx)
        if (boldthk(odx).eq.0) odx = odx + 1
        if (bnewthk(ndx).eq.0) ndx = ndx + 1
      end do

C normalize layers to make each row sum to 1
        
      do odx = 1, oldlay
        do ndx = 1, newlay
          xfmarr(ndx,odx) = xfmarr(ndx,odx) / newthk(ndx)
        enddo
      enddo
      end
