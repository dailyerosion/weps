!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine updres(isr)

!     Update geometric properties of the decomp residue pools

      include 'p1werm.inc'
      include 'p1const.inc'
      include 's1layr.inc'
      include 'd1glob.inc'
      include 'd1gen.inc'
      include 'decomp/decomp.inc'

!     + + + ARGUMENT DECLARATIONS + + +

      integer isr

!     + + + LOCAL VARIABLES + + +

      integer idx
      
!     + + + END SPECIFICATIONS + + +

      ! update derived globals for all decomposition pools
      call poolupdate(                                                  &
     &   admstandstem(1,isr), admstandleaf(1,isr), admstandstore(1,isr),&
     &   admflatstem(1,isr), admflatleaf(1,isr), admflatstore(1,isr),   &
     &   admflatrootstore(1,isr), admflatrootfiber(1,isr),              &
     &   admbgstemz(1,1,isr), admbgleafz(1,1,isr), admbgstorez(1,1,isr),&
     &   admbgrootstorez(1,1,isr), admbgrootfiberz(1,1,isr),            &
     &   adzht(1,isr), addstm(1,isr), adxstmrep(1,isr), adgrainf(1,isr),&
     &   admbgstem(1,isr), admbgleaf(1,isr), admbgstore(1,isr),         &
     &   admbgrootstore(1,isr), admbgrootfiber(1,isr),                  &
     &   adm(1,isr), admst(1,isr), admf(1,isr), admrt(1,isr),           &
     &     admrtz(1,1,isr), admbg(1,isr), admbgz(1,1,isr),              &
     &   adrsai(1,isr), adrlai(1,isr), adrsaz(1,1,isr), adrlaz(1,1,isr),&
     &   adffcv(1,isr), adfscv(1,isr), adftcv(1,isr), adfcancov(1,isr), &
     &   adrcd(1,isr), addstmtot(1), adrsaitot(1), adrlaitot(1),        &
     &   adrcdtot(1), admtot(1), admtotto4(1), admsttot(1), admftot(1), &
     &   admbgtot(1), admbgtotto4(1), admrttot(1), admrttotto4(1),      &
     &   adffcvtot(1), adfscvtot(1), adftcvtot(1), adftcancov(1),       &
     &   nslay(isr), aszlyd(1,isr), covfact(1,isr), adxstm(1,isr),      &
     &   ad0sla(1,isr), ad0ck(1,isr) )

      return
      end
