      real function falvel(spg,dia)
      implicit none
!
!     function falvel returns the fall velocity for a specific size  
!     class particle given specific gravity (spg), diameter (dia), 
!     kinematic viscosity of the water (kinvis), and acceleration
!     of gravity (accgav)
!
!     module adapted from wepp version 2004.7 and called from the
!     main program
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 10-12-2004
!
!     + + + argument declarations + + +     
!
      real, intent(in) :: spg, dia
!      
!     + + + argument definitions + + +     
!             
!     spg - specific gravity
!     dia - particle size diameter
!
!     + + + local declarations + + + 
!                          
      real kinvis, accgav, rtsid, rey, cdre(9), cdre2(9)
      integer i
!
!     + + + local variable definitions + + + 
!
!     + + + data initializations + + +
!
!     begin function falvel
!
      data cdre /-6.90775, -4.60517, -2.30258, 0.0, 2.30258, 4.60517,   &
     &    6.90775, 9.21034, 11.51292/
      data cdre2 / -4.50986, -1.51413, 0.78846, 3.12676, 6.04025,       &
     &    9.30565, 13.08154, 17.50439, 22.29188/
!
      kinvis = 1.0e-06
      accgav = 9.807
!     
!     dimensionless parameter (rtsid=cd*rey*rey) for drag coeffient
!     
      rtsid = ((spg-1.0)*accgav*(dia**3)/(kinvis**2)) * (8.0/6.0)
!     
!     compute falvel from tabled values for larger particles
!     
      if (rtsid.ge.0.024) then
         rtsid = alog(rtsid)
         do i = 2, 9
            if (cdre2(i).gt.rtsid) then
               rey = exp((rtsid-cdre2(i-1))/(cdre2(i)-cdre2(i-1))*(     &
     &             cdre(i)-cdre(i-1))+cdre(i-1))
               falvel = rey * kinvis / dia
               return
            end if
         end do
!        
!        trap values which are larger than table values
!        
         write (6,*) ' cdre2 out of range.  spg=', spg, '   dia=', dia
!        
         falvel = exp(cdre(9)) * kinvis / dia
!     
!     falvel using stokes solution for spheres for small particles
!     
      else
         falvel = ((dia**2)*(spg-1.0)*(accgav)) / (kinvis*18.0)
      end if
!     
      return
!     
      end
