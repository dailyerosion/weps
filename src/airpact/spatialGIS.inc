! Defintion of emit variable
     
      integer hight,width,cell_size
      integer cell_grid
      parameter (hight = 95)
      parameter (width = 95)
      parameter (cell_grid = 12)

      integer soilDim(hight,width,cell_grid,cell_grid)
      integer landcov(hight,width,cell_grid,cell_grid)
      integer rdx(cell_grid,cell_grid)
!      real  erstotal(59,108)
!      real  erssp(59,108)
!      real  erspm10(59,108)   
!  soilDim - soil cover dimension
! landcov - landcov dimension
! rdx - regional index which will replace isr for regional run of WEPS
      
      common /spatialGIS/soilDim,landcov,rdx
      save :: /spatialGIS/
 
!   soilDim - soil spatial dimenion
  
