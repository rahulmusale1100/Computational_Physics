
      !program :To find the area of unit hypershere
      
      
      program hyper
      implicit none
      
      
      integer :: i,nc=0,k,j,l
      real :: n,x,y,m,vol,rad,vol2,pi=3.1426
      
      write(*,*)"give the how many random nuber you want to genrate"
      read(*,*)k
      write(*,*)k
      
      write(*,*)"Dimension of hypersphere"
      read(*,*)l
      write(*,*)l
    
      do i=1,k
      
          rad=0.0
        do j=1,l
         call random_number(x)
        rad=rad+x*x
       enddo
      
         if(rad<=1.0)then
         nc=nc+1
         end if
         end do
         vol=(2.0**float(l))*float(nc)/float(k)
         vol2=(pi**(l/2.0))/(5*4*3*2)
         write(*,*)"area under the circle is=",vol,vol2
      end program
         
