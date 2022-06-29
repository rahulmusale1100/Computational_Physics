
      !program :To find the area of unit circle
      
      
      program  hyper
      implicit none
      
      
      integer :: i=1,nc=0,k
      real :: n,x,y,m,area
      
      write(*,*)"give the how many random nuber you want to genrate"
      read(*,*)k
      write(*,*)k
      
    
    
      do i=1,k
         call random_number(x)
         call random_number(y)
       
      
         if(y**2+x**2<=1.0)then
         nc=nc+1
         end if
         end do
         area=4.0*float(nc)/float(k)
         write(*,*)"area under the circle is=",area
      end program
         
