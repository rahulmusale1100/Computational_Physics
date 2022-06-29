
      !program :To find the area of f(x)=x**2
      
      
      program  parabolaAREA
      implicit none
      
      
      integer :: i=1,nc=0,k
      real :: n,x,y,m,area
      
      write(*,*)"give the how many random nuber you want to genrate"
      read(*,*)k
      write(*,*)k
      
    
    
      do i=1,k
         call random_number(x)
         call random_number(y)
       
      
         if(y<=x*x)then
         nc=nc+1
         end if
         end do
         area=2.0*float(nc)/float(k)
         write(*,*)"area under the curve y=x**2=",area
      end program
         
