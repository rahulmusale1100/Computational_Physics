 
    ! prgram = euler 1st diffential equation
    
    
    
    program  differential1
    implicit none
    integer :: i
    real::x,x0,dt,t=0,a
    open(unit=1,file="euler1.data")
    
    write(*,*)"give the initial boundry condition dt,x0,a"
    read(*,*)dt,x0,a
    write(*,*)dt,x0,a
    do i=1,1000
       x=x0+dt*(a*x0)
       write(1,*)t,x
       t=t+dt
       x0=x
   enddo
   end program
   
    
