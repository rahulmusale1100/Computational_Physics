
    ! prgram = euler 1st diffential equation poppulation stability
    
    
    
    program  differential2
    implicit none
    integer :: i
    real::x,x0,dt,t=0,a,b
    open(unit=11,file="euler2.dat")
    
    write(*,*)"give the initial boundry condition dt,x0,a,b"
    read(*,*)dt,x0,a,b
    write(*,*)dt,x0,a,b
    
    do i=1,1000
       x=x0+dt*(a*x0-b*(x0**3))
       write(11,*)t,x
       t=t+dt
       x0=x
   enddo
   end program
   
    
