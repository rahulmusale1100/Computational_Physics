
    ! prgram = differntial equation of pendulum by euler 
    
    
    
    program  differential3
    implicit none
    integer :: i
    real::x,x0,y,y0,dt,t=0,a,b
    open(unit=12,file="euler3.dat")
    
    write(*,*)"give the initial boundry condition dt,a,b,x0"
    read(*,*)dt,a,b,x0
    write(*,*)dt,a,b,x0
    do i=1,10000
      
       y=y0+dt*(-(a/b)*sin(x0))
        x=x0+dt*(y0)
       write(12,*)t,x
       t=t+dt
       x0=x
       y0=y
   enddo
   end program
   
    
