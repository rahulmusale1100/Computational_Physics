
    ! program -lorenz attractor by euler method
    
    program lorenz
    implicit none 
    integer::i,j,k
    real::x,y,z,t=0,dt,x0,y0,z0,a,b,c
    
    open(unit=11,file="lorenz.out")
    !write(*,*)"enter the initial value at t=0 for a,b,c"
    !read(*,*)a,b,c
    write(*,*)"enter the dt "
    read(*,*)dt
    a=28.00
    b=10.00
    c=8.0/3.0
    x0=sqrt(c*(a-1))+0.4    
    y0=sqrt(c*(a-1))+0.4
    z0=(a-1)+0.5
    
    do i=1,100000
       x=x0+dt*(b*(y0-x0))
       y=y0+dt*((a*x0)-(x0*z0)-y0)
       z=z0+dt*((x0*y0)-((c)*z0))
       write(11,*)t,x,y,z
       t=t+dt
       x0=x
       y0=y
       z0=z
    enddo
    
    end program
    
    
