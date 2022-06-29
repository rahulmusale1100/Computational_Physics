
    ! prgram =  diffential equation  poppulation stability by rk 4th order 
    
    
    
    program  differential16
    implicit none
    integer :: i
    real::x,x0,dt,t=0,a,s1,s2,s3,s4,x10,x20,x30,t10,t20,t30,b
    open(unit=15,file="rk42.data")
    
    write(*,*)"give the initial boundry condition dt,x0,a,b"
    read(*,*)dt,x0,a,b
    
    do i=1,1000
       s1=a*x0-b*(x0**3)
       t10=t+dt*0.5
       x10=x0+s1*dt*0.5
       s2=a*x10-b*(x10**3)
       x20=x0+s2*dt*0.5
       s3=a*x20-b*(x20**3)
       x30=x0+s3*dt
       t30=t+dt
       s4=a*x30-b*(x30**3)
       x=x0+dt*(s1+s2+s3+s4)/6
       write(15,*)t,x
       t=t+dt
       x0=x
   enddo
   end program
   
    
