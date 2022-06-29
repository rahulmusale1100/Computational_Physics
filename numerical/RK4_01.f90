
    ! prgram =  diffential equation by rk 4th order 
    
    
    
    program  differential12
    implicit none
    integer :: i
    real::x,x0,dt,t=0,a,s1,s2,s3,s4,x10,x20,x30,t10,t20,t30
    open(unit=15,file="rk41.data")
    
    write(*,*)"give the initial boundry condition dt,x0,a"
    read(*,*)dt,x0,a
    
    do i=1,1000
       s1=(a*x0)
       t10=t+dt*0.5
       x10=x0+s1*dt*0.5
       s2=(a*x10)
       x20=x0+s2*dt*0.5
       s3=(a*x20)
       x30=x0+s3*dt
       t30=t+dt
       s4=(a*x30)
       x=x0+dt*(s1+2*s2+2*s3+s4)/6
       write(15,*)t,x
       t=t+dt
       x0=x
   enddo
   end program
   
    
