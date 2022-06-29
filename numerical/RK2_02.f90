
    ! prgram = differtial equation of poppulation growth then stability by rk 2nd order
    
    
    program rk2
    implicit none
    integer::i
    real::t=0,dt,x0,s1,s2,a,x10,t10,x,b
    
    open(unit=14,file="rk22.data")
    
    write(*,*)"give the initial guess value x0,dt,a,b"
    read(*,*)x0,dt,a,b
    write(*,*)x0,dt,a,b
    do i=1,1000
       s1=a*x0-b*(x0**3)
       t10=t+dt
       x10=x0+s1*dt
       s2=a*x10-b*(x10**3)
       x=x0+dt*(s1+s2)*(0.5)
       write(14,*)t,x
       t=t+dt
       x0=x
   enddo
   
   end program
    
    
