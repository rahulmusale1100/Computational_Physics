
    ! prgram = differtial equation of poppulation growth by rk 2nd order
    
    
    program rk2
    implicit none
    integer::i
    real::t=0,dt,x0,s1,s2,a,x10,t10,x
    
    open(unit=13,file="rk21.data")
    
    write(*,*)"give the initial guess value x0,dt,a"
    read(*,*)x0,dt,a
    write(*,*)x0,dt,a
    do i=1,1000
       s1=a*x0
       t10=t+dt
       x10=x0+s1*dt
       s2=a*x10
       x=x0+dt*(s1+s2)*(0.5)
       write(13,*)t,x
       t=t+dt
       x0=x
   enddo
   
   end program
    
    
