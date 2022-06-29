	program lorentz
	implicit none
	real :: a,b,p,x1,y1,z1,x2,y2,z2,t=0.0,dt
	write(*,*)"We have three coupled eqns as:"
	write(*,*)"dx/dt=a(y-x),dy/dt=(p-z)x-y, dz/dt=xy-bz"
	write(*,*)"Give values for constants a,b,p and time interval dt"
	read(*,*) a,b,p,dt
	write(*,16) a,b,p,dt
	x1=sqrt(b*(p-1))+0.5
	y1=sqrt(b*(p-1))+0.5
	z1=p-1+0.5
	open(unit=11,file="lorentzop.dat")
	do
		write(11,*) t,x1,y1,z1
		x2=x1+a*(y1-x1)*dt
		y2=y1+((p-z1)*x1-y1)*dt
		z2=z1+(x1*y1-b*z1)*dt
		x1=x2;y1=y2;z1=z2
		t=t+dt
		if(t>100.0)exit
	end do
	16 format(4(f6.3,3x))
	end program lorentz
	 
	
