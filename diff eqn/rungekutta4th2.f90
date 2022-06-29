	program rungekutta4th2
	implicit none
	real :: t=0.0,dt,n_i,n_iplus1,s1,s2,s3,s4,a,b
	open(unit=11,file="rungekutta4th2op.dat")
	write(*,*)"We have differential equation as dn/dt=a*n-b*n^3"
	write(*,*)"Give initial value of population n(i),time interval dt,constant a & b"
	read(*,*) n_i,dt,a,b
	write(*,16) n_i,dt,a,b
	do
		s1=a*n_i-b*n_i**3
		s2=a*(n_i+s1*dt/2)-b*(n_i+s1*dt/2)**3
		s3=a*(n_i+s2*dt/2)-b*(n_i+s2*dt/2)**3
		s4=a*(n_i+s3*dt)-b*(n_i+s3*dt)**3
		n_iplus1=n_i+dt/6.0*(s1+2*s2+2*s3+s4)
		write(11,*) t,n_i
		n_i=n_iplus1
		t=t+dt
		if(t>10.0)exit
	end do
	16 format(4(f6.3,3x))
	end program
