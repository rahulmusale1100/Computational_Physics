	program euler2
	implicit none
	real :: t=0.0,dt,n_i,n_iplus1,b,a
	open(unit=11,file="euler2op3.dat")
	write(*,*)"We have differential equation as dn/dt=a*n-b*n^3"
	write(*,*)"Give initial value of population n(i),time interval dt,constant a & b"
	read(*,*) n_i,dt,a,b
	write(*,16) n_i,dt,a,b
	do 
		n_iplus1=n_i+dt*(a*n_i-b*n_i**3)
		write(11,*) t,n_i
		n_i=n_iplus1
		t=t+dt
		if(t>10.0)exit
	end do
	16 format(4(f6.3,3x))
	end program

