
	
	program euler1
	implicit none
	real :: t=0.0,dt,n_i,n_iplus1,b
	open(unit=11,file="euler1op1.dat")
	write(*,*)"We have differential equation as dn/dt=b*n"
	write(*,*)"Give initial value of population n(i),time interval dt,constant b"
	read(*,*) n_i,dt,b
	write(*,16) n_i,dt,b
	do 
		n_iplus1=n_i+(dt*n_i*b)
		write(11,*) t,n_i
		n_i=n_iplus1
		t=t+dt
		if(t>10.0)exit
	end do
	16 format(3(f6.3,3x))
	end program


	
