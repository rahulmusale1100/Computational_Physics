	program euler3
	implicit none
	real :: t=0.0,dt=0.0001,x1=acos(-1.0)/6,x2,dx1=0.1,dx2,g=9.8,l=1.0
	open(unit=11,file="euler3op.dat")
	!write(*,*)"We have differential equation as d^2x/dt^2=-g*sinx/l"
	!write(*,*)"Give initial value of angle theta x1,dx1 at x=0,time interval dt,constant g & l"
	!read(*,*) x1,dx1,dt,g,l
	!write(*,16) x1,dx2,dt,g,l
	do 
		x2=x1+dt*(dx1)
		dx2=dx1-dt*(g/l)*sin(x1)
		write(11,*) t,x1
		x1=x2
		dx1=dx2
		t=t+dt
		if(t>10.0)exit
	end do
	16 format(7(f6.3,3x))
	end program

