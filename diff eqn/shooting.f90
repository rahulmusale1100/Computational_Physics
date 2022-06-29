
	program shooting
	implicit none
	real :: a=0.0,b=1.57,alpha=0.0,beta=0.0,wa,w_a,dx=0.001,yb,y_b,x,y1,y2,w1,w2,w,y3,y4,w3,w4
	open(unit=11,file="shootingop.dat")
	write(*,*)"We have diff. eqn. as y''(x)+y(x)=x(1-x),y(0)=0 & y(1.57)=0"
	write(*,*)"Give guess values of slope at initial point wa & w_a"
	read(*,*) wa,w_a
	write(*,*) wa,w_a
	x=a;y1=alpha;w1=wa;y3=alpha;w3=w_a 
	do 
		if(x>b)exit
		
		y2=y1+dx*w1	
		w2=w1+dx*(x-x**2-y1)
		y1=y2;w1=w2
		
		y4=y3+dx*w3	
		w4=w3+dx*(x-x**2-y3)
		y3=y4;w3=w4
		
		x=x+dx
	end do
	yb=y1
	y_b=y3
	w=wa+(beta-yb)*(wa-w_a)/(yb-y_b)
	write(*,*)"slope at initial value using shooting method is=",w
	x=a;y1=alpha;w1=w
	do 
		if(x>b)exit
		write(11,*) x,y1
		y2=y1+dx*w1
		w2=w1+dx*(x-x**2-y1)
		y1=y2;w1=w2
		x=x+dx
	end do
	end program
	
	
