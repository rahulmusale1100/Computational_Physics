
	!	PROGRAM : Molecular dynamics
	!___________________________________
	
	program moldy
	implicit none
	integer :: i,j,i1,nsize,nparticle,particle,flag,density
	real :: r1,r2,velocity(10000),avgvelocity,temp,rmsvelocity,force(10000),ekinetic,epotential,etotal
	real :: t,tfinal,dt,rpast(10000),rpresent(10000),rfuture(10000),kenergy(10000),penergy(10000)
	real :: length,dist1,dist2,sigma,dist,eps,tenergy(10000),maxvel,width,start,diff1,diff2,diff
	!_____________Parameters________________
	
	nsize=10; nparticle=3; temp=1.0; dt=0.001; tfinal=100.0; sigma=1.0; eps=10.0
	
	!_______________________________________
	
	length=sigma*real(nsize)
	open(unit=11,file="moldyop1.dat")
	open(unit=12,file="moldyop2.dat")
	open(unit=13,file="moldyop3.dat")
	
	!_____________Define initial position for particles____________
	
	particle=1
	do 
		call random_number(r1)
		i1=int(r1*nsize)+1
		flag=0
		do i=1,particle-1
			if(i1==rpresent(i))then
				flag=1
				exit
			end if
		end do
		if(flag==0)then
			rpresent(particle)=i1
			particle=particle+1
		end if
		if(particle>nparticle)exit		
	end do
	
	!_____________Provide velocity to particles________________
	
	avgvelocity=0.0
	do i=1,nparticle
		call random_number(r2)
		velocity(i)=2*(r2-0.5)
		avgvelocity=avgvelocity+velocity(i)
	end do
	avgvelocity=avgvelocity/real(nparticle)	
	do i=1,nparticle
		velocity(i)=velocity(i)-avgvelocity
	end do
	rmsvelocity=0.0
	do i=1,nparticle
		rmsvelocity=rmsvelocity+(velocity(i)*velocity(i))
	end do
	rmsvelocity=sqrt(rmsvelocity)
	do i=1,nparticle
		velocity(i)=sqrt(temp)*velocity(i)/rmsvelocity
	end do
	
	t=0.0
	do
		ekinetic=0.0
		epotential=0.0
		etotal=0.0
		
		!_____________Calculating force on each particle______________
		
		do i=1,nparticle
			force(i)=0.0
			penergy(i)=0.0
			do j=1,nparticle
				if(i/=j)then
					dist1=abs(rpresent(i)-rpresent(j))
					dist2=abs(length-dist1)
					dist=min(dist1,dist2)
					if(dist==dist1)then
						force(i)=force(i)+4.0*eps/dist*(12.0*(sigma/dist)**12-6.0*(sigma/dist)**6)*(rpresent(i)-rpresent(j))/dist1
					else 
						force(i)=force(i)-4.0*eps/dist*(12.0*(sigma/dist)**12-6.0*(sigma/dist)**6)*(rpresent(i)-rpresent(j))/dist1
					end if
					penergy(i)=penergy(i)+4.0*eps*((sigma/dist)**12-(sigma/dist)**6)
				end if				
			end do
			penergy(i)=penergy(i)/2.0
			kenergy(i)=velocity(i)*velocity(i)/2.0
			tenergy(i)=penergy(i)+kenergy(i)
			epotential=epotential+penergy(i)
			ekinetic=ekinetic+kenergy(i)
			etotal=etotal+tenergy(i)
		end do
		
		write(11,*) t,rpresent(1),velocity(1),rpresent(2),velocity(2),rpresent(3),velocity(3)
		write(12,*) t,epotential,ekinetic,etotal
				
		!________________Evoultion of motion of particle___________________
		
		do i=1,nparticle
			if(t==0.0)then
					rpast(i)=rpresent(i)-velocity(i)*dt+force(i)*dt*dt/2.0
					if(rpast(i)>length)then            
						rpast(i)=rpast(i)-length        
					end if
					if(rpast(i)<0.0)then
						rpast(i)=rpast(i)+length
					end if
			end if
			
			rfuture(i)=2*rpresent(i)+force(i)*dt*dt-rpast(i)	
			
			if(rfuture(i)>length)then            
				rfuture(i)=rfuture(i)-length        
			end if
			if(rfuture(i)<0.0)then
				rfuture(i)=rfuture(i)+length
			end if
			
			diff1=abs(rfuture(i)-rpast(i))
			diff2=abs(length-diff1)
			diff=min(diff1,diff2)
			if(diff==diff1)then
				diff=diff*(rfuture(i)-rpast(i))/diff1
			else
				diff=-diff*(rfuture(i)-rpast(i))/diff1
			end if
			
			velocity(i)=diff/(2*dt)
			
			rpast(i)=rpresent(i)
			rpresent(i)=rfuture(i)
		end do
		
		write(*,*) t
		t=t+dt
		if(t>tfinal)exit
	end do
	
	!_____________________Graph ploting__________________________
	
	maxvel=0.0
	do i=1,nparticle
		if(abs(velocity(i))>maxvel)then
			maxvel=abs(velocity(i))
		end if
	end do
	width=maxvel/10.0
	start=-maxvel

	do 
		density=0
		do i=1,nparticle
			if(velocity(i)>start.and.velocity(i)<(start+width))then 
				density = density +1
			end if 
		end do 
		write(13,*) start+width/2.0,real(density)
		start=start+width
		if(start>maxvel)exit
	end do 	
	
	!______________________________________________________________
	
	end program moldy
	
	
	
	
	
	