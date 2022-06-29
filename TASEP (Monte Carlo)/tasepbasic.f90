

	program tasepbasic
	implicit none
	integer :: i,j,k,t,i1,i2,nsite,nparticle,particlecount,time
	real :: alpha,beta,r1,r2,r3,r4
	integer,allocatable :: lattice(:)
	write(*,*)"Please give the values of no. of sites,no. particles,entry rate and exit rate"
	read(*,*) nsite,nparticle,alpha,beta
	write(*,*) nsite,nparticle,alpha,beta
	allocate(lattice(nsite))
	time=int(nsite*1000/(min(alpha,beta)))
	
	!	* Initialization of lattice *
	
	do i=1,nsite
		lattice(i)=0
	end do
	particlecount=0
	do 
		call random_number(r1)
		i1=int(r1*nsite+1)
		if(lattice(i1)==0)then
			lattice(i1)=1
			particlecount=particlecount+1
		end if
		if(particlecount>=nparticle)exit
	end do
	
	!	* Monte carlo moves *
	
	t=0
	do
		call random_number(r2)
		i2=int(r2*nsite+1)
		if(lattice(i2)==1)then
			if(i2==nsite)then
				call random_number(r3)
				if(r3<beta)then
					lattice(i2)=0
				end if
			else
				if(lattice(i2+1)==0)then
					lattice(i2)=0
					lattice(i2+1)=1
				end if
			end if
		else
			if(i2==1)then
				call random_number(r4)
				if(r4<alpha)then
					lattice(i2)=1
				end if 
			end if
		end if
		t=t+1
		if(t>=time)exit		
	end do
	
	do i=1,nsite
		write(*,*)lattice(i)
	end do
	
	end program tasepbasic
				
				
		
		
	
	