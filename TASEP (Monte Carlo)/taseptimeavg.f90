
	! Program name : Time average of current & density for TASEP using Monte carlo method

	program taseptimeavg
	implicit none
	integer :: i,j,k,t,i1,i2,nsite,nparticle,particlecount,time,nexp
	real :: alpha,beta,r1,r2,r3,r4
	integer,allocatable :: lattice(:),occupationcount(:),current(:),currentcount(:)
	real,allocatable :: occupationprob(:),currentprob(:)
	write(*,*)"Please give the values of no. of sites,no. particles,entry rate and exit rate, no. of exp."
	read(*,*) nsite,nparticle,alpha,beta,nexp
	write(*,*) nsite,nparticle,alpha,beta,nexp
	open(unit=11,file="taseptimeavg1.dat")
	allocate(lattice(nsite),occupationcount(nsite),occupationprob(nsite),current(nsite),currentcount(nsite),currentprob(nsite))
	time=int(nsite*1000/(min(alpha,beta)))
	
	!	* Initialization of lattice *
	
	do i=1,nsite
		lattice(i)=0
		occupationcount(i)=0
		current(i)=0
		currentcount(i)=0
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
	
	do j=1,nexp
	
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
						current(i2)=1
					else
						current(i2)=0
					end if
				else
					if(lattice(i2+1)==0)then
						lattice(i2)=0
						lattice(i2+1)=1
						current(i2)=1
					else 
						current(i2)=0
					end if
				end if
			else
				current(i2)=0
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
		
		!--------------------------------------
		if(j==1)then
			time=time/100
		end if
		do i=1,nsite
			occupationcount(i)=occupationcount(i)+lattice(i)
			currentcount(i)=currentcount(i)+current(i)
		end do
		!write(*,*) j
	end do
	
	do i=1,nsite
		occupationprob(i)=real(occupationcount(i))/real(nexp)
		currentprob(i)=real(currentcount(i))/real(nexp)
		write(11,*) i,occupationprob(i),currentprob(i)
	end do
	
	end program taseptimeavg
				
				
