
	
	program isingbasic
	implicit none
	integer :: nsize,spin(0:100,0:100),i,j,spinsum,i2,i3,deltaspinsum1,deltaspinsum2,t,tfinal
	real :: jj,beta,r1,initialenergy,r2,r3,r4,deltaenergy1,deltaenergy2,finalenergy
	open(unit=11,file="plot5003.dat")
	
	jj=1.0; beta=0.3; nsize=50; tfinal=200
	
	!_________________Random filling of sites with up or down spins___________________________
	
	do i=1,nsize
		do j=1,nsize
			call random_number(r1)
			if(r1<0.65)then
				spin(i,j)=1
			else
				spin(i,j)=-1
			end if
		end do
	end do
	
	!______________________________lattice rolling_________________________________________________
	
	spin(:,0)=spin(:,nsize)
	spin(:,nsize+1)=spin(:,1)
	spin(0,:)=spin(nsize,:)
	spin(nsize+1,:)=spin(1,:)
	
	!___________________________Initial energy_____________________________________________________
	
	spinsum=0
	do i=1,nsize
		do j=1,nsize
			spinsum=spinsum+spin(i,j)*(spin(i,j-1)+spin(i,j+1)+spin(i-1,j)+spin(i+1,j))
		end do
	end do
	initialenergy=-(jj/2.0)*spinsum
	write(*,*)"initialenergy=",initialenergy
	
	
	!_____________________________Monte Carlo_______________________________________________________
	
	do t=0,tfinal
		do i=1,nsize*nsize
			call random_number(r2)
					i2=int(r2*nsize)+1
			call random_number(r3)
					i3=int(r3*nsize)+1	

			spin(:,0)=spin(:,nsize)
			spin(:,nsize+1)=spin(:,1)
			spin(0,:)=spin(nsize,:)
			spin(nsize+1,:)=spin(1,:)
			
			deltaspinsum1=2*(spin(i2,i3)*(spin(i2-1,i3)+spin(i2+1,i3)+spin(i2,i3-1)+spin(i2,i3+1)))
			deltaenergy1=-(jj/2.0)*deltaspinsum1
			
			spin(i2,i3)=-spin(i2,i3)
			
			deltaspinsum2=2*(spin(i2,i3)*(spin(i2-1,i3)+spin(i2+1,i3)+spin(i2,i3-1)+spin(i2,i3+1)))
			deltaenergy2=-(jj/2.0)*deltaspinsum2
			
			finalenergy=initialenergy-deltaenergy1+deltaenergy2
			
			if(finalenergy<initialenergy)then
				initialenergy=finalenergy
				cycle
			else
				call random_number(r4)
				if(r4<exp(-(finalenergy-initialenergy)*beta))then
					initialenergy=finalenergy				
					cycle
				else
					spin(i2,i3)=-spin(i2,i3)	
				end if
			end if
		end do
	end do
	do i=1,nsize
		do j=1,nsize
			if(spin(i,j)==1)then
				write(11,*) i,j
			end if
		end do
	end do
	!________________________________________________________________________________________________
	
	end program isingbasic
	