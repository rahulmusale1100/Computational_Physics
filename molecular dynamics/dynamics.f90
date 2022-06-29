
	program moldyn
	implicit none
	integer ::i,i1,j,n,np,c,particle
	real ::r1,r2,pos(100)
	
	
	n=10;np=5
	
	
	!-------------position--------------
	
	do i=1,np
	   pos(i)=0.0
	enddo
	
	
	particle=1
	
	do i=1,np
	   call random_number(r1)
	   i1=float(int(r1*n))+1.0
	   c=0
	   do j=1,particle-1
	     if(pos(j)==i1)then
	        c=1
	        exit
	     end if
	     
	   enddo
	  if(c==0)then
	     pos(particle)=i1
	    
	  endif
	  if(particle>np)exit   
	   particle=particle+1
	enddo
	
	
	do i=1,np
	   write(*,*)i,pos(i)
	enddo
	end program moldyn
