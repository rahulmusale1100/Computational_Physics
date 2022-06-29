	
	! PROGRAM NAME : Gambler ruin problem using Monte Carlo method
	
	
	program gambler
	implicit none
	integer :: allpaisa,mypaisa,uwon=0,i,mymoney
	real :: r,prowin,progamewin
	write(*,*)"Welcome to PHYSICS DA CASINO"
	write(*,*)"What is the total investment in the game in rupees ?"
	read(*,*) allpaisa
	write(*,*) allpaisa
	write(*,*)"How much money you will invest ?"
	read(*,*) mypaisa
	write(*,*) mypaisa
	write(*,*)"What probability you want of winning particular game ?"
	read(*,*) prowin
	write(*,*) prowin
	do i=1,10000
		mymoney=mypaisa
		do
		call random_number(r)
		if(r<=prowin)then
			mymoney=mymoney+1
		else
			mymoney=mymoney-1
		end if
		if(mymoney==allpaisa)then
			uwon=uwon+1
			exit
		end if
		if(mymoney==0)exit
		end do
	end do
	progamewin=real(uwon)/10000.0
	write(*,*)"Sir aapke jitane ki probability hai",progamewin
	end program gambler
			