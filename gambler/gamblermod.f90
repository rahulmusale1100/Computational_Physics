	
	program gambler
	implicit none
	integer :: allpaisa,mypaisa,uwon,i,mymoney
	real :: r,prowin,progamewin
	open(unit=11,file="gamblerop1.dat")
	write(*,*)"Welcome to PHYSICS DA CASINO"
	write(*,*)"What is the total investment in the game in rupees ?"
	read(*,*) allpaisa
	write(*,*) allpaisa
	write(*,*)"What probability you want of winning particular game ?"
	read(*,*) prowin
	write(*,*) prowin
	do mypaisa=0,allpaisa,5
		uwon=0
		do i=1,1000
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
		write(11,*) mypaisa,progamewin
	end do
	end program gambler
			