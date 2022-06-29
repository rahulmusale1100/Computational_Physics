
      !program :draw the histohram using random number
      
      
      program  ran2
      implicit none
      integer::i=1,m
      real::n
      real,allocatable :: x(:)
      open(unit=18,file="random1.dat")
     
      write(*,*)"number of random no."
      read(*,*)m
      write(*,*)m
      allocate(x(m))
      
      do 
        call random_number(n)
        x(i)=n
        write(18,*)i,x(i)
        y=int(x(i)*10)
        sumx=sumx+x(i)
        sumx0=sumx0+x(i)*x(i)
        i=i+1
        if(i>m)exit
      enddo
      avrg=sumx/float(m)
      avrgx2=sumx0/float(m)
      write(*,*)"average of random number is given by=",avrg
      write(*,*)"average  square of random number is given by=",avrgx2
      end program
      
      
