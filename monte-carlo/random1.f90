
      !program :Find the mean by using random number 
      
      
      program  ran
      implicit none
      integer::i=1,m
      real::n,sumx,avrg,sumx0,avrgx2
      real,allocatable :: x(:)
      open(unit=18,file="random1.dat")
      sumx=0.0
      sumx0=0.0
      write(*,*)"number of random no."
      read(*,*)m
      write(*,*)m
      allocate(x(m))
      
      do 
        call random_number(n)
        x(i)=n
         write(18,*)i,x(i)
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
      
      
