
      !program :shooting 
      
      program shooting
      implicit none
      
      integer::i,j
      real::a=0.0,b=1.57,alpha=0.0,beta=0.0,wa,wb,x1,w1,w3,y3,y1,dx=0.001,y4,w4,w,w2,y2
      
      open(unit=16,file="shoot1.data")
     
      write(*,*)"Differntial equation is given by y''+y=x(1-x),y(0)=0 & y(1.57)=0"
      write(*,*)"enter the initial guess slope at initial  point wa,wb"
      read(*,*)wa,wb
      write(*,*)wa,wb
      
      x1=a;y1=alpha;w1=wa;y3=alpha;w3=wb
    
     
      do 
        
         if(x1>b)exit
         
         y2=y1+dx*w1
         w2=w1+dx*(-y1+x1*(1.0-x1))
         y1=y2
         w1=w2
         
         y4=y3+dx*w3
         w4=w3+dx*(-y3+x1*(1.0-x1))
         y3=y4
         w3=w4
         x1=x1+dx
         
      enddo
      
      w=wa+(beta-y1)*(wa-wb)/(y1-y3)
      write(*,*)"enter at initial value using interpolation is w= ",w
      x1=a;y1=alpha;w1=w
      
      do 
       if(x1>b)exit
       write(16,*)x1,y1
       y2=y1+dx*w1
       w2=w1+dx*(x1-x1**2-y1)
       y1=y2
       w1=w2
       x1=x1+dx  
      enddo
      end program  
      
      
      
      
      
      
      
      
        
