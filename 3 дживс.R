hg<-function(ff, f, a, eps, h, max){
  ex=c(1,0)
  ey=c(0,1)
  x <-seq(-10,10,by=1)  
  y <-seq(-10,10,by=1)   
  n <- 1
  z <- outer(x,y,ff)  
  #r <- exp(-sqrt(z))*cos(3*z)
  par(mfcol=c(2,1))
  persp(x,y,z)
  contour(x,y,z,nlevels=20)
  points(a[1],a[2],col='black',pch=20,cex=2)
  a1 <- a
  while(h>eps){
    if((f(a1+h*ex) < f(a1) & max==F)|(f(a1+h*ex) > f(a1) & max==T)){
      a1 <- a+h*ex
      n <- n+1
    } 
    if((f(a1-h*ex) < f(a1) & max==F)|(f(a1-h*ex) > f(a1) & max==T)){
      a1 <- a-h*ex
      n <- n+1
    }
    if((f(a1+h*ey) < f(a1) & max==F)|(f(a1+h*ey) > f(a1) & max==T)){
      a1 <- a+h*ey
      n <- n+1
    } 
    if((f(a1-h*ey) < f(a1) & max==F)|(f(a1-h*ey) > f(a1) & max==T)){
      a1 <- a-h*ey
      n <- n+1
    } 
  
    if(any(a1!=a)){
      segments(a[1],a[2],a1[1],a1[2])
      points(a1[1],a1[2],col='brown', pch=20)
      while((f(2*a1-a) < f(a1) & max==F)|(f(2*a1-a) > f(a1) & max==T)){
        a <- a1     
        a1 <- 2*a1-a
        n <- n+1
        segments(a[1],a[2],a1[1],a1[2])
        points(a1[1],a1[2],col='brown', pch=20)
      }
    }else{
      h <- h/2
    }
    a <- a1
    points(a[1],a[2],col='brown',pch=20)
  }  
  ax <- a[1]  
  ay <- a[2]
  maxmin <- ff(ax, ay)
  if (max==T){
    return(paste0("Точки максимума: ", ax, " и ", ay, ". Максимум: ", maxmin, ". Кол-во шагов - ", n, "."))
  }else{
    return(paste0("Точки минимума: ", ax, " и ", ay, ". Минимум: ", maxmin, ". Кол-во шагов - ", n, "."))
  }
}

hg(function(x,y){-x^2-y^2+10}, function(a){-a[1]^2-a[2]^2+10}, c(-10,-5), 0.01, 11, max=T)
