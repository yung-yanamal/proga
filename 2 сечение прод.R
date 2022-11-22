ff<-function(x) {sqrt(9-(x-5)^2)}
p.x<-seq(0,12,by=0.01)
p.y<-ff(p.x)

sech <- function(f, p.x, p.y, a, b, eps, step, extr){
  argmin <- 0
  argmax <- 0
  min <- 0
  max <- 0
  n <- 1
  z <- (sqrt(5)+1)/2
  xright<-a+(b-a)/z
  xleft<-b-(b-a)/z 
  par(mfcol=c(ceiling((step + 1)/2),2))
  plot(p.x,p.y,type='l',main=paste('step',0))
  abline(h=0,col='grey')
  abline(v=0,col='grey')
  abline(v=c(a,b,xleft,xright),
         col=c('red','red','green','blue'),
         lwd=c(3,3,2,2))
  
  if (extr==TRUE){
    for (i in 1:step){
      if (f(xright)>f(xleft)){
        plot(p.x,p.y,type='l',main=paste('step',i))
        abline(h=0,col='grey')
        abline(v=0,col='grey')
        a <- xleft
        xleft<-xright 
        xright<-a+(b-a)/z ;
        abline(v=c(a,b,xleft,xright),
               col=c('red','red','green','blue'),
               lwd=c(3,3,2,2))
      }else{
        plot(p.x,p.y,type='l',main=paste('step',i))
        abline(h=0,col='grey')
        abline(v=0,col='grey')
        b <- xright
        xright<-xleft
        xleft<-b-(b-a)/z ;
        abline(v=c(a,b,xleft,xright),
               col=c('red','red','green','blue'),
               lwd=c(3,3,2,2))
      }
    }
    n <- n+step
    
    while (abs(a-b)>eps){
      if (f(xright)>f(xleft)){
        a <- xleft
        xleft<-xright 
        xright<-a+(b-a)/z
        n <- n+1
      }else{
        b <- xright
        xright<-xleft 
        xleft<-b-(b-a)/z
        n <- n+1
      }
    }
  }
  
  
  if (extr==FALSE){
    for (i in 1:step){
      if (f(xright)<f(xleft)){
        plot(p.x,p.y,type='l',main=paste('step',i))
        abline(h=0,col='grey')
        abline(v=0,col='grey')
        a <- xleft
        xleft<-xright 
        xright<-a+(b-a)/z ;
        abline(v=c(a,b,xleft,xright),
               col=c('red','red','green','blue'),
               lwd=c(3,3,2,2))
      }else{
        plot(p.x,p.y,type='l',main=paste('step',i))
        abline(h=0,col='grey')
        abline(v=0,col='grey')
        b <- xright
        xright<-xleft
        xleft<-b-(b-a)/z ;
        abline(v=c(a,b,xleft,xright),
               col=c('red','red','green','blue'),
               lwd=c(3,3,2,2))
      }
    }
    n <- n+step
    
    while (abs(a-b)>eps){
      if (f(xright)<f(xleft)){
        a <- xleft
        xleft<-xright 
        xright<-a+(b-a)/z
        n <- n+1
      }else{
        b <- xright
        xright<-xleft
        xleft<-b-(b-a)/z
        n <- n+1
      }
    }
  }
  
  m <- c(a, b, xleft, xright, (xleft+xright)/2)
  print(m)
  r <- (sapply(m, f))
  print(r)
  if (extr==TRUE){
    max <- max(r)
    argmax <- m[which.max(r)]
    return(list(argmax = argmax, max = max, n = n))
  }
  if (extr==FALSE){
    min <- min(r)
    argmin <- m[which.min(r)]
    return(list(argmin = argmin, min = min, n = n))
  }
}


sech(ff, p.x, p.y, 3, 7, 0.01, 3, FALSE)




