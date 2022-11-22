f<-function(x,y) {x^2+y^2}
x<-seq(-10,10,by=1);x
y<-seq(-10,10,by=1);y
z<-f(x,y)
z<-outer(x,y,f)
z

par(mfcol=c(1,2))
persp(x,y,z)
persp(x,y,z,col='red')
persp(x,y,z,expand=0.5)
persp(x,y,z,theta=30)
persp(x,y,z,phi=30)
persp(x,y,z,phi=90)
image(x,y,z) #heatmap
contour(x,y,z,nlevels=20) 
a <- c(2,-8)
points(a[1],a[2],col='red',pch=16,cex=2)
range(z)

persp(x,y,z)
par(mfcol=c(1,1))
contour(x,y,z,nlevels=10) 
points(-8,5,col='red',pch=19,cex=2)
points(-6,5,col='blue',pch=19,cex=1)
segments(-8,5,-6,5)

hg<-function(....) {
  1) N Z ex, ey, n=1,
  2) ris -> x,y,z,par(mfcol=c(2,1)),persp,contour
x<-seq(-10,10,by=.1)
y<-seq(-10,10,by=.1)
z<-outer(x,y,f)
par(mfcol=c(2,1),mar=c(1,1,1,1))
persp(x,y,z)
contour(x,y,z) 
  3) a - on contour
a<-c(5,8) #delete - input
points(a[1],a[2],col='red',pch=19,cex=2)
  4) while (condition) {
  1) find direction 
  2) check if new a1
2.1) no new a1 - h/2
2.2) new a1 - 
  a1<-c(5,4) #delete, by formula
  3) find by sample
segments(a[1],a[2],a1[1],a1[2])
points(a1[1],a1[2],col='blue',pch=19) 
a2<-2*a1-a
points(a2[1],a2[2],col='black') 
while (condition) {
  a<-a1
  a1<-a2
  a2<-2*a1-a
  points(a1[1],a1[2],col='black',pch=19) 
  points(a2[1],a2[2],col='black')
  segments(a[1],a[2],a1[1],a1[2])
}
}
}