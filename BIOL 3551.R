help()
3+5
?mean
A=5
B=3
A+B
(Outcome=A+B)
SampleVector=1:10
(SampleVector=1:100)
(Vector2=c(1,2,5,10,100))
(Vector3=seq(3,25,0.5))
A=seq(0.5,1.5,0.05)
(B=A^3)
plot(A,B)
C <- 1:10
D <- C+(1.5*rnorm(10))
plot(C,D,xlab="Values in vector C",ylab="Values in vector D",pch=19,col="red",cex=1.3,bty="l")
plot(C,D,xlab="Values in vector C",ylab="Values in vector D",type="l", 	lwd=2,lty=2,col="blue",bty="n")
mod<-lm(D~C)
summary(mod)
plot(C,D,xlab="Values in vector C",ylab="Values in vector D",pch=5,col="red",cex=0.8,bty="l")
abline(mod,lty=2,col="blue")
Some.text <- c("Hello", "world")
print(Some.text)
for (L in 1:10) print(Some.text)
for (loop.var in seq(0.5,5,0.5)){
  X<-loop.var/2+1
  print(X)
}
LV<-seq(0.5,5,0.5)
(X<-LV/2+1)
N<-rep(0,10) # sets up a vector of 10 zeros that we can change using calculations
N[1]<-100
for (t in 2:10) N[t]<-N[t-1]*(1+rnorm(1)/10)
plot(1:10,N,type="l")
my.matrix<-matrix(nrow=5,ncol=3,0)
my.matrix<-matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
DF <- read.csv("limpet.csv",header=TRUE)
write.table(DF,"An_output_file.csv",row.names=F,sep=",")
my.matrix[1,3]
my.matrix[3,2]
my.matrix[2,]
my.matrix[,2]
my.matrix[1:2,]
my.matrix[3,2:3]
