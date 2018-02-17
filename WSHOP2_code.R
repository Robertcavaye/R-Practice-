rm(list=ls())
Years <- 100
R <- 0.2
N <- c(10,rep(0,Years-1))
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")
R <-0.05
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")
K <- 100
N <- c(10,rep(0,Years-1))
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R*(1 - N[t-1]/K)
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")
R<-3
N <- c(10,rep(0,Years-1))
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R*(1 - N[t-1]/K)
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")
Rs <- seq(1.5,3,0.01)
Years <- 250
K <- 100
stable.sizes <- lapply(Rs,function(R){
  N <- c(10,rep(0,Years-1))			                         
  for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R*(1 - N[t-1]/K)	
  sizes <- N[(Years-99):Years]                            
  return(data.frame(PGR=R,Pop.sizes=sizes))               
})
stable.sizes <- do.call('rbind',stable.sizes)
stable.sizes$colour <- 'red'
stable.sizes$colour[stable.sizes$Pop.sizes < K] <- 'blue'
par(las=1)
plot(stable.sizes$PGR,stable.sizes$Pop.sizes,cex=0.3,pch=19,col=stable.sizes$colour,xlab='Population growth rate',ylab='Stable sizes')
#Next bit of workshop
rm(list=ls())
M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
N <- c(10,10,10)
N <- M%*%N
Years<-15
M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
N <- matrix( c(rep(10,3), rep(0,(Years-1)*3) ), nrow=3)
for (t in 2:Years) N[,t] <- M%*%N[,t-1]
par(las=1)
plot(c(0,Years),c(1,max(N)),xlab="Time",ylab="Number",type="n",log="y",bty="l")
cols=c("black","brown","darkgreen")
for (L in 1:3) lines(1:Years,N[L,],col=cols[L],lwd=3)
labs <- c("Pre-juveniles","Juveniles","Adults")
legend(	x=0,	
        y=3000000,	
        legend=labs,
        lty=1, lwd=3,	
        col=cols, 
        box.lty=0)
lambda.est = sum(N[,Years])/sum(N[,Years-1])
round(lambda.est,2)
SSD.est = N[,Years]/sum(N[,Years])
round(SSD.est,3)
Final <- numeric(3)  

for (s in 1:3) {
  N[,1] <- c(0,0,0)
  N[s,1] = 1
  for (t in 2:Years) N[,t] = M%*%N[,t-1]
  Final[s] = sum(N[,Years])
}
RV.est <- Final/Final[1]
round(RV.est,2)
lambda.true <- eigen(M)$values[1]
( lambda.true <- round(Re(eigen(M)$values[1]),2) )
SSD.true <- eigen(M)$vectors[,1]/sum(eigen(M)$vectors[,1])
( SSD.true <- round(Re(SSD.true),3) )
RV.true <- eigen(t(M))$vectors[,1]
RV.true <- Re(RV.true/RV.true[1])	
round(RV.true,3)
M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
w <- eigen(M)$vectors[,1]/sum(eigen(M)$vectors[,1])
v <- eigen(t(M))$vectors[,1]/eigen(t(M))$vectors[1,1]
s = 3
v.w <- sum(v*w)
sensitivity <- matrix(nrow=s,ncol=s,0)
for (i in 1:s)
  for (j in 1:s)
    sensitivity[i,j] = v[i]*w[j]/v.w
( sensitivity <- round(Re(sensitivity),4) )
Lambda <- eigen(M)$values[1]
elasticity <- sensitivity * M / Lambda
( elasticity <- round(Re(elasticity),4) )