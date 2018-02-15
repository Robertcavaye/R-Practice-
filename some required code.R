
# It's good practice to start your code with a line that clears R's memory:
rm(list=ls())

# 6.  The assignment operator and commenting on your code
ZZ <- seq(1,2,0.01)
# comments are preceded by the hash sign (#), like this one

# 7.  Simple graphics

# The following 3 lines will produce a simple graph
A <- 1:10
B <- A+(1.5*rnorm(10))
plot(A,B)
# The next line specifies more details of the graphic output
plot(A,B,xlab="Values in vector A",ylab="Values in vector B",pch=19,col="red",cex=1.3,bty="l")
# Alternatively
plot(A,B,xlab="Values in vector A",ylab="Values in vector B",type="l", 	lwd=2,lty=2,col="blue",bty="n")
# Experiment with producing graphs of varying appearance. 
# Note that the axes ranges can be set using the xlim and ylim commands (see the help file under ?plot.default).  

# 8.	Simple statistics
# lm is used for a regression where y is a function of x, by writing y ~ x.  e.g.
mod <- lm(B~A)
summary(mod)
# add a best fit line
par(las=1) # I often use this command. It changes a graphical PARameter, so that y-axis labels are vertical, so you don't need to turn your head on its side to read them
plot(A,B,xlab="Values in vector A",ylab="Values in vector B",pch=5,col="red",cex=0.8,bty="l")
abline(mod,lty=2,col="blue")

# 9.	Loops and iteration
Some.text <- c("Hello","world")
for (L in 1:10) print(Some.text)
for (loop.var in seq(0.5,5,0.5)){
  X <- loop.var/2+1
  print(X)
}
# More elegantly:
LV <- seq(0.5,5,0.5)
(X <- LV/2+1)

N <- rep(0,10)		# sets up a vector of 10 zeros that we can change using calculations
N[1] <- 100
for (t in 2:10)	N[t] <- N[t-1]*(1+rnorm(1)/10)
plot(1:10,N,type="l")

# 10.	From vectors to matrices and data frames
my.matrix <- matrix(nrow=5,ncol=3,0)

my.matrix <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)

DF <- read.csv("limpet.csv",header=TRUE)

write.table(DF,"An_output_file.csv",row.names=F,sep=",")

my.matrix[1,3]
my.matrix[3,2]
my.matrix[2,]
my.matrix[,2]
my.matrix[1:2,]
my.matrix[3,2:3]



