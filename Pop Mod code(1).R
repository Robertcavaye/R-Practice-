
# It's good practice to start your code with a line that clears R's memory:
rm(list=ls())

# 2.	Simple population models
Years <- 100				# sets the duration of the simulation
R <- 0.2					  # sets the population’s growth rate
N <- c(10,rep(0,Years-1))			# defines a vector to store the population sizes (an initial 
                              # size of 10, followed by 0 repeated for every other year)
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R	# loops through the remaining years of the simulation, 
                                            # calculating the new population size each time
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")

Years <- 100				# sets the duration of the simulation
R <- 3.0					# sets the population’s maximum growth rate
K <- 100					# sets the carrying capacity
N <- c(10,rep(0,Years-1))			# defines a vector to store the population sizes (an initial 
                              # size of 10, followed by 0 repeated for every other year)
for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R*(1 - N[t-1]/K)	# loops through the remaining years of the
                                                          # simulation, calculating the new 
                                                          # population size each time
plot(1:Years,N, bty= "l", xlab= "Year", ylab= "Population size", type= "l")


# 3.  The effect of growth rate
Rs <- seq(1.5,3,0.01)

Years <- 250				                                      # sets the duration of the simulation
K <- 100					                                        # sets the carrying capacity

stable.sizes <- lapply(Rs,function(R){
  N <- c(10,rep(0,Years-1))			                          # defines a vector to store the population sizes (an initial 
                                                          # size of 10, followed by 0 repeated for every other year)
  for (t in 2:Years) N[t] <- N[t-1]+N[t-1]*R*(1 - N[t-1]/K)	# loops through the remaining years of the
                                                          # simulation, calculating the new 
                                                          # population size each time
  sizes <- N[(Years-99):Years]                            # record only the last 100 years' worth of population sizes
  return(data.frame(PGR=R,Pop.sizes=sizes))               # store those population sizes as a data frame
                                                          # within the "stable.sizes" object; the first column contains the population growth rate
})
stable.sizes <- do.call('rbind',stable.sizes)             # bind all the data frames together as a single object
stable.sizes$colour <- 'red'                              # make another column in the data frame, with the word 'red' in each row
stable.sizes$colour[stable.sizes$Pop.sizes < K] <- 'blue' # change the colour column to be 'blue' for all
                                                          # rows in which the population sizes are less than the carrying capacity

par(las=1)                                                # change the graphical parameter that controls axis tick-mark label orientation
                                                          # and plot the graph:
plot(stable.sizes$PGR,stable.sizes$Pop.sizes,cex=0.3,pch=19,col=stable.sizes$colour,xlab='Population growth rate',ylab='Stable sizes')

  
# 4.	 Matrices, simulations and population characteristics
rm(list=ls())
setwd("C:/My documents/R")	# or whatever represents a good directory for you to work in

M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
N <- c(10,10,10)			# note that this can also be written N <- rep(10,3)
N <- M%*%N			      # NB. Matrix multiplication is sensitive to order, unlike normal 
                      # multiplication (i.e. M%*%N ≠ N%*%M)

# set the duration of the simulation
Years <- 15

# define the transition matrix
M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)
# define a matrix with 3 rows and ‘Years’ columns to store population sizes.  Make the first 3 
# numbers (to fill the first column) 10 (the initial population sizes) and all subsequent numbers 
# zero (because we’ll calculate what those numbers should be in subsequent steps)
N <- matrix( c(rep(10,3), rep(0,(Years-1)*3) ), nrow=3)
# compute population sizes 
for (t in 2:Years) N[,t] <- M%*%N[,t-1]
# plot the outcome
par(las=1)
plot(c(0,Years),c(1,max(N)),xlab="Time",ylab="Number",type="n",log="y",bty="l")
cols=c("black","brown","darkgreen")
for (L in 1:3) lines(1:Years,N[L,],col=cols[L],lwd=3)

# add a legend
labs <- c("Pre-juveniles","Juveniles","Adults")
legend(	x=0,		# an x coordinate for the legend’s top left corner
        y=3000000,	# a y coordinate for the legend’s top left corner
        legend=labs,	# tells R that the text for the legend is in the vector ‘labs’
        lty=1, lwd=3,	# specifies the line characteristics
        col=cols, 	# tells R that the colours for the legend are in the vector ‘cols’
        box.lty=0) 	# suppresses a surrounding box

lambda.est = sum(N[,Years])/sum(N[,Years-1])
round(lambda.est,2)		# reduces the number of decimal places to 2

SSD.est = N[,Years]/sum(N[,Years])
round(SSD.est,3)

Final <- numeric(3)  # this is one way to declare an empty vector of numbers

for (s in 1:3) {
  N[,1] <- c(0,0,0)
  N[s,1] = 1
  for (t in 2:Years) N[,t] = M%*%N[,t-1]
  Final[s] = sum(N[,Years])
}
RV.est <- Final/Final[1]
round(RV.est,2)

# 5.	Population characteristics and matrix properties
lambda.true <- eigen(M)$values[1]
( lambda.true <- round(Re(eigen(M)$values[1]),2) )
SSD.true <- eigen(M)$vectors[,1]/sum(eigen(M)$vectors[,1])
( SSD.true <- round(Re(SSD.true),3) )
RV.true <- eigen(t(M))$vectors[,1]
RV.true <- Re(RV.true/RV.true[1])	# this makes all reproductive values relative to that of the 1st stage class
round(RV.true,3)

# 6.	Sensitivities, elasticities and other uses of matrix models
M <- matrix(c(0,0.024,0,52,0.08,0.25,279.5,0,0.43),nrow=3)	# the matrix
w <- eigen(M)$vectors[,1]/sum(eigen(M)$vectors[,1])		# the SSD
v <- eigen(t(M))$vectors[,1]/eigen(t(M))$vectors[1,1]		# reproductive values
s = 3								# number of life stages
# Summed product of RV and SSD for each stage class:
v.w <- sum(v*w)
# Now calculate sensitivity for each matrix element:
sensitivity <- matrix(nrow=s,ncol=s,0)
for (i in 1:s)
  for (j in 1:s)
    sensitivity[i,j] = v[i]*w[j]/v.w
( sensitivity <- round(Re(sensitivity),4) )
# Elasticities can then be computed by:
Lambda <- eigen(M)$values[1]
elasticity <- sensitivity * M / Lambda
( elasticity <- round(Re(elasticity),4) )

