# BIOL 3551: Species distributions in R

# an anonymous function:
foo <- lapply(c(10,20,50),function(x){
  fnm <- paste('Histogram, random sample, SD =',x,'.jpg',sep='')
  dat <- rnorm(200,200,x)
  jpeg(fnm,width=4,height=3,units='in',res=200)
  hist(dat,main=paste('Mean = 200, SD =',x,', n = 200',sep=''),xlab='Value')
  dev.off()
})

# a named function:
# Function to install packages
check.install.packages <- function(list.packages){
  foo <- lapply(list.packages, function(req.lib){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib, character.only = TRUE)
  })
}

# plotting data:
jpeg('Dartford_Warbler_distribution.jpg',height=6,width=4.5,units='in',res=200)
par(las=1)
plot(SD.data$lon,SD.data$lat,pch=19,cex=0.7,col='grey90',xlab='Longitude',ylab='Latitude')
pres <- subset(SD.data, presence == 1)
points(pres$lon,pres$lat,pch=19,cex=0.7,col='darkgreen')
dev.off()

# plotting bioclimatic data:
map.data <- function(dat, varnm, period){
  # create a file name for output
  filenm <- paste(period,'_',varnm,'.jpg',sep='')  
  # set up output and plotting parameters
  jpeg(filenm,height=6,width=9,units='in',res=200)
  par(mfrow=c(1,2),las=1)  
  # identify variable roles
  x <- as.numeric(dat[,1]); y <- as.numeric(dat[,2]); z <- as.numeric(dat[,3])  
  # find range of z variable to map z values to colours
  lo <- min(z); hi <- max(z); range <- hi-lo  
  # map each z value to a number between 1 and 255
  cols <- 1 + 254 * (z - lo)/range  
  # create the plot
  plot(x,y,
       pch=19,cex=0.6,
       col=colorRampPalette(c("blue", "yellow", "red"))(255)[cols],
       xlab='Longitude',ylab='Latitude')  
  # create a key
  plot(c(0,1),c(0,1),type='n',bty='n',axes=F,xlab='',ylab='') # a dummy plot to set up the boundaries within which we must produce the key
  for (i in 1:255) lines(c(0,0.3),rep(1-i/255,2),
                         col=colorRampPalette(c("blue", "yellow", "red"))(255)[i],lwd=2)
  mtext(varnm,at=0.15)
  for (i in c(1,255)){
    if (i == 1) val <- lo else val <- hi
    text(0.33,1-i/255,format(round(val,3),nsmall=3),pos=4)
  }
  dev.off()
}

# mapping more data:
period <- '1961-1990'
foo <- lapply(4:6, function(C){
  focal.data <- Biocli[,c(2,3,C)]
  focal.name <- toupper(names(Biocli[C]))
  map.data(focal.data,focal.name,period)
})

# calling the revised map.data function:
period <- c('1961-1990','2080')
foo <- lapply(period, function(P){
  foo <- lapply(4:6, function(C){
    if (P == '2080') focal.data <- Future.clim[,c(2,3,C)] else focal.data <- Biocli[,c(2,3,C)]
    focal.name <- toupper(names(focal.data[3]))
    lo <- min(Biocli[,C],Future.clim[,C])
    hi <- max(Biocli[,C],Future.clim[,C])
    map.data(focal.data,focal.name,P,lo,hi)
  })
})

