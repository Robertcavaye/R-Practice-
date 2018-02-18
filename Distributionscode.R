foo <- lapply(c(10,20,50),function(x){
  fnm <- paste('Histogram, random sample, SD =',x,'.jpg',sep='')
  dat <- rnorm(200,200,x)
  jpeg(fnm,width=4,height=3,units='in',res=200)
  hist(dat,main=paste('Mean = 200, SD =',x,', n = 200',sep=''),xlab='Value')
  dev.off()
})
check.install.packages <- function(list.packages){
  foo <- lapply(list.packages, function(req.lib){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib, character.only = TRUE)
  })
}
list.packages	<- c('PresenceAbsence')
check.install.packages(list.packages)
SD.data <- read.csv("Sylvia_undata_dist(1).csv")
jpeg('Dartford_Warbler_distribution.jpg',height=6,width=4.5,units='in',res=200)
par(las=1)
plot(SD.data$lon,SD.data$lat,pch=19,cex=0.7,col='grey90',xlab='Longitude',ylab='Latitude')
pres <- subset(SD.data, presence == 1)
points(pres$lon,pres$lat,pch=19,cex=0.7,col='darkgreen')
dev.off()
Biocli <- read.csv("Bioclimate_1961-1990(1).csv")
map.data <- function(dat, varnm, period){
filenm <- paste(period,'_',varnm,'.jpg',sep='')  
jpeg(filenm,height=6,width=9,units='in',res=200)
par(mfrow=c(1,2),las=1)  
x <- as.numeric(dat[,1]); y <- as.numeric(dat[,2]); z <- as.numeric(dat[,3])  
lo <- min(z); hi <- max(z); range <- hi-lo  
cols <- 1 + 254 * (z - lo)/range  
plot(x,y,
pch=19,cex=0.6,
col=colorRampPalette(c("blue", "yellow", "red"))(255)[cols],
xlab='Longitude',ylab='Latitude')  
plot(c(0,1),c(0,1),type='n',bty='n',axes=F,xlab='',ylab='')
for (i in 1:255) lines(c(0,0.3),rep(1-i/255,2),
col=colorRampPalette(c("blue", "yellow", "red"))(255)[i],lwd=2)
mtext(varnm,at=0.15)
for (i in c(1,255)){
if (i == 1) val <- lo else val <- hi
text(0.33,1-i/255,format(round(val,3),nsmall=3),pos=4)
  }
dev.off()
}
period <- '1961-1990'
foo <- lapply(4:6, function(C){
  focal.data <- Biocli[,c(2,3,C)]
  focal.name <- toupper(names(Biocli[C]))
  map.data(focal.data,focal.name,period)
})
all.dat	<- merge(Biocli,SD.data)
SDM	<- glm(presence	~	poly(gdd5,3)	+	poly(mtco,3)	+	poly(apet,3),	data=all.dat,	family='binomial')
all.dat$Suitability	<- predict(SDM,	type='response')
map.data(all.dat[,c(2,3,8)],'Suitability','Present day')
eval.data	<- all.dat[,c(1,7,8)]
(AUC	<- round(auc(eval.data,st.dev=FALSE),3))
Future <- read.csv("HadGem A1 2080(1).csv")
Future.clim <- merge(all.dat[,1:3],Future)
map.data(all.dat[,c(2,3,8)],'Suitability','Future 1')
map.data <- function(dat, varnm, period,lo=NA,hi=NA){
  filenm <- paste(period,'_',varnm,'.jpg',sep='')  
  jpeg(filenm,height=6,width=9,units='in',res=200)
  par(mfrow=c(1,2),las=1)  
  x <- as.numeric(dat[,1]); y <- as.numeric(dat[,2]); z <- as.numeric(dat[,3])  
  if(is.na(lo) | is.na(hi)){
    lo <- min(z); hi <- max(z)}
  range<-hi-lo
  cols <- 1 + 254 * (z - lo)/range  
  plot(x,y,
       pch=19,cex=0.6,
       col=colorRampPalette(c("blue", "yellow", "red"))(255)[cols],
       xlab='Longitude',ylab='Latitude')  
  plot(c(0,1),c(0,1),type='n',bty='n',axes=F,xlab='',ylab='')
  for (i in 1:255) lines(c(0,0.3),rep(1-i/255,2),
                         col=colorRampPalette(c("blue", "yellow", "red"))(255)[i],lwd=2)
  mtext(varnm,at=0.15)
  for (i in c(1,255)){
    if (i == 1) val <- lo else val <- hi
    text(0.33,1-i/255,format(round(val,3),nsmall=3),pos=4)
  }
  dev.off()
}
period	<- c('1961-1990','2080')
foo	<- lapply(period,	function(P){
  foo	<- lapply(4:6,	function(C){
    if	(P	==	'2080')	focal.data	<- Future.clim[,c(2,3,C)]	else	focal.data	<- Biocli[,c(2,3,C)]
    focal.name	<- toupper(names(focal.data[3]))
    lo	<- min(Biocli[,C],Future.clim[,C])
    hi	<- max(Biocli[,C],Future.clim[,C])
    map.data(focal.data,focal.name,P,lo,hi)
  })
})
Future.clim$suitability	<- predict(SDM,	newdata=Future.clim,	type='response')
period	<- c('Present_day','2080')
foo	<- lapply(period,	function(P){
  if	(P	==	'2080')	focal.data	<- Future.clim[,c(2,3,7)]	else	focal.data	<- all.dat[,c(2,3,8)]
  map.data(focal.data,'Suitability',P,lo=0,hi=1)
})