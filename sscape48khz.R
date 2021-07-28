rm(list=ls())
library(seewave)
library(stringr)
library(dplyr)
library(bioacoustics)
library(plot.matrix)
library(tuneR)
library(reshape2)
##this is a rounding function used in the spectrogram code, don't worry about it
mround <- function(x,base){
  base*(floor(x/base))
}
###this function is what creates the spectrogram and summarises how much sound is in each frequency bin
soundscapespec2 <- function(wave, 
                            f,
                            channel = 1,
                            wl = 1024,
                            wn = "hamming", 
                            ovlp = 50,
                            plot = TRUE, 
                            xlab = "Frequency (kHz)",
                            ylim = c(0,1),
                            ...     
)
  
{
  ## INPUT
  input <- inputw(wave=wave,f=f,channel=channel) ; wave <- input$w ; f <- input$f ; rm(input)
  ## WELCH SPEC
  n <- nrow(wave)   ## number of samples in wave
  step <- seq(1, n+1-wl, wl-(ovlp * wl/100))   ## positions of the sliding window for the STFT # +1 added @ 2017-04-20
  n_recs <- length(step) # EPK: Number of records processed
  x <- seq(f/wl, (f/2) - (f/wl), length.out = wl%/%2)/1000 
  #x <- seq(f/wl, (f/2) - (f/wl), length.out =985)/1000 ## frequency values of the spectrum       
  y <- stdft(wave = wave, f = f, wl = wl, zp = 0, step = step, wn = wn, fftw = FALSE, scale = FALSE) ## STFT with the parameters set above
  y <- y^2     # EPK: square of the summed spectrum
  y <- apply(y, MARGIN = 1, FUN = sum)     ## sum of successive spectra
  y <- y / (n_recs*f) # EPK: normalize for number of records and frequency
  dd<-as.data.frame(cbind(x,y))
  dd$hz<-x*1000
  ## frequency bining 
  breaks <- seq(0,f/2,by=200)
  ##create bin column
  dd$bin<-mround(dd$hz,200)
  #freq <- trunc(x)  ## truncate frequency digits to get only 0, 1 etc values
  #freq <- freq[freq!=0]   ## eliminate frequency values between 0 and 1 kHz
  #spec <- y[trunc(x)!=0]  ## eliminate amplitude values between 0 and 1 kHz
  
  ## 1 kHz frequency bins
  #bin  <- unique(freq) 
  bin<-unique(dd$bin)
  #bin.length <- numeric(length(bin))
  bin.length <- numeric(length(bin))
  # for(i in bin) {bin.length[i] <- length(freq[freq==i])}  ## width of each frequency bin
  ## remove small frequency bin at the extreme right of the spectrum
  bin.width <- 200 * wl / f
  bin.out <- which(bin.length < trunc(bin.width))
  #if(length(bin.out)!=0) {bin.new <- bin[-bin.out]} else {bin.new <- bin}
  o<-dd %>% group_by(as.factor(bin)) %>% summarise(s=sum(y))
  ##remove less than 1kz
  #o<-tail(o,-3)
  o<-tail(o,-5)
  ## compute the energy of each frequency bin
  #val <- numeric(length(bin.new))
  #for (i in bin.new) val[i] <- sum(spec[freq==bin.new[i]])
  vlen<-sqrt(sum(o$s^2))
  #vlen <- sqrt(sum(val^2)) # EPK: Compute the vector length
  #val <- val / vlen        # EPK: Normalize to the range [0,1]
  o$s2<-o$s/vlen
  o<-as.data.frame(o)
  ## OUTPUT
  #res <- cbind(frequency = bin.new, amplitude = val, deparse.level=0)
  #if(plot)
  # {
  #  barplot(val, names=as.character(bin.new), xlab=xlab, ylim=ylim, ...)    
  # invisible(res)    
  #}
  return(o)
}

###main code starts here
###lists all the files of interest (run this in your site files, make sure it can find all the days you need)

fi<-list.files(pattern=".WAV", recursive = TRUE)
d<-as.data.frame(fi)

###this takes the 10th to the 13th characters in the file name which is the minute of recording
d$minute<-substr(d$f,10,13)
##organise by minute
u<-sort(unique(d$minute))
####set up output dataframe
#f<-data.frame(freq=seq(1000,23800,by=200))#diurnal

out<-matrix(ncol=length(u),nrow=115)
###this is the loop that works out the average soundscape at each minute
##how it works: it finds all files recorded at the same minute, runs the spectrogram code on each one, 
#### and collects all the outputs in a dataframe (called "am") 
####The minute means are calculated as minmean, which are then collated into the final output table "out"
for(i in 1:length(u)){
  g<-subset(d,d$minute==paste0(u[i]))
  am<-matrix(ncol=nrow(g),nrow=115)
  for (j in 1:nrow(g)){
    s<-readWave(paste0(g$fi[j]))
    a<-soundscapespec2(s,f=48000)
    am[,j]<-a$s2
    
    
  }

  
  minmean<-rowMeans(am)
  #print(paste0(i,"/",length(u)))
  out[,i]<-minmean
 
}

###transforms out into a dataframe 
out2<-as.data.frame(out)
###adds the minute as the column name
colnames(out2)<-u
####this flips the row order so that the bottom row corresponds to the lowest frequency
out2 <- out2[ order(-as.numeric(as.character(rownames(out2)))), ]
##transforms into matrix so you can visualise it
om<-as.matrix(out2)

##plotting the matrix (summary soundscape)
plot(om,border=NA,breaks=seq(from=0,to=0.4,by=0.005))

##saves the matrix in the required format for later analysis
write.csv(om,"path_to folder")