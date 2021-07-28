##script for data set up for PCOA and PCOA code for acoustic composition analysis
##set directory of where all previously made summary soundscapes are saved
library(dplyr)
library(reshape2)
setwd("E:/soundscapes")

mats<-list.files(pattern = "soundscape.*?\\.csv$")
t<-data.frame(matrix(nrow=20160))###nrow should be no.of cols mulitplied by no. of rows in each site's matrix 
f<-as.matrix(read.csv(paste(mats[1])),row.names=1)
###for some reason I imported one matrix initially to set up the output matrix more easily
f2<-f[,-1]##removes first row
rownames(f2)<-f[,1]#makes the first column into the rownames
g<-melt(f2)
colnames(g)<-c("frequency","minute","amplitude")
t$frequency<-g$frequency
t$minute<-g$minute
t$matrix.nrow...20160.<-NULL###empties the matrix before importing all the data (change to same number as nrow in row 8)
for (i in 1: length(mats)){
  f<-as.matrix(read.csv(paste(mats[i])),row.names=1)
  tr<-paste0(mats[i])
  f2<-f[,-1]
  rownames(f2)<-f[,1]
  g<-melt(f2)
  colnames(g)<-c("frequency","minute",paste0(tr))
  print(dim(f))
  t<-merge(t,g,by=c("frequency","minute"),all=TRUE)
  
  
}
t$minute<-as.character(t$minute)
for (i in 1:nrow(t)){
  if (str_sub(t$minute[i],-1) != "1" && str_sub(t$minute[i],-1) != "6"){
    #print(t$minute[i])
    t <- t[-i, ]
  }
  if (t$minute[i] =="X.1"){
    t <- t[-i, ]
  }
}


##save the big matrix

write.csv(t,"all_sscapes_matrix2.csv")




######****PCOA******######
##once all data have been collated into a single matrix, start pcoa set up

library(vegan)
library(ape)
library(reshape2)
library(dplyr)
library(ggplot2)
##insert name of full matrix
t<-read.csv("cleanssday.csv",header=TRUE):
  t<-t(t)
t<-t[-1,]
cnames<-paste0(t[1,],"_",t[2,])

colnames(t) = cnames # the first row will be the header
t <- t[-c(1,2), ]          
t[is.na(t)] <- 0
class(t) <- "numeric"
t<-t[apply(t[,-1], 1, function(x) !all(x==0)),]



##distance matrix (pcoa)
dm<-vegdist(t,method = "bray")

cs<-cmdscale(dm,k=5,eig=T)##play with k
##pcoa, you can change the correction if you wish, I used lingoes correction 
#### because after some reading I thought it was better, but it made little difference to the final result
pc<-pcoa(dm, correction="lingoes", rn=NULL)
####

round(cs$eig*100/sum(cs$eig),1)

##you need to make a metadata csv with each site as a row and columns for environmental variables of interest 
##(make sure the order is the same as the order of the matrices )
meta<-read.csv("all_metadat2.csv",header = TRUE,row.names=1)

meta<-na.omit(meta)
meta<-meta[rownames(t),]

a<-cbind(meta,cs$points)

##play with a (cs$points should give you the vector of pcoa outputs)
