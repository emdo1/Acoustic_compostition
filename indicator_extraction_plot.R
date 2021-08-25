####Indicator frequency/time detection
##Author: Elise Damstra emd16@ic.ac.uk
##Data format required:
##1: have your acoustic matrix arranged so each site is a row and each column is a time/frequency value
##2: have your metadata in a separate .csv so each row is a site (in same order as acoustic matrix)
library(labdsv)
library(indicspecies)

##load in acoustic soundscape and metadata files
f<-read.csv("acoustics.csv",header=TRUE)
fmeta<-read.csv("metadata.csv",header=TRUE)
##some wrangling
rownames(f)<-f$X
f<-f[,-1]
f[is.na(f)] <- 0
##create a vector of the variable that you are trying to tease apart
fl<-as.factor(fmeta$Landuse)
f[ f<0.1 ] <- 0 ##remove quiet values (sounds that take up less than 10% of the soundscape, you can alter this if it is too high)

## run indicator value calculator
is<-indval(f,fl,numitr = 10000)
##create output matrix ncol = ncols for acoustic matrix, nrow is no. of categories
ind<-matrix(ncol=10028,nrow=5)

colnames(ind)<-colnames(f)
rownames(ind)<-unique(as.vector(fl))
##get pvalues from indval
pv<-(as.vector(is$pval))

##make a dataframe of significant indicator values
iv<-data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("Column","Class","Indicator_Value","pvalue"))))
for (i in 1:length(is$pval)){
  if (is$pval[[i]]<=0.05){
    #print(i)
    class<-is$maxcls[[i]]
    col<-names(is$pval)[i]
    val<-is$indcls[[i]]
    pv<-is$pval[[i]]
    vals<-c(col,class,val,pv)
    iv<-rbind(iv,vals,stringsAsFactors = FALSE)
    
  }
  
}

colnames(iv)<-c("Column","Class","Indicator_Value","pvalue")
##split time and frequency
d1<- data.frame(do.call(rbind, strsplit(as.vector(iv$Column), split = "_")))
colnames(d1)<-c("Frequency","Time")
##wrangling
d1$Time<-gsub("X","",d1$Time)
d1$Frequency<-gsub("X.","",d1$Frequency, fixed=TRUE )

iv<-cbind(iv,d1)
iv$Frequency<-gsub("X","",iv$Frequency)
#iv$Column<-gsub("X."," ",iv$Column,fixed = TRUE)
library(stringr)
##put classes into numerical order
iv <- iv[order(as.numeric(iv$Class)),]
###EDIT save location at the end of this loop
for (i in 1: length(unique(iv$Class))){
  
  cl<-as.numeric(unique(iv$Class)[i])
  print(cl)
  df<-subset(iv,iv$Class==cl)
  
  index<-data.frame(matrix(0L,ncol=length(unique(iv$Time)),
                           nrow=length(unique(iv$Frequency))))
  rownames(index)<-sort(unique(as.numeric(iv$Frequency)))
  colnames(index)<-sort(unique(as.numeric(iv$Time)))
  
  
  for (j in 1:nrow(df)){
    r<-match(df$Frequency[j],rownames(index))
    c<-match(df$Time[j],colnames(index))
    #print(j)
    index[r,c]<-as.numeric(df$Indicator_Value[j])
    
    
  }
  filen<-paste0("Folder_you_want_to_save_into", unique(fl)[i],".csv")
  write.csv(index,filen)
  
}
##This should save each category's indicator matrices into different csv files named (1.csv 2.csv etc)





###INDICATOR PLOTTING

###run this from where you saved the .csv for each category above
rm(list = ls())
setwd("SAVED_FILE_LOCATION")

library(plot.matrix)
##makes a list of your files
f<-list.files(pattern=(".csv"))
##read one in to check
o<-read.csv(f[1])
#turn it upside down so low frequencies are at the bottom
o<-o[order(-o$X),]

rownames(o)<-o$X
o$X<-NULL
om<-as.matrix(o)

p<-as.matrix(o)
##p is an empty matrix which you will fill with the numbers from your csvs so 1,2,3,4 etc are the indicators from landuse types 1,2,3,4
p[which(p != 0)] <- 0

##looping through csv to fill p
for (i in 1: length(f)){
  o<-read.csv(f[i])
  print(i)
  o<-o[order(-o$X),]
  
  rownames(o)<-o$X
  o$X<-NULL
  om<-as.matrix(o)
  om[which(om != 0)] <- i
  p<-p+om
  
  
}

colnames(p)<-gsub("X","",colnames(p))
##creating a colour scale for your plot (customise as much as you want)
colv<-c("midnightblue","white","moccasin","yellow","palegreen3","darkred","slateblue")
##plotting code. breaks should equal number of categories +1 (for background)
plot(p,border=NA,breaks=c(0,1,2,3,4,5,6,7),col=colv,main=" Indicator Frequencies for landuse",xlab="Time",ylab="Frequency (Hz)")

##feel free to customise the plot as much as you want



