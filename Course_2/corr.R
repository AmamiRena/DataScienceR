corr<-function(directory,threshold=0){
  corrvalue<-c()
  for (f in dir(directory)) {
    air<-read.csv(paste(directory,f,sep='/'))
    sorted_air<-air[complete.cases(air),]
    if (nrow(sorted_air)>threshold) {
      corrvalue<-c(corrvalue,cor(sorted_air$sulfate,sorted_air$nitrate))
    }
  }
  return(corrvalue)
}
head(corr('specdata',150))
summary(corr('specdata',150))
head(corr('specdata',400))
summary(corr('specdata',400))
summary(corr('specdata',5000))
length(corr('specdata',5000))
summary(corr('specdata'))
length(corr('specdata'))