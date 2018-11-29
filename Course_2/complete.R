complete<-function(directory,id=1:332){
  nobs<-c()
  for (f in dir(directory)[id]) {
    air<-read.csv(paste(directory,f,sep='/'))
    nob<-nrow(air[complete.cases(air),])
    nobs<-c(nobs,nob)
  }
  return(data.frame(id,nobs))
}
complete('specdata',1)
complete('specdata',c(2, 4, 8, 10, 12))
complete('specdata',30:25)
complete('specdata',3)
