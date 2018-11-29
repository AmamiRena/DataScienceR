pollutantmean<-function(directory,pollutant,id=1:332){
  sumvalue<-0
  rowvalue<-0
  for (f in dir(directory)[id]) {
    air<-read.csv(paste(directory,f,sep='/'))
    if(pollutant=='sulfate'){
      sumv<-sum(air$sulfate[!is.na(air$sulfate)])
      rowv<-length(air$sulfate[!is.na(air$sulfate)])
    }else if(pollutant=='nitrate'){
      sumv<-sum(air$nitrate[!is.na(air$nitrate)])
      rowv<-length(air$nitrate[!is.na(air$nitrate)])
    }
    sumvalue<-sumvalue+sumv
    rowvalue<-rowvalue+rowv
    }
  return(sumvalue/rowvalue)
}
pollutantmean('specdata','sulfate',1:10)
pollutantmean('specdata', 'nitrate', 70:72)
pollutantmean('specdata','nitrate',23)