best<-function(state,outcome){
  data1<-read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses='character')
  df<-data1[,c(2,7,11,17,23)]
  names(df)<-c('names','states','heart.attack','heart.failure','pneumonia')
  state_names<-unique(df$states)
  symptom<-names(df)[3:5]
  outcome<-gsub(' ','.',outcome)
  if (!state %in% state_names){
    stop('invalid state name')
  }else if (!outcome %in% symptom){
    stop('invalid outcome')
  }else{
    minval<-suppressWarnings(min(as.numeric(df[[outcome]][df$states==state]),na.rm = TRUE))
    df$names[which(df$states==state&df[[outcome]]==format(round(minval,1),nsmall=1))]
  }
}