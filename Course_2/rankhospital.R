rankhospital<-function(state,outcome,num='best'){
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
    chosen<-subset(df,states==state)
    df1<-suppressWarnings(as.numeric(chosen[,outcome]))
    final<-chosen$names[order(df1,chosen$names,na.last = NA)]
    if(num=='best'){
      num<-1
    }else if(num=='worst'){
      num<-length(final)
    }
    final[num]
  }
}