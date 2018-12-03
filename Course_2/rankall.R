rankall<-function(outcome,num='best'){
  data1<-read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses='character')
  df<-data1[,c(2,7,11,17,23)]
  names(df)<-c('names','states','heart.attack','heart.failure','pneumonia')
  state_names<-sort(unique(df$states))
  symptom<-names(df)[3:5]
  outcome<-gsub(' ','.',outcome)
  if (!outcome %in% symptom){
    stop('invalid outcome')
  }else{
    hospital_list<-c()
    state_list<-c()
    for(index in state_names){
      chosen<-subset(df,states==index)
      df1<-suppressWarnings(as.numeric(chosen[,outcome]))
      final<-chosen$names[order(df1,chosen$names,na.last = NA)]
      if(num=='best'){
        num<-1
      }else if(num=='worst'){
        num<-length(final)
      }
      hospital_list<-c(hospital_list,final[num])
      state_list<-c(state_list,index)
    }
  }
  dff<-as.data.frame(cbind(hospital_list,state_list))
  names(dff)<-c('hospital','state')
  return(dff)
}