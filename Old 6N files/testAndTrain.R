testAndTrain<-function(matchdata){
  set.seed(1234)
  ind<-sample(2,nrow(matchdata),replace=TRUE,prob=c(0.7,0.3))
  trainData<-matchdata[ind==1,]
  testData<-matchdata[ind==2,]
  
  return(ls(trainData,testData))
}