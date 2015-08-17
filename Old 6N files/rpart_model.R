rpart_model<-function(train.data,formula,minsplit=10){
  set.seed(42)
  # create rpart model
  rpm<-rpart(formula,data=train.data,control=rpart.control(minsplit=minsplit))
  
  # find optimum tree
  opt<-which.min(rpm$cptable[,"xerror"])
  
  # store complexity parameter
  cp<-rpm$cptable[opt,"CP"]
  
  # prune the tree to the optimum
  rpm_prune<-prune(rpm,cp=cp)
  
  #plot the tree
  par(mar=c(1,1,1,1))
#   install.packages("rpart.plot")
#   linrary("rpart.plot")
  rpart.plot(rpm_prune,type=0,extra=104,varlen=0)
  
  #return(rpm_prune)
}