# this function determines the most significant variables for a
# classification tree by creating a random forest

sigVariables<-function(df,seed=42,ntree=1000){
  # set the seed to be able to reproduce the result
  set.seed(seed)
  
  # create controls for the tree
  df.controls<-cforest_unbiased(ntree=ntree,mtry=round(sqrt(ncol(df))))
  
  # run the random forest
  df.cforest<-cforest(result~.,data=df,controls=df.controls)
  
  # determine the variable importance
  df.cforest.varimp<-varimp(df.cforest,conditional=TRUE)
  
  # write the variable importance to a text file to compare seeds
  FileLoc<-getwd()
  FileEnd<-".csv"
  FileName<-paste("/VarImp_seed_",seed,sep="")
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  write.csv(df.cforest.varimp,path)
  
  # plot a bar plot with the variables and the cut-off line of significant
  # variables. Significant variables are those which are more important
  # than the absolute value of the least important variable.
  # to save the plot prepare the device then plot the graph
  FileEnd<-".pdf"
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  pdf(path)
  par(mar=c(5,10,1,1))
  # plot
  barplot(sort(df.cforest.varimp),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(df.cforest.varimp)),col="red",lty="longdash",lwd=2)
  dev.off()
  
  # plot is repeated so that it is returned within R-Studio
  par(mar=c(5,10,1,1))
  barplot(sort(df.cforest.varimp),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(df.cforest.varimp)),col="red",lty="longdash",lwd=2)
}