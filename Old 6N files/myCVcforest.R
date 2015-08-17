# this function uses k-fold CV on the cforest to determine the
# variable importance and show how well the model is predicting the outcomes

myCVcforest<-function(df,s=42,ntree=10000,k=10){
  # time stamp for performance
  start<-Sys.time()
  
  # find the number of rows in k equal subsets
  n<-floor(nrow(df)/k)
  
  # initiate the matricies for the varimportance table
  s.df.cforest.varimp<-matrix(nrow=(ncol(df)-1),ncol=0)
  k.df.cforest.varimp<-matrix(nrow=(ncol(df)-1),ncol=0)
  
  # for each of the seeds
  for(seed in s){
    
    # set seed for reproduceability
    set.seed(seed)
    
    # loop for k folds
    for(i in 1:k){
      s1<-((i-1)*n+1) # start of the subset
      s2<-(i*n)       # end of the subset
      subset<-s1:s2
      
      # define the test and train sets
      cv.train<-df[-subset,]  # all rows apart from a k-th of the data
      cv.test<-df[subset,]    # all remaining rows
      
      # create controls for the tree
      df.controls<-cforest_unbiased(ntree=ntree,mtry=round(sqrt(ncol(cv.train))))
      
      # run the random forest
      df.cforest<-cforest(result~.,data=cv.train,controls=df.controls)
      
      # determine the variable importance
      k.cforest.varimp<-varimp(df.cforest,conditional=TRUE)
     
      # append the results of the varimp to find an average
      k.df.cforest.varimp<-cbind(k.df.cforest.varimp,k.cforest.varimp)
    } ##### END OF Folds LOOP
    
    # mean the variable importance of each variable for the FOLD
    fold.means<-apply(k.df.cforest.varimp,1,mean)
    
    # append the FOLD means to the table
    s.df.cforest.varimp<-cbind(s.df.cforest.varimp,fold.means)
  } ##### END OF Seeds LOOP
  
  # names the columns with the seed
  colnames(s.df.cforest.varimp)<-paste("VI seed",s,sep=" ")
  
  # mean the variable importance of each variable for the SEED
  seed.means<-apply(s.df.cforest.varimp,1,mean)
  
  # append the SEED means to the table
  s.df.cforest.varimp<-cbind(s.df.cforest.varimp,seed.means)
  
  # write the variable importance to a text file to compare seeds
  FileLoc<-getwd()
  FileEnd<-".csv"
  FileName<-paste("/VarImp_seeds_",length(s),sep="")
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  write.csv(s.df.cforest.varimp,path)
  
  # plot a bar plot with the variables and the cut-off line of significant
  # variables. Significant variables are those which are more important
  # than the absolute value of the least important variable.
  # to save the plot prepare the device then plot the graph
  FileEnd<-".pdf"
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  pdf(path)
  par(mar=c(5,10,1,1))
  # plot
  barplot(sort(s.df.cforest.varimp[,"seed.means"]),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(s.df.cforest.varimp[,"seed.means"])),col="red",lty="longdash",lwd=2)
  dev.off()
  
  # plot is repeated so that it is returned within R-Studio
  par(mar=c(5,10,1,1))
  barplot(sort(s.df.cforest.varimp[,"seed.means"]),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(s.df.cforest.varimp[,"seed.means"])),col="red",lty="longdash",lwd=2)
  
  finish<-Sys.time()
  return(difftime(finish, start, unit="hours"))
}