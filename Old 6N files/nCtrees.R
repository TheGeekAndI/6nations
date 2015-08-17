# this function determines the most significant variables for a
# classification tree by creating a random forest

nCtrees<-function(df,s=42,ntree=10000){
  # time stamp for performance
  start<-Sys.time()
  
  #initialise var imp matrix
  n.df.cforest.varimp<-matrix(nrow=(ncol(df)-1),ncol=0)
  
    for(seed in s){
    
    # set the seed to be able to reproduce the result
    set.seed(seed)
    
    # create controls for the tree
    df.controls<-cforest_unbiased(ntree=ntree,mtry=round(sqrt(ncol(df))))
    
    # run the random forest
    df.cforest<-cforest(result~.,data=df,controls=df.controls)
    
    # determine the variable importance
    df.cforest.varimp<-varimp(df.cforest,conditional=TRUE)
    
    # append the results of the varimp to find an average
    n.df.cforest.varimp<-cbind(n.df.cforest.varimp,df.cforest.varimp)
    }
    # names the columns with the seed
    colnames(n.df.cforest.varimp)<-paste("VI seed",s,sep=" ")
  
    # mean the variable importance of each variable
    means<-apply(n.df.cforest.varimp,1,mean)
  
    # append the means to the table
    n.df.cforest.varimp<-cbind(n.df.cforest.varimp,means)
    
    # write the variable importance to a text file to compare seeds
    FileLoc<-getwd()
    FileEnd<-".csv"
    FileName<-paste("/VarImp_seeds_",length(s),sep="")
    path<-paste(FileLoc,FileName,FileEnd,sep="")
    write.csv(n.df.cforest.varimp,path)
  
  # plot a bar plot with the variables and the cut-off line of significant
  # variables. Significant variables are those which are more important
  # than the absolute value of the least important variable.
  # to save the plot prepare the device then plot the graph
  FileEnd<-".pdf"
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  pdf(path)
  par(mar=c(5,10,1,1))
  # plot
  barplot(sort(n.df.cforest.varimp[,"means"]),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(n.df.cforest.varimp[,"means"])),col="red",lty="longdash",lwd=2)
  dev.off()
  
  # plot is repeated so that it is returned within R-Studio
  par(mar=c(5,10,1,1))
  barplot(sort(n.df.cforest.varimp[,"means"]),horiz=T,
          xlab="Variable Importance in the DF\n (predictors to right of dashed line are significant)",
          las=1)
  abline(v=abs(min(n.df.cforest.varimp[,"means"])),col="red",lty="longdash",lwd=2)
  
  finish<-Sys.time()
  return(difftime(finish, start,units="hours"))
}