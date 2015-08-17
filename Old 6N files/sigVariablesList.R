# this function returns the list of significant variables
# found from the random forest

sigVariablesList<-function(seed){
  # generate the path to the variable importance table by the seed
  FileLoc<-getwd()
  FileEnd<-".csv"
  FileName<-paste("/VarImp_seed_",seed,sep="")
  path<-paste(FileLoc,FileName,FileEnd,sep="")
  
  # read in the data
  varImpTable<-read.csv(path)
  
  # determine whether the variable is significant
  varImpTable[,"sig"]<-varImpTable[,2]>abs(min(varImpTable[,2]))
  
  # select only the significant variables
  impVar<-varImpTable[varImpTable[,"sig"]==T,]
  
  return(impVar[order(-impVar[,2]),])
}