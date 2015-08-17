result<-function(matchdata){
  for(i in 1:nrow(matchdata)){
    if(matchdata[i,"Wins"]==1){matchdata[i,"result"]="win"}
    if(matchdata[i,"Draws"]==1){matchdata[i,"result"]="draw"}
    if(matchdata[i,"Losses"]==1){matchdata[i,"result"]="lose"}
  }
  matchdata$result<-as.factor(matchdata$result)
  matchdata[,"HT.Points.Diff"]<-as.integer(matchdata[,"Half.Time.Points.For"]-matchdata[,"Half.Time.Points.Against"])
  matchdata[is.na(matchdata)]<-as.integer(0)
  return(matchdata)
}
