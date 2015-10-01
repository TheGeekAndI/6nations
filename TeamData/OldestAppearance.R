# gathers data of oldest appearances in the tournament as a measure of experience

# 2015 tournament: comp_id=479
# 2014 tournament: comp_id=448

# England: club_id=116
# France: club_id=
# Ireland: club_id=
# Italy: club_id=122
# Scotland: club_id=118
# Wales: club_id=117

#read 2015 and 2014 data
oldest2015<-readLines('http://rugby.statbunker.com/competitions/OldestAppearances?comp_id=479')
oldest2014<-readLines('http://rugby.statbunker.com/competitions/OldestAppearances?comp_id=448')

# extract date
myexp<-'<tr><td>([1-31]-[a-zA-Z+]-[2015])</td>'
datalines<-grep(myexp,oldest2015[173],value = T)
