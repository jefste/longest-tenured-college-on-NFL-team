
#Load complete roster info
roster.master.list<-read.csv('complete-roster-info.csv')
#eliminates the first column, which was previously the number
roster.master.list<-roster.master.list[,-1]
#eliminate the first row, which was the 'x' initialization placeholder
roster.master.list<-roster.master.list[-1,]
#remove the period in column names, as reading from CSV does not retain format 
# of original col names
colnames(roster.master.list)<-gsub('\\.',' ',colnames(roster.master.list))


library(dplyr)
#grab the name, DOB and college for each position on the roster
number.players.each.college<-select(roster.master.list,name,DOB,college)%>%
    #grab only the distinct values for name,DOB and college
    distinct%>%
    #group by college
    group_by(college)%>%
    #counts the number of players coming from each college
    summarise(player.com=n())%>%
    #sort by player count, in descending order
    arrange(desc(player.com))
    

head(number.players.each.college,30)
library(ggplot2)
qplot(number.players.each.college$player.com)
table(number.players.each.college$player.com)
summary(number.players.each.college$player.com)
