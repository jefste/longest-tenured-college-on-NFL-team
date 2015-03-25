
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
players.years.active<-select(roster.master.list,name,DOB,college,`roster year`)%>%
    #group by name DOB and college (should be a unique identifier for each player)
    group_by(name,DOB,college)%>%
    #counts the number of occurances for roster years for times appearing on roster
    summarise(roster.years=n())%>%
    #ungroup so data can be sorted by roster years
    ungroup%>%
    #sort by player count, in descending order
    arrange(desc(roster.years))

#historgram of how many years a player plays in league
hist(players.years.active$roster.years)
#summary of how many years a player plays in league
summary(players.years.active$roster.years)

