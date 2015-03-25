
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
position.years.active<-select(roster.master.list,name,DOB,college,`roster year`,position)%>%
    #group by name DOB and college (should be a unique identifier for each player)
    group_by(name,DOB,college,position)%>%
    #counts the number of occurances for roster years for times appearing on roster
    summarise(roster.years=n())%>%
    #ungroup so data can be grouped by position
    ungroup%>%
    group_by(position)%>%
    summarise(mean.years=mean(roster.years),max.year=max(roster.years),
              median.years=median(roster.years),number.players=n())%>%
    #ungroup so data can be grouped by mean roster years
    ungroup%>%
    arrange(desc(mean.years))
#shows data sorted by mean years
position.years.active

position.roster.years<-select(roster.master.list,name,DOB,college,`roster year`,position)%>%
    #group by name DOB and college (should be a unique identifier for each player)
    group_by(name,DOB,college,position)%>%
    #counts the number of occurances for roster years for times appearing on roster
    summarise(roster.years=n())%>%
    #ungroup so data can be grouped by position
    ungroup%>%
    select(position,roster.years)

library(ggplot2)
#creates a histogram faceted by position
qplot(data=position.roster.years,x=roster.years,binwidth=1)+
    facet_wrap(~ position,nrow = 3)+
    ggtitle('Histogram for Number of Years on Roster by Position')

#plot as a density
ggplot(data=position.roster.years,aes(x=roster.years))+ 
    #plot historgram as density
    geom_histogram(aes(y = ..density..),binwidth=1) +
    #facet by position
    facet_wrap(~ position, nrow = 3)+
    ggtitle('Density Histogram for Number of Years on Roster by Position')


#histogram of offensive line players
ggplot(data=filter(position.roster.years, position %in% c('C','OG','OL','OL')),
       aes(x=roster.years))+ 
    #plot historgram as density
    geom_histogram(aes(y = ..density..),binwidth=1)+
    ggtitle('Density Histogram for Number of Years on Roster for Offensive Line')
    

ggplot(data=filter(position.roster.years, position %in% c('DL','DT','DE')),
       aes(x=roster.years))+ 
    #plot historgram as density
    geom_histogram(aes(y = ..density..),binwidth=1)+
    ggtitle('Density Histogram for Number of Years on Roster for Defensive Line')



ggplot(data=filter(position.roster.years, position %in% c('K','KR','P','LS')),
       aes(x=roster.years))+ 
    #plot historgram as density
    geom_histogram(aes(y = ..density..),binwidth=1)+
    ggtitle('Density Histogram for Number of Years on Roster for Special Teams')


ggplot(data=filter(position.roster.years, position %in% c('QB','OE','RB','TE','WR')),
       aes(x=roster.years))+ 
    #plot historgram as density
    geom_histogram(aes(y = ..density..),binwidth=1)+
    ggtitle('Density Histogram for Number of Years on Roster for Offensive Skill Players')
