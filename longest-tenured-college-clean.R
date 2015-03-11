---
title: "Longest Tenured College on Each Team in the NFL"
author: "Jeff Stevens"
date: "03/11/2015"
output: html_document
---



Gets the names for teams
```{r}
library(XML)

#strip out names for urls
getnameshtml<-htmlTreeParse('http://www.footballdb.com/teams/index.html',useInternalNodes = T)
src = xpathApply(getnameshtml, "//a[@href]", xmlGetAttr, "href")
teams.bool<-grepl(src,pattern = '/teams/nfl/')
team.name.list<-sapply(src[teams.bool],strsplit,"/")
team.name.list<-unique(unlist(team.name.list))[4:35]
```


Function to grab urls for all rosters
```{r}
#currently only select 1 element
getRosterURLs<-function(team){
    roster.urls<-paste0('http://www.footballdb.com/teams/nfl/', team, '/roster')
    team.roster.list<-htmlTreeParse(roster.urls,useInternalNodes = T)
    each.year.url<-xpathSApply(team.roster.list,"//div[@class='fifth fleft']/a[@href]",xmlGetAttr,'href')
    return(each.year.url)
}
getRosterURLs(team.name.list[2])

```

```{r}
#initialize the master list
roster.master.list<-matrix(rep('x',9),nrow=1)
colnames(roster.master.list)<-c('number','name','position','games played','games started','DOB','college','roster year','team')
```


Function to determine year from the URL
```{r}
getLast4 <- function(x){
    #grabs the last 4 characters of the string
    year<-substr(x, nchar(x)-3, nchar(x))
    #if the last 4 charaters are 'ster' (the url ends in 'roster'), that means
    #it is the most current roster, and thus 2014
    if(year=='ster'){
        year<-'2014'
        }
    return(year)
  }
```
keep this one!
Only need to run once!
This creates the master list. 
This generates a list of all rosters available on footballdb.com.
```{r}
for(team in team.name.list){
    all.roster.urls<-getRosterURLs(team)
    for(url in all.roster.urls){
        current.url<-paste0('http://www.footballdb.com',url) 
        current.html<-htmlTreeParse(current.url,useInternalNodes = T)
        values.in.table<-xpathSApply(current.html,"//td",xmlValue)
        roster.current.year<-matrix(values.in.table,nrow = length(values.in.table)/7,byrow=T)
        roster.master.list<-rbind(roster.master.list,cbind(roster.current.year,getLast4(url),team))
    
        }
    }



#save to file
write.csv(roster.master.list,file='complete-roster-info.csv')


```



Read running.list
```{r}
running.list<-data.frame('name','school','9999', stringsAsFactors = F)
colnames(running.list)<-c('team','school','year')
running.list
str(running.list)
```

Initialize running.list
```{r}
running.list<-data.frame('name','school','9999', stringsAsFactors = F)
colnames(running.list)<-c('team','school','year')
running.list
str(running.list)
```


```{r}


for (team in team.name.list){
    last.year<-'2014'
    bool.team<-roster.master.list[,'team']==team 
    bool.year<-roster.master.list[,'roster year']=='2014'
    college.list<-unique(roster.master.list[bool.team & bool.year,'college'])
    year.list<-unique(roster.master.list[bool.team, 'roster year'])
    year.list<-(sort(year.list,decreasing = T))
    for (year in year.list){
        bool.year<-roster.master.list[,'roster year']==year
        college.list.next<-unique(roster.master.list[bool.year & bool.team,'college'])
        college.list<-intersect(college.list,college.list.next)
        if (length(college.list)<1) {
            if(length(old.college.list)>1){
                print(old.college.list)
                for(name in old.college.list){
                    add.to.list<-c(team,name,last.year)
                    running.list<-rbind(running.list,add.to.list)
                }
                break
            }
            else {
                add.to.list<-c(team,old.college.list,last.year)
                running.list<-rbind(running.list,add.to.list)
                break
            }
            
            
            #running.list<-rbind(running.list,add.to.list)
                    
        }
        old.college.list<-college.list
        last.year<-year
    }
    #if(length(college.list)>0){}
}


running.list
```

```{r}
#get rid of initialization value
nice.list<-running.list[-1,]

#get rid of -
nice.list[,1]<-gsub("-"," ",nice.list[,1])

#function to capitalizes the first letter of each separte string
##found on StackEx?
.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}
#capitalize team names
nice.list[,1]<-sapply(nice.list[,1],.simpleCap)
nice.list


write.csv(nice.list,file='team_college_nice.csv')

```

