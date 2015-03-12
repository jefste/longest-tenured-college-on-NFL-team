---
title: "Longest Tenured College on Each Team in the NFL"
author: "Jeff Stevens"
date: "03/12/2015"
output: html_document
---

Load XML library. This is needed to scrape web.
```{r}
library(XML)
```

Scrape teams/index.html to get team names. 
Team names typically in the format 'locality-mascot' so for instance, 
the Buffalo Bills would be 'buffalo-bills' and the New England Patriots would be
'new-england-patriots'.
```{r}
#gets html code for webpage
getnameshtml<-htmlTreeParse('http://www.footballdb.com/teams/index.html',useInternalNodes = T)
#gets the href value for all <a href='relative url'></a> in the page
src = xpathApply(getnameshtml, "//a[@href]", xmlGetAttr, "href")
#search all urls to look for the pattern '/teams/nfl/' and creates a boolean vector
teams.bool<-grepl(src,pattern = '/teams/nfl/')
#split the the string where there are forward slashes '/'
team.name.list<-sapply(src[teams.bool],strsplit,"/")
#unlist to create a simplified vector
#use unique incase there are duplicates
#subset the 4th through 35th elements (done by inspection, better method available?)
team.name.list<-unique(unlist(team.name.list))[4:35]
```
The variable team.name.list generates a list of names for all 32 teams

Function to grab urls for all rosters
```{r}

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

