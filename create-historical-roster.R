#Load XML library. This is needed to scrape web.
library(XML)

#Scrape teams/index.html to get team names. 
#Team names typically in the format 'locality-mascot' so for instance, 
#the Buffalo Bills would be 'buffalo-bills' and the New England Patriots would be
#'new-england-patriots'.

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

#The variable team.name.list generates a list of names for all 32 teams

#On each teams default roster page (for their most current season, 2014), there
#is a section that contains links to all past rosters.  Since team names (such as the 
#Cardinals being the Phoenix Cardinals and then the Arizona Cardinals) and league 
#affiliations change (such as some teams being in the AFL before the NFL-AFL merger),
#grabbing the URLs for all past rosters bypasses this problem.

getRosterURLs<-function(team){
    #goes to default roster page
    roster.urls<-paste0('http://www.footballdb.com/teams/nfl/', team, '/roster')
    #grabs html code for that page
    team.roster.list<-htmlTreeParse(roster.urls,useInternalNodes = T)
    #looks in each div that has the class 'fifth fleft' returns the value of the
    #href within each anchor. This gives the URL for each year's roster
    each.year.url<-xpathSApply(team.roster.list,
                               "//div[@class='fifth fleft']/a[@href]",
                               xmlGetAttr,'href')
    #returns all the urls
    return(each.year.url)
}

#This creates a master list of where all the roster data is stored for each year.
#initialize the master list using 'x' as a place holder
roster.master.list<-matrix(rep('x',9),nrow=1)
#name each column
colnames(roster.master.list)<-c('number','name','position','games played','games started','DOB','college','roster year','team')


#Function to determine year from the URL. Since all URLs for each yearly roster
#contain the year as the last 4 digits, except for the most current year (2014),
#that just ends in 'roster', so for these cases the year is set to 2014
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

#This generates a list of all rosters available on footballdb.com and creates a 
#master list. This can take a long time to run (I never timed it,
#but maybe 10 minutes or so?), and really only needs to be run one time.


#do this for each team that is found in the team list
for(team in team.name.list){
    #retrieve the URLs for the current team
    all.roster.urls<-getRosterURLs(team)
    #for each URL
    for(url in all.roster.urls){
        #create the url
        current.url<-paste0('http://www.footballdb.com',url) 
        #grab the html
        current.html<-htmlTreeParse(current.url,useInternalNodes = T)
        #grab all the values found in each table data 'td' element
        ## this is stored as a 1D vector
        values.in.table<-xpathSApply(current.html,"//td",xmlValue)
        #create a matrix to convert the 1D vector into a 2D matrix
        ## Since each row is 7 elements long, the number of rows is 
        ## the total length of the vector/y
        roster.current.year<-matrix(values.in.table,
                                    nrow = length(values.in.table)/7,byrow=T)
        #bind the newly created matrix with the roster year and the team name
        roster.current.year.complete<-cbind(roster.current.year,getLast4(url),team)
        #bind the current years roster to the master list
        roster.master.list<-rbind(roster.master.list,roster.current.year.complete)
        }
    }


#saves to CSV file
write.csv(roster.master.list,file='complete-roster-info.csv')

