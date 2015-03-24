
#Gets the names for teams
library(XML)

#strip out names for urls
getnameshtml<-htmlTreeParse('http://www.footballdb.com/teams/index.html',useInternalNodes = T)
src = xpathApply(getnameshtml, "//a[@href]", xmlGetAttr, "href")
teams.bool<-grepl(src,pattern = '/teams/nfl/')
team.name.list<-sapply(src[teams.bool],strsplit,"/")
team.name.list<-unique(unlist(team.name.list))[4:35]

#Load complete roster info
roster.master.list<-read.csv('complete-roster-info.csv')
#eliminates the first column, which was previously the number
roster.master.list<-roster.master.list[,-1]
#eliminate the first row, which was the 'x' initialization placeholder
roster.master.list<-roster.master.list[-1,]
#remove the period in column names, as reading from CSV does not retain format 
# of original col names
colnames(roster.master.list)<-gsub('\\.',' ',colnames(roster.master.list))

    


#Initialize running.list

#create list with placeholder variables
## use placeholder so that when adding variables strings do not get 
## added as factors
running.list<-data.frame('name','school','9999', stringsAsFactors = F)
# name the columns of the data frame
colnames(running.list)<-c('team','school','year')

#This chunk sorts through the master list to find which college has the longest
#current tenure for each NFL team.

# loops through for every team
for (team in team.name.list){
    ### the first section essentially initializes the variable before
    ### stepping through each year for the current team
    #create boolean vector that gives True for entries that match the
    #current team in for loop
    bool.team<-roster.master.list[,'team']==team 
    #create list of years to look loop through 
    year.list<-unique(roster.master.list[bool.team, 'roster year'])
    #sort year list so that it starts with 2014
    year.list<-(sort(year.list,decreasing = T))
    #create boolean vector giving True for entries that match 2014
    bool.year<-roster.master.list[,'roster year']==year.list[1]
    #initalize last year to 2014
    last.year<-year.list[1]
    #create a list of all unique college names that match bool.team and bool.year
    college.list<-unique(roster.master.list[bool.team & bool.year,'college'])
    #loops through all years. [-1] omits the first year since it was used for 
    # initialization
    for (year in year.list[-1]){
        #create boolean vector returning True to match current year
        bool.year<-roster.master.list[,'roster year']==year
        #create college list of unique names that matches the current year and team
        college.list.next<-unique(roster.master.list[bool.year & bool.team,'college'])
        # determine the intersection of the previous years college list to the 
        # current years college list
        college.list<-intersect(college.list,college.list.next)
        # if there is no intersection, there are no elements in college.list
        if (length(college.list)<1) {
            # use for loop in case the old college list had more than one entry
            for(name in old.college.list){
                #create a vector to add the name team and year to running list
                add.to.list<-c(team,name,last.year)
                #bind the row just created to the running list
                running.list<-rbind(running.list,add.to.list)
                }
            #break loop after writing all entries to list
            break
        }
        old.college.list<-college.list
        last.year<-year
    }
}

#show the head and tail to confirm list was made correctly
head(running.list)
tail(running.list)

#Format the list for nicer output and save to CSV. This step is basically 
#cosmetic before saving as a CSV.

#get rid of initialization value
nice.list<-running.list[-1,]

#get rid of dash '-' and replace with a space in the first column
nice.list[,1]<-gsub("-"," ",nice.list[,1])

#function to capitalizes the first letter of each separte string
##found this function on on StackEx?
.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

#capitalize first letter of team names
nice.list[,1]<-sapply(nice.list[,1],.simpleCap)

#save to csv file, omit row names
write.csv(nice.list,file='team_college_nice.csv',row.names=F)

