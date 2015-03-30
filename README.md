Scripts in this repository
=========================
This repository contains a script that scapes the web to create a historical roster of all players on NFL rosters from 1960 - 2014, save to complete-roster-info.csv

From this historical roster, several scripts run including:
 * Determine the longest tenured college for each NFL team using complete-roster-info.csv
 * Determine the number of players each college has had from 1960 - 2014
 * Determine the number of years in the league for each player from 1960 - 2014
 * Determine the mean number of years for each position from 1960 - 2014

create-historical-roster.R
==========================
This R script contains the instructions for scraping the web for rosters and compiling a list of all team rosters over all years (1960-2014) and is found in the script
'create-historical-roster.R'. This script generates a file called 'complete-roster-info.csv'. 


Longest Tenured College for Every NFL team
==========================================
From the complete-roster-info.csv, the script 'longest-tenured-college-on-each-team.R' can be run which determines the longest tenured college for each NFL team
from the 'complete-roster-info.csv' file. The information is saved to a csv file, 'team-college-nice.csv'.


Number of college players for each college
==========================================
This script loads complete-roster-info.csv, then determines how many unique players there for a given name, then counts the number of occurances there are for unique college names.


Number of years in league for each player
=========================================
This script loads complete-roster-info.csv, then determines how many years they have been in the league and sorts by the maximum number of years.


Mean number of years for each position
======================================
This script loads the complete-roster-info.csv and determines the mean number of years by position.
