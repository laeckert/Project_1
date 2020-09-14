
library(tidyverse)
library(httr) 
library(jsonlite)
library(DT)

#base_URL <- "https://statsapi.web.nhl.com/api/v1/teams"
#teamID <- 1
#modf <- "?expand=team.schedule.next"
#full_URL <- paste0(base_URL,modf)


#Get franchise function
getFranchise <- function(){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise")
  fromJSON(content(GET(url),"text"),flatten = T)
}

#Get franchise data to dataframe 
franchise <- getFranchise()
attributes(franchise)
franchise_data <- as_tibble(franchise$data)
datatable(franchise_data)

#Get franchise team totals function

getFranTotal <- function(){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-team-totals")
  fromJSON(content(GET(url),"text"),flatten = T)
}

#Get franchise team totals data to dataframe 
franchise_totals <- getFranTotal()
attributes(franchise_totals)
franch_teamTotals <- as_tibble(franchise_totals$data)
#franch_teamTotals %>% group_by(franchiseId)


#Get franchise season record function
getFranSeasonRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-season-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

#Get franchise season record data to dataframe 
season_records <- getFranSeasonRecord()
attributes(franch_seasonRecords)
franch_seasonRecords <- as_tibble(season_records$data)


#Get franchise goalie record function
getFranGoalieRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-goalie-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

#Get goalie record data to dataframe 
goalie_records <- getFranGoalieRecord()
attributes(goalie_records)
franch_goalieRecords <- as_tibble(goalie_records$data)

#Get franchise skater record function
getFranSkaterRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-skater-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

#Get skater record data to dataframe 
skater_records <- getFranSkaterRecord()
attributes(skater_records)
franch_skaterRecords <- as_tibble(skater_records$data)

#names(getFranSkaterRecord()[[1]])
#table(getFranSkaterRecord()[[1]]$franchiseId)
#dim(getFranSkaterRecord(ID=5)[[1]])


#function to get data from stats endpoint
getStatData <- function(expand=NULL, season=NULL, teamID=NULL, stats=NULL){
  url <- "https://statsapi.web.nhl.com/api/v1/teams"
  if (!is.null(expand)){
    url <- paste0(url, "?expand=", expand)
  }
  if (!is.null(season)){
    url <- paste0(url, "&season=", season)
  }
  if(!is.null(teamID)){
    teams <- paste(teamID, collapse = ",")
    url <- paste0(url, "?teamId=", teams)
  }
  if(!is.null(stats)){
    url <- paste0(url, "?stats=", stats)
  }
  print(url)
  fromJSON(content(GET(url),"text"),flatten = T)
}
getStatData

A <- getStatData(expand="team.roster", season="20142015")
B <- getStatData(expand="team.schedule.previous")
C <- getStatData(expand="team.schedule.next")
D <- getStatData(expand="person.names")
E <- getStatData(expand="team.stats")
View(A[[2]])

#need wrapper function

