library(DBI)
library(RSQLite)
library(tidyverse)

library(httr) 
library(jsonlite)

base_URL <- "https://statsapi.web.nhl.com/api/v1/teams"
teamID <- 1
modf <- "?expand=team.schedule.next"
full_URL <- paste0(base_URL,modf)



getFranchise <- function(){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise")
  fromJSON(content(GET(url),"text"),flatten = T)
}
getFranchise()

getFranTotal <- function(){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-team-totals")
  fromJSON(content(GET(url),"text"),flatten = T)
}
getFranTotal()

getFranSeasonRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-season-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

getFranGoalieRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-goalie-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

getFranSkaterRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-skater-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  fromJSON(content(GET(url),"text"),flatten = T)
}

names(getFranSkaterRecord()[[1]])
table(getFranSkaterRecord()[[1]]$franchiseId)
dim(getFranSkaterRecord(ID=5)[[1]])

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


A <- getStatData(expand="team.roster", season="20142015")
B <- getStatData(expand="team.schedule.previous")
C <- getStatData(expand="team.schedule.next")
D <- getStatData(expand="person.names")
E <- getStatData(expand="team.stats")
View(A[[2]])


