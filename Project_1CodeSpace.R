
library(tidyverse)
library(httr) 
library(jsonlite)
library(DT)

#base_URL <- "https://statsapi.web.nhl.com/api/v1/teams"
#teamID <- 1
#modf <- "?expand=team.schedule.next"
#full_URL <- paste0(base_URL,modf)


#Get franchise function - GOOD, in Rmd
getFranchise <- function(name=NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise")
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
  if (!is.null(name)){
    dat <- dat %>% filter(teamCommonName %in% name)
  }
  if (!is.null(ID)){
  dat <- dat %>% filter(id %in% ID)
  }
  dat
}

FranchiseInfo <- getFranchise()


#Get franchise data to dataframe DONT NEED IN RMD
franchise <- getFranchise()
franchise_canad <- getFranchise(name=c("Maroons"))
franchise_canad <- getFranchise(ID=c(7))
attributes(franchise)
franchise_data <- as_tibble(franchise$data)
datatable(franchise_data)

#Get franchise team totals function GOOD, In RMD

getFranTotal <- function(name=NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-team-totals")
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
  if (!is.null(name)){
    dat <- dat %>% filter(teamName %in% name)
  }
  if (!is.null(ID)){
    dat <- dat %>% filter(teamId %in% ID)
  }
  dat
}

FranchiseTots <- getFranTotal(ID = 6)

#Get franchise team totals data to dataframe DONT NEED IN RMD
franchise_totals <- getFranTotal()
attributes(franchise_totals)
franch_teamTotals <- as_tibble(franchise_totals$data)
#franch_teamTotals %>% group_by(franchiseId)




#Get franchise season record function. ADD NAME FUNCTION
getFranSeasonRecord <- function(name = NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-season-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
  if (!is.null(name)){
    dat <- dat %>% filter(franchiseName %in% name)
  }
  if (!is.null(ID)){
    dat <- dat %>% filter(franchiseId %in% ID)
  }
  dat
}

Franch_SnsRcrds <- getFranSeasonRecord(name = "Boston Bruins")



#Get franchise season record data to dataframe 
season_records <- getFranSeasonRecord()
attributes(franch_seasonRecords)
franch_seasonRecords <- as_tibble(season_records$data)




#Get franchise goalie record function in RMD, GOOD
getFranGoalieRecord <- function(name = NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-goalie-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
  if (!is.null(name)){
    dat <- dat %>% filter(franchiseName %in% name)
  }
  if (!is.null(ID)){
    dat <- dat %>% filter(franchiseId %in% ID)
  }
  dat
}

Franch_GoalRcrds <- getFranGoalieRecord(name = "Boston Bruins")
Franch_GoalRcrds

#Get goalie record data to dataframe DONT NEED
goalie_records <- getFranGoalieRecord()
attributes(goalie_records)
franch_goalieRecords <- as_tibble(goalie_records$data)



#Get franchise skater record function
getFranSkaterRecord <- function(name = NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-skater-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
  if (!is.null(name)){
    dat <- dat %>% filter(franchiseName %in% name)
  }
  if (!is.null(ID)){
    dat <- dat %>% filter(franchiseId %in% ID)
  }
  dat
}

Franch_Sk8rRcrds <- getFranSkaterRecord(name = "Boston Bruins")
Franch_Sk8rRcrds

#Get skater record data to dataframe 
skater_records <- getFranSkaterRecord()
attributes(skater_records)
franch_skaterRecords <- as_tibble(skater_records$data)


#names(getFranSkaterRecord()[[1]])
#table(getFranSkaterRecord()[[1]]$franchiseId)
#dim(getFranSkaterRecord(ID=5)[[1]])


#function to get data from stats endpoint
getStatData <- function(name = NULL, expand=NULL, season=NULL, teamID=NULL, stats=NULL){
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
  dat <- as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$teams)
  if (!is.null(name)){
    dat <- dat %>% filter(teamName %in% name)
  }
  if (!is.null(teamID)){
    dat <- dat %>% filter(franchiseId %in% teamID)
  }
  dat
}



A <- getStatData(expand="team.roster", season="20142015")
B <- getStatData(expand="team.schedule.previous")
C <- getStatData(expand="team.schedule.next")
D <- getStatData(expand="person.names")
E <- getStatData(expand="team.stats")
F <- getStatData()

View(A[[2]])

#wrapper function
wrapper <- function(baseAPI="Record", EndPoint="Franchise", franID=NULL, name=NULL, 
                    expand=NULL, season=NULL, teamID=NULL, stats=NULL){
  
  if (toupper(baseAPI)=="RECORD"){
    if (grepl("SKATE", toupper(EndPoint))){
      dat <- getFranSkaterRecord(ID=franID)
    }
    else if (grepl("GOAL", toupper(EndPoint))){
      dat <- getFranGoalieRecord(ID=franID)
    }
    else if (grepl("SEASON", toupper(EndPoint))){
      dat <- getFranSeasonRecord(ID=franID)
    }
    else if (grepl("TOTAL", toupper(EndPoint))){
      dat <- getFranTotal()
    }
    else if (grepl("FRANCHISE", toupper(EndPoint))){
      dat <- getFranchise()
    }
  }
  else if (toupper(baseAPI)=="STATS"){
    dat <- getStatData(expand=expand, season=season, teamID=teamID, stats=stats)
  }
  else{
    stop("There is no function to call from different APIs")
  }
  dat
}

skate <- wrapper(baseAPI="Record", EndPoint="skate", franID=NULL)
wrappertest <- wrapper()
