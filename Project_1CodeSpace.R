
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

FranchiseTots <- getFranTotal()

#Get franchise team totals data to dataframe DONT NEED IN RMD
franchise_totals <- getFranTotal()
attributes(franchise_totals)
franch_teamTotals <- as_tibble(franchise_totals$data)
#franch_teamTotals %>% group_by(franchiseId)




#Get franchise season record function. ADD NAME FUNCTION
getFranSeasonRecord <- function(name = NULL, ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-season-records")
  if (is.null(ID)==FALSE){
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

Franch_SnsRcrds <- getFranSeasonRecord()



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

Bruin_Sk8rRcrds <- getFranSkaterRecord(name = "Boston Bruins")
Bruin_Sk8rRcrds



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
skatsestat <- wrapper(baseAPI="Record", EndPoint="skate", franID=NULL)
wrappertest <- wrapper(EndPoint="season")



#Some analysis

#What divisions have the most teams? dont use, kinda boring
plot_col <- ggplot(data = E, aes(x = division.name))
plot_col + geom_bar() + labs(x = "Teams per Division")


correlation <- cor(Bruin_Sk8rRcrds$penaltyMinutes, Bruin_Sk8rRcrds$points)
scatter <- ggplot(Bruin_Sk8rRcrds, aes(x = penaltyMinutes, y = points))
scatter + geom_point() + geom_smooth(method = lm, color = "yellow") + 
  geom_text(x = 1500, y = 1250, size = 5, label = paste0("Correlation = ", 
          round(correlation, 2))) + ylab("Total Career Points") + xlab("Total Career Penalty Minutes") +ggtitle("Correlation Between Career Points and Penalty Minutes")


#what skater positions score most points
#first make new variable which is average points per season

positions <- wrapper(baseAPI="Record", EndPoint="skate", franID=NULL)
posit_andAvg <- mutate(positions, avgGls = (positions$goals/positions$seasons))
table1 <- posit_andAvg %>% group_by(positionCode) %>%   summarise(Average = mean(avgGls))
kable(table1)

#new dataset
#Looks at the isolate only the current franchises. To do this, I'll pull the franchises
#that are included in the Endpoint that lists upcoming games. Then, I'll join it with 
#season record. We now have a massive dataset of current franchises full of lots of
#useful statss
C <- getStatData(expand="team.schedule.next")
new <- inner_join(C, franch_seasonRecords, by = "franchiseId")


Bruin_Rcrds <- getFranSkaterRecord(name = "Boston Bruins")
Bruin_Sk8rRcrds <- getFranSkaterRecord(name = "Boston Bruins")


Bruin_Sk8rRcrds$rookiePoints <- ifelse(Bruin_Sk8rRcrds$rookiePoints >= 41, "High",
       ifelse(Bruin_Sk8rRcrds$rookiePoints >= 21, "Elevated",
              ifelse(Bruin_Sk8rRcrds$rookiePoints >= 11, "Moderate", "Average")))

Bruin_Sk8rRcrds$goals <- ifelse(Bruin_Sk8rRcrds$goals >= 99, "Rockstar",
                                       ifelse(Bruin_Sk8rRcrds$goals >= 31, "Star",
                                              ifelse(Bruin_Sk8rRcrds$goals >= 11, "Valuable", "Ok")))

#Does early talent indicate career success?
Bruin_Sk8rRcrds$rookiePoints <- ifelse(Bruin_Sk8rRcrds$rookiePoints >= 41, "High",
                                       ifelse(Bruin_Sk8rRcrds$rookiePoints >= 21, "Elevated",
                                              ifelse(Bruin_Sk8rRcrds$rookiePoints >= 11, "Moderate", "Average")))

Bruin_Sk8rRcrds$goals <- ifelse(Bruin_Sk8rRcrds$goals >= 99, "Rockstar",
                                ifelse(Bruin_Sk8rRcrds$goals >= 31, "Star",
                                       ifelse(Bruin_Sk8rRcrds$goals >= 11, "Valuable", "Ok")))

Bruin_Rcrds <- getFranSkaterRecord(name = "Boston Bruins")
selectBruins <- Bruin_Rcrds %>% select(penaltyMinutes, goals, seasons, assists, positionCode)
bruinstable <- function(position){
  data <- selectBruins %>% filter(positionCode == position) %>% select(-positionCode)
  kable(apply(data, 2, summary), caption = paste("Summary of Position", position), 
        digit = 1)
}
bruinstable("C")
bruinstable("D")
bruinstable("L")
bruinstable("R")



Bruin_Sk8rRcrds <- getFranSkaterRecord(name = "Boston Bruins")

Bruin_Sk8rRcrds$rookiePoints <- ifelse(Bruin_Sk8rRcrds$rookiePoints >= 41, "High",
                                       ifelse(Bruin_Sk8rRcrds$rookiePoints >= 21, "Elevated",
                                              ifelse(Bruin_Sk8rRcrds$rookiePoints >= 11, "Moderate", "Average")))

Bruin_Sk8rRcrds$goals <- ifelse(Bruin_Sk8rRcrds$goals >= 99, "Rockstar",
                                ifelse(Bruin_Sk8rRcrds$goals >= 31, "Star",
                                       ifelse(Bruin_Sk8rRcrds$goals >= 11, "Valuable", "Ok")))

ggplot(Bruin_Sk8rRcrds, na.rm = TRUE, aes(x = rookiePoints, na.rm = TRUE)) + geom_bar(aes(fill = goals), position = "stack") + xlab("Boston Bruins Rookie Points") + scale_fill_discrete(name = "")

tbl <- table(Bruin_Sk8rRcrds$goals, Bruin_Sk8rRcrds$rookiePoints, Bruin_Sk8rRcrds$positionCode)
kable(tbl[,,1], caption = "Centers")

tbl <- table(Bruin_Sk8rRcrds$goals, Bruin_Sk8rRcrds$rookiePoints, Bruin_Sk8rRcrds$positionCode)
kable(tbl[,,2], caption = "Defense")

tbl <- table(Bruin_Sk8rRcrds$goals, Bruin_Sk8rRcrds$rookiePoints, Bruin_Sk8rRcrds$positionCode)
kable(tbl[,,3], caption = "Left Wing")

tbl <- table(Bruin_Sk8rRcrds$goals, Bruin_Sk8rRcrds$rookiePoints, Bruin_Sk8rRcrds$positionCode)
kable(tbl[,,4], caption = "Right Wing")

Bruin_Rcrds <- getFranSkaterRecord(name = "Boston Bruins")
ggplot(Bruin_Rcrds, aes(x = positionCode, y = mostAssistsOneSeason)) + geom_boxplot() + 
  geom_jitter(aes(color = positionCode)) + ggtitle("Boxplot for Career Goals") + scale_fill_discrete(name = "Positions", labels = c("Center", "Defense", "Left Wing", "Right Wing"))


