Return Parsed Franchise Data

``` r
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

FranchiseInfo <- getFranchise(name = "Bruins")
```

    ## No encoding supplied: defaulting to UTF-8.

Return Franchise Team Totals

``` r
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
```

    ## No encoding supplied: defaulting to UTF-8.

Return Franchise Season Records

``` r
#NEEDS NAME SEARCH
getFranSeasonRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-season-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
}

Franch_SnsRcrds <- getFranSeasonRecord(ID = 6)
```

    ## No encoding supplied: defaulting to UTF-8.

Return Goalie Records

``` r
#needs NAME SEARCH
getFranGoalieRecord <- function(ID=NULL){
  base_url <- "https://records.nhl.com/site/api"
  url <- paste0(base_url, "/", "franchise-goalie-records")
  if (is.null(ID)==F){
    url <- paste0(url, "?cayenneExp=franchiseId=", ID)
  }
  as_tibble(fromJSON(content(GET(url),"text"),flatten = T)$data)
}

Franch_GoalRcrds <- getFranGoalieRecord(ID = 6)
```

    ## No encoding supplied: defaulting to UTF-8.
