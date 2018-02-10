setwd("D:/Het Project/Voetbal predictions/Premier-League")
library(rvest)
library(XML)
library(RCurl)

# connecting to the db and check what the last entry was (based on the amount of entries for current season)
# SQLite connection
library("RSQLite")
library(DBI)
# connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
last_game_in_db <- as.numeric(dbGetQuery(con,'SELECT 
                          COUNT(*)
                          FROM 
                            "2017"'))

#check what the last match was of which the data is available
raw.data.18 = read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv")
last_game_available <- nrow(raw.data.18)

if (last_game_in_db != last_game_available){
  new_games_for_db <- cbind((last_game_in_db+1:last_game_available),raw.data.18[(last_game_in_db+1:last_game_available),])
  
  #Insert the new rows into the db 69 columns of which the first one is just the row number
  add_matches=dbSendQuery(con, 'INSERT INTO "2017"  
                                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )')
  for (i in 1:nrow(new_games_for_db)){
    dbBind(add_matches, unlist(new_games_for_db[i,], use.names = FALSE))
  }
  
  dbClearResult(add_matches)
}
dbDisconnect(con)
# 
# 
# 
# match_lineups=matrix(rep(0,25),nrow=1)
# colnames(match_lineups)=c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11")
# 
# 
# 
# 
# # to scrape all previous seasons, only make sense to run once. Updating it on a weekly basis should at max only the last season be used
# for (start_year in 2018){
#   
#   # Url of all the season games
#   weblink <- paste("https://www.11v11.com/competitions/premier-league/",start_year,"/matches/",sep = "")
#   
#   # read in the html structure
#   raw_page <- read_html(weblink)
#   
#   # Getting the link to all the match details for every match, has a lot of duplicates
#   all_matches_link=raw_page %>% html_nodes(xpath='//div[@class="knockout-stage-list"]//a') %>% html_attr('href')
#   
#   # selecting the unique match links, so every match has one linke
#   unique_matches_link=unique(all_matches_link)
#   # matches which are not yet tracked, taking 10 more back then the database to make sure that doing this halfway a round doesn't give an error
#   non_captured_matches <- unique_matches_link[max(1,last_game_in_db-10):last_game_available]
#   
#   for (match_link in non_captured_matches){
#     url <- paste("https://www.11v11.com",match_link,sep = "")
#     
#     page  <- read_html(url)
#     
#     #Team name and active players
#     home_team <- page %>% html_nodes(xpath='//div[@class="home"]//a') %>% html_text()
#     away_team <- page %>% html_nodes(xpath='//div[@class="away"]//a') %>% html_text()
#     
#     # extracting club name and starting player names from above vector
#     home_name <- home_team[1]
#     away_name <- away_team[1]
#     
#     home_starting11 <- home_team[2:12]
#     away_starting11 <- away_team[2:12]
#     
#     # Getting the date of the match
#     raw_match_details <- page %>% html_nodes(xpath='//div[@class="match-report"]//h1') %>% html_text()
#     raw_match_date <- sub("^.*\\,", "", raw_match_details)
#     
#     months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
#     month=as.numeric(which(sapply(months_all, function(x) grepl(x, raw_match_date))))
#     
#     day_year_time=gsub('\\D','',raw_match_date)
#     day=substr(day_year_time,1,2)
#     year=substr(day_year_time,3,6)
#     
#     # putting day month and year together in date format equal to the one in the db
#     match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))
#     
#     
#     lineup = c(match_date,home_name,away_name,home_starting11,away_starting11)
#     match_lineups = rbind(match_lineups,lineup)
#   }
# }
# 
# 
# 
