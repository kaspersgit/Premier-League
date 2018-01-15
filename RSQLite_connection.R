import_data_db <- function(){
  setwd("D:/Het Project/Voetbal predictions/Premier-League")
  # SQLite connection
  library("RSQLite")
  library(DBI)
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
  main_data <- dbGetQuery(con,'SELECT 
                              	emh.*,
                              	emsm.home_start_mv,
                              	emsm.away_start_mv,
                                season
                              FROM 
                              	ENG_matches_hist AS emh
                              LEFT JOIN 
                              	ENG_match_startmv AS emsm 
                              	ON emsm.match_date = emh.Date
                              	AND emsm.hometeam = emh.HomeTeam
                              	AND emsm.awayteam = emh.AwayTeam' )
  dbDisconnect(con)
  return(main_data)
}
