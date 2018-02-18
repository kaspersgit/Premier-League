ENG_getting_next_matches <- function(){
  
  # Open connectino to the db
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
  
  # Delete content of input table (because we just use this table for input, a trigger adds it to the main table)
  dbSendQuery(con, 'DELETE FROM ENG_next_matches')
  add_next_matches =dbSendQuery(con, 'INSERT INTO ENG_next_matches  
                           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)')
  
  for (i in 1:nrow(match_lineups)){
    dbBind(add_next_matches, unlist(match_lineups_clean[i,], use.names = FALSE))
  }
  
  dbClearResult(add_next_matches)
  
  dbDisconnect(con)
  
}