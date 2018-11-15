ENG_insert_predictions <- function(real_and_predicted){

  library("RSQLite")
  library("DBI")
  
  # Open connectino to the db
  con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
  
  # Delete content of input table (because we just use this table for input, a trigger adds it to the main table)
  add_predictions =dbSendQuery(con, 'INSERT INTO ENG_match_prediction
                                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)')
  
  for (i in 1:nrow(real_and_predicted)){
    dbBind(add_predictions, unname(real_and_predicted[i,]))
  }
  
  dbClearResult(add_predictions)
  
  dbDisconnect(con)
}