setwd("D:/Het Project/Data")
library("RSQLite")
library("DBI")
# connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname="England_football.db")
# get a list of all tables
alltables = dbListTables(con)
# get the populationtable as a data.frame
p1 = dbGetQuery( con,'select * from Premier_League' )
