ENG_exp_lineups_V2 <- function(){
  library(XML)
  library(rvest)
  library(RCurl)
  library(httr)
  
  exp_matches_lineups=setNames(data.frame(matrix(ncol = 25, nrow = 0)), c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"))
  
  # Overview of the expected lineups in a league 
  exp_url <- paste0("https://www.rotowire.com/soccer/soccer-lineups.htm")
  
  geturl <- getURL(exp_url)
  
  # get the html text behind it
  exp_rawpage <- htmlTreeParse(geturl, useInternalNodes = TRUE)
  # look for the sentence where the distance is included
  exp_lineup_tables <- xpathSApply(exp_rawpage, "//div[@class='span15 offset1']//div[@class='span15 dlineups-datestatus']",xmlValue)
  
  # getting all the data for all the 10 next games
  exp_lineup_matchdate <- xpathSApply(exp_rawpage, '//div[@class="span5 dlineups-bigtimeonly"]//span',xmlValue)
  exp_lineup_teams <- xpathSApply(exp_rawpage, '//div[@class="span15 dlineups-teamsnba"]',xmlValue)
  
  # Get the title of the names, because the name shown on the page is sometimes shortened for the first name
  exp_lineup_home <- xpathSApply(exp_rawpage, '//div[@class="home_lineup"]//div[@class="dlineups-vplayer"]//a', xmlGetAttr, "title")
  exp_lineup_away <- xpathSApply(exp_rawpage, '//div[@class="visit_lineup"]//div[@class="dlineups-vplayer"]//a',xmlGetAttr, "title")
  
  # line up split per game 
  line_ups_home <- matrix(exp_lineup_home, ncol = 11, byrow = TRUE)
  line_ups_away <- matrix(exp_lineup_away, ncol = 11, byrow = TRUE)
  
  # team names are in string e.g. "/t/tArsenal - Tottenham/t/t/" so we have to split up on the dash and look at both pieces seperately
  url.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford City","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry City","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull City","Ipswich","Leeds","Leicester","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle","Norwich","Portsmouth FC","QPR","Reading","Sheffield Utd.","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
  team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
  
  
  splitted_teams <- strsplit(exp_lineup_teams,"vs.")
  playing_teams <- matrix(vector(mode="character", length = 2*length(splitted_teams)),ncol = 2)
  
  for (i in 1:length(splitted_teams)){
    # Matches already played will look like t/tArsenal - Tottenham 2-0/t/t/ splitting on the dash will give more than 2 pieces
    if (length(splitted_teams[[i]])==2){
      for (n in 1:2){
        playing_teams[i,n]=team.names[sapply(url.names, grepl, splitted_teams[[i]][n])]
      }
    }
  }
  
  # Check if date of match is "Today" and then use sys.date
  check_if_today <- "Today"
  
  # all the matches for which the expected line up is given on this page (always 10??)
  date_time_match <- as.numeric(gsub("[^\\d]+", "", exp_lineup_matchdate, perl=TRUE))
  day <- date_time_match
  year <- rep(format(Sys.Date(),"%Y"),length(exp_lineup_matchdate))
  
  # go through date and check for "Today"
  for (t in 1:length(exp_lineup_matchdate)){
    if (exp_lineup_matchdate[t] == check_if_today){
      day[t] <- as.numeric(format(as.Date(Sys.Date()),"%d"))
      year[t] <- as.numeric(format(as.Date(Sys.Date()),"%Y"))
    }
  }
  

  months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
  month <- vector(mode = "numeric", length = length(exp_lineup_matchdate))
  
  for (i in 1:length(exp_lineup_matchdate)){
    if (exp_lineup_matchdate[i] == check_if_today){
      month[i] <- as.numeric(format(as.Date(Sys.Date()),"%m"))
    }else{
      month[i] <- as.numeric(which(sapply(months_all, function(x) grepl(x, exp_lineup_matchdate[i]))))
    }
  }
  
  # putting the year, month and day together and put it in date format
  match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))
  
  exp_matches_lineups <- rbind(exp_matches_lineups,cbind(match_date,playing_teams,line_ups_home,line_ups_away))
  
  # Add column names again to be sure
  colnames(exp_matches_lineups) <- c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11")
  
  
  # because of the logic of splitting team names string we can exclude the rows in which there is no team name
  exp_matches_lineups <- exp_matches_lineups[!is.na(exp_matches_lineups[,1]),]

return(exp_matches_lineups)  
}
