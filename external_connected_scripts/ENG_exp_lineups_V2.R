ENG_exp_lineups_V2 <- function(){
  library(XML)
  library(rvest)
  library(RCurl)
  library(httr)
  
  exp_matches_lineups=setNames(data.frame(matrix(ncol = 25, nrow = 0)), c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"))
  
  # Overview of the expected lineups in a league 
  exp_url <- paste0("https://www.rotowire.com/soccer/lineups.php")
  
  # Don't exactly know how this part works but it enables for easier extraction afterwards
  exp_rawpage <- html_session(exp_url)
  
  # getting all the data for all the 10 next games
  exp_lineup_matchdate <- html_nodes(exp_rawpage, ".lineup__time") %>% html_text() %>% as.character()
  exp_lineup_teams <- html_nodes(exp_rawpage, ".lineup__mteam") %>% html_text() %>% as.character()
  exp_lineup_teams <- trimws(exp_lineup_teams)
  
  # extract links to players as they are abbreviated on the original page
  exp_lineup_home_links <- html_nodes(exp_rawpage, ".is-home a") %>% html_attr("href")
  
  # Create empty dataframe and fill it up with players names taken from their personal page
  exp_lineup_home <- NULL
  for (p in 1:length(exp_lineup_home_links)){
    exp_lineup_home_player <- html_nodes(html_session(paste0("https://www.rotowire.com",exp_lineup_home_links[p])), ".mb-0.hide-until-md") %>% html_text() %>% as.character() 
    exp_lineup_home <- rbind(exp_lineup_home,exp_lineup_home_player)
    }
  
  exp_lineup_away_links <- html_nodes(exp_rawpage, ".is-visit a") %>% html_attr("href")
  
  exp_lineup_away <- NULL
  for (p in 1:length(exp_lineup_away_links)){
    exp_lineup_away_player <- html_nodes(html_session(paste0("https://www.rotowire.com",exp_lineup_away_links[p])), ".mb-0.hide-until-md") %>% html_text() %>% as.character() 
    exp_lineup_away <- rbind(exp_lineup_away,exp_lineup_away_player)
  }
  
  # Cleaning names
  Encoding(exp_lineup_home) <- "UTF-8"
  exp_lineup_home <- iconv(trimws(gsub("ð","d",exp_lineup_home)), from = "UTF-8", to="ASCII//TRANSLIT")
  
  Encoding(exp_lineup_away) <- "UTF-8"
  exp_lineup_away <- iconv(trimws(gsub("ð","d",exp_lineup_away)), from = "UTF-8", to="ASCII//TRANSLIT")
  
  # line up split per game 
  line_ups_home <- matrix(exp_lineup_home, ncol = 11, byrow = TRUE)
  line_ups_away <- matrix(exp_lineup_away, ncol = 11, byrow = TRUE)
  
  # team names are in string e.g. "/t/tArsenal - Tottenham/t/t/" so we have to split up on the dash and look at both pieces seperately
  url.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford City","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry City","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull City","Ipswich","Leeds","Leicester","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle","Norwich","Portsmouth FC","QPR","Reading","Sheffield Utd.","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolverhampton","Wimbledon")
  team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
  
  
  playing_teams <- matrix(vector(mode="character", length = length(exp_lineup_teams)),ncol = 2)
  
  for (i in 1:length(exp_lineup_teams)){
    h = ((i+1) %% 2)+1
    v = ceiling(i/2)
    playing_teams[v,h]=team.names[sapply(url.names, grepl, exp_lineup_teams[i])]
  }
  
  # Check if date of match is "Today" and then use sys.date
  check_if_today <- "Today"
  
  # all the matches for which the expected line up is given on this page (always 10??)
  ## TODO fix the regexp to get the date correct
  date_time_match <- vector(length = length(exp_lineup_matchdate), mode = 'numeric')
  for (i in 1:length(exp_lineup_matchdate)){
    date_time_match[i] <- as.numeric(gsub("[^\\d]", "",strsplit(exp_lineup_matchdate,' ')[[i]][2], perl = TRUE))
  }
  
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
