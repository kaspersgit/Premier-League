library(XML)
library(rvest)

exp_matches_lineups=setNames(data.frame(matrix(ncol = 25, nrow = 0)), c("match_date","hometeam","awayteam","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"))

# Overview of the expected lineups in a league 
exp_url <- paste0("http://www.teamfeed.co.uk/lineups/competition/england/premier-league/expected")

# read html file of page
exp_html <- read_html(exp_url)

# extract all the players on the expected line ups page (multiple teams so take per block of 11)
exp_players <- exp_html %>% html_nodes(".player-name") %>% html_text() %>% as.character()
line_ups <- matrix(exp_players, ncol = 22, byrow = TRUE)

# extract names of the teams playing against each other
team_names <- exp_html %>% html_nodes("h2 a") %>% html_text() %>% as.character()

# team names are in string e.g. "/t/tArsenal - Tottenham/t/t/" so we have to split up on the dash and look at both pieces seperately
url.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford City","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry City","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull City","Ipswich","Leeds","Leicester","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle","Norwich","Portsmouth FC","QPR","Reading","Sheffield Utd.","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")

splitted_teams <- strsplit(team_names,"-")
playing_teams <- matrix(vector(mode="character", length = 2*length(splitted_teams)),ncol = 2)
for (i in 1:length(splitted_teams)){
  # Matches already played will look like t/tArsenal - Tottenham 2-0/t/t/ splitting on the dash will give more than 2 pieces
  if (length(splitted_teams[[i]])==2){
    for (n in 1:2){
      playing_teams[i,n]=team.names[sapply(url.names, grepl, splitted_teams[[i]][n])]
    }
  }
}

# all the matches for which the expected line up is given on this page (max 5??)
exp_matches <- exp_html %>% html_nodes(".pitch-header") %>% html_text() %>% as.character()
date_time_match <- as.numeric(gsub("[^\\d]+", "", exp_matches, perl=TRUE))
day <- substr(date_time_match,1,2)
year <- substr(date_time_match,3,6)
months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
month <- vector(mode = "numeric", length = length(exp_matches))
for (i in 1:length(exp_matches)){
  month[i] <- as.numeric(which(sapply(months_all, function(x) grepl(x, exp_matches[i]))))
}
match_date=format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d"))

exp_matches_lineups <- rbind(exp_matches_lineups,cbind(match_date,playing_teams,line_ups))

# because of the logic of splitting team names string we can exclude the rows in which there is no team name
exp_matches_lineups <- exp_matches_lineups[!is.na(exp_matches_lineups[,1]),]
