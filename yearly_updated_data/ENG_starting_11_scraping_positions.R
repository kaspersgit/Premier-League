setwd("D:/Het Project/Voetbal predictions/Premier-League")
library(rvest)
library(XML)
library(RCurl)
library("RSQLite")
library(DBI)
library(dplyr)

# for in case we have to change the team names to make them in line with db names
url.names=c("Arsenal","Aston Villa","Birmingham","Blackburn Rovers","Blackpool","Bolton","AFC Bournemouth","Bradford City","Brighton and Hove Albion","Burnley","Cardiff City","Charlton","Chelsea","Coventry City","Crystal Palace","Derby","Everton","Fulham","Huddersfield Town","Hull City","Ipswich","Leeds","Leicester City","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle United","Norwich","Portsmouth FC","QPR","Reading","Sheffield Utd.","Southampton","Stoke City","Sunderland","Swansea City","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United","Wigan","Wolverhampton Wanderers","Wimbledon")
team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")


# Create dataframe with one row and the correct colnames
match_lineups=data.frame(matrix(rep("",44),nrow=1))
colnames(match_lineups)=c("match_date","hometeam","awayteam"
                          ,"hg1","hd1","hd2","hd3","hd4","hd5","hd6","hd7","hm1","hm2","hm3","hm4","hm5","hm6","hm7","hf1","hf2","hf3","hf4","hf5"
                          ,"ag1","ad1","ad2","ad3","ad4","ad5","ad6","ad7","am1","am2","am3","am4","am5","am6","am7","af1","af2","af3","af4","af5","season")
match_lineups2 = match_lineups

# This doesn't need to be a loop as we only do it for current season, will need some fix to make it more adaptable for next season
for (start_year in 2005:2019){
  
  # Url of all the season games
  weblink <- paste("https://www.11v11.com/competitions/premier-league/",start_year,"/matches/",sep = "")
  
  # read in the html structure
  raw_page <- read_html(weblink)
  
  # Getting the link to all the match details for every match, has a lot of duplicates
  all_matches_link=raw_page %>% html_nodes(xpath='//div[@class="knockout-stage-list"]//a') %>% html_attr('href')
  
  # selecting the unique match links, so every match has one linke
  unique_matches_link=unique(all_matches_link)
  
  # For every link of the matches not yet in the db scrape the lineups
  for (match_link in unique_matches_link){
    url <- paste("https://www.11v11.com",match_link,sep = "")
    
    page  <- read_html(url)
    
    #Team name and active players
    home_team <- page %>% html_nodes(xpath='//div[@class="home"]//a') %>% html_text()
    away_team <- page %>% html_nodes(xpath='//div[@class="away"]//a') %>% html_text()
    
    # get positions
    positions <- page %>% html_nodes(".goals+ .lineup .position") %>% html_text() %>% as.character()
    if (length(positions)==0){positions <- page %>% html_nodes(".headers+ .lineup .position") %>% html_text() %>% as.character()}
    positions_short <- tolower(substring(trimws(positions),1,1))
    home_positions <- positions_short[1:11]
    away_positions <- positions_short[12:22]
    
    
    # extracting club name and starting player names from above vector
    home_name <- home_team[1]
    away_name <- away_team[1]
    
    # getting the names correct should be done
    #home_name <- team.names[which(home_name==url.names)]
    #away_name <- team.names[which(away_name==url.names)]
    
    home_starting11 <- home_team[2:12]
    away_starting11 <- away_team[2:12]
  
    
    # Cbinding the position and the name together to be able to add empty cells for non used positions
    positions_all <- cbind(positions_short,c(home_starting11,away_starting11),c(rep('h',11),rep('a',11)))
    positions_all <- as.data.frame(positions_all)
    colnames(positions_all) <- c("position","player","home_away")
    
  # getting the names correct for furter use
    home_goal = positions_all[positions_all$position == "g" & positions_all$home_away == "h",]
    if (nrow(home_goal) > 0){home_goal$nr = c(1:nrow(home_goal))
    }else{home_goal$nr = NULL}
    
    away_goal = positions_all[positions_all$position == "g" & positions_all$home_away == "a",]
    if (nrow(away_goal) > 0){away_goal$nr = c(1:nrow(away_goal))
    }else{away_goal$nr = NULL}
    
    home_def = positions_all[positions_all$position == "d" & positions_all$home_away == "h",]
    if (nrow(home_def) > 0){home_def$nr = c(1:nrow(home_def))
    }else{home_def$nr = NULL}
    
    away_def = positions_all[positions_all$position == "d" & positions_all$home_away == "a",]
    if (nrow(away_def) > 0){away_def$nr = c(1:nrow(away_def))
    }else{away_def$nr = NULL}
    
    home_mid = positions_all[positions_all$position == "m" & positions_all$home_away == "h",]
    if (nrow(home_mid) > 0){home_mid$nr = c(1:nrow(home_mid))
    }else{home_mid$nr = NULL}
    
    away_mid = positions_all[positions_all$position == "m" & positions_all$home_away == "a",]
    if (nrow(away_mid) > 0){away_mid$nr = c(1:nrow(away_mid))
    }else{away_mid$nr = NULL}
    
    home_for = positions_all[positions_all$position == "f" & positions_all$home_away == "h",]
    if (nrow(home_for) > 0){home_for$nr = c(1:nrow(home_for))
    }else{home_for$nr = NULL}
    
    away_for = positions_all[positions_all$position == "f" & positions_all$home_away == "a",]
    if (nrow(away_for) > 0){away_for$nr = c(1:nrow(away_for))
    }else{away_for$nr = NULL}
      
      
    starting_11 <- rbind(home_goal
                         ,home_def
                         ,home_mid
                         ,home_for
                         ,away_goal
                         ,away_def
                         ,away_mid
                         ,away_for)
    starting_11$pos <- paste0(starting_11$home_away,starting_11$position,starting_11$nr)
  
    
    # Cleaning names
    starting_11$player <- as.character(starting_11$player)
    Encoding(starting_11$player) <- "UTF-8"
    starting_11$player <- iconv(trimws(gsub("ð","d",starting_11$player)), from = "UTF-8", to="ASCII//TRANSLIT")
    
    
    # Getting the date of the match
    raw_match_details <- page %>% html_nodes(xpath='//div[@class="match-report"]//h1') %>% html_text()
    raw_match_date <- sub("^.*\\,", "", raw_match_details)
    
    months_all=c("January","February","March","April","May","June","July","August","September","October","November","December")
    month=as.numeric(which(sapply(months_all, function(x) grepl(x, raw_match_date))))
    
    day_year_time=gsub('\\D','',raw_match_date)
    day=substr(day_year_time,1,2)
    year=substr(day_year_time,3,6)
    
    # putting day month and year together in date format equal to the one in the db
    match_date=as.Date(format(as.Date(paste(year,month,day, sep = "-"),"%Y-%m-%d")))
    season = ifelse(strftime(match_date,format = "%V") < 26, paste0(as.numeric(format(match_date,"%Y"))-1,as.numeric(format(match_date,"%Y"))),paste0(as.numeric(format(match_date,"%Y")),as.numeric(format(match_date,"%Y"))+1))
    match_date = as.character(match_date)
    
    lineup = c(match_date,season,home_name,away_name,starting_11$player)
    lineup = as.data.frame(t(lineup))
    colnames(lineup) <- c("match_date","season","hometeam","awayteam",starting_11$pos)
    match_lineups <- bind_rows(match_lineups, lineup)
    # match_lineups2 = merge(match_lineups2,lineup,by="col.names",all=TRUE)
  }
  print(match_lineups$season[nrow(match_lineups)])
}

# Get the name of the teams as we have in the db
urlteams=c('Arsenal FC','Aston Villa','Birmingham City','Blackburn Rovers','Blackpool FC','Bolton Wanderers','AFC Bournemouth','Bradford','Brighton & Hove Albion','Burnley FC','Cardiff City','Charlton Athletic','Chelsea FC','Coventry','Crystal Palace','Derby County','Everton FC','Fulham FC','Huddersfield Town','Hull City','Ipswich','Leeds','Leicester City','Liverpool FC','Manchester City','Manchester United','Middlesbrough FC','Newcastle United','Norwich City','Portsmouth FC','Queens Park Rangers','Reading FC','Sheffield United','Southampton FC','Stoke City','Sunderland AFC','Swansea City','Tottenham Hotspur','Watford FC','West Bromwich Albion','West Ham United','Wigan Athletic','Wolverhampton Wanderers','Wimbledon')
urlteams2=c('Arsenal FC','Aston Villa','Birmingham City','Blackburn Rovers','Blackpool FC','Bolton Wanderers','AFC Bournemouth','Bradford','Brighton & Hove Albion','Burnley FC','Cardiff City','Charlton Athletic','Chelsea FC','Coventry','Crystal Palace','Derby County','Everton FC','Fulham FC','Huddersfield Town','Hull City','Ipswich','Leeds','Leicester City','Liverpool FC','Manchester City','Manchester United','Middlesbrough FC','Newcastle United','Norwich City','Portsmouth FC','Queens Park Rangers','Reading FC','Sheffield United','Southampton FC','Stoke City','Sunderland AFC','Swansea City','Tottenham Hotspur','Watford FC','West Bromwich Albion','West Ham United','Wigan Athletic','Wolverhampton Wanderers','Wimbledon')
team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
team.names2=c("Arsenal","Aston","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man.*City","Man.*U","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Queen","Reading","Sheffield","Southampton","Stoke","Sunderland","Swansea","Tottenham","spurs","Watford","West Brom","West Ham","Wigan","Wolve","Wimbledon")

match_home_teams <- sapply(tolower(team.names2),function(x) regexpr(x,tolower(match_lineups$hometeam)))
match_away_teams <- sapply(tolower(team.names2),function(x) regexpr(x,tolower(match_lineups$awayteam)))

for (n in 1:nrow(match_lineups)){
  match_lineups$hometeam[n]=team.names[which(match_home_teams[n,]>=1)]
  match_lineups$awayteam[n]=team.names[which(match_away_teams[n,]>=1)]
}


#delete the first row with only zero's caused by the creation of the vector in the first place
match_lineups <- match_lineups[match_lineups$match_date!="",]
write.csv(match_lineups,"hist_match_lineups_with_position.csv")

