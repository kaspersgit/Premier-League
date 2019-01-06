# Using the fifa scores per player historically
library(rvest)
library(curl)

# https://www.fifaindex.com/teams/fifa19_15/?league=13
# the number 15 in this case can range from 1 to 300 at the moment, it determines at which point at time we look
# The first fifa's only looked at the beginning of the season, later on also halfway the season and every MW
# https://www.fifaindex.com/teams/fifa19_1/?league=13 -> Fifa 05 8 october 2004
# https://www.fifaindex.com/teams/fifa19_2/?league=13 -> Fifa 06 24 augustus 2005
# https://www.fifaindex.com/teams/fifa19_3/?league=13 -> Fifa 07 28 augustus 2006
# https://www.fifaindex.com/teams/fifa19_4/?league=13 -> Fifa 08
# https://www.fifaindex.com/teams/fifa19_5/?league=13 -> Fifa 09
# https://www.fifaindex.com/teams/fifa19_6/?league=13 -> Fifa 10
# https://www.fifaindex.com/teams/fifa19_7/?league=13 -> Fifa 11
# https://www.fifaindex.com/teams/fifa19_8/?league=13 -> Fifa 12 first half
# https://www.fifaindex.com/teams/fifa19_9/?league=13 -> Fifa 12 second half
# https://www.fifaindex.com/teams/fifa19_10/?league=13 -> Fifa 13 start of next season
# https://www.fifaindex.com/teams/fifa19_11/?league=13 -> Fifa 13 start season
# https://www.fifaindex.com/teams/fifa19_12/?league=13 -> Fifa 14 start of  season
# https://www.fifaindex.com/teams/fifa19_13/?league=13 -> Fifa 14 start of next season 
# https://www.fifaindex.com/teams/fifa19_16/?league=13 -> Fifa 15 start of season
# https://www.fifaindex.com/teams/fifa19_15/?league=13 -> Fifa 15 end of season 
# https://www.fifaindex.com/teams/fifa19_19/?league=13 -> Fifa 16 start of season 
# https://www.fifaindex.com/teams/fifa19_40/?league=13 -> Fifa 16 start of second half season 
# https://www.fifaindex.com/teams/fifa19_75/?league=13 -> Fifa 17 start of season 
# https://www.fifaindex.com/teams/fifa19_108/?league=13 -> Fifa 17 start of second half season 
# https://www.fifaindex.com/teams/fifa19_175/?league=13 -> Fifa 18 start of season 
# https://www.fifaindex.com/teams/fifa19_211/?league=13 -> Fifa 18 start of second half season 
# https://www.fifaindex.com/teams/fifa19_280/?league=13 -> Fifa 19 start of season 

# which links are giving the good moment in time
moments <- c(1:9,11,12,15,16,19,40,75,108,175,211,280)
# Premier league 13
league = 13 

# let it run for every season 
for (s in moments){
  # Overview of the teams in te league in certain year, used for the links to team pages
  league_url <- paste0("https://www.fifaindex.com/teams/fifa19_",s,"/?league=",league)
  
  
    # read html file of page
    league_html <- read_html(league_url)
  
  # Date of stats
  stats_date <- league_html %>% html_nodes(".dropdown+ .dropdown .dropdown-toggle") %>% html_text() %>% as.character()
  stats_date <- trimws(stats_date)
  if (substring(stats_date,1,4)=="Sept"){stats_date = paste0(substring(stats_date,1,3),substring(stats_date,5,nchar(stats_date)))}
  stats_date <- as.Date(stats_date, "%b. %d, %Y")
  
  # Get the season based on the date of the stats
  season = ifelse(strftime(stats_date,format = "%V") < 26, paste0(as.numeric(format(stats_date,"%Y"))-1,as.numeric(format(stats_date,"%Y"))),paste0(as.numeric(format(stats_date,"%Y")),as.numeric(format(stats_date,"%Y"))+1))
  season_part = ifelse(strftime(stats_date,format = "%V") < 26,2,1)
  season = paste0(season,"_",season_part)
  
  #extract the links to the team pages
  teams_urls <- league_html %>% html_nodes("td+ td .link-team") %>% html_attr("href") %>% as.character()
  
  # assigning value to a name
  assign(paste0("players_value_",season),setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("season", "stats_date", "team", "player", "fifa_score")))
  
  # Loop over the different team pages
  for (team_url in teams_urls){
    # paste the team links behind the main site adress 
    url <- paste0("https://www.fifaindex.com",team_url)
    
    # read html file of page
    html_url <- read_html(url)
    
    # extract club name
    team_name <- html_url %>% html_nodes("h1") %>% html_text() %>% as.character()
    
    # extract the player names and current market value
    players_name <- html_url %>% html_nodes("td:nth-child(6) .link-player") %>% html_text() %>% as.character()
    players_values <- html_url %>% html_nodes(".row+ .table-rounded .rating:nth-child(1)") %>% html_text() %>% as.character()
    
    team <- rep(team_name,length(players_name))
    
    players_values_team <- cbind(season,stats_date,team,players_name,players_values)
    colnames(players_values_team) <- c("season","stats_date","team","name","fifa_score")
    assign(paste0("players_value_",season),rbind(get(paste0("players_value_",season)),players_values_team))
  }
}

# get all seasons in one table
player_values_raw = rbind(players_value_20052006_1,
                          players_value_20062007_1,
                          players_value_20072008_1,
                          players_value_20082009_1,
                          players_value_20092010_1,
                          players_value_20102011_1,
                          players_value_20112012_1,
                          players_value_20112012_2,
                          players_value_20122013_1,
                          players_value_20132014_1,
                          players_value_20142015_1,
                          players_value_20152016_1,
                          players_value_20152016_2,
                          players_value_20162017_1,
                          players_value_20162017_2,
                          players_value_20172018_1,
                          players_value_20172018_2,
                          players_value_20182019_1)

urlteams=c('Arsenal FC','Aston Villa','Birmingham City','Blackburn Rovers','Blackpool FC','Bolton Wanderers','AFC Bournemouth','Bradford','Brighton & Hove Albion','Burnley FC','Cardiff City','Charlton Athletic','Chelsea FC','Coventry','Crystal Palace','Derby County','Everton FC','Fulham FC','Huddersfield Town','Hull City','Ipswich','Leeds','Leicester City','Liverpool FC','Manchester City','Manchester United','Middlesbrough FC','Newcastle United','Norwich City','Portsmouth FC','Queens Park Rangers','Reading FC','Sheffield United','Southampton FC','Stoke City','Sunderland AFC','Swansea City','Tottenham Hotspur','Watford FC','West Bromwich Albion','West Ham United','Wigan Athletic','Wolverhampton Wanderers','Wimbledon')
urlteams2=c('Arsenal FC','Aston Villa','Birmingham City','Blackburn Rovers','Blackpool FC','Bolton Wanderers','AFC Bournemouth','Bradford','Brighton & Hove Albion','Burnley FC','Cardiff City','Charlton Athletic','Chelsea FC','Coventry','Crystal Palace','Derby County','Everton FC','Fulham FC','Huddersfield Town','Hull City','Ipswich','Leeds','Leicester City','Liverpool FC','Manchester City','Manchester United','Middlesbrough FC','Newcastle United','Norwich City','Portsmouth FC','Queens Park Rangers','Reading FC','Sheffield United','Southampton FC','Stoke City','Sunderland AFC','Swansea City','Tottenham Hotspur','Watford FC','West Bromwich Albion','West Ham United','Wigan Athletic','Wolverhampton Wanderers','Wimbledon')
team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
team.names2=c("Arsenal","Aston","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man.*City","Man.*U","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Queen","Reading","Sheffield","Southampton","Stoke","Sunderland","Swansea","Tottenham","spurs","Watford","West Brom","West Ham","Wigan","Wolve","Wimbledon")

player_values_raw$team = as.character(player_values_raw$team)
match_teams <- sapply(tolower(team.names2),function(x) regexpr(x,tolower(player_values_raw$team)))


for (n in 1:nrow(player_values_raw)){
  player_values_raw$team[n]=team.names[which(match_teams[n,]>=1)]
}

# Get the seasons correct again for later use
player_values_raw$season = as.integer(substring(player_values_raw$season,1,8))

# Cleaning names
Encoding(player_values_raw$name) <- "UTF-8"
player_values_raw$name <- iconv(trimws(gsub("ð","d",player_values_raw$name)), from = "UTF-8", to="ASCII//TRANSLIT")

# Take the last updated number per season
# order data by first the new stats
player_values_raw <- player_values_raw[order(player_values_raw$stats_date, decreasing=TRUE),]

# Delete duplicates (keep the first one/most updated one)
player_values_raw <- player_values_raw [!duplicated(player_values_raw[c(1,3,4)]),]

# save as csv
write.csv(player_values_raw,"fifa_scored_players_v2.csv")
