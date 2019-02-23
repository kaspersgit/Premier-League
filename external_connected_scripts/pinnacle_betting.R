give_pinnacle_odds <- function(country = 'ENG',pwd){
  library(pinnacle.API)
  library(httr)
  library(jsonlite)
  library(askpass)
  AcceptTermsAndConditions(accepted = TRUE)
  
  user <- "KD1049991"
  #pwd <- askpass()
  
  base <- "https://api.pinnaclesports.com/"
  
  # using the api pinnacle package
  SetCredentials(user, pwd)
  
  # Get Sports
  sport_data <- GetSports()
  # Get Soccer id - 29
  soccer_id <- with(sport_data, id[name == 'Soccer'])
  # Get league id
  # Swedish allsvenska 1728
  # English Premier league 1980
  #with(GetLeaguesByID(29), leagues.id[grepl("england - premier league",tolower(leagues.name))])
  
  eng_pl_id <- with(GetLeaguesByID(29), leagues.id[leagues.name=="England - Premier League"])
  
  # Get eventid
  
  # Get Odds
  soccer_data <- showOddsDF(soccer_id, leagueids=1980 ,tableformat = "mainlines",oddsformat = "DECIMAL")
  
  # Translating to our models output structure
  soccer_data <- soccer_data[soccer_data$events.periods.number==0,]
  
  structured_odds <- soccer_data[,c('leagues.name','events.periods.cutoff','league.events.home','league.events.away'
                                    ,'periods.moneyline.home','periods.moneyline.draw','periods.moneyline.away')]
  # rename the columns
  names(structured_odds) <- c('league','Date','HomeTeam','AwayTeam','P_H_odds','P_D_odds','P_A_odds')
  
  # To get team names similar to already used teamnames in main script
  p.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton and Hove Albion","Burnley","Cardiff City","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield Town","Hull","Ipswich","Leeds","Leicester City","Liverpool","Manchester City","Manchester United","Middlesbrough","Newcastle United","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham Hotspur","Watford","West Brom","West Ham United","Wigan","Wolverhampton Wanderers","Wimbledon")
  team.names=c("Arsenal","AstonVilla","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","CrystalPalace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","ManCity","ManUnited","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","SheffieldUnited","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","WestBrom","WestHam","Wigan","Wolves","Wimbledon")
  for (i in 1:nrow(structured_odds)){
    structured_odds$HomeTeam[i]=team.names[which(p.names==structured_odds$HomeTeam[i])]
    structured_odds$AwayTeam[i]=team.names[which(p.names==structured_odds$AwayTeam[i])]
  }
  
  # Placing a bett (BE CAREFULL WITH THIS)
  # periodNumber represents the period of the match. For example, for soccer we have 0 (Game), 1 (1st Half) & 2 (2nd Half)
  # PlaceBet(
  #   stake = 100/4.85, 
  #   sportId = 29,
  #   eventId = 901498168,
  #   periodNumber = 0,
  #   lineId = 598210299,
  #   betType = 'MONEYLINE',
  #   team = 'TEAM1'
  # )
  
  # take a look at kelly criterion for betting
  return(structured_odds)
}
