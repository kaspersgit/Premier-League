give_bf_odds <- function(country){
  if (country=="ENG"){
    country="GB"
    competitionId = 10932509
  }else if (country=="DE"){
    competitionId = 59
  }
  
  
  # install.packages("devtools")
  # devtools::install_git("https://gitlab.com/phillc73/abettor.git")
  library("abettor")
  # Requires a minimum of version 1.95-4.3
  require("RCurl")
  # Requires a minimum of version 0.9.12
  require("jsonlite")
  
  # tutorial
  #https://github.com/phillc73/abettor/blob/master/vignettes/abettor-placeBet.Rmd 
  
  loginBF(username = "moneymanchezz@hotmail.com", password = "air1992Dwizgh?!", applicationKey = "FOzz0vtLaJpIe25v")
  # listEventTypes() -- football is eventType.id = 1
  # listCountries(eventTypeIds = "1") -- UK: countryCode GB  Germany: DE
  #listMarketTypes(eventTypeIds = "1")
  
  # use fromDate and toDate to specify search  to specific period (from now until one week from now)
  marketCat <- listMarketCatalogue(eventTypeIds = "1", marketCountries = country, marketTypeCodes = "MATCH_ODDS" , competitionIds = competitionId,
                                     fromDate = (format(Sys.time(), "%Y-%m-%dT%TZ")),
                                     toDate = (format(Sys.time() + 604800, "%Y-%m-%dT%TZ")))

  


  bf_oddstable <- data.frame(Home=character(),
                                Away=character(),
                                H_odds=double(),
                                D_odds=double(),
                                A_odds=double(),
                                stringsAsFactors=FALSE)
  
  # per match, length(marketCat[,1]) gives how many matches ahead, now depending on the actual games in the following week
  if (nrow(marketCat)!=0){
    for (i in 1:length(marketCat[,1])){
      
      ourSpecificRace <- marketCat[i,]
      
      # selecting home and away team 
      ourMarketId=ourSpecificRace$marketId
      teams=ourSpecificRace$runners[[1]]
      homeid=teams$selectionId[1]
      awayid=teams$selectionId[2]
      drawid=teams$selectionId[3]
      hometeam=ourSpecificRace$runners[[1]]$runnerName[1]
      awayteam=ourSpecificRace$runners[[1]]$runnerName[2]
      
      # To get team names similar to already used teamnames in main script
      BF.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","C Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man Utd","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
      team.names=c("Arsenal","AstonVilla","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","CrystalPalace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","ManCity","ManUnited","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","WestBrom","WestHam","Wigan","Wolves","Wimbledon")
      hometeam=team.names[which(BF.names==hometeam)]
      awayteam=team.names[which(BF.names==awayteam)]
      
      # getting time of the match
      MatchTime <- ourSpecificRace$event$openDate
      
      # looking for the prices
      ourMarketIdPrices <- listMarketBook(marketIds = ourMarketId, priceData = "EX_ALL_OFFERS")
      allRunnersPrices <- ourMarketIdPrices$runners
      homeIdPrices <- allRunnersPrices[[1]][which(allRunnersPrices[[1]]$selectionId == homeid),]
      drawIdPrices <- allRunnersPrices[[1]][which(allRunnersPrices[[1]]$selectionId == drawid),]
      awayIdPrices <- allRunnersPrices[[1]][which(allRunnersPrices[[1]]$selectionId == awayid),]
      
      # get Back odds and sizes (not lay)
      homeIdPricesDF <- data.frame(homeIdPrices$ex[[1]])
      drawIdPricesDF <- data.frame(drawIdPrices$ex[[1]])
      awayIdPricesDF <- data.frame(awayIdPrices$ex[[1]])
      
      # get best odds
      bf_hodds <- homeIdPricesDF[1,1]
      bf_dodds <- drawIdPricesDF[1,1]
      bf_aodds <- awayIdPricesDF[1,1]
      
      # create dataframe with BF odds
      match_details=data.frame(hometeam,awayteam,bf_hodds,bf_dodds,bf_aodds,MatchTime)
      names(match_details)=c("HomeTeam","AwayTeam","BF_H_odds","BF_D_odds","BF_A_odds","Date")
      
      # add this as a row to the created dataframe
      bf_oddstable <- rbind(bf_oddstable,match_details)
    }
  }else{bf_oddstable=0}
  
  return(bf_oddstable)
}



