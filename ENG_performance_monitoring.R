setwd("D:/Het Project/Voetbal predictions/Premier-League")
library(rvest)
library(XML)
library(RCurl)
library("RSQLite")
library(DBI)
library("caret") 
library(lubridate)

# connect to the sqlite db
con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")

# Check how many games of season 2018-2019 are already in the db 
predicted_games <- dbGetQuery(con,"SELECT 
	                                        date(pred.MatchDate) as matchdate
                                         , pred.HomeTeam
                                         , pred.AwayTeam
                                         , hist.FTR 
                                         , CASE 
                                         WHEN PredictedOutcome = pred.HomeTeam THEN 'H'
                                         WHEN PredictedOutcome = pred.AwayTeam THEN 'A'
                                         ELSE 'D'
                                         END pred_FTR
                                         , pred.best_ratio_outcome
                                         , pred.best_ratio
                                         , pred.Homeodd
                                         , pred.Drawodd
                                         , pred.Awayodd
                                         , pred.BF_H_odds
                                         , pred.BF_D_odds
                                         , pred.BF_A_odds
                                         FROM 
                                         ENG_match_prediction AS pred
                                         INNER JOIN 
                                         ENG_matches_hist AS hist 
                                         ON date(pred.MatchDate)=hist.`Date` 
                                         AND pred.HomeTeam = replace(hist.HomeTeam,' ','')
                                         AND pred.AwayTeam = replace(hist.AwayTeam,' ','')
                                         
                                         ")

# close connection 
dbDisconnect(con)

confusionMatrix(factor(predicted_games$pred_FTR),
                factor(predicted_games$FTR))

# add some extra columns
for (i in 1:nrow(predicted_games)){
  predicted_games$winning_odds[i] = predicted_games[i,paste0('BF_',predicted_games$FTR[i],'_odds')]
  predicted_games$pred_odds[i] = predicted_games[i,paste0('BF_',predicted_games$pred_FTR[i],'_odds')]
  predicted_games$ratio_odds[i] = predicted_games[i,paste0('BF_',predicted_games$best_ratio_outcome[i],'_odds')]
  
  
  predicted_games$month[i] = substr(predicted_games$matchdate[i],1,7)
}

predicted_games$pred_correct = predicted_games$FTR == predicted_games$pred_FTR
predicted_games$ratio_correct = predicted_games$FTR == predicted_games$best_ratio_outcome

predicted_games$pred_cost = 10/predicted_games$pred_odds
predicted_games$ratio_cost = 10/predicted_games$ratio_odds

predicted_games$revenue = predicted_games$pred_odds * 0 + 10

# betting on all predicted outcomes

calc_profit <- function(strategy){
  if(grepl('pred',strategy)){
    strategy_outcome = 'pred_FTR'
    strategy_odds = 'pred_odds'
  }else if(grepl('ratio',strategy)){
    strategy_outcome = 'best_ratio_outcome'
    strategy_odds = 'ratio_odds'
  }
  revenue = sum(predicted_games[,strategy_outcome]==predicted_games$FTR)*10
  cost = sum(ifelse(predicted_games[,strategy_outcome]==predicted_games[,strategy_outcome],10,0)/predicted_games[,strategy_odds])
  profit = revenue - cost
  profit_rate = profit / cost
  return(cat("Betting summary\n",
                  "Betting cost: \t",cost, "\n",
                  "Revenue: \t",revenue, "\n",
                  "Profit total\t",profit,"\n",
                  "Profit rate\t",profit_rate))
}

calc_profit('strategy')


# get over time
strategy = 'prediction'
min_ratio = 1.05
max_ratio = 1.7
{
if(grepl('pred',strategy)){
  strategy_correct = 'pred_correct'
  strategy_outcome = 'pred_FTR'
  strategy_odds = 'pred_odds'
  strategy_cost = 'pred_cost'
  plot_data <- predicted_games[-which(predicted_games$pred_FTR %in% c('D','H')),c(strategy_correct,strategy_cost,'month')]
}else if(grepl('ratio',strategy)){
  strategy_correct = 'ratio_correct'
  strategy_outcome = 'best_ratio_outcome'
  strategy_odds = 'ratio_odds'
  strategy_cost = 'ratio_cost'
  plot_data <- predicted_games[-which(predicted_games$pred_FTR %in% c('D','H') | predicted_games$best_ratio > max_ratio | predicted_games$best_ratio < min_ratio)
                               ,c(strategy_correct,strategy_cost,'month')]
}

plot_data$pred_rev <- plot_data[,strategy_correct] * 10
plot_data$pred_profit <- plot_data$pred_rev - plot_data[,strategy_cost]

aggdata <-aggregate(plot_data[,c(strategy_correct,'pred_rev',strategy_cost,'pred_profit')], by=list(plot_data$month), 
                    FUN=sum, na.rm=TRUE)
names(aggdata) = c('month',strategy_correct,'pred_rev',strategy_cost,'pred_profit')
aggdata$cum_pred_profit <- cumsum(aggdata$pred_profit)

# plot profit and cumulative profit
ggplot(data=aggdata, aes(x=month, y=cum_pred_profit, group=1, colour = "cum. profit")) +
  geom_line()+
  geom_point()+
  geom_line(aes(y = pred_profit, colour = "profit"))
}
