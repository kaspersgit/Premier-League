setwd("D:/Het Project/Voetbal predictions/Premier-League")
library(neuralnet)
library(stringdist)
library("caret")
# Get the outcome for every match linked to the lineup


# SQLite connection
library("RSQLite")
library(DBI)
library("qlcMatrix")
# connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname="historic_data/football.db")
main_data <- dbGetQuery(con,'SELECT 
                        emh.Date
                        , emh.HomeTeam
                        , emh.AwayTeam
                        , emh.season
                        , emh.FTR
                        , o.IWH
                        , o.IWD
                        , o.IWA
                        
                        FROM 
                        ENG_matches_hist AS emh
                        INNER JOIN 
                        ENG_match_odds AS o 
                        ON o.HomeTeam = emh.HomeTeam 
                        AND o.AwayTeam = emh.AwayTeam
                        AND o.Date = emh.Date')

dbDisconnect(con)

hist_lineups = read.csv("hist_match_lineups_with_position.csv")

hist_lineups_with_result = merge(hist_lineups,main_data,by.y = c('Date','HomeTeam','AwayTeam','season'), by.x = c('match_date','hometeam','awayteam','season'))
hist_lineups_with_result = hist_lineups_with_result[,c(names(hist_lineups),'FTR','IWH','IWD','IWA')]
hist_lineups_with_result = hist_lineups_with_result[, colSums(is.na(hist_lineups_with_result)) != nrow(hist_lineups_with_result)]

# Delete everything before and including 20072008
hist_lineups_with_result = hist_lineups_with_result[hist_lineups_with_result$season > 20072008,]
hist_lineups_with_result$hometeam = as.character(hist_lineups_with_result$hometeam)
hist_lineups_with_result$awayteam = as.character(hist_lineups_with_result$awayteam)

# Get the line up translated into fifa scores (matching the names)
players_scores = read.csv("yearly_updated_data/fifa_scored_players_v2.csv")
players_scores <- players_scores[players_scores$season > 20072008,]
players_scores$name <- as.character(players_scores$name)
players_scores$team <- as.character(players_scores$team)

# Getting the best match 
ClosestMatch = function(string, stringVector){
  if (!is.na(string)){
    if (max(grepl(string,stringVector))==1){
      stringVector <- stringVector[grepl(string,stringVector)]
    }else if(max(grepl(paste(stringVector,collapse = "|"),string))==1){
      vect_match <- vector(mode = 'logical',length = length(stringVector))
      for (m in 1:length(stringVector)){
        vect_match[m] <- grepl(stringVector[m],string)
      }
      stringVector <- stringVector[vect_match]
    }
    vect_match <- vector(mode = 'numeric',length = length(stringVector))
    for (i in 1:length(stringVector)){
      dist_match <- stringdist(tolower(string),tolower(stringVector[i]))/nchar(paste0(string,stringVector[i]))
      vect_match[i] <- dist_match
    }
    matching_name <- stringVector[min(vect_match)==vect_match]
  }else{
    matching_name <- NA
  }
  if(length(matching_name) > 1){matching_name = 'unfindable'}
  return(matching_name)
}

# get the column names which consist of the player names
pos_names <- colnames(hist_lineups_with_result)[grepl('^[a-z][a-z][0-9]$',names(hist_lineups_with_result))]

# Find the best match for each player
for (i in 1:nrow(hist_lineups_with_result)){
  hometeam <- hist_lineups_with_result$hometeam[i]
  awayteam <- hist_lineups_with_result$awayteam[i]
  season <- hist_lineups_with_result$season[i]
  
  for (n in pos_names){
    if(substring(n,1,1)=='h'){team=hometeam}else{team=awayteam}
    hist_lineups_with_result[i,paste0(n,'_match')] <- ClosestMatch(hist_lineups_with_result[i,n],players_scores[players_scores$team==team & players_scores$season == season,'name'])
  }
}


# Connect the matched names to the fifa score values
clean_dataset <- as.data.frame(hist_lineups_with_result[,c('match_date','FTR','IWH','IWD','IWA')])
pos_match <- names(hist_lineups_with_result)[grepl('_match',names(hist_lineups_with_result))]
for (i in 1:nrow(hist_lineups_with_result)){
  hometeam <- hist_lineups_with_result$hometeam[i]
  awayteam <- hist_lineups_with_result$awayteam[i]
  season <- hist_lineups_with_result$season[i]
  lookup_teams <- players_scores[players_scores$team %in% c(hometeam,awayteam) & players_scores$season == season,]
  
  for (n in pos_match){
    if(substring(n,1,1)=='h'){team=hometeam}else{team=awayteam}
    lineup_name = hist_lineups_with_result[i,n]
    if (is.na(lineup_name)){score_values=0
    }else if(lineup_name == 'unfindable'){score_values = -1
    }else{score_values = lookup_teams[lookup_teams$name == lineup_name & lookup_teams$team == team,'fifa_score']}
    clean_dataset[i,paste0(substring(n,1,3),'_score')] <- score_values
  }
}

# The unfindable players will get a score equal to the average of the team
pos_score <- names(clean_dataset)[grepl('_score',names(clean_dataset))]
for (i in 1:nrow(clean_dataset)){
  for (n in pos_score){
    if(clean_dataset[i,n] == -1){
      avg_pos <- names(clean_dataset)[substring(names(clean_dataset),1,1)=='h']
      team_scores <- clean_dataset[i,avg_pos]
      team_scores <- team_scores[team_scores > 0]
      est_fifa_score <- round(mean(team_scores))
      clean_dataset[i,n] <- est_fifa_score
    }
  }
}

# prepare for neural network training
# split up FTR column into multiple boolean columns
clean_dataset$FTR_H <- ifelse(clean_dataset$FTR == 'H',1,0)
clean_dataset$FTR_A <- ifelse(clean_dataset$FTR == 'A',1,0)
clean_dataset$FTR_D <- ifelse(clean_dataset$FTR == 'D',1,0)

# Save clean dataset for use in the keras script
write.csv(clean_dataset,"clean_dataset_with_fifascores.csv")

clean_dataset <- clean_dataset[,!(colnames(clean_dataset) %in% c("match_date"))]
index <- sample(1:nrow(clean_dataset),round(0.75*nrow(clean_dataset)))


train <- clean_dataset[index,!(colnames(clean_dataset) %in% c("FTR"))]
test <- clean_dataset[-index,]
lm.fit <- glm(FTR_H~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

cols = names(clean_dataset)[grepl('_score',names(clean_dataset))]
clean_dataset[cols] = scale(clean_dataset[cols])

# Scaling action can make columns NaN (happens because 0/0), which don't work with neural network
#clean_dataset <- data.frame(matrix(unlist(clean_dataset), nrow=nrow(clean_dataset), byrow=T),stringsAsFactors=FALSE)
for (r in 1:nrow(clean_dataset)){
  for (c in 1:ncol(clean_dataset)){
    if (is.nan(clean_dataset[r,c])){
      clean_dataset[r,c] <- 0
    }
  }
}

# Training Neural network model (use https://www.datacamp.com/community/tutorials/keras-r-deep-learning)


# Training Neural network model using neuralnet package
n <- names(train)
p <- "FTR_H + FTR_A + FTR_D ~"
f <- as.formula(paste(p, paste(n[!n %in% c("FTR_H","FTR_A","FTR_D","FTR")], collapse = " + ")))

# ReLu = max(x,0)
relu <- function(x) {max(x,0)}
custom <- function(x) {log(1+exp(x))}
custom <- function(x) {x/(1+exp(-2*100*x))}
nn <- neuralnet(f,data=train,hidden=c(10,5), stepmax = 300000, lifesign = "full", threshold = 0.01,
linear.output=T,act.fct = "logistic")
plot(nn)

# Validating model
#Test the resulting output
head(test)
nn.results <- compute(nn, test[,n[!n %in% c("FTR_H","FTR_A","FTR_D","FTR")]])

names(nn.results$net.result) <- c("H","A","D")
prediction <- names(nn.results$net.result)[apply(nn.results$net.result, 1, which.max)] 
results <- data.frame(actual = test[,c("FTR")], prediction = prediction)

confusionMatrix(results$actual, results$prediction)

# to calculate profits made using Inter Wetten odds

