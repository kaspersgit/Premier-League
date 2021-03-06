setwd("D:/Het Project/Voetbal predictions/Premier-League")


used.packages=c("xgboost","stringr","dplyr","caret","DiagrammeR","qlcMatrix","e1071","jsonlite","RSQLite","DBI","digest","rvest","abettor","XML","RCurl")
not.installed=!(used.packages %in% rownames(installed.packages()))
if(length(used.packages[not.installed])>0){
  install.packages(used.packages[not.installed])
}

library("xgboost")  # the main algorithm
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("dplyr")    # for some data preperation
library("stringr")
library("DiagrammeR")
library("qlcMatrix")
library("RSQLite")
library("DBI")
library("askpass")

if(!exists("foo", mode="function")) source("external_connected_scripts/ENG_db_updating.R")
if(!exists("foo", mode="function")) source("external_connected_scripts/ENG_exp_lineups_V2.R")
if(!exists("foo", mode="function")) source("external_connected_scripts/ENG_exp_lineups.R")
if(!exists("foo", mode="function")) source("external_connected_scripts/ENG_db_connection.R")
if(!exists("foo", mode="function")) source("external_connected_scripts/pinnacle_betting.R")
if(!exists("foo", mode="function")) source("external_connected_scripts/ENG_db_save_prediction.R")
if(!exists("foo", mode="function")) source("ENG_cleaningandpreparing.R")

# amount of teams per season in the highest league of the country
n_teams = 20

# get odds for coming matches from pinnacle
pinnacle_odds = give_pinnacle_odds(country = 'ENG',pwd = askpass())

# Based on if there is a match within a week according to BetFair then run script
# Should look into using pinnacle API so bets can be made automatically
if (nrow(pinnacle_odds)!=0){
  
  # Before starting the ENG_db_updating function we need to make sure the internet connection is good
  #If not this will end in an error halfway updating the DB, meaning we have to delete rows in some tables manually
  # First update the db
  ENG_db_updating(n_teams)
  
  # import and prepare the data and eventually save it as csv
  ENG_preparation(n_teams,FALSE)
  
  # download that csv for further use and prediction
  dataf=read.csv("ENG_final_dataset.csv")
  
  # Separate into feature set and target variable
  #FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
  x_all = dataf[,-which(names(dataf)=='FTR')]
  y_all = dataf['FTR']
  
  #Standardising the data
  #Center to the mean and component wise scale to unit variance.
  cols = c('HTGD','ATGD','HTP','ATP','DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV','HTS','ATS','HTST','ATST','HM3','AM3','HM5','AM5','HM10','AM10','HMH1','AMA1')
  x_all[cols] = scale(x_all[cols])
  
  x_featured=x_all[,c('HTP', 'ATP','HTGD', 'ATGD',
                      "DiffPts",'HM3','AM3','HM5','AM5','HM10','AM10','HMH1','AMA1', 
                      'DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV',
                      'home_start_mv','away_start_mv', 
                      'HTS','ATS','HTST','ATST')]
  
  df=cbind(x_featured,y_all)
  
  # FTR naar cijfers converteren, beginnend met 0
  df$FTRC=ifelse(df$FTR=="H",0,ifelse(df$FTR=="D",1,2))
  dat=df[-which(names(df)=="FTR")]
  
  # set seed for reproducibility
  set.seed(999)
  # Make split index based on the last ten rows are the coming matches
  fixtures.coming=c((nrow(dat)-(n_teams/2-1)):nrow(dat))
  train_index <- dat[-fixtures.coming,]
  # Full data set
  data_variables <- as.matrix(dat[,-which(names(dat)=="FTRC")])
  data_label <- dat[,"FTRC"]
  data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
  # split train data and make xgb.DMatrix
  train_data   <- data_variables[-fixtures.coming,]
  train_label  <- data_label[-fixtures.coming]
  train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
  # split test data and make xgb.DMatrix
  predict_data  <- data_variables[fixtures.coming,]
  predict_label <- data_label[fixtures.coming]
  predict_matrix <- xgb.DMatrix(data = predict_data, label = predict_label)
  
  numberOfClasses <- length(unique(dat$FTRC))
  xgb_params <- list("max_depth"=3,"eta"=0.2,
                     "colsample_bytree"=0.9,
                     "objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "min_child_weight"=7,
                     "subsample"=0.8,
                     "alpha"=0,
                     "lambda"=1,
                     "num_class" = numberOfClasses)
  nround    <- 20 # number of XGBoost rounds
  cv.nfold  <- 10
  
  bst_model <- xgb.train(params = xgb_params,
                         data = train_matrix,
                         nrounds = nround)
  
  # Predict hold-out test set
  test_pred <- predict(bst_model, newdata = predict_matrix)
  test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                            ncol=length(test_pred)/numberOfClasses) %>%
    t() %>%
    data.frame() %>%
    mutate(max_prob = max.col(., "last"))
  
  test_prediction[,c("Date","HomeTeam","AwayTeam")]=x_all[fixtures.coming,c("Date","HomeTeam","AwayTeam")]
  test_prediction$HomeTeam=as.character(test_prediction$HomeTeam)
  test_prediction$AwayTeam=as.character(test_prediction$AwayTeam)
  names(test_prediction)[c(1:3)]=c("Home","Draw","Away")
  test_prediction$PredictedOutcome=ifelse(test_prediction$max_prob==1,test_prediction$HomeTeam,ifelse(test_prediction$max_prob==3,test_prediction$AwayTeam,"Draw"))
  test_prediction$Homeodd=1/test_prediction$Home
  test_prediction$Drawodd=1/test_prediction$Draw
  test_prediction$Awayodd=1/test_prediction$Away
  
  # Using Pinnacle odds to see what the best bet is
  # Getting the odds from Pinnacle
  pinnacle_odds = pinnacle_odds[1:(n_teams/2),]
  pinnacle_odds = data.frame(pinnacle_odds)
  
  # Adding our predicted odds
  test_prediction = data.frame(test_prediction[,c("HomeTeam","AwayTeam","PredictedOutcome","Homeodd","Drawodd","Awayodd")])
  real_and_predicted = merge(x = pinnacle_odds, y = test_prediction, by = c("HomeTeam","AwayTeam"), all.x = TRUE)
  
  # See what the best deal is according to the ratio between BF odds and our odds
  pred_real_oddratio = real_and_predicted[,c("P_H_odds","P_D_odds","P_A_odds")]/real_and_predicted[,c("Homeodd","Drawodd","Awayodd")]
  best_ratio = apply(pred_real_oddratio,1,max)
  best_ratio_outcome = max.col(pred_real_oddratio)
  best_ratio_outcome = ifelse(best_ratio_outcome == 1, "H",ifelse(best_ratio_outcome == 2,"D","A"))
  
  # Put all columns together 
  real_and_predicted = real_and_predicted[,c("Date","HomeTeam","AwayTeam","PredictedOutcome","Homeodd","Drawodd","Awayodd",
                                             "P_H_odds","P_D_odds","P_A_odds")]
  names(real_and_predicted) =c("MatchDate","HomeTeam","AwayTeam","PredictedOutcome","Homeodd","Drawodd","Awayodd",
                                             "P_H_odds","P_D_odds","P_A_odds")
  real_and_predicted = cbind(real_and_predicted,best_ratio,best_ratio_outcome)
  real_and_predicted = real_and_predicted[order(real_and_predicted$MatchDate),]
  
  # Inserting timestamp on when the prediction was made and add bookmaker of which we use the odds
  real_and_predicted$timestamp = Sys.time()
  real_and_predicted$bookmaker_source = 'Pinnacle'
  
  # select only correctly predicted games (no missing data)
  new_predicted_games <- real_and_predicted[!is.na(real_and_predicted$best_ratio),]
  
  # Wright prediction in csv file with mw and dat of first game as title, should be written into an sql table soon
  write.csv(new_predicted_games,paste("predictions_per_MW/prediction_MW",(nrow(dataf) %% (n_teams*(n_teams-1)))/(n_teams/2),"_",as.Date(head(real_and_predicted$MatchDate,n=1)),".csv",sep = ""))

  # Wright predictions in sqlite database table
  ENG_insert_predictions(new_predicted_games)
  
  if (!is.null(new_predicted_games$Drawodd[1])) {
    print(paste("SUCCESFULLY PREDICTED", nrow(new_predicted_games),"GAMES"))
  }
}

