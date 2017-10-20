setwd("D:/Het Project/Premier league/Voetbal-voorspellen")

used.packages=c("xgboost","archdata","caret","DiagrammeR","rBayesianOptimization")
not.installed=!(used.packages %in% rownames(installed.packages()))
if(length(used.packages[not.installed])>0){
  install.packages(used.packages[not.installed])
}

library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("dplyr")    # for some data preperation
library("DiagrammeR")
library("rBayesianOptimization")
library("qlcMatrix")

datac=read.csv("final_dataset.csv")

# excluding non played matches
dataf=datac[-c((nrow(datac)-10):nrow(datac)),]

# Separate into feature set and target variable
#FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
x_all = dataf[,-which(names(dataf)=='FTR')]
y_all = dataf['FTR']

#Standardising the data
#Center to the mean and component wise scale to unit variance.
cols = c('HTGD','ATGD','HTP','ATP','DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV','HTS','ATS','HTST','ATST')
x_all[cols] = scale(x_all[cols])

#last 3 matches for both sides
x_all$HM3 = ifelse((x_all$HM3-x_all$HM2)==3,"W",ifelse((x_all$HM3-x_all$HM2)==1,"D",ifelse((x_all$HM3-x_all$HM2)==0&x_all$MW>1,"L","NM")))
x_all$HM2 = ifelse((x_all$HM2-x_all$HM1)==3,"W",ifelse((x_all$HM2-x_all$HM1)==1,"D",ifelse((x_all$HM2-x_all$HM1)==0&x_all$MW>2,"L","NM")))
x_all$HM1 = ifelse(x_all$HM1==3,"W",ifelse(x_all$HM1==1,"D",ifelse((x_all$HM1)==0&x_all$MW>3,"L","NM")))

x_all$AM3 = ifelse((x_all$AM3-x_all$AM2)==3,"W",ifelse((x_all$AM3-x_all$AM2)==1,"D",ifelse((x_all$AM3-x_all$AM2)==0&x_all$MW>1,"L","NM")))
x_all$AM2 = ifelse((x_all$AM2-x_all$AM1)==3,"W",ifelse((x_all$AM2-x_all$AM1)==1,"D",ifelse((x_all$AM2-x_all$AM1)==0&x_all$MW>2,"L","NM")))
x_all$AM1 = ifelse(x_all$AM1==3,"W",ifelse(x_all$AM1==1,"D",ifelse((x_all$AM1)==0&x_all$MW>3,"L","NM")))

# Change categorial columns into dummy columns
n <- names(x_all)
f <- as.formula(paste("~ -1 +", paste(n[!n %in% c("X","Date")], collapse = "+")))

A <- model.matrix(f,x_all) 
head(A)
A=as.data.frame(A)

# with interwetten columns
# x_featured=A[,c('HTP', 'ATP', 'HM1L', 'HM1W','HM1NM', 'HM2L', 'HM2W','HM2NM', 'HM3L', 'HM3W','HM3NM',
#                 'AM1L','AM1NM', 'AM1W', 'AM2L', 'AM2W','AM2NM', 'AM3L', 'AM3W','AM3NM', 'HTGD', 'ATGD',
#                 "DiffPts", 'DiffFormPts', 'DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV',
#                 'HTS','ATS','HTST','ATST','IWH','IWD','IWA')]

x_featured=A[,c('HTP', 'ATP', 'HM1L', 'HM1W','HM1NM', 'HM2L', 'HM2W','HM2NM', 'HM3L', 'HM3W','HM3NM',
                'AM1L','AM1NM', 'AM1W', 'AM2L', 'AM2W','AM2NM', 'AM3L', 'AM3W','AM3NM', 'HTGD', 'ATGD',
                "DiffPts", 'DiffFormPts', 'DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV',
                'HTS','ATS','HTST','ATST')]

df=cbind(x_featured,y_all)

# FTR naar cijfers converteren, beginnend met 0
df$FTRC=ifelse(df$FTR=="H",0,ifelse(df$FTR=="D",1,2))
dat=df[-which(names(df)=="FTR")]

# set seed for reproducibility
set.seed(999)
# Make split index
train_index <- sample(1:nrow(dat), nrow(dat)*0.75)
# Full data set
data_variables <- as.matrix(dat[,-which(names(dat) %in% c("FTRC"))]) # putting 'IWH','IWD','IWA' in the "FTRC" vector to work with interwetten odds
# odds=dat[train_index,c('IWH','IWD','IWA')]
data_label <- dat[,"FTRC"]
data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

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

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = TRUE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)


## checking if profit would be made with certain strategy
pred.and.odds=cbind(OOF_prediction,odds)
pred.and.odds$H=pred.and.odds$X1*pred.and.odds$IWH
pred.and.odds$D=pred.and.odds$X2*pred.and.odds$IWD
pred.and.odds$A=pred.and.odds$X3*pred.and.odds$IWA
pred.and.odds$beton=max.col(pred.and.odds[,c("H","D","A")])
pred.and.odds$maxprof=apply(pred.and.odds[,c("H","D","A")], 1, max)

# for a profit margin of at least profmarg we look at how much we would have earned
profmarg=1.25
gamesbetted=pred.and.odds[pred.and.odds$maxprof>profmarg,]
gamescorrect=gamesbetted[gamesbetted$beton==gamesbetted$label,]
oddsmatrix=gamescorrect[,c('IWH','IWD','IWA')]
revenue=rep(0,nrow(gamescorrect))
for (i in 1:nrow(gamescorrect)){
  revenue[i]=oddsmatrix[i,gamescorrect$beton[i]]
}
profit=sum(revenue)-nrow(gamesbetted)
profitperc=sum(revenue)/nrow(gamesbetted)
profitperc
profit



### checking if between two prbabilities the fraction of correct predictions is the same
LB=0.7
UB=0.8
preds=OOF_prediction
preds$high.prob=apply(preds[,c(1:3)],1,max)
part.preds=preds[preds$high.prob>LB&preds$high.prob<UB,]
fraction.correct=sum(part.preds$max_prob==part.preds$label)/nrow(part.preds)
cat("For predictions with probability between ",100*LB,"% and ",100*UB,"%\n",
    "the fraction that is correctly predicted is ",100*fraction.correct,"%",sep = "")

# confusion matrix, how many time the model confuses the classes
idx=as.character(na.omit(factor(OOF_prediction$max_prob)))
realx=as.character(na.omit(factor(OOF_prediction$label)))
confusionMatrix(realx, idx)

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(test_prediction$label,
                test_prediction$max_prob)

#plot tree
xgb.plot.tree(names(x_featured),bst_model,n_first_tree = 1)

#plot importance of features
importance_matrix = xgb.importance(feature_names = names(x_featured), model = bst_model)
head(importance_matrix)
gp = xgb.plot.importance(importance_matrix)
print(gp)


### Bayesian hyperparameter optimization
# Example 2: Parameter Tuning
library(xgboost)
cv_folds <- KFold(train_label, nfolds = 5,
                  stratified = TRUE, seed = 0)

# Cross validating the xgboost model based on the input hyperparameters and the data
xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample, eta, nround,lambda,alpha) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = eta,
                             max_depth = max.depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree =colsample,
                             lambda = lambda, alpha = alpha,
                             objective = "multi:softprob",
                             num_class=numberOfClasses,
                             eval_metric = "mlogloss"),
               data = train_matrix, nround = nround,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = -cv$evaluation_log[, max(test_mlogloss_mean)],
       Pred = cv$pred)
}

# optimizing based on the parameters in bounds with there value ranges in the brackets
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth = c(2L, 8L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 0.8),
                                              colsample =c(0.5, 0.9),
                                              eta=c(0.01,0.5),
                                              lambda=c(0,1),
                                              alpha=c(0,1),
                                              nround=c(50L,500L)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

print(OPT_Res$Best_Par)
