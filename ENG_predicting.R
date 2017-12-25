setwd("D:/Het Project/Voetbal predictions/Premier League")

used.packages=c("xgboost","stringr","qlcMatrix","e1071")
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

if(!exists("foo", mode="function")) source("ENG_cleaningandpreparing.R")

# import and prepare the data and eventually save it as csv
ENG_preparation(FALSE)

# download that csv for further use and prediction
dataf=read.csv("ENG_final_dataset.csv")

# Separate into feature set and target variable
#FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
x_all = dataf[,-which(names(dataf)=='FTR')]
y_all = dataf['FTR']

#Standardising the data
#Center to the mean and component wise scale to unit variance.
cols = c('HTGD','ATGD','HTP','ATP','DiffLP','Distance','AwayAvgAge','HomeAvgAge','HomeAvgMV','AwayAvgMV','HTS','ATS','HTST','ATST')
x_all[cols] = scale(x_all[cols])

#last 3 matches for both sides
x_all$HM3 = ifelse((x_all$HM3-x_all$HM2)==3,"W",ifelse((x_all$HM3-x_all$HM2)==1,"D",ifelse((x_all$HM3-x_all$HM2)==0&x_all$MW>3,"L","NM")))
x_all$HM2 = ifelse((x_all$HM2-x_all$HM1)==3,"W",ifelse((x_all$HM2-x_all$HM1)==1,"D",ifelse((x_all$HM2-x_all$HM1)==0&x_all$MW>2,"L","NM")))
x_all$HM1 = ifelse(x_all$HM1==3,"W",ifelse(x_all$HM1==1,"D",ifelse((x_all$HM1)==0&x_all$MW>1,"L","NM")))

x_all$AM3 = ifelse((x_all$AM3-x_all$AM2)==3,"W",ifelse((x_all$AM3-x_all$AM2)==1,"D",ifelse((x_all$AM3-x_all$AM2)==0&x_all$MW>3,"L","NM")))
x_all$AM2 = ifelse((x_all$AM2-x_all$AM1)==3,"W",ifelse((x_all$AM2-x_all$AM1)==1,"D",ifelse((x_all$AM2-x_all$AM1)==0&x_all$MW>2,"L","NM")))
x_all$AM1 = ifelse(x_all$AM1==3,"W",ifelse(x_all$AM1==1,"D",ifelse((x_all$AM1)==0&x_all$MW>1,"L","NM")))

# Change categorial columns into dummy columns
n <- names(x_all)
f <- as.formula(paste("~ -1 +", paste(n[!n %in% c("X","Date")], collapse = "+")))

A <- model.matrix(f,x_all) 
A=as.data.frame(A)
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
# Make split index based on the last ten rows are the coming matches
fixtures.coming=c((nrow(dat)-9):nrow(dat))
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

write.csv(test_prediction,paste("predictions_per_MW/prediction_MW",(nrow(dataf) %% 380)/10,"_",tail(dataf$Date,n=1),".csv",sep = ""))
