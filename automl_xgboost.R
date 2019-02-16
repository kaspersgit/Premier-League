# ~ CODE GENERATED USING autoML LIBRARY
# ~ CODE CREATION DATE: 2019-01-19 22:16:49
# ~ PROJECT PREDICTS: FTR


# ~ PROJECT SETTINGS
# ~ REMOVED DUPLICATED OBSERVATIONS: TRUE
# ~ CORRECTED MISSING VALUE FORMATTING: TRUE
# ~ FORMATTED FEATURES: TRUE
# ~ IMPUTED MISSING DATA: TRUE
# ~ CREATED TRACKING FEATURES: TRUE
# ~ CLIPPED OUTLIERS: TRUE
# ~ TRANSFORMED FEATURES: TRUE
# ~ CREATED INTERACTIONS: TRUE
# ~ CREATED UNSUPERVISED FEATURES: TRUE


predict_autoML <- function(trainedModel, x){


set.seed(1991)
options(scipen = 999)
library(stringr)
library(tm)
library(ranger)
library(mlr)


names(x) <- make.names(names(x))


# ************************************************** 
# ****** ENCODING OF INCORRECT MISSING VALUES ****** 
# ************************************************** 
x <- x[,setdiff(names(x), c('af5_score'))]




# ******************************************** 
# ****** FORMATTING OF NUMERIC FEATURES ****** 
x[,'ad3_score'] <- as.numeric(x[,'ad3_score'])
x[,'af1_score'] <- as.numeric(x[,'af1_score'])
x[,'am1_score'] <- as.numeric(x[,'am1_score'])
x[,'hd3_score'] <- as.numeric(x[,'hd3_score'])
x[,'hm1_score'] <- as.numeric(x[,'hm1_score'])
x[,'hm2_score'] <- as.numeric(x[,'hm2_score'])
x[,'hm3_score'] <- as.numeric(x[,'hm3_score'])


# ***************************************************************************************** 
# ****** IMPUTATION, OUTLIER CLIPPING, LOW PROPORTIONAL LEVELS AND TRACKING FEATURES ****** 
# ***************************************************************************************** 
x[,'XEC_Outlier_hd3_score'] <- ifelse(is.na(x[,'hd3_score']) == TRUE, 0,
																ifelse(round(x[,'hd3_score'],6) < 64 | round(x[,'hd3_score'],6) > 88, 1, 0))
x[,'hd3_score'] <- ifelse(round(x[,'hd3_score'],6) < 64 | round(x[,'hd3_score'],6) > 88, 77, x[,'hd3_score'])
x[,'XEC_Miss_hd3_score'] <- ifelse(is.na(x[,'hd3_score']) == TRUE, 1, 0)
x[,'hd3_score'] <- ifelse(is.na(x[,'hd3_score']) == TRUE, 77, x[,'hd3_score'])
x[,'XEC_Outlier_hm1_score'] <- ifelse(is.na(x[,'hm1_score']) == TRUE, 0,
																ifelse(round(x[,'hm1_score'],6) < 62.5 | round(x[,'hm1_score'],6) > 90.5, 1, 0))
x[,'hm1_score'] <- ifelse(round(x[,'hm1_score'],6) < 62.5 | round(x[,'hm1_score'],6) > 90.5, 76, x[,'hm1_score'])
x[,'XEC_Miss_hm1_score'] <- ifelse(is.na(x[,'hm1_score']) == TRUE, 1, 0)
x[,'hm1_score'] <- ifelse(is.na(x[,'hm1_score']) == TRUE, 76, x[,'hm1_score'])
x[,'XEC_Outlier_hm2_score'] <- ifelse(is.na(x[,'hm2_score']) == TRUE, 0,
																ifelse(round(x[,'hm2_score'],6) < 62.5 | round(x[,'hm2_score'],6) > 90.5, 1, 0))
x[,'hm2_score'] <- ifelse(round(x[,'hm2_score'],6) < 62.5 | round(x[,'hm2_score'],6) > 90.5, 76, x[,'hm2_score'])
x[,'XEC_Miss_hm2_score'] <- ifelse(is.na(x[,'hm2_score']) == TRUE, 1, 0)
x[,'hm2_score'] <- ifelse(is.na(x[,'hm2_score']) == TRUE, 76, x[,'hm2_score'])
x[,'XEC_Outlier_hm3_score'] <- ifelse(is.na(x[,'hm3_score']) == TRUE, 0,
																ifelse(round(x[,'hm3_score'],6) < 60 | round(x[,'hm3_score'],6) > 92, 1, 0))
x[,'hm3_score'] <- ifelse(round(x[,'hm3_score'],6) < 60 | round(x[,'hm3_score'],6) > 92, 76, x[,'hm3_score'])
x[,'XEC_Miss_hm3_score'] <- ifelse(is.na(x[,'hm3_score']) == TRUE, 1, 0)
x[,'hm3_score'] <- ifelse(is.na(x[,'hm3_score']) == TRUE, 76, x[,'hm3_score'])
x[,'XEC_Outlier_ad3_score'] <- ifelse(is.na(x[,'ad3_score']) == TRUE, 0,
																ifelse(round(x[,'ad3_score'],6) < 64 | round(x[,'ad3_score'],6) > 88, 1, 0))
x[,'ad3_score'] <- ifelse(round(x[,'ad3_score'],6) < 64 | round(x[,'ad3_score'],6) > 88, 76, x[,'ad3_score'])
x[,'XEC_Miss_ad3_score'] <- ifelse(is.na(x[,'ad3_score']) == TRUE, 1, 0)
x[,'ad3_score'] <- ifelse(is.na(x[,'ad3_score']) == TRUE, 76, x[,'ad3_score'])
x[,'XEC_Outlier_am1_score'] <- ifelse(is.na(x[,'am1_score']) == TRUE, 0,
																ifelse(round(x[,'am1_score'],6) < 62.5 | round(x[,'am1_score'],6) > 90.5, 1, 0))
x[,'am1_score'] <- ifelse(round(x[,'am1_score'],6) < 62.5 | round(x[,'am1_score'],6) > 90.5, 76, x[,'am1_score'])
x[,'XEC_Miss_am1_score'] <- ifelse(is.na(x[,'am1_score']) == TRUE, 1, 0)
x[,'am1_score'] <- ifelse(is.na(x[,'am1_score']) == TRUE, 76, x[,'am1_score'])
x[,'XEC_Outlier_af1_score'] <- ifelse(is.na(x[,'af1_score']) == TRUE, 0,
																ifelse(round(x[,'af1_score'],6) < 63.5 | round(x[,'af1_score'],6) > 91.5, 1, 0))
x[,'af1_score'] <- ifelse(round(x[,'af1_score'],6) < 63.5 | round(x[,'af1_score'],6) > 91.5, 77, x[,'af1_score'])
x[,'XEC_Miss_af1_score'] <- ifelse(is.na(x[,'af1_score']) == TRUE, 1, 0)
x[,'af1_score'] <- ifelse(is.na(x[,'af1_score']) == TRUE, 77, x[,'af1_score'])


# ************************************* 
# ****** MAX FEATURE SCALING ****** 
# ************************************* 
x[,'hd3_score'] <- x[,'hd3_score'] / max(x[,'hd3_score'])
x[,'hm1_score'] <- x[,'hm1_score'] / max(x[,'hm1_score'])
x[,'hm2_score'] <- x[,'hm2_score'] / max(x[,'hm2_score'])
x[,'hm3_score'] <- x[,'hm3_score'] / max(x[,'hm3_score'])
x[,'ad3_score'] <- x[,'ad3_score'] / max(x[,'ad3_score'])
x[,'am1_score'] <- x[,'am1_score'] / max(x[,'am1_score'])
x[,'af1_score'] <- x[,'af1_score'] / max(x[,'af1_score'])


# *********************************************** 
# ****** NUMERICAL FEATURE TRANSFORMATIONS ****** 
# *********************************************** 
x[,'XEC_Sqrt_ad3_score'] <- sqrt(x[,'ad3_score'])
x[,'XEC_Log_af1_score'] <- log((x[,'af1_score'] + 1))
# ******************************************** 
# ****** NUMERICAL FEATURE INTERACTIONS ****** 
# ******************************************** 
x[,'XEC_Mult_hm3_score_INT_hm2_score'] <- x[,'hm3_score'] * x[,'hm2_score']
x[,'XEC_Add_hm3_score_INT_hm2_score'] <- x[,'hm3_score'] + x[,'hm2_score']
x[,'XEC_Mult_hm3_score_INT_hd3_score'] <- x[,'hm3_score'] * x[,'hd3_score']
x[,'XEC_Add_hm3_score_INT_hd3_score'] <- x[,'hm3_score'] + x[,'hd3_score']
x[,'XEC_Mult_hm3_score_INT_hm1_score'] <- x[,'hm3_score'] * x[,'hm1_score']
x[,'XEC_Add_hm3_score_INT_hm1_score'] <- x[,'hm3_score'] + x[,'hm1_score']
x[,'XEC_Div_hm3_score_INT_am1_score'] <- ifelse(x[,'am1_score'] == 0, x[,'hm3_score'] / (x[,'am1_score'] + 1), x[,'hm3_score'] / x[,'am1_score'])
x[,'XEC_Sub_hm3_score_INT_am1_score'] <- x[,'hm3_score'] - x[,'am1_score']
x[,'XEC_Div_hm3_score_INT_ad3_score'] <- ifelse(x[,'ad3_score'] == 0, x[,'hm3_score'] / (x[,'ad3_score'] + 1), x[,'hm3_score'] / x[,'ad3_score'])
x[,'XEC_Sub_hm3_score_INT_ad3_score'] <- x[,'hm3_score'] - x[,'ad3_score']
x[,'XEC_Mult_hm2_score_INT_hd3_score'] <- x[,'hm2_score'] * x[,'hd3_score']
x[,'XEC_Add_hm2_score_INT_hd3_score'] <- x[,'hm2_score'] + x[,'hd3_score']
x[,'XEC_Add_hm2_score_INT_hm1_score'] <- x[,'hm2_score'] + x[,'hm1_score']
x[,'XEC_Div_hm2_score_INT_am1_score'] <- ifelse(x[,'am1_score'] == 0, x[,'hm2_score'] / (x[,'am1_score'] + 1), x[,'hm2_score'] / x[,'am1_score'])
x[,'XEC_Sub_hm2_score_INT_am1_score'] <- x[,'hm2_score'] - x[,'am1_score']
x[,'XEC_Sub_hm2_score_INT_ad3_score'] <- x[,'hm2_score'] - x[,'ad3_score']
x[,'XEC_Mult_hd3_score_INT_hm1_score'] <- x[,'hd3_score'] * x[,'hm1_score']
x[,'XEC_Add_hd3_score_INT_hm1_score'] <- x[,'hd3_score'] + x[,'hm1_score']
x[,'XEC_Div_hd3_score_INT_am1_score'] <- ifelse(x[,'am1_score'] == 0, x[,'hd3_score'] / (x[,'am1_score'] + 1), x[,'hd3_score'] / x[,'am1_score'])
x[,'XEC_Sub_hd3_score_INT_am1_score'] <- x[,'hd3_score'] - x[,'am1_score']
x[,'XEC_Div_hd3_score_INT_ad3_score'] <- ifelse(x[,'ad3_score'] == 0, x[,'hd3_score'] / (x[,'ad3_score'] + 1), x[,'hd3_score'] / x[,'ad3_score'])
x[,'XEC_Sub_hd3_score_INT_ad3_score'] <- x[,'hd3_score'] - x[,'ad3_score']
x[,'XEC_Div_hm1_score_INT_am1_score'] <- ifelse(x[,'am1_score'] == 0, x[,'hm1_score'] / (x[,'am1_score'] + 1), x[,'hm1_score'] / x[,'am1_score'])
x[,'XEC_Sub_hm1_score_INT_am1_score'] <- x[,'hm1_score'] - x[,'am1_score']
x[,'XEC_Div_hm1_score_INT_ad3_score'] <- ifelse(x[,'ad3_score'] == 0, x[,'hm1_score'] / (x[,'ad3_score'] + 1), x[,'hm1_score'] / x[,'ad3_score'])
x[,'XEC_Sub_hm1_score_INT_ad3_score'] <- x[,'hm1_score'] - x[,'ad3_score']
x[,'XEC_Mult_am1_score_INT_ad3_score'] <- x[,'am1_score'] * x[,'ad3_score']
x[,'XEC_Add_am1_score_INT_ad3_score'] <- x[,'am1_score'] + x[,'ad3_score']
# *********************************** 
# ****** UNSUPERVISED FEATURES ****** 
# *********************************** 
x <- x[,trainedModel$features]
pred <- predict(trainedModel, newdata = x[,trainedModel$features])$data
return(pred)}
