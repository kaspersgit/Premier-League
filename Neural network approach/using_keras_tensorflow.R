# install packages being used
used.packages=c("keras","tensorflow")
not.installed=!(used.packages %in% rownames(installed.packages()))
if(length(used.packages[not.installed])>0){
  install.packages(used.packages[not.installed])
}

# Load in the keras package
library(keras)
library(tensorflow)

# Install TensorFlow
install_tensorflow()

# Read in `lineup` data
hist_linup <- read.csv("clean_dataset_with_fifascores.csv", header = TRUE) 
hist_linup <- hist_linup[,colSums(is.na(hist_linup)) != nrow(hist_linup)]
scores_cols <- names(hist_linup)[grepl('^[a-z][a-z][0-9]_score$',names(hist_linup))]
other_cols <- names(hist_linup[,!names(hist_linup) %in% scores_cols])
hist_linup <- hist_linup[,!names(hist_linup) %in% names(hist_linup[,scores_cols])[colSums(hist_linup[,scores_cols]) == 0]]

## Data discovery 

# Return the first part of `hist_linup`
head(hist_linup)

# Inspect the structure
str(hist_linup)

# Obtain the dimensions
dim(hist_linup)

# Short summary statistics for every column
summary(hist_linup)

# Data exploration
plot(rowSums(hist_linup[,grepl('^h[a-z][0-9]_score$',names(hist_linup))]), 
     rowSums(hist_linup[,grepl('^a[a-z][0-9]_score$',names(hist_linup))]), 
     pch=21, bg=c("red","green3","blue")[unclass(hist_linup$FTR)], 
     xlab="Home Team starting 11 strength", 
     ylab="Away Team starting 11 strength")

## pre process the data
# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `hist_linup` data
scores_norm <- as.data.frame(lapply(hist_linup[,grepl('^[a-z][a-z][0-9]_score$',names(hist_linup))], normalize))
hist_linup_norm <- cbind(hist_linup[,c('FTR_H','FTR_D','FTR_A','IWH','IWD','IWA')],scores_norm)
colnames(hist_linup_norm)[1:6] <- c('FTR_H','FTR_D','FTR_A','IWH','IWD','IWA')

# Return the first part of `hist_linup_norm` 
head(hist_linup_norm)

# Delete column names but first make numeric vectors to know which column names is training and which is target
score_columns <- which(grepl('^[a-z][a-z][0-9]_score$',names(hist_linup_norm)))
target_columns <- which(grepl('^FTR_.*$',names(hist_linup_norm)))
odds_columns <- which(grepl('^IW.$',names(hist_linup_norm)))

# Turn `hist_linup` into a matrix
hist_linup_norm <- as.matrix(hist_linup_norm)

# Set hist_linup `dimnames` to `NULL`
dimnames(hist_linup_norm) <- NULL

# Set the random seed
set.seed(999)

# Determine sample size
ind <- sample(2, nrow(hist_linup_norm), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
hist_linup.training <- hist_linup_norm[ind==1, score_columns]
hist_linup.test <- hist_linup_norm[ind==2, score_columns]

# Split the class attribute
hist_linup.trainingtarget <- hist_linup_norm[ind==1, target_columns]
hist_linup.testtarget <- hist_linup_norm[ind==2, target_columns]

## Constructing the model

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(40)) %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  layer_dense(units = 3, activation = 'softmax')

# Summary of model
  # Print a summary of a model
  summary(model)
  
  # Get model configuration
  get_config(model)
  
  # Get layer configuration
  get_layer(model, index = 1)
  
  # List the model's layers
  model$layers
  
  # List the input tensors
  model$inputs
  
  # List the output tensors
  model$outputs

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# Fit the model 
history <- model %>% fit(
  hist_linup.training, 
  hist_linup.trainingtarget, 
  epochs = 50, 
  batch_size = 10, 
  validation_split = 0.2,
  verbose = 1
)

# Plot the loss and accuracy of the model fitted above
  # Plot the model loss of the training data
  plot(history$metrics$loss, main="Model Loss", ylim = c(min(history$metrics$loss,history$metrics$val_loss),max(history$metrics$loss,history$metrics$val_loss)), xlab = "epoch", ylab="loss", col="blue", type="l")
  
  # Plot the model loss of the test data
  lines(history$metrics$val_loss, col="green")
  
  # Add legend
  legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
  
  # Plot the accuracy of the training data 
  plot(history$metrics$acc, main="Model Accuracy", ylim = c(min(history$metrics$acc,history$metrics$val_acc),max(history$metrics$val_acc)), xlab = "epoch", ylab="accuracy", col="blue", type="l")
  
  # Plot the accuracy of the validation data
  lines(history$metrics$val_acc, col="green")
  
  # Add Legend
  legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Predict the classes for the test data
classes <- model %>% predict_classes(hist_linup.test, batch_size = 128)
test_target <- ifelse(hist_linup.testtarget[,1]==1,0,ifelse(hist_linup.testtarget[,2]==1,1,2))

# Confusion matrix
table(test_target, classes)

