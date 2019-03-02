# Part 1: Multi-class classification with ANN & Multinimial logistic regression
# Performance evaluation function for multi-class classification ----------
perf_eval_multi <- function(cm){
  
  # Simple Accuracy
  ACC = sum(diag(cm))/sum(cm)
  
  # Balanced Correction Rate
  BCR = 1
  for (i in 1:dim(cm)[1]){
    BCR = BCR*(cm[i,i]/sum(cm[i,])) 
  }
  
  BCR = BCR^(1/dim(cm)[1])
  
  return(c(ACC, BCR))
}

# Install nnet package and prepare it
install.packages("nnet")
library(nnet)

# Multi-class classification
ctgs_data <- read.csv("ctgs.csv")
n_instance <- dim(ctgs_data)[1]
n_var <- dim(ctgs_data)[2]

# Conduct normalization
ctgs_input <- ctgs_data[,-n_var]
ctgs_target <- ctgs_data[,n_var]

ctgs_input <- scale(ctgs_input, center = TRUE, scale = TRUE)
ctgs_target <- as.factor(ctgs_target)

ctgs_data_normalized <- data.frame(ctgs_input, Class = ctgs_target)

# Initialize performance matrix
perf_summary <- matrix(0, nrow = 2, ncol = 2)
colnames(perf_summary) <- c("ACC", "BCR")
rownames(perf_summary) <- c("Multi_Logit", "ANN")

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:n_instance, round(0.8*n_instance))
ctgs_trn <- ctgs_data_normalized[trn_idx,]
ctgs_tst <- ctgs_data_normalized[-trn_idx,]

# Multinomial logistic regression -----------------------------------------
# Train multinomial logistic regression
ml_logit <- multinom(Class ~ ., data = ctgs_trn)

# Check the coefficients
summary(ml_logit)
t(summary(ml_logit)$coefficients)

# Predict the class label
ml_logit_prey <- predict(ml_logit, newdata = ctgs_tst)

cfmatrix <- table(ctgs_tst$Class, ml_logit_prey)
cfmatrix
perf_summary[1,] <- perf_eval_multi(cfmatrix)
perf_summary

# Artificial Neural Network -----------------------------------------------
# Train ANN
ann_trn_input <- ctgs_trn[,-n_var]
ann_trn_target <- class.ind(ctgs_trn[,n_var])

# Find the best number of hidden nodes in terms of BCR
# Candidate hidden nodes
nH <- seq(from=5, to=30, by=5)

# 5-fold cross validation index
val_idx <- sample(c(1:5), dim(ann_trn_input)[1], replace = TRUE, prob = rep(0.2,5))
val_perf <- matrix(0, length(nH), 3)

ptm <- proc.time()

for (i in 1:length(nH)) {
  
  cat("Training ANN: the number of hidden nodes:", nH[i], "\n")
  eval_fold <- c()
  
  for (j in c(1:5)) {
    
    # Training with the data in (k-1) folds
    tmp_trn_input <- ann_trn_input[which(val_idx != j),]
    tmp_trn_target <- ann_trn_target[which(val_idx != j),]    
    tmp_nnet <- nnet(tmp_trn_input, tmp_trn_target, size = nH[i], decay = 5e-4, maxit = 500)
    
    # Evaluate the model withe the remaining 1 fold
    tmp_val_input <- ann_trn_input[which(val_idx == j),]
    tmp_val_target <- ann_trn_target[which(val_idx == j),]    
    
    eval_fold <- rbind(eval_fold, cbind(max.col(tmp_val_target), 
                                        max.col(predict(tmp_nnet, tmp_val_input))))
    
  }
  
  # Confusion matrix
  cfm <- table(eval_fold[,1], eval_fold[,2])
  
  # nH
  val_perf[i,1] <-nH[i]
  # Record the validation performance
  val_perf[i,2:3] <- perf_eval_multi(cfm)
}

proc.time() - ptm

ordered_val_perf <- val_perf[order(val_perf[,3], decreasing = TRUE),]
colnames(ordered_val_perf) <- c("nH", "ACC", "BCR")
ordered_val_perf

# Find the best number of hidden node
best_nH <- ordered_val_perf[1,1]

# Test the ANN
ann_tst_input = ctgs_tst[,-n_var]
ann_tst_target = class.ind(ctgs_tst[,n_var])

ctgs_nnet <- nnet(ann_trn_input, ann_trn_target, size = best_nH, decay = 5e-4, maxit = 500)

# Performance evaluation
prey <- predict(ctgs_nnet, ann_tst_input)
tst_cm <- table(max.col(ann_tst_target), max.col(prey))
tst_cm

perf_summary[2,] <- perf_eval_multi(tst_cm)
perf_summary

# Part 2: Regression with MLR, k-NN, and ANN
# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

# Concrete strength data
concrete <- read.csv("concrete.csv", header = FALSE)
n_instance <- dim(concrete)[1]
n_var <- dim(concrete)[2]

RegX <- concrete[,-n_var]
RegY <- concrete[,n_var]

# Data Normalization
RegX <- scale(RegX, center = TRUE, scale = TRUE)

# Combine X and Y
RegData <- as.data.frame(cbind(RegX, RegY))

# Split the data into the training/test sets
set.seed(12345)
trn_idx <- sample(1:n_instance, round(0.7*n_instance))
trn_data <- RegData[trn_idx,]
tst_data <- RegData[-trn_idx,]

perf_summary_reg <- matrix(0,3,3)
rownames(perf_summary_reg) <- c("MLR", "k-NN", "ANN")
colnames(perf_summary_reg) <- c("RMSE", "MAE", "MAPE")

# Multiple linear regression
full_model <- lm(RegY ~ ., data = trn_data)
mlr_prey <- predict(full_model, newdata = tst_data)

perf_summary_reg[1,] <- perf_eval_reg(tst_data$RegY, mlr_prey)
perf_summary_reg

# Evaluate the k-NN with the test data
# k-Nearest Neighbor Learning (Regression) --------------------------------
install.packages("FNN", dependencies = TRUE)
library(FNN)

knn_reg <- knn.reg(trn_data[,-n_var], test = tst_data[,-n_var], trn_data$RegY, k=3)

knn_prey <- knn_reg$pred
perf_summary_reg[2,] <- perf_eval_reg(tst_data$RegY, knn_prey)
perf_summary_reg

# Find the best number of hidden nodes in terms of BCR
# Candidate hidden nodes
nH <- seq(from=2, to=20, by=2)

# 5-fold cross validation index
val_idx <- sample(c(1:5), length(trn_idx), replace = TRUE, prob = rep(0.2,5))
val_perf <- matrix(0, length(nH), 4)

ptm <- proc.time()

for (i in 1:length(nH)) {
  
  cat("Training ANN: the number of hidden nodes:", nH[i], "\n")
  eval_fold <- c()
  
  for (j in c(1:5)) {
    
    # Training with the data in (k-1) folds
    tmp_trn_data <- trn_data[which(val_idx != j), ]
    tmp_nnet <- nnet(RegY ~ ., data = tmp_trn_data, size = nH[i], linout = TRUE, 
                     decay = 5e-4, maxit = 500)
    
    # Evaluate the model withe the remaining 1 fold
    tmp_val_input <- trn_data[which(val_idx == j),-n_var]
    tmp_val_target <- trn_data[which(val_idx == j),n_var]    
    
    eval_fold <- rbind(eval_fold, cbind(tmp_val_target, predict(tmp_nnet, tmp_val_input)))
    
  }
  
  # nH
  val_perf[i,1] <-nH[i]
  # Record the validation performance
  val_perf[i,2:4] <- perf_eval_reg(eval_fold[,1],eval_fold[,2])
}

proc.time() - ptm

ordered_val_perf <- val_perf[order(val_perf[,3], decreasing = FALSE),]
colnames(ordered_val_perf) <- c("nH", "RMSE", "MAE", "MAPE")
ordered_val_perf

# Find the best number of hidden node
best_nH <- ordered_val_perf[1,1]

# Train ANN with the best hidden node
best_nnet <- nnet(RegY ~ ., data = trn_data, size = best_nH, linout = TRUE, 
                 decay = 5e-4, maxit = 500)

# Test the model and compare the performance
ann_prey <- predict(best_nnet, tst_data[,-n_var])
perf_summary_reg[3,] <- perf_eval_reg(tst_data$RegY, ann_prey)
perf_summary_reg
