# Artificial Neural Network -----------------------------------------------
# nnet package install
install.packages("nnet", dependencies = TRUE)
library(nnet)

# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
  # True positive rate: TPR
  TPR = cm[2,2]/sum(cm[2,])
  # True negative rate: TNR
  TNR = cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC = (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR = sqrt(TPR*TNR)
  
  return(c(TPR, TNR, ACC, BCR))
}

RawData <- read.csv("wdbc.csv", header = FALSE)
head(RawData)

# Normlaize the input data
Class <- RawData[,31]
InputData <- RawData[,1:30]
ScaledInputData <- scale(InputData, center = TRUE, scale = TRUE)
head(ScaledInputData)

# Divide the dataset into the training (50%) and test (50%) datasets
trn_idx <- sample(1:length(Class), round(0.5*length(Class)))
trnInputs <- ScaledInputData[trn_idx,]
trnTargets <- Class[trn_idx]
tstInputs <- ScaledInputData[-trn_idx,]
tstTargets <- Class[-trn_idx]

trnData <- data.frame(trnInputs, trnTargets)
colnames(trnData)[31] <- "Target"
tstData <- data.frame(tstInputs, tstTargets)
colnames(tstData)[31] <- "Target"

# Train ANN
ann_trn_input <- trnInputs
ann_trn_target <- class.ind(trnTargets)

# Find the best number of hidden nodes in terms of BCR
# Candidate hidden nodes
nH <- seq(from=2, to=20, by=2)
# 5-fold cross validation index
val_idx <- sample(c(1:5), dim(ann_trn_input)[1], replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2))
val_perf <- matrix(0, length(nH), 5)

ptm <- proc.time()

for (i in 1:length(nH)) {
  
  cat("Training ANN: the number of hidden nodes:", nH[i], "\n")
  eval_fold <- c()
  
  for (j in c(1:5)) {
    
    # Training with the data in (k-1) folds
    tmp_trn_input <- ann_trn_input[which(val_idx != j),]
    tmp_trn_target <- ann_trn_target[which(val_idx != j),]    
    tmp_nnet <- nnet(tmp_trn_input, tmp_trn_target, size = nH[i], decay = 5e-4, maxit = 300)
    
    # Evaluate the model withe the remaining 1 fold
    tmp_val_input <- ann_trn_input[which(val_idx == j),]
    tmp_val_target <- ann_trn_target[which(val_idx == j),]    
    
    eval_fold <- rbind(eval_fold, cbind(max.col(tmp_val_target), max.col(predict(tmp_nnet, tmp_val_input))))
    
  }
  
  # Confusion matrix
  cfm <- table(eval_fold[,1], eval_fold[,2])
  
  # nH
  val_perf[i,1] <-nH[i]
  # Record the validation performance
  val_perf[i,2:5] <- t(perf_eval(cfm))
}

proc.time() - ptm

ordered_val_perf <- val_perf[order(val_perf[,5], decreasing = TRUE),]
colnames(ordered_val_perf) <- c("nH", "TPR", "TNR", "ACC", "BCR")
ordered_val_perf
# Find the best number of hidden node
best_nH <- ordered_val_perf[1,1]

# Test the ANN
ann_tst_input = tstInputs
ann_tst_target = class.ind(tstTargets)

wdbc_nnet <- nnet(ann_trn_input, ann_trn_target, size = best_nH, decay = 5e-4, maxit = 300)

# Performance evaluation
prey <- predict(wdbc_nnet, ann_tst_input)
tst_cm <- table(max.col(ann_tst_target), max.col(prey))

perf_eval(tst_cm)
