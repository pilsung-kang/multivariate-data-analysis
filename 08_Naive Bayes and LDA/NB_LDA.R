# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Result summary
Perf.Table <- matrix(0, nrow = 2, ncol = 6)
rownames(Perf.Table) <- c("Naive Bayes", "LDA")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the wdbc data
Wdbc.Data <- read.csv("wdbc.csv", header = FALSE)

# Divide the dataset into the training (70%) and Test (30%) datasets
trn.idx <- sample(1:nrow(Wdbc.Data), round(0.7*nrow(Wdbc.Data)))

# Classifier 1: Naive Bayesian Classifier ---------------------------------
# e1071 package install
install.packages("e1071")
library(e1071)

# Use the dataset without normalization
NB.Trn.Data <- Wdbc.Data[trn.idx,]
colnames(NB.Trn.Data)[31] <- "Target"

NB.Tst.Data <- Wdbc.Data[-trn.idx,]
colnames(NB.Tst.Data)[31] <- "Target"

# Training the Naive Bayesian Classifier
NB.model <- naiveBayes(Target ~ ., data = NB.Trn.Data)
NB.model$apriori
NB.model$tables

# Predict the new input data based on Naive Bayesian Classifier
NB.posterior = predict(NB.model, NB.Tst.Data, type = "raw")
NB.prey = predict(NB.model, NB.Tst.Data, type ="class")

NB.cfm <- table(NB.Tst.Data[,31], NB.prey)
NB.cfm

Perf.Table[1,] <- perf_eval(NB.cfm)
Perf.Table


# Classifier 2: Linear Discriminant Analysis ------------------------------
install.packages("MASS")
library(MASS)

# Use the dataset without normalization
LDA.Trn.Data <- Wdbc.Data[trn.idx,]
colnames(LDA.Trn.Data)[31] <- "Target"

LDA.Tst.Data <- Wdbc.Data[-trn.idx,]
colnames(LDA.Tst.Data)[31] <- "Target"

# Training LDA
LDA.model <- lda(Target ~ ., data = LDA.Trn.Data)

# Training result of LDA
plot(LDA.model)

# Predict the unknown observations based on the LDA
LDA.Predict <- predict(LDA.model, LDA.Tst.Data)

names(LDA.Predict)
LDA.Predict$class
LDA.Predict$posterior
LDA.Predict$x

LDA.cfm <- table(LDA.Tst.Data$Target, LDA.Predict$class)
LDA.cfm

Perf.Table[2,] <- perf_eval(LDA.cfm)
Perf.Table



