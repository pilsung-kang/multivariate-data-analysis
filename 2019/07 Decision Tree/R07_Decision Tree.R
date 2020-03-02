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

# Performance table
Perf.Table <- matrix(0, nrow = 1, ncol = 6)
rownames(Perf.Table) <- c("CART")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
Ploan <- read.csv("Personal Loan.csv")
input.idx <- c(2,3,4,6,7,8,9,11,12,13,14)
target.idx <- 10

Ploan.input <- Ploan[,input.idx]
Ploan.target <- as.factor(Ploan[,target.idx])

Ploan.data <- data.frame(Ploan.input, Ploan.target)

trn.idx <- 1:1500
tst.idx <- 1501:2500

# Classification and Regression Tree (CART) -------------------------------
install.packages("tree")
library(tree)

CART.trn <- data.frame(Ploan.input[trn.idx,], PloanYN = Ploan.target[trn.idx])
CART.tst <- data.frame(Ploan.input[tst.idx,], PloanYN = Ploan.target[tst.idx])

# Training the tree
CART.model <- tree(PloanYN ~ ., CART.trn)
summary(CART.model)

# Plot the tree
plot(CART.model)
text(CART.model, pretty = 1)

# Find the best tree
set.seed(12345)
CART.model.cv <- cv.tree(CART.model, FUN = prune.misclass)

# Plot the pruning result
plot(CART.model.cv$size, CART.model.cv$dev, type = "b")
CART.model.cv

# Select the final model
CART.model.pruned <- prune.misclass(CART.model, best = 6)
plot(CART.model.pruned)
text(CART.model.pruned, pretty = 1)

# Prediction
CART.prey <- predict(CART.model.pruned, CART.tst, type = "class")
CART.cfm <- table(CART.tst$PloanYN, CART.prey)
CART.cfm

Perf.Table[1,] <- perf_eval(CART.cfm)
Perf.Table

