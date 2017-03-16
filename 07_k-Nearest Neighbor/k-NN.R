# k-Nearest Neighbor Illustration -----------------------------------------
install.packages("ElemStatLearn", dependencies = TRUE)
install.packages("class", dependencies = TRUE)
library(ElemStatLearn)
library(class)

# 2-D artificial data example with k=1
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod1 <- knn(x, xnew, g, k=50, prob=TRUE)
prob1 <- attr(mod1, "prob")
prob1 <- ifelse(mod1=="1", prob1, 1-prob1)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob1 <- matrix(prob1, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob1, levels=0.5, labels="", xlab="", ylab="", main= "50-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob1>0.5, "coral", "cornflowerblue"))
box()


# k-Nearest Neigbor Learning (Classification) -----------------------------
# kknn package install & call
install.packages("kknn", dependencies = TRUE)
library(kknn)

# Load the wdbc data
RawData <- read.csv("wdbc.csv", header = FALSE)
head(RawData)

# k-NN Classification: WDBC data
# Normlaize the input data
Class <- RawData[,31]
InputData <- RawData[,1:30]
ScaledInputData <- scale(InputData, center = TRUE, scale = TRUE)
head(ScaledInputData)

# Divide the dataset into the training (70%) and Validation (30%) datasets
trn_idx <- sample(1:length(Class), round(0.7*length(Class)))
trnInputs <- ScaledInputData[trn_idx,]
trnTargets <- Class[trn_idx]
valInputs <- ScaledInputData[-trn_idx,]
valTargets <- Class[-trn_idx]

trnData <- data.frame(trnInputs, trnTargets)
colnames(trnData)[31] <- "Target"
valData <- data.frame(valInputs, valTargets)
colnames(valData)[31] <- "Target"

# Perform k-nn classification with k=1, Distance = Euclidean, and weighted scheme = majority voting
kknn <- kknn(Target ~ ., trnData, valData, k=1, distance=2, kernel = "rectangular")

# View the k-nn results
summary(kknn)
kknn$CL
kknn$W
kknn$D

# Visualize the classification results
knnfit <- fitted(kknn)
table(valTargets, knnfit)
pcol <- as.character(as.numeric(valTargets))
pairs(valData[c(1,2,5,6)], pch = pcol, col = c("blue", "red")[(valTargets != knnfit)+1])

table(valTargets, kknn$fitted.values)

# Leave-one-out validation for finding the best k
knntr <- train.kknn(Target ~ ., trnData, kmax=10, distance=2, kernel="rectangular")

knntr$MISCLASS
knntr$best.parameters

# Perform k-nn classification with the best k, Distance = Euclidean, and weighted scheme = majority voting
kknn_opt <- kknn(Target ~ ., trnData, valData, k=knntr$best.parameters$k, distance=2, kernel = "rectangular")
fit_opt <- fitted(kknn_opt)
cfmatrix <- table(valTargets, fit_opt)
cfmatrix

# Summarize the classification performances
Cperf <- matrix(0, nrow=1,ncol=3)
colnames(Cperf) <- c("Accuracy", "BCR", "F1")

# Simple Accuracy
Cperf[1,1] <- (cfmatrix[1,1]+cfmatrix[2,2])/sum(cfmatrix)

# Balanced correction rate (BCR)
Cperf[1,2] <- sqrt((cfmatrix[1,1]/(cfmatrix[1,1]+cfmatrix[1,2]))*(cfmatrix[2,2]/(cfmatrix[2,1]+cfmatrix[2,2])))

# F1-measure
Recall <- cfmatrix[2,2]/(cfmatrix[2,1]+cfmatrix[2,2])
Precision <- cfmatrix[1,1]/(cfmatrix[1,1]+cfmatrix[1,2])
Cperf[1,3] <- 2*Recall*Precision/(Recall+Precision)

Cperf

# k-Nearest Neighbor Learning (Regression) --------------------------------
install.packages("FNN", dependencies = TRUE)
library(FNN)
# Concrete strength data
concrete <- read.csv("concrete.csv")

RegX <- concrete[,1:8]
RegY <- concrete[,9]

# Data Normalization
RegX <- scale(RegX, center = TRUE, scale = TRUE)

# Combine X and Y
RegData <- as.data.frame(cbind(RegX, RegY))

# Split the data into the training/test sets
trn_idx <- sample(1:1029, round(0.7*1029))
trn_data <- RegData[trn_idx,]
test_data <- RegData[-trn_idx,]

# Find the best k using leave-one-out validation
nk <- c(1:10)
trn.n <- dim(trn_data)[1]
trn.v <- dim(trn_data)[2]

val.rmse <- matrix(0,length(nk),1)

for (i in 1:length(nk)){
  
  cat("k-NN regression with k:", nk[i], "\n")
  tmp_residual <- matrix(0,trn.n,1)
  
  for (j in 1:trn.n){
    
    # Data separation for leave-one-out validation
    tmptrnX <- trn_data[-j,1:(trn.v-1)]
    tmptrnY <- trn_data[-j,trn.v]
    tmpvalX <- trn_data[j,1:(trn.v-1)]
    tmpvalY <- trn_data[j,trn.v]
    
    # Train k-NN & evaluate
    tmp.knn.reg <- knn.reg(tmptrnX, test = tmpvalX, tmptrnY, k=nk[i])
    tmp_residual[j,1] <- tmpvalY - tmp.knn.reg$pred
    
  }
  
  val.rmse[i,1] <- sqrt(mean(tmp_residual^2))
}

# find the best k
best.k <- nk[which.min(val.rmse)]

# Evaluate the k-NN with the test data
test.knn.reg <- knn.reg(trn_data[,1:ncol(trn_data)-1], test = test_data[,1:ncol(test_data)-1], 
                        trn_data[,ncol(trn_data)], k=best.k)

tgt.y <- test_data[,ncol(trn_data)]
knn.haty <- test.knn.reg$pred

# Train the MLR for comparison
full_model <- lm(RegY ~ ., data = trn_data)
mlr.haty <- predict(full_model, newdata = test_data)

# Regression performance comparison in terms of MAE
mean(abs(tgt.y-knn.haty))
mean(abs(tgt.y-mlr.haty))

# Plot the result
plot(tgt.y, knn.haty, pch = 1, col = 1, xlim = c(0,80), ylim = c(0, 80))
points(tgt.y, mlr.haty, pch = 2, col = 4, xlim = c(0,80), ylim = c(0,80))
abline(0,1,lty=3)

