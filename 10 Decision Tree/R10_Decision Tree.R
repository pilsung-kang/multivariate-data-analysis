# For CART
install.packages("party")
library(party)

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

# Classification and Regression Tree (CART) -------------------------------
# Personal Loan Prediction
ploan <- read.csv("Personal Loan.csv")

ploan.x <- ploan[,-c(1,5,10)]
ploan.y <- as.data.frame(as.factor(ploan[,10]))

trn_idx <- sample(1:dim(ploan.y)[1], round(0.7*dim(ploan.y)[1]))

ploan.trn <- cbind(ploan.x[trn_idx,], ploanYN = ploan.y[trn_idx,])
ploan.val <- cbind(ploan.x[-trn_idx,], ploanYN = ploan.y[-trn_idx,])
ploan.all <- rbind(ploan.trn, ploan.val)

# construct single tree and evaluation
# tree parameter settings
min_criterion = c(0.9, 0.95, 0.99)
min_split = c(10, 30, 50, 100)
max_depth = c(0, 10, 5)

tree_result = matrix(0,length(min_criterion)*length(min_split)*length(max_depth),10)
colnames(tree_result) <- c("min_criterion", "min_split", "max_depth", 
                           "TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")

iter_cnt = 1

for (i in 1:length(min_criterion))
{
  for ( j in 1:length(min_split))
  {
    for ( k in 1:length(max_depth))
    {
      
      cat("CART Min criterion:", min_criterion[i], ", Min split:", min_split[j], ", Max depth:", max_depth[k], "\n")
      tmp_control = ctree_control(mincriterion = min_criterion[i], minsplit = min_split[j], maxdepth = max_depth[k])
      tmp_tree <- ctree(ploanYN ~ ., data = ploan.trn, controls = tmp_control)
      tmp_tree_val_prediction <- predict(tmp_tree, newdata = ploan.val)

      tmp_tree_val_cm <- table(ploan.val$ploanYN, tmp_tree_val_prediction)
      
      # parameters
      tree_result[iter_cnt,1] <- min_criterion[i]
      tree_result[iter_cnt,2] <- min_split[j]
      tree_result[iter_cnt,3] <- max_depth[k]
      
      tree_result[iter_cnt, 4:9] <- perf_eval(tmp_tree_val_cm)
      
      # Number of leaf nodes
      tree_result[iter_cnt,10] = length(nodes(tmp_tree, unique(where(tmp_tree))))
      iter_cnt = iter_cnt + 1
    }
  }
}

# Find the best set of parameters
tree_result <- tree_result[order(tree_result[,9], decreasing = T),]
tree_result

best_criterion <- tree_result[1,1]
best_split <- tree_result[1,2]
best_depth <- tree_result[1,3]

# Construct the best tree
tree_control = ctree_control(mincriterion = best_criterion, minsplit = best_split, maxdepth = best_depth)
tree <- ctree(ploanYN ~ ., data = ploan.all, controls = tree_control)
tree_all_prediction <- predict(tree, newdata = ploan.all)

# Performance of the best tree
tree_all_cm <- table(ploan.all$ploanYN, tree_all_prediction)

# Initialize the performance matrix
best_result <- matrix(0,1,7)
colnames(best_result) <- c("TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")

# Evaluate the performance
best_result[1,1:6] = perf_eval(tree_all_cm)

# Number of leaf nodes
best_result[1,7] = length(nodes(tree, unique(where(tree))))
best_result

# Plot the best tree
plot(tree)
plot(tree, type="simple")
