# Association Rules -------------------------------------------------------
# arules and arulesViz packages install
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)

library(arules)
library(arulesViz)
library(wordcloud)

# Load titanic data set
titanic <- read.delim("titanic.txt", dec=",")
str(titanic)
head(titanic)

# Remove "Name" column and group "Age" column
titanic_ar <- titanic[,2:5]
titanic_ar$Age = as.character(titanic_ar$Age)
c_idx <- which(as.numeric(titanic_ar$Age) < 20)
a_idx <- which(as.numeric(titanic_ar$Age) >= 20)
na_idx <- which(is.na(titanic_ar$Age))

titanic_ar$Age[c_idx] <- "Child"
titanic_ar$Age[a_idx] <- "Adult"
titanic_ar$Age[na_idx] <- "Unknown"

# Convert the attribues to factor
titanic_ar$Age <- as.factor(titanic_ar$Age)
titanic_ar$Survived <- as.factor(titanic_ar$Survived)

# Rule generation by Apriori algorithm with default settings
rules <- apriori(titanic_ar)
inspect(rules)

# Rule generation by Apriori algorithm with custom settings
rules <- apriori(titanic_ar, parameter = list(minlen = 3, support = 0.1, conf = 0.8),
                 appearance = list(rhs = c("Survived=0", "Survived=1"), default="lhs"))
inspect(rules)

# Plot the rules
plot(rules, method="scatterplot")
plot(rules, method="graph", control=list(type = "items", alpha = 1))
plot(rules, method="paracoord", control=list(reorder=TRUE))

# Load transaction data "Groceries"
data("Groceries")
summary(Groceries)
str(Groceries)
inspect(Groceries)

# Item inspection
itemName <- itemLabels(Groceries)
itemCount <- itemFrequency(Groceries)*9835

col <- brewer.pal(8, "Dark2")
wordcloud(words = itemName, freq = itemCount, min.freq = 1, scale = c(7, 0.2), col = col , random.order = FALSE)

itemFrequencyPlot(Groceries, support = 0.05, cex.names=0.8)

# Rule generation by Apriori
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules

# List the first three rules with the highest lift values
inspect(head(sort(rules, by="lift"),3))

# Save the rules in a text file
write.csv(as(rules, "data.frame"), "Groceries_rules.csv", row.names = FALSE)

# Plot the rules
plot(rules)
plot(rules, method="grouped")
