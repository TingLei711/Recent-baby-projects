# Import Dataset
library(readr)
library(ggplot2)
library(dplyr)
library(class)
library(gmodels)
df <- read_csv("Auto.csv")
View(df)

# a. Adding variable 'mpg01'
for (i in 1:nrow(df)){
    if (df$mpg[i] > median(df$mpg)){
    df$mpg01[i] <- 1
  } else {
    df$mpg01[i] <- 0
  }
}
View(df) 

# b. Data Visualization
# Transform string to factor 
df$cylinders <- factor(df$cylinders)
df$mpg01 <- factor(df$mpg01)
# Delete rows with "?" Values
Lines.containing.questionmarks=grep("\\?",df$horsepower)
df <- df[-Lines.containing.questionmarks,]
df$horsepower <- as.double(df$horsepower)
str(df)
table(df$mpg01,df$cylinders)
# Relationship of mpg01 & Cylinders
ggplot(data = df,mapping = aes(x = mpg01, y = cylinders))+geom_point(position = "jitter")

ggplot(df,mapping = aes(x = mpg01,y = displacement)) + geom_boxplot()
ggplot(df,aes(x=mpg01,y=weight)) + geom_col()
ggplot(df,aes(x=mpg01,y=horsepower)) + geom_boxplot(color = "Green")
ggplot(df,aes(x=mpg01, y = acceleration)) + geom_point(alpha = 0.8,color = 'purple',position = 'jitter')
ggplot(df,aes(x=mpg01,y=year)) + geom_boxplot(color = "Green")
round(cor(df[,c(4,5,6)]),2)

# Spliting data into training set and test set.
set.seed(71)
# Feature selection
df1 <- df %>% select(-mpg,-origin, -name,-displacement,-horsepower,-acceleration)
View(df1)
train_ind <- sample(seq_len(nrow(df1)), size = 0.80*nrow(df1))

train <- df1[train_ind,]
test <- df1[-train_ind,]

# Logistic Regression
logistic.fits <- glm(mpg01~ ., data = train, family = binomial)
summary(logistic.fits)

logistic.probs <- predict(logistic.fits, test, type = "response")
glm.pred <- rep(0,79)
glm.pred[logistic.probs > .5] <- 1
table(glm.pred,test$mpg01)
accuracy_of_LR <- mean(glm.pred == test$mpg01)
1-accuracy_of_LR       

# KNN
# Preparation
train_labels <- train[['mpg01']]
test_labels <- test[['mpg01']]

test_pred <- knn(train, test, cl = train_labels, k = 5)
Acc5 <- 100 * sum(test_labels == test_pred) / NROW(test_labels)
CrossTable(x = test_labels, y = test_pred, prop.chisq = FALSE)
Acc5

test_pred4 <- knn(train, test, cl = train_labels, k = 4)
Acc4 <- 100 * sum(test_labels == test_pred4) / NROW(test_labels)
accutable4 <- CrossTable(x = test_labels, y = test_pred4, prop.chisq = FALSE)
Acc4

test_pred3 <- knn(train, test, cl = train_labels, k = 3)
Acc3 <- 100 * sum(test_labels == test_pred3) / NROW(test_labels)
accutable4 <- CrossTable(x = test_labels, y = test_pred3, prop.chisq = FALSE)
Acc3

test_pred2 <- knn(train, test, cl = train_labels, k = 2)
Acc2 <- 100 * sum(test_labels == test_pred2) / NROW(test_labels)
accutable2 <- CrossTable(x = test_labels, y = test_pred2, prop.chisq = FALSE)
Acc2

test_pred1 <- knn(train, test, cl = train_labels, k = 1)
Acc1 <- 100 * sum(test_labels == test_pred1) / NROW(test_labels)
accutable1 <- CrossTable(x = test_labels, y = test_pred1, prop.chisq = FALSE)
Acc1