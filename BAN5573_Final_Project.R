# Library Packages
library(readr)
library(dplyr)
library(caret)
library(tree)
library(randomForest)
library(varhandle)
library(e1071)
library(plot.matrix)
# Read Data
df_b <- read_csv("~/Desktop/BAN5573/Project_df_SampleSuperstore.csv")
View(df_b)

# Adding variables 
df_b$unit_price <- df_b$Sales / df_b$Quantity
df_b$profit_ratio <- (df_b$Profit / df_b$Sales) * 100 

# Rename Variables
df_b <- rename(df_b, shipmode = 'Ship Mode')
# transform dependent variable from numerical values to categorized values.
for (i in 1:nrow(df_b)){
  if (df_b$profit_ratio[i] > 40){
    df_b$Profit_Category[i] <- 'Extremely Profitable'
  } else if (df_b$profit_ratio[i] > 20 && df_b$profit_ratio[i] <= 40) {
    df_b$Profit_Category[i] <- 'Somewhat Profitable'
  } else if (df_b$profit_ratio[i] > 0 && df_b$profit_ratio[i] <= 20) {
    df_b$Profit_Category[i] <- 'Slightly Profitable'
  } else if (df_b$profit_ratio[i] > -50 && df_b$profit_ratio[i] <= 0){
    df_b$Profit_Category[i] <- 'Slightly loss'
  } else if (df_b$profit_ratio[i] > -50 && df_b$profit_ratio[i] <= -100){
    df_b$Profit_Category[i] <- 'somewhat loss'
  } else {
    df_b$Profit_Category[i] <- 'Huge loss'
  }
} 
# Data filtering
df_b <- df_b %>% select(-'Country',-City,-Sales,-'Postal Code')
View(df_b)
# Features engineering ####
# Encoding variable 'ship mode' to an ordinal array 0,1,2,3
df_b$shipmode <- factor(df_b$shipmode, 
                           levels = c('Standard Class','Second Class','First Class','Same Day'),
                           labels = c(0,1,2,3))
df_b$Profit_Category <- factor(df_b$Profit_Category, 
                        levels = c('Huge loss','somewhat loss','Slightly loss','Slightly Profitable','Somewhat Profitable','Extremely Profitable'),
                        labels = c(1,2,3,4,5,6))
# Recategorize variable 'state' according to state gdp per capita
high_income <- c('District of Columbia','Massachusetts','New York','Alaska','North Dakota','California')
relatively_high_income <- c('Connecticut','Washington','Wyoming','Delaware','New Jersey','Maryland','Illinois','Texas','Colorado','Minnesota')
mid_income <- c('Nebraska','Hawaii','New Hampshire','Virginia','Pennsylvania','Iowa','Kansas','South Dakota','Oregon','Ohio','Wisconsin','Rhode Island','Louisiana','Utah','Oklahoma','Georgia','Nevada')
relatively_low_income <- c('Indiana','Vermont','North Carolina','Tennessee','Michigan','Missouri','New Mexico','Florida','Arizona','Montana','Maine','Kentucky','South Carolina','Alabama','Idaho','West Virginia')
low_income <- c('Arkansas','Mississippi')

for (i in 1:nrow(df_b)){
  if (df_b$State[i] %in% high_income) {
    df_b$state[i] <- 4
  } else if(df_b$State[i] %in% relatively_high_income){
    df_b$state[i] <- 3
  } else if (df_b$State[i] %in% mid_income) {
    df_b$state[i] <- 2
  } else if (df_b$State[i] %in% relatively_low_income){
    df_b$state[i] <- 1
  } else if (df_b$State[i] %in% low_income) {
    df_b$state[i] <- 0
  }
}
# Encoding variable 'Segment' and 'Category'

dmy <- dummyVars("~Segment + Category",data = df_b)
df_b1 <- data.frame(predict(dmy,newdata = df_b))
df_b2 <- cbind(df_b,df_b1)

#Create a new categorical variable seperating profitable and unprofitable transactions.
for (i in 1:nrow(df_b2)) {
  if (df_b2$profit_ratio[i] >= 0) {
    df_b2$pro_cate[i] = 1
  } else {
    df_b2$pro_cate[i] = 0
  }
}

# Filter for regression decision tree 
df_b2_tree <- df_b2 %>% select(-State,-Segment,-Region,-Category,-`Sub-Category`,-profit_ratio,-Profit_Category)
View(df_b2_tree)

# Modeling #######

# Regression ======

# Regression Decision Tree -------------------------
# Fitting Regression Trees
set.seed(71)
train_ind <- sample(seq_len(nrow(df_b2_tree)), size = 0.8*nrow(df_b2_tree))
train <- df_b2_tree[train_ind, ]
test <- df_b2_tree[-train_ind, ]
View(train)

tree.df <- tree(Profit ~ .,train)
summary(tree.df)
plot(tree.df)
text(tree.df, pretty = 0)

# Testing decision tree model
yhat1 <- predict(tree.df, newdata = test)
df.test <- test[,"Profit"]
plot(yhat1,df.test)
abline(0,1)
MSE_tree <- mean((yhat1 - df.test)^2)
sqrt(MSE_tree)

# Random Forest ------------
rf.df <- randomForest(Profit ~., data = train)
yhat.rf <- predict(rf.df, newdata = test)
MSE_rf <- mean((yhat.rf - df.test)^2)
sqrt(MSE_rf)
importance(rf.df)

# Part Conclusion --------
# The result shows that random forest yields an improvement over regression
# decision tree, but the squared MSE still maintains around 160,
# Which is substantially higher than the mean profit.
# Therefore, we would adopt classification models to get a better fit.

# Classification Models ==========

df_b2_classification <- df_b2 %>% select(-State,-Segment,-Region,-Category,-`Sub-Category`,-Profit,-profit_ratio,-Profit_Category)

str(df_b2_classification)
View(df_b2_classification)


train_cla <- df_b2_classification[train_ind, ]
test_cla <- df_b2_classification[-train_ind, ]
# Logistic Regression --------

logistic.fits <- glm(pro_cate~ ., data = train_cla, family = binomial)
summary(logistic.fits)

logistic.probs <- predict(logistic.fits, test_cla, type = "response")
glm.pred <- rep(0,1999)
glm.pred[logistic.probs > .5] <- 1
table(glm.pred,test_cla$pro_cate)
accuracy_of_LR <- mean(glm.pred == test_cla$pro_cate)

# Supply Vector Machine for Multi-class Classification -----


# SVM with variable 'Profit_Category'
df_b2_svm <-df_b2 %>% select(-State,-Segment,-Region,-Category,-`Sub-Category`,-Profit,-profit_ratio,)
View(df_b2_svm)
train_svm <- df_b2_svm[train_ind, ]
test_svm <- df_b2_svm[-train_ind, ]

svm1 <- svm(Profit_Category ~.-pro_cate, data = train_svm, method = "C-classification", kernel = "radial",gamma = 0.1, cost = 15)
summary(svm1)
svm1$SV

# Testing SVM

svm_prediction <- predict(svm1,test_svm)
svm_tab <- table(test_svm$Profit_Category, svm_prediction)
svm_tab

overall_accuracy <- mean(test_svm$Profit_Category == svm_prediction) 
overall_accuracy

# Visualizing Results:
plot(svm_tab)
plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)


svm_file <- data.frame(test_svm$Profit_Category,svm_prediction)
write.csv(svm_file, file = "svm_file.csv")

heatmap(svm_tab)
