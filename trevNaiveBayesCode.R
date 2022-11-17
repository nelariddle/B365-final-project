# Code used from https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

data = read.csv("data-with-salary-class.csv")
dimnames(data)

library(dplyr)

# Data cleaning from Nela
# add a column for percent GPA grades
data$PCT.GPA <- data$GPA.GRADES / data$TOTAL.GRADES
# dont care about career and thesis courses
data <- data %>% filter(LETTER != "Y" & LETTER != "G")
# only undergrad
data <- data %>% filter(COURSE. < 500)

## More Data cleaning
# Select only important columns
data = data[,c("COURSE.", "PERCENT.MAJORS", "AVG.SECT.GPA", "SALARYCLASS")]
# Flooring the course numbers to only 1,2,3, and 4
data["COURSE."] = floor(data["COURSE."] / 100)

split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")
train
test
#train_scale <- scale(train[, 5])
#test_scale <- scale(test[, 5])

set.seed(120)
classifier_cl <- naiveBayes(SALARYCLASS ~ ., data = train)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test)

cm <- table(test$SALARYCLASS, y_pred)
cm

confusionMatrix(cm)

# Trying again but with the new classes

data = read.csv("data-with-salary-class2.csv")
data$PCT.GPA <- data$GPA.GRADES / data$TOTAL.GRADES
data <- data %>% filter(LETTER != "Y" & LETTER != "G")
data <- data %>% filter(COURSE. < 500)
data = data[,c("COURSE.", "PERCENT.MAJORS", "AVG.SECT.GPA", "SALARYCLASS2")]
data["COURSE."] = floor(data["COURSE."] / 100)

split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")
train
test

set.seed(120)
classifier_cl <- naiveBayes(SALARYCLASS2 ~ ., data = train)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test)

cm <- table(test$SALARYCLASS2, y_pred)
cm

confusionMatrix(cm)
