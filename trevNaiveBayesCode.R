
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

split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")

train_scale <- scale(train[, 17:38])
test_scale <- scale(test[, 17:38])

set.seed(120)
classifier_cl <- naiveBayes(SALARYCLASS ~ ., data = train)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test)

cm <- table(test$SALARYCLASS, y_pred)
cm

confusionMatrix(cm)
