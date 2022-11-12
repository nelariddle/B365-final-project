data<-read.csv("data-with-inflation.csv",stringsAsFactors = TRUE)
library(ggplot2)
library(dplyr)

## Data cleaning Nela did:
# dont care about career and thesis courses
data<-data%>%filter(LETTER!="Y"&LETTER!="G")
# only undergrad
data<-data%>%filter(COURSE.<500)

## More Data cleaning
# Select only important columns
data = data[,c("YEAR", "COURSE.", "PERCENT.MAJORS", "NEWSALARY", "AVG.SECT.GPA")]

# Only use if you want to put the course numbers into bins
#data["COURSE."] = floor(data["COURSE."] / 100)

# Test linear model with each of the variables for the best model
allSE = rep(0, 4)
for(i in 1:(length(data[1,])-1)){
  d = cbind(rep(1,length(data[,1])), data[,i])
  a = solve(t(d) %*% d, t(d) %*% data[,length(data)])
  yhat = d %*% a
  error = data[,5] - yhat
  se = sum(error ^ 2)
  allSE[i] = se
  print(paste("Squared Error for", colnames(data)[i], "is", round(se, 3)))
}

# Printing out the order of how good the predictors were
ordered = order(allSE)
print(paste("The best predictor for GPA is", colnames(data)[ordered[1]], "with squared error", allSE[ordered[1]]))
print(paste("The second best predictor for GPA is", colnames(data)[ordered[2]], "with squared error", allSE[ordered[2]]))
print(paste("The third best predictor for GPA is", colnames(data)[ordered[3]], "with squared error", allSE[ordered[3]]))
print(paste("The worst predictor for GPA is", colnames(data)[ordered[4]], "with squared error", allSE[ordered[4]]))

# Bar chart of the SSE for the single variable regression
# Not used in paper or presentation
ggplot() +
  geom_bar(aes(x = (colnames(data)[1:4]), y = allSE, fill = (colnames(data)[1:4])), stat = "identity") + 
  geom_text(aes(x = (colnames(data)[1:4]), y = allSE, label = round(allSE, 2)), vjust = 2)

# Test linear model for 2nd best predictor
# Not important because the one below does the same thing but better
# Only used if you want to do the 2-variable graph because the one below
#     isn't set up to calculate yhat2 or the equation
for(i in 1:4){
  if(i != 1){
    d = cbind(rep(1,length(data[,1])), data[,i], data[,1])
    a = solve(t(d) %*% d, t(d) %*% data[,length(data)])
    yhat2 = d %*% a
    error = data[,5] - yhat2
    se = sum(error ^ 2)
    print(paste("Squared Error for year and", colnames(data)[i], "is", round(se, 3)))
  }
}

print("Second best predictor combined with year is course number")
print(paste("The linear model for the effect of year and course number on GPA is ",
            round(a[1], 7)," + ", round(a[2], 7),"[COURSE.] + ", round(a[3], 7), "[YEAR]", sep = ""))

# Not using this plot- ggplot is just better
#plot(data[,"COURSE."], data[,"AVG.SECT.GPA"])
#lines(sort(data[,"COURSE."]), sort(yhat2))

# Creating the ggplot of the linear relationship between GPA and year
#     (with course number as the color)
ggplot() +
  geom_point(data = data, aes(x = data[,"YEAR"], y = data[,"AVG.SECT.GPA"], color = data[,"COURSE."])) +
  geom_smooth(aes(x = sort(data[,"YEAR"]), y = sort(yhat)), method = "lm") +
  labs(color = "Course Number", title = "Effect of Year on GPA", x = "Year",
       y = "Average Student GPA")

# Linear relationship with both year and course number considered
ggplot() +
  geom_point(data = data, aes(x = data[,"YEAR"], y = data[,"AVG.SECT.GPA"], color = data[,"COURSE."])) +
  geom_smooth(aes(x = sort(data[,"YEAR"]), y = sort(yhat)), method = "lm") +
  geom_smooth(aes(x = sort(data[,"YEAR"]), y = sort(yhat2)), method = "lm", color = "red") +
  labs(color = "Course Number", title = "Effect of Year on GPA", x = "Year",
       y = "Average Student GPA")

# Finding the graph of the SSE as the number of attributes used increases
bestSSE = rep(0,4)
n = length(data[,1])
ncol = length(data) - 1
used = rep(0, (n-1))
prev = rep(1,length(data[,1]))
for(j in 1:ncol){
  allSE = rep(0, 4)
  for(i in 1:ncol){
    if(sum(used == i) == 0){
      d = cbind(prev, data[,i])
      a = solve(t(d) %*% d, t(d) %*% data[,length(data)])
      yhat = d %*% a
      error = data[,5] - yhat
      se = sum(error ^ 2)
      allSE[i] = se
      print(paste("Squared Error for", colnames(data)[i], "is", round(se, 3)))
    }
  }
  o = order(allSE)
  used[j] = o[j]
  prev = cbind(prev, data[,o[j]])
  bestSSE[j] = allSE[o[j]]
  print(paste("Attribute",j, "is", colnames(data)[o[j]]))
}
print(bestSSE)

# The graph of the change in SSE as more attributes are added
ggplot() +
  geom_point(aes(x = 1:4, y = bestSSE)) + 
  geom_smooth(aes(x = 1:4, y = bestSSE), method = "lm") + 
   xlab("Number of attributes used") +
  ylab("Sum of squared error") + 
  ggtitle("Sum of squared error predicting GPA of a class")



