data<-read.csv("data-with-inflation.csv",stringsAsFactors = TRUE)
library(ggplot2)

## Data cleaning Nela did:
# dont care about career and thesis courses
data<-data%>%filter(LETTER!="Y"&LETTER!="G")
# only undergrad
data<-data%>%filter(COURSE.<500)

## More Data cleaning
# Select only important columns
data = data[,c("YEAR", "COURSE.", "PERCENT.MAJORS", "NEWSALARY", "AVG.SECT.GPA")]
data["COURSE."] = floor(data["COURSE."] / 100)

# Test linear model with each of the variables for the best model
for(i in 1:(length(data[1,])-1)){
  d = cbind(rep(1,length(data[,1])), data[,i])
  a = solve(t(d) %*% d, t(d) %*% data[,length(data)])
  yhat = d %*% a
  error = data[,5] - yhat
  se = sum(error ^ 2)
  print(paste("Squared Error for", colnames(data)[i], "is", round(se, 3)))
}

print("Year is the 1st best predictor for the GPA of the students")

# Test linear model for 2nd best predictor
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

plot(data[,"COURSE."], data[,"AVG.SECT.GPA"])
lines(sort(data[,"COURSE."]), sort(yhat2))

ggplot() +
  geom_point(data = data, aes(x = data[,"YEAR"], y = data[,"AVG.SECT.GPA"], color = factor(data[,"COURSE."]))) +
  geom_smooth(aes(x = sort(data[,"YEAR"]), y = sort(yhat)), method = "lm")

ggplot() +
  geom_point(data = data, aes(x = data[,"COURSE."], y = data[,"AVG.SECT.GPA"], color = data[,"YEAR"])) +
  geom_smooth(aes(x = sort(data[,"COURSE."]), y = sort(yhat2)), method = "lm") +
  scale_color_gradient(low = "red", high = "blue")





