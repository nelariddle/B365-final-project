setwd("C:/Users/nelar/OneDrive/Documents/fall 2022/b365/Final project")
data<-read.csv("data-with-inflation.csv",stringsAsFactors = TRUE)
library(dplyr)

# add a column for percent GPA grades
data$PCT.GPA<-data$GPA.GRADES/data$TOTAL.GRADES
# dont care about career and thesis courses
data<-data%>%filter(LETTER!="Y"&LETTER!="G")
# only undergrad
data<-data%>%filter(COURSE.<500)
# dont care about 100 level
data<-data%>%filter(COURSE.>199)

summary(lm(data$AVG.SECT.GPA~data$NEWSALARY+data$YEAR+data$COURSE.+data$PCT.GPA+data$PERCENT.MAJORS))
