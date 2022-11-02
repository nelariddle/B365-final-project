setwd("C:/Users/nelar/OneDrive/Documents/fall 2022/b365/Final project")
data<-read.csv("data-with-inflation.csv",stringsAsFactors = TRUE)
library(dplyr)

# add a column for percent GPA grades
data$PCT.GPA<-data$GPA.GRADES/data$TOTAL.GRADES
# dont care about career and thesis courses
data<-data%>%filter(LETTER!="Y"&LETTER!="G")
# only undergrad
data<-data%>%filter(COURSE.<500)

# linear model
summary(lm(data$AVG.SECT.GPA~data$NEWSALARY+data$YEAR+data$COURSE.+data$PCT.GPA+data$PERCENT.MAJORS))

# total grades, gpa grades given by course
aggregate(data$GPA.GRADES, by=list(course=data$COURSE.), FUN=sum)
grade.by.course<-aggregate(cbind(data$GPA.GRADES,data$TOTAL.GRADES), by=list(course=data$COURSE.), FUN=sum)
grade.by.course<-cbind(grade.by.course,grade.by.course[,2]/grade.by.course[,3])
colnames(grade.by.course)[4]<-"percent.gpa"
colnames(grade.by.course)[3]<-"total"
colnames(grade.by.course)[2]<-"gpa"


