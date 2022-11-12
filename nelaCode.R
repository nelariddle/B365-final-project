setwd("C:/Users/nelar/OneDrive/Documents/fall 2022/b365/Final project")
data <-
  read.csv("data-with-salary-class.csv", stringsAsFactors = TRUE)
library(dplyr)
library(ggplot2)

# add a column for percent GPA grades
data$PCT.GPA <- data$GPA.GRADES / data$TOTAL.GRADES
# dont care about career and thesis courses
data <- data %>% filter(LETTER != "Y" & LETTER != "G")
# only undergrad
data <- data %>% filter(COURSE. < 500)
#data <- data %>% filter(COURSE. > 199)
# salary class
#data$bruh<-floor((data$NEWSALARY-75000)/25000)+1

# linear model
summary(
  lm(
    data$NEWSALARY ~ data$AVG.SECT.GPA + data$YEAR + data$COURSE. + data$PCT.GPA +
      data$PERCENT.MAJORS + data$LETTER
  )
)

# linear model 2
useful.data <-
  instructor.data %>% select(
    YEAR,
    INSTRUCTOR.NAME,
    LETTER,
    COURSE.,
    TOTAL.GRADES,
    AVG.SECT.GPA,
    AVG.STDNT.CUM.GPA,
    PERCENT.MAJORS,
    TOTAL.GRADES.INSTRUCTOR,
    AVG.GPA.INSTRUCTOR,
    NEWSALARY
  )
for (i in 1:7) {
  X <- as.matrix(useful.data[, 4:10])[,-i]
  y <- as.matrix(useful.data[, 11])
  a <- solve(t(X) %*% X, t(X) %*% y)
  yhat <- X %*% a
  error <- y - yhat
  sse <- sum(error ^ 2)
  cat("sse from omitting", colnames(useful.data)[i + 3], ":", sse, "\n")
}

# create instructor data
instructor.data <-
  data %>% group_by(INSTRUCTOR.NAME, YEAR) %>% mutate(
    TOTAL.GRADES.INSTRUCTOR =
      sum(TOTAL.GRADES),
    AVG.GPA.INSTRUCTOR =
      sum(AVG.SECT.GPA *
            TOTAL.GRADES) /
      sum(TOTAL.GRADES)
  )

# group by instructor, plot total grades
instructor.data %>%
  ggplot(aes(TOTAL.GRADES.INSTRUCTOR, NEWSALARY, color = COURSE.)) + geom_point() +
  geom_smooth(color = "purple")

# group by instructor, plot average GPA
instructor.data %>% ggplot(aes(AVG.GPA.INSTRUCTOR, NEWSALARY, color = YEAR)) + geom_point()

# linear model with total grades
fit <- lm(data = instructor.data
          , NEWSALARY ~ TOTAL.GRADES.INSTRUCTOR)


# total grades, gpa grades given by course
aggregate(data$GPA.GRADES,
          by = list(course = data$COURSE.),
          FUN = sum)
grade.by.course <-
  aggregate(
    cbind(data$GPA.GRADES, data$TOTAL.GRADES),
    by = list(course = data$COURSE.),
    FUN = sum
  )
grade.by.course <-
  cbind(grade.by.course, grade.by.course[, 2] / grade.by.course[, 3])
colnames(grade.by.course)[4] <- "percent.gpa"
colnames(grade.by.course)[3] <- "total"
colnames(grade.by.course)[2] <- "gpa"

# tree
library(rpart)
tree <-
  rpart(data$NEWSALARY ~ data$AVG.SECT.GPA + data$COURSE. + data$PERCENT.MAJORS)
plot(tree, uniform = TRUE, margin = 0.2)
text(tree,
     use.n = TRUE,
     all = TRUE,
     cex = .8)