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

# practice linear model
summary(
  lm(
    data$NEWSALARY ~ data$AVG.SECT.GPA + data$YEAR + data$COURSE. + data$PCT.GPA +
      data$PERCENT.MAJORS
  )
)

# create instructor data
instructor.data <-
  data %>% group_by(INSTRUCTOR.NAME, YEAR) %>% mutate(
    TOTAL.GRADES.INSTRUCTOR =
      sum(TOTAL.GRADES),
    AVG.GPA.INSTRUCTOR =
      sum(AVG.SECT.GPA *
            TOTAL.GRADES) /
      sum(TOTAL.GRADES)
  ) %>% ungroup()

# group by instructor, plot total grades
instructor.data %>%
  ggplot(aes(TOTAL.GRADES.INSTRUCTOR, NEWSALARY, color = COURSE.)) + geom_point() +
  geom_smooth(color = "purple")

# group by instructor, plot average GPA
instructor.data %>% ggplot(aes(AVG.GPA.INSTRUCTOR, NEWSALARY, color = YEAR)) + geom_point()

# linear model with total grades
fit <- lm(
  data = instructor.data
  ,
  NEWSALARY ~ TOTAL.GRADES.INSTRUCTOR + AVG.SECT.GPA + YEAR + COURSE. + PCT.GPA +
    PERCENT.MAJORS
)
summary(fit)

# single variable analysis
library(english)
X <-
  instructor.data %>% select(
    TOTAL.GRADES.INSTRUCTOR,
    AVG.SECT.GPA,
    YEAR,
    COURSE.,
    PCT.GPA,
    PERCENT.MAJORS,
    AVG.GPA.INSTRUCTOR,
    AVG.STDNT.CUM.GPA
  ) %>% as.matrix()
y <- instructor.data %>% select(NEWSALARY) %>% as.matrix()
n_attrs <- ncol(X)
sses <- rep(0, n_attrs)
for (i in 1:n_attrs) {
  plot(X[, i], y, xlab = colnames(X)[i], ylab = colnames(y)[1])
  d <- cbind(X[, i] ^ 0, X[, i] ^ 1)
  a <- solve(t(d) %*% d, t(d) %*% y)
  abline(a[1], a[2], col = "purple")
  yhat <- d %*% a
  error <- y - yhat
  sse <- sum(error ^ 2)
  sses[i] <- sse
  cat(
    "sse for",
    colnames(X)[i],
    "is",
    round(sse, 3),
    "with intercept",
    a[1],
    "and slope",
    a[2],
    "\n"
  )
}
i <- 1
for (attr in colnames(X)[order(sses)]) {
  cat("the", ordinal(i), "best attribute is", attr, "\n")
  i = i + 1
}

# variable selection
X <- instructor.data %>% select(
  TOTAL.GRADES.INSTRUCTOR,
  AVG.SECT.GPA,
  YEAR,
  COURSE.,
  PCT.GPA,
  PERCENT.MAJORS,
  AVG.GPA.INSTRUCTOR,
  AVG.STDNT.CUM.GPA
) %>% as.matrix()
y <- instructor.data %>% select(NEWSALARY) %>% as.matrix()
n_attrs <- ncol(X)
used <-
  rep(FALSE, n_attrs)
var <- rep(0, n_attrs)
bestsse <-
  rep(10000000000000, n_attrs)
for (j in 1:n_attrs)  {
  for (i in which(used == FALSE)) {
    used[i] <- TRUE
    XX <- cbind(X[, 1] ^ 0, X[, used])
    a <- solve(t(XX) %*% XX , t(XX) %*% y)
    yhat <- XX %*% a
    error <- y - yhat
    sse <- sum(error ^ 2)
    if (sse < bestsse[j]) {
      bestsse[j] <- sse
      var[j] <- i
    }
    used[i] <- FALSE
    
  }
  used[var[j]] <-
    TRUE
  cat("the", ordinal(j), "best attribute is", colnames(X)[j], '\n')
}
plot(bestsse,
     main = "squared error vs. num attributes used",
     xlab = "num attributes",
     ylab = "squared error")



# total grades, gpa grades given by course
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
  rpart(data=instructor.data,NEWSALARY ~ TOTAL.GRADES.INSTRUCTOR,AVG.SECT.GPA + COURSE. + PERCENT.MAJORS)
plot(tree, uniform = TRUE, margin = 0.2)
text(tree,
     use.n = TRUE,
     all = TRUE,
     cex = .8)