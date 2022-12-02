# setwd("C:/Users/nelar/OneDrive/Documents/fall 2022/b365/Final project")
data <-
  read.csv("data-with-salary-class.csv", stringsAsFactors = TRUE)
# uncomment if not yet installed
# install.packages("dplyr")
# install.packages("english")
library(dplyr)
library(english)

# add a column for percent GPA grades
data$PCT.GPA <- data$GPA.GRADES / data$TOTAL.GRADES
# dont care about career and thesis courses
data <- data %>% filter(LETTER != "Y" & LETTER != "G")
# only undergrad
data <- data %>% filter(COURSE. < 500)

# create instructor data
# TOTAL.GRADES.INSTRUCTOR is the total # of grades given by an instructor that year
# AVG.GPA INSTRUCTOR is the average GPA of grades given by an instructor that year
instructor.data <-
  data %>% group_by(INSTRUCTOR.NAME, YEAR) %>% mutate(
    TOTAL.INSTRUCTOR.GRADES =
      sum(TOTAL.GRADES),
    AVG.INSTRUCTOR.GPA =
      sum(AVG.SECT.GPA *
            TOTAL.GRADES) /
      sum(TOTAL.GRADES)
  ) %>% ungroup()

# single variable analysis
library(english)
X <-
  instructor.data %>% select(
    TOTAL.INSTRUCTOR.GRADES,
    AVG.SECT.GPA,
    YEAR,
    COURSE.,
    PERCENT.MAJORS,
    AVG.INSTRUCTOR.GPA,
    AVG.STDNT.CUM.GPA
  ) %>% as.matrix()
y <- instructor.data %>% select(NEWSALARY) %>% as.matrix()
n_attrs <- ncol(X)
sses <- rep(0, n_attrs)
attr_perf <- array(0, dim = c(n_attrs, 4))
for (i in 1:n_attrs) {
  plot(X[, i], y, xlab = colnames(X)[i], ylab = colnames(y)[1])
  d <- cbind(X[, i] ^ 0, X[, i] ^ 1)
  a <- solve(t(d) %*% d, t(d) %*% y)
  abline(a[1], a[2], col = "purple")
  yhat <- d %*% a
  error <- y - yhat
  sse <- sum(error ^ 2)
  sses[i] <- sse
  attr_perf[i, ] <- c(colnames(X)[i], round(sse, 3), a[1], a[2])
}
for (rank in order(sses)) {
  cat(
    attr_perf[rank, 1],
    "\nerror:",
    round(as.numeric(attr_perf[rank, 2], 3)),
    "intercept:",
    round(as.numeric (attr_perf[rank, 3]), 3),
    "slope:",
    round  (as.numeric (attr_perf[rank, 4]), 3),
    "\n"
  )
}

# variable selection
X <- instructor.data %>% select(
  AVG.SECT.GPA,
  TOTAL.INSTRUCTOR.GRADES,
  YEAR,
  COURSE.,
  PERCENT.MAJORS,
  AVG.INSTRUCTOR.GPA,
  AVG.STDNT.CUM.GPA
) %>% as.matrix()
y <- instructor.data %>% select(NEWSALARY) %>% as.matrix()
n_attrs <- ncol(X)




used <- rep(FALSE, n_attrs)
var <- rep(0, n_attrs)
bestsse <-
  rep(10000000000000, n_attrs)
bestA <- rep(0, 3)
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
      bestA <- a
    }
    used[i] <- FALSE
  }
  used[var[j]] <- TRUE
  # cat(bestA, "\n")
  cat("the",
      ordinal(j),
      "best attribute is",
      colnames(X)[var[j]],
      ", error:",
      bestsse[j],
      "\n")
  result <- paste(colnames(y), "=", bestA[1], "+")
  for (i in 2:(j + 1)) {
    result <- paste(result, bestA[i])
    result <- paste(result, "(", rownames(bestA)[i], ") +")
  }
  result <- substring(result, 1, nchar(result) - 2)
  cat(result, "\n\n")
}
plot(bestsse,
     main = "squared error vs. num attributes used",
     xlab = "num attributes",
     ylab = "squared error")

# overall predictions
instructor.data <- instructor.data %>% mutate(PREDICTION)
instructor.data <- instructor.data %>% mutate(PREDICTION.DIFF)
for (row in 1:nrow(instructor.data)) {
  obs <- c(1, X[row, ])
  salary <- y[row, ]
  output <- t(as.matrix(obs)) %*% as.matrix(bestA)
  instructor.data$PREDICTION[row] <- output
  instructor.data$PREDICTION.DIFF[row] <- output - salary
}
plot(
  instructor.data$NEWSALARY,
  instructor.data$PREDICTION,
  xlab = "salary",
  ylab = "predicted salary",
  main = "really good salary predictions",
  col = "purple"
)
lines(c(60000, 150000), c(60000, 150000))

# individual predictions
rowsToPredict <- c(1732, 1667, 1702, 1628)
for (row in rowsToPredict) {
  cat(
    "name:",
    toString(instructor.data$INSTRUCTOR.NAME[row]),
    "| term:",
    toString(instructor.data$TERM.DESCRIPTION[row]),
    "| class:",
    toString(instructor.data$COURSE.DESCRIPTION[row]),
    "\nguess:",
    instructor.data$PREDICTION[row],
    "| actual salary:",
    instructor.data$NEWSALARY[row],
    "\n"
  )
}

