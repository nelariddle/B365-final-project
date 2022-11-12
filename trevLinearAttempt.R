data <- read.csv("data-with-salary-class.csv", stringsAsFactors = TRUE)
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
data = data[,c("YEAR", "COURSE.", "PERCENT.MAJORS", "NEWSALARY", "AVG.SECT.GPA")]
# Flooring the course numbers to only 1,2,3, and 4
data["COURSE."] = floor(data["COURSE."] / 100)


# Attempting simple regression using some of our identified useful variables
# Testing course number
x = data$COURSE.
y = data$NEWSALARY
n = length(x)
plot(x,y)

xbar = sum(x)/n
ybar = sum(y)/n
xybar = sum(x*y)/n
xsqbar = sum(x*x)/n

b = (ybar*xsqbar-xbar*xybar)/ (xsqbar - xbar*xbar)  # from our calculations
a = (ybar - b)/xbar

abline(b,a)

# Testing average section GPA
x = data$AVG.SECT.GPA
plot(x,y)

xbar = sum(x)/n
ybar = sum(y)/n
xybar = sum(x*y)/n
xsqbar = sum(x*x)/n

b = (ybar*xsqbar-xbar*xybar)/ (xsqbar - xbar*xbar)  # from our calculations
a = (ybar - b)/xbar

abline(b,a)

# Testing year
x = data$YEAR
plot(x,y)

xbar = sum(x)/n
ybar = sum(y)/n
xybar = sum(x*y)/n
xsqbar = sum(x*x)/n

b = (ybar*xsqbar-xbar*xybar)/ (xsqbar - xbar*xbar)  # from our calculations
a = (ybar - b)/xbar

abline(b,a)
