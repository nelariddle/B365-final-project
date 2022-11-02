data = read.csv("data-with-inflation.csv")
data
dimnames(data)
data[1,"NEWSALARY"]
# Creating classes for salaries
for (i in 1:nrow(data)) {
  if (data[i,"NEWSALARY"] <= 75000) {
    data[i,"SALARYCLASS"] = 0
  }
  else if (data[i,"NEWSALARY"] <= 100000) {
    data[i,"SALARYCLASS"] = 1
  }
  else if (data[i,"NEWSALARY"] <= 125000) {
    data[i,"SALARYCLASS"] = 2
  }
  else if (data[i,"NEWSALARY"] <= 150000) {
    data[i,"SALARYCLASS"] = 3
  }
  else if (data[i,"NEWSALARY"] <= 175000) {
    data[i,"SALARYCLASS"] = 4
  }
  else if (data[i,"NEWSALARY"] <= 200000) {
    data[i,"SALARYCLASS"] = 5
  }
  else if (data[i,"NEWSALARY"] > 100000) {
    data[i,"SALARYCLASS"] = 6
  }
}

data[1,"SALARYCLASS"]

write.csv(data, "data-with-salary-class.csv")
