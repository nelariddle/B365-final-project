data = read.csv("data-with-inflation.csv")
data
dimnames(data)
data[1,"NEWSALARY"]
# Creating classes for salaries
for (i in 1:nrow(data)) {
  if (data[i,"NEWSALARY"] <= 75000) {
    data[i,"SALARYCLASS2"] = 0
  }

  else if (data[i,"NEWSALARY"] <= 100000) {
    data[i,"SALARYCLASS2"] = 1
  }
  else if (data[i,"NEWSALARY"] <= 125000) {
    data[i,"SALARYCLASS2"] = 2
  }
  else if (data[i,"NEWSALARY"] <= 150000) {
    data[i,"SALARYCLASS2"] = 3
  }
  else if (data[i,"NEWSALARY"] > 150000) {
    data[i,"SALARYCLASS2"] = 4
  }
}

data[1,"SALARYCLASS2"]

write.csv(data, "data-with-salary-class2.csv")
