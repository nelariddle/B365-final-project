data <- read.csv("data-working.csv")
#data
#dimnames(data)
unique(data[,"YEAR"])
inflation = c(1.97, 1.92, 1.87, 1.84, 1.81, 1.76, 1.70, 1.68, 1.63, 1.60, 1.56, 1.50, 
              1.47, 1.41, 1.41, 1.37, 1.35, 1.31, 1.29, 1.27, 1.27, 1.25, 1.22, 1.20, 
              1.18, 1.15, 1.13, 1.06)

#class(data[1,"YEAR"])
for(i in 1:nrow(data)){
  index = data[i, "YEAR"] - 1994
  data[i, "NEWSALARY"] = data[i, "SALARY"] * inflation[index]
}
#data[1,]

write.csv(data, "data-with-inflation.csv")
