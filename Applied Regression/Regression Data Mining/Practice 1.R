# DATA MINING PRACTICAL CLASS 1 #

data <- read.csv("Masters(GLEB)/loan_train.csv",stringsAsFactors=TRUE)
View(data)

class(data)
str(data)
anyNA(data)
data1=na.omit(data)
B=data[,1:6]
summary(data)
mean(data$Applicant_Income )
attach(data)
hist(Applicant_Income)
table(Education)
