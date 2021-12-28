training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

sapply(training.data.raw,function(x) sum(is.na(x)))

sapply(training.data.raw, function(x) length(unique(x)))

library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
# drop Cabin and Passengerid

data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

summary(model)

unique(data$Embarked)


#####
# test on the cervical cancer dataset

data2.raw <- read.csv("risk_factors_cervical_cancer.csv", na.strings=c("?"))
dim(data2.raw)

sapply(data2.raw,function(x) sum(is.na(x)))
sapply(data2.raw, function(x) length(unique(x)))
missmap(data2.raw, main = "Missing values vs observed")

data2 <- subset(data2.raw,select=c(8,5,4,10,13,36))

data2 <- data2[!is.na(data2$Num.of.pregnancies),]
data2 <- data2[!is.na(data2$Smokes),]
data2 <- data2[!is.na(data2$Hormonal.Contraceptives),]
data2 <- data2[!is.na(data2$IUD),]
data2 <- data2[!is.na(data2$STDs..number.),]
data2 <- data2[!is.na(data2$Biopsy),] 

sapply(data2,function(x) sum(is.na(x)))

model <- glm(Biopsy ~.,family=binomial(link='logit'),data=data2)

summary(model)
