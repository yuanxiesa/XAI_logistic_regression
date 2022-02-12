data.raw <- read.csv("raw_data/risk_factors_cervical_cancer.csv", na.strings=c("?"))
dim(data.raw)

library(Amelia)
sapply(data.raw,function(x) sum(is.na(x)))
sapply(data.raw, function(x) length(unique(x)))
missmap(data.raw, main = "Missing values vs observed")

data <- subset(data.raw,select=c(8,5,4,10,13,36))

data <- data[!is.na(data$Num.of.pregnancies),]
data <- data[!is.na(data$Smokes),]
data <- data[!is.na(data$Hormonal.Contraceptives),]
data <- data[!is.na(data$IUD),]
data <- data[!is.na(data$STDs..number.),]
data <- data[!is.na(data$Biopsy),] 

sapply(data,function(x) sum(is.na(x)))

## use all parameters
model <- glm(Biopsy ~.,family=binomial(link='logit'),data=data)
summary(model) 

## test different models

model_1 <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(1,6)))
summary(model_1) # 0.902

model_2 <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(2,6)))
summary(model_2) # 0.295

model_3 <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(3,6)))
summary(model_3) # 0.2

model_4 <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(4,6)))
summary(model_4) # 0.05

model_5 <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(5,6)))
summary(model_5) # 0.0026

# select predictors
model <- glm(Biopsy ~.,family=binomial(link='logit'),data=dplyr::select(data,c(4,5,6)))
summary(model)



