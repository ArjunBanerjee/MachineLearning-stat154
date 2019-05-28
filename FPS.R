
data = read.csv("dc-wikia-data.csv", header = TRUE)
data2 = read.csv("marvel-wikia-data.csv", header = TRUE)
colnames(data2)[13] = "YEAR"
# keep only variables of interest

data = data.frame(data$ID, data$ALIGN, data$EYE, data$HAIR, data$SEX, data$ALIVE)
data2 = data.frame(data2$ID, data2$ALIGN, data2$EYE, data2$HAIR, data2$SEX, data2$ALIVE)
#take out missing values

data[data==""] <- NA
data = na.omit(data)

data2[data2==""] <- NA
data2 = na.omit(data2)
#summarizes the data

summary(data)

#create factor variables
                                                                 
data$ID.f = factor(data$data.ID)
data$ALIGN.f = factor(data$data.ALIGN)
data$SEX.f = factor(data$data.SEX)
data$ALIVE.f = factor(data$data.ALIVE)

data2$ID.f = factor(data2$data2.ID)
data2$ALIGN.f = factor(data2$data2.ALIGN)
data2$SEX.f = factor(data2$data2.SEX)
data2$ALIVE.f = factor(data2$data2.ALIVE)

#create dependent variable: good guys versus all the others

data$GOOD <- ifelse(data$ALIGN.f == 'Good Characters', 1, 0)
data$GOOD.f = factor(data$GOOD)
summary(data$GOOD.f)

data2$GOOD <- ifelse(data2$ALIGN.f == 'Good Characters', 1, 0)
data2$GOOD.f = factor(data2$GOOD)
summary(data2$GOOD.f)
#create dependent variable: good guys versus all the others

condition1 = (data$data.EYE == 'Black Eyes'|data$data.EYE == 'Blue Eyes'|data$data.EYE=='Brown Eyes'|data$data.EYE=='Green Eyes')
data$EYE <- ifelse(condition1, data$data.EYE, 'Other')
data$EYE.f = factor(data$EYE)

condition1 = (data2$data2.EYE == 'Black Eyes'|data2$data2.EYE == 'Blue Eyes'|data2$data2.EYE=='Brown Eyes'|data2$data2.EYE=='Green Eyes')
data2$EYE <- ifelse(condition1, data2$data2.EYE, 'Other')
data2$EYE.f = factor(data2$EYE)

condition2 = (data$data.HAIR == 'Blond Hair'|data$data.HAIR == 'Brown Hair')
data$HAIR <- ifelse(condition2, data$data.HAIR, 'Other')
data$HAIR.f = factor(data$HAIR)

condition2 = (data2$data2.HAIR == 'Blond Hair'|data2$data2.HAIR == 'Brown Hair')
data2$HAIR <- ifelse(condition2, data2$data2.HAIR, 'Other')
data2$HAIR.f = factor(data2$HAIR)
summary(data)

#divide in two datasets

set.seed(123)
train=sample(1:nrow(data), 2*nrow(data)/3)
data.train <- data[train,]
data.test <- data[-train,]

train=sample(1:nrow(data2), 2*nrow(data2)/3)
data2.train <- data2[train,]
data2.test <- data2[-train,]
#fit logistic regression

#fit random forest
library(e1071)
library(randomForest)
library(class)



#Tuning
tuned = tune.svm(GOOD.f~ID.f + EYE.f + HAIR.f + SEX.f + ALIVE.f, data=data.train, gamma = 10^(-5:-1), cost = 10^(-1:3)) 
summary(tuned)

svmfit <- svm(GOOD.f~ID.f + EYE.f + HAIR.f + SEX.f + ALIVE.f, data=data.train, kernel="radial", gamma=.01, cost=100)
svm1 <- predict(svmfit,data.test)
tab <- table(true=data.test$GOOD.f,pred=svm1)
1-mean(svm1==data.test$GOOD.f)
tab

tuned2 = tune.svm(GOOD.f~ID.f + EYE.f + HAIR.f + SEX.f + ALIVE.f, data=data2.train, gamma = 10^(-5:-1), cost = 10^(-1:3)) 
summary(tuned2)

svmfit2 <- svm(GOOD.f~ID.f + EYE.f + HAIR.f + SEX.f + ALIVE.f, data=data2.train, kernel="radial", gamma=10^-4, cost=1000)
svm2 <- predict(svmfit2,data2.test)
tab2 <- table(true=data2.test$GOOD.f,pred=svm2)
1-mean(svm2==data2.test$GOOD.f)
tab2
