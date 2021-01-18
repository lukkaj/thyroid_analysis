tdata <- read.csv('Thyroid_data.csv',header = TRUE,sep = ',')
str(tdata)

# checking for missing data
any(is.na(tdata))
any(is.null(tdata))
# no missing data

table(tdata$CLASS)

library(rpart)
library(rpart.plot)
library(caTools)
library(class)

observations<-length(tdata$CLASS)
summary(tdata)

# Class
ggplot(tdata,aes(CLASS,fill=factor(CLASS)))+
  geom_histogram(bins=3)
summary(tdata$CLASS)
boxplot(tdata$CLASS,col='blue')
table(tdata$CLASS)
# T3
ggplot(tdata,aes(T3,fill=factor(CLASS)))+
  geom_histogram()
summary(tdata$T3)
ggplot(tdata,aes(CLASS,T3,fill=factor(CLASS)))+
  geom_boxplot()
vizt3<-ggplot(tdata,aes(1:observations,T3,col=factor(CLASS),xlabel='Observations',ylab='T3 Levels'))+
  geom_point()
vizt3 + labs(x = 'Observations',y = 'T3 Levels')

#TST
ggplot(tdata,aes(TST,fill=factor(CLASS)))+
  geom_histogram()
summary(tdata$TST)
viztst<-ggplot(tdata,aes(1:observations,TST,col=factor(CLASS),xlabel='Observations',ylab='T3 Levels'))+
  geom_point()
viztst + labs(x = 'Observations',y = 'TST Levels')

#TSTR
ggplot(tdata,aes(TSTR,fill=factor(CLASS)))+
  geom_histogram(bins=10)
summary(tdata$TSTR)
viztstr<-ggplot(tdata,aes(1:observations,TSTR,col=factor(CLASS),xlabel='Observations',ylab='T3 Levels'))+
  geom_point()
viztstr + labs(x = 'Observations',y = 'TST Levels')

#TSH
ggplot(tdata,aes(TSH,fill=factor(CLASS)))+
  geom_histogram()
summary(tdata$TSH)
viztsh<-ggplot(tdata,aes(1:observations,TSH,col=factor(CLASS),xlabel='Observations',ylab='T3 Levels'))+
  geom_point()
viztsh + labs(x = 'Observations',y = 'TSH Levels')
ggplot(tdata,aes(CLASS,TSH,fill=factor(CLASS)))+
  geom_boxplot()

#MAD.TSH
ggplot(tdata,aes(MAD.TSH,fill=factor(CLASS)))+
  geom_histogram(bins=10)
summary(tdata$MAD.TSH)
vizmad<-ggplot(tdata,aes(1:observations,MAD.TSH,col=factor(CLASS),xlabel='Observations',ylab='T3 Levels'))+
  geom_point()
vizmad + labs(x = 'Observations',y = 'MAD-TSH Levels')
ggplot(tdata,aes(CLASS,MAD.TSH,fill=factor(CLASS)))+
  geom_boxplot()

# Let's normalizes the data

library(scales)

newtdata = apply(tdata,2,rescale,to=c(0,1))


# Created function to do the job
normalize <- function(x) {
  return((x-min(x))/max(x-min(x)))
} 

norm_tdata<-data.frame(tdata[1],normalize(tdata[2:6]))
str(norm_tdata)
summary(norm_tdata)
table(norm_tdata$CLASS)
# Every variable is in the range 0-1
trainEvector = c()
testEvector = c()

TREETrain=matrix(0,nrow=50,ncol=200)
TREETest=matrix(0,nrow=50,ncol=200)

for (i in 1:50) {
  for(j in 1:200) {
    treemdl1 <- rpart(CLASS~.,data=train,method = 'class',minbucket=i)
    treeTrain = predict(treemdl1,train,type='class')
    treeTest = predict(treemdl1,test,type='class')
    TREETrain[i,j]<-1-mean(train$CLASS==treeTrain)
    TREETest[i,j]<-1-mean(test$CLASS==treeTest)
  }
}

TREETrain<-rowMeans(TREETrain)
TREETest<-rowMeans(TREETest)

resultstree <- data.frame(Leaves=1:50,train_perf=TREETrain,test_perf=TREETest)
ggplot(resultstree,aes(Leaves,train_perf))+
  geom_line(col='blue')+
  geom_line(aes(Leaves,test_perf),col='red')



for (i in 1:10) {
sample <- sample.split(norm_tdata$CLASS,SplitRatio = 0.7)
train <- subset(norm_tdata,sample==TRUE)
test <- subset(norm_tdata,sample==FALSE)

# decision tree
treemdl1 <- rpart(CLASS~.,data=train,method = 'class',minbucket=4)
rpart.plot(treemdl1)
rpart.rules(treemdl1)

# predictions
treeTrain = predict(treemdl1,train,type='class')
treeTest = predict(treemdl1,test,type='class')

# accuracy
conf_mattrain <- table(train$CLASS,treeTrain)
accuracytrain <- sum(diag(conf_mattrain))/sum(conf_mattrain)

conf_mattest <- table(test$CLASS,treeTest)
accuracytest <- sum(diag(conf_mattest))/sum(conf_mattest)

# errors
plotcp(treemdl1,upper = c('size'))
printcp(treemdl1)

train_error <- 1-accuracytrain
test_error <- 1-accuracytest

trainEvector[i] <- train_error
testEvector[i]<- test_error
}

# inspecting the train and test errors after loop

plot(1:length(trainEvector),trainEvector,type='o',col='red',xlim=c(0,11),ylim=c(0,0.2))
lines(1:length(testEvector),testEvector,type='o',col='blue',xlim=c(0,11),ylim=c(0,0.2))
mean(trainEvector)
mean(testEvector)
accuracytest
accuracytrain
mean(1 - trainEvector)

# K-means neigbour model


trainpred <- knn(train[,2:6],train[,2:6],as.factor(train$CLASS),5)
testpred <- knn(train[,2:6],test[,2:6],factor(train$CLASS),5)

accuracy <- function(x) { sum(diag(x))/sum(x)}

tabtrain <- table(train$CLASS,trainpred)
tabtest <- table(test$CLASS,testpred)
accu_trainneighbour<-accuracy(tabtrain)
accu_testneighbour<-accuracy(tabtest)

KNNTrain_error <- 1-accu_trainneighbour
KNNTest_error <- 1-accu_testneighbour

# loop to see how neighbour impact results

KNNTrain=matrix(0,nrow=50,ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)

for (i in 1:50) {
  for(j in 1:200) {
    trainpred <- knn(train[,2:6],train[,2:6],as.factor(train$CLASS),i)
    testpred <- knn(train[,2:6],test[,2:6],as.factor(train$CLASS),i)
    KNNTrain[i,j]<-1-mean(train$CLASS==trainpred)
    KNNTest[i,j]<-1-mean(test$CLASS==testpred)
  }
}

KNNTrain<-rowMeans(KNNTrain)
KNNTest<-rowMeans(KNNTest)

resultsknn <- data.frame(neighbors=1:50,train_perf=KNNTrain,test_perf=KNNTest)
ggplot(resultsknn,aes(neighbors,train_perf))+
  geom_line(col='blue')+
  geom_line(aes(neighbors,test_perf),col='red')

ggplot(resultsknn,aes(neighbors,1-train_perf))+
  geom_line(col='blue')+
  geom_line(aes(neighbors,1-test_perf),col='red')




# same with non-normalized data.


sample <- sample.split(tdata$CLASS,SplitRatio = 0.7)
train2 <- subset(tdata,sample==TRUE)
test2 <- subset(tdata,sample==FALSE)

KNNTrain2=matrix(0,nrow=50,ncol=200)
KNNTest2=matrix(0,nrow=50,ncol=200)

trainpred2 <- knn(train2[,2:6],train2[,2:6],as.factor(train2$CLASS),5)
testpred2 <- knn(train2[,2:6],test2[,2:6],as.factor(train2$CLASS),5)

tabtrain2 <- table(train2$CLASS,trainpred2)
tabtest2 <- table(test2$CLASS,testpred2)

for (i in 1:50) {
  for(j in 1:200) {
    trainpred2 <- knn(train2[,2:6],train2[,2:6],as.factor(train2$CLASS),i)
    testpred2 <- knn(train2[,2:6],test2[,2:6],as.factor(train2$CLASS),i)
    KNNTrain2[i,j]<-1-mean(train2$CLASS==trainpred2)
    KNNTest2[i,j]<-1-mean(test2$CLASS==testpred2)
  }
}

KNNTrain2<-rowMeans(KNNTrain2)
KNNTest2<-rowMeans(KNNTest2)

resultsknn2 <- data.frame(neighbors=1:50,train_perf2=KNNTrain2,test_perf2=KNNTest2)
ggplot(resultsknn2,aes(neighbors,train_perf2))+
  geom_line(col='blue')+
  geom_line(aes(neighbors,test_perf2),col='red')

ggplot(resultsknn2,aes(neighbors,1-train_perf2))+
  geom_line(col='blue')+
  geom_line(aes(neighbors,1-test_perf2),col='red')

