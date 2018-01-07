library(dplyr)
library(readr)

#cleaned data for 2017 with regions instead of states for locations
cleaned2017 <- read_csv("D:/Dropbox/WPI/DS 502/Project/cleaned2017_regions.csv")
cleaned2017=cleaned2017[-c(1)]
unique(cleaned2017$STATUS)
barplot(prop.table(table(cleaned2017$STATUS)),density=c(75,80,90,100),col=c('red','blue','green','brown'),main="Status Class Distribution (Before)")
# Considering "CERTIFED-WITHDRAWN", "WITHDRAWN", "DENIED" as a single category called "NON-CERTIFIED"
cleaned2017[-which(cleaned2017$STATUS=="CERTIFIED"),]$STATUS="NOT CERTIFIED"
cleaned2017$STATUS=as.factor(cleaned2017$STATUS)
barplot(prop.table(table(cleaned2017$STATUS)),density=c(90,100),col=c('red','blue'),main="Status Class Distribution (After)")

jobs=cleaned2017 %>% group_by(LCA_CASE_SOC_NAME) %>% summarise(count=n(),percent=round(count*100/nrow(cleaned2017),5)) %>% arrange(desc(count))
#SOC_NAME converted to a binary factor, 1 if one of the top 5 occupational groups, 0 otherwise
cleaned2017$LCA_CASE_SOC_NAME_1 = 0
cleaned2017 <- within(cleaned2017, LCA_CASE_SOC_NAME_1[LCA_CASE_SOC_NAME=="COMPUTER SYSTEMS ANALYSTS" | LCA_CASE_SOC_NAME=="COMPUTER PROGRAMMERS" | grepl("ENGINEERS", LCA_CASE_SOC_NAME) | LCA_CASE_SOC_NAME=="SOFTWARE DEVELOPERS, APPLICATIONs"] <- 1)
cleaned2017$LCA_CASE_SOC_NAME_1=as.factor(cleaned2017$LCA_CASE_SOC_NAME_1)
summary(cleaned2017)

#Converting string variables to factor
cleaned2017$LCA_CASE_SUBMIT=as.factor(cleaned2017$LCA_CASE_SUBMIT)
cleaned2017$DECISION_DATE=as.factor(cleaned2017$DECISION_DATE)
cleaned2017$LCA_CASE_EMPLOYMENT_START_DATE=as.factor(cleaned2017$LCA_CASE_EMPLOYMENT_START_DATE)
cleaned2017$LCA_CASE_EMPLOYMENT_END_DATE=as.factor(cleaned2017$LCA_CASE_EMPLOYMENT_END_DATE)
cleaned2017$LCA_CASE_EMPLOYER_STATE=as.factor(cleaned2017$LCA_CASE_EMPLOYER_STATE)
cleaned2017$LCA_CASE_WORKLOC1_STATE=as.factor(cleaned2017$LCA_CASE_WORKLOC1_STATE)
cleaned2017$FULL_TIME_POSITION=as.factor(cleaned2017$FULL_TIME_POSITION)
cleaned2017$PW_SOURCE_1=as.factor(cleaned2017$PW_SOURCE_1)
summary(cleaned2017)
names(cleaned2017)
#Removing decision related variables, state variables (because we already have regions), SOC_NAME (original)
data2017=cleaned2017[-c(3,6,8,13,15,17,18)]

#Scaling all numeric data
library(MASS)
index=sapply(data2017,is.numeric)
data2017[index]=lapply(data2017[index],scale)
View(data2017)

#Sampling data from original dataset to split as train and test, we did this initially as we were having memory problems
## set the seed to make your partition reproductible
set.seed(123)
smp_size <- floor(1 * nrow(data2017))
sample_ind <- sample(seq_len(nrow(data2017)), size = smp_size)
samples <- data2017[sample_ind, ]
train_size=floor(0.8*nrow(samples))
train_ind <- sample(seq_len(nrow(samples)), size = train_size)
train=samples[train_ind,]
test=samples[-train_ind,]

#install.packages("ROSE")
library(ROSE)
#Undersampling the majority class to overcome class imbalance
trainunder=ovun.sample(STATUS~.,data=train,method="under")
barplot(table(trainunder$data$STATUS),density=c(90,100),col=c('red','blue'),main="Status Class Distribution after Undersampling")
#Oversampling the minority class to overcome class imbalance
trainover=ovun.sample(STATUS~.,data=train,method="over")
barplot(table(trainover$data$STATUS),density=c(90,100),col=c('red','blue'),main="Status Class Distribution after Oversampling")

#Applying both
trainboth=ovun.sample(STATUS~.,data=train,method="both")
barplot(table(trainboth$data$STATUS),density=c(90,100),col=c('red','blue'),main="Status Class Distribution after using both Over and Undersampling")
#Performing logistic regression on each on of the samples
logregunder=glm(STATUS~.,data=trainunder$data,family=binomial)
logregover=glm(STATUS~.,data=trainover$data,family=binomial)
logregboth=glm(STATUS~.,data=trainboth$data,family=binomial)

predictionsover=predict(logregover,newdata=test,type='response')
predictionsunder=predict(logregunder,newdata=test,type='response')
predictionsboth=predict(logregboth,newdata=test,type='response')

predover=rep(1,length(predictionsover))
predover[predictionsover>mean(predictionsover)]=2
predover[predover==1]="CERTIFIED"
predover[predover==2]="NOT CERTIFIED"
accuracyover=mean(predover==test$STATUS)
fourfoldplot(table(predover,test$STATUS),main="Logistic Regression Oversampling Accuracy 0.6623835, AUC=0.654")

predunder=rep(1,length(predictionsunder))
predunder[predictionsunder>mean(predictionsunder)]=2
predunder[predunder==1]="CERTIFIED"
predunder[predunder==2]="NOT CERTIFIED"
accuracyunder=mean(predunder==test$STATUS)
fourfoldplot(table(predunder,test$STATUS),main="Logistic Regression Undersampling Accuracy 0.6588689,AUC=0.654")

predboth=rep(1,length(predictionsboth))
predboth[predictionsboth>mean(predictionsboth)]=2
predboth[predboth==1]="CERTIFIED"
predboth[predboth==2]="NOT CERTIFIED"
accuracyboth=mean(predboth==test$STATUS)
fourfoldplot(table(predboth,test$STATUS),main="Logistic Regression using both Accuracy 0.66141480,AUC=0.654")

#Area under the curve
#install.packages('ROSE')
#library(ROSE)
roc.curve(test$STATUS,predunder)
roc.curve(test$STATUS,predover)
roc.curve(test$STATUS,predboth)


employers=visa %>% group_by(EMPLOYER_NAME) %>% summarise(count=n(),percent=round(count*100/nrow(visa),5)) %>% arrange(desc(count))

#Performing Recursive Partitioning and Regression Trees
library(rpart)
tree.rose=rpart(STATUS~.,data=train)
#Cp is lowered to increase number of terminal nodes
tree.rose.under=rpart(STATUS~.,data=trainunder$data,cp=0.001,xval=10)
tree.rose.over=rpart(STATUS~.,data=trainover$data,cp=0.001,xval=10)
tree.rose.both=rpart(STATUS~.,data=trainboth$data,cp=0.001,xval=10)
tree.pred.under=predict(tree.rose.under,test,type="class")
tree.pred.over=predict(tree.rose.under,test,type="class")
tree.pred.both=predict(tree.rose.under,test,type="class")
tree.pred=predict(tree.rose,test,type="class")
mean(tree.pred.under==test$STATUS)
mean(tree.pred.over==test$STATUS)
fourfoldplot(table(tree.pred.both,test$STATUS),main="Recursive Partitioning and Regression Trees using both Accuracy 0.8324527,AUC=0.688")



mean(tree.pred.both==test$STATUS)
mean(tree.pred==test$STATUS)
roc.curve(test$STATUS,tree.pred.under)
roc.curve(test$STATUS,tree.pred.over)
roc.curve(test$STATUS,tree.pred.both)
roc.curve(test$STATUS,tree.pred)
#Plotting the decision tree
rpart.plot(tree.rose.both)

summary(logregover)
