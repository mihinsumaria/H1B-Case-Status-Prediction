library(readr)
h1b=read_csv("C://JK//Masters Studies//Fall 2017//DS502//cleaned2017_REGIONS.csv")
h1b=h1b[-c(1)]

#Start: Data Cleaning
h1b$LCA_CASE_SOC_NAME_1 = 0
h1b <- within(h1b, LCA_CASE_SOC_NAME_1[LCA_CASE_SOC_NAME=="COMPUTER SYSTEMS ANALYSTS" | LCA_CASE_SOC_NAME=="COMPUTER PROGRAMMERS" | grepl("ENGINEERS", LCA_CASE_SOC_NAME) | LCA_CASE_SOC_NAME=="SOFTWARE DEVELOPERS, APPLICATIONs"] <- 1)

unique(h1b$LCA_CASE_SOC_NAME_1)

cleaned2017 = h1b
# write.csv(h1b,file="cleaned2017.csv")


names(cleaned2017)
summary(cleaned2017)
cleaned2017[-which(cleaned2017$STATUS=="CERTIFIED"),]$STATUS="NOT CERTIFIED"
cleaned2017$STATUS=as.factor(cleaned2017$STATUS)

cleaned2017$LCA_CASE_SUBMIT=as.factor(cleaned2017$LCA_CASE_SUBMIT)
cleaned2017$DECISION_DATE=as.factor(cleaned2017$DECISION_DATE)
cleaned2017$LCA_CASE_EMPLOYMENT_START_DATE=as.factor(cleaned2017$LCA_CASE_EMPLOYMENT_START_DATE)
cleaned2017$LCA_CASE_EMPLOYMENT_END_DATE=as.factor(cleaned2017$LCA_CASE_EMPLOYMENT_END_DATE)
cleaned2017$LCA_CASE_EMPLOYER_STATE=as.factor(cleaned2017$LCA_CASE_EMPLOYER_STATE)
cleaned2017$LCA_CASE_WORKLOC1_STATE=as.factor(cleaned2017$LCA_CASE_WORKLOC1_STATE)
cleaned2017$FULL_TIME_POSITION=as.factor(cleaned2017$FULL_TIME_POSITION)
cleaned2017$PW_SOURCE_1=as.factor(cleaned2017$PW_SOURCE_1)

# Dropping Employer State & Workloc State
data2017=cleaned2017[-c(6,13)]

# Dropping SOC Name
data2017=data2017[-c(7)]

#End: Data Cleaning

#Start: Exploratory Analysis
library(ggplot2)

x=cleaned2017_REGIONS[cleaned2017_REGIONS$STATUS=="CERTIFIED",]
k=cleaned2017_REGIONS[cleaned2017_REGIONS$STATUS=="NOT CERTIFIED",]

m=(cleaned2017_REGIONS[cleaned2017_REGIONS$FULL_TIME_POSITION=="Y",])
n=(cleaned2017_REGIONS[cleaned2017_REGIONS$FULL_TIME_POSITION=="N",])
t=cleaned2017_REGIONS
mean(t$PW_1)
#73215.87
median(t$PW_1)
#70221
t$Wage200 = 0 
t <- within(t, Wage200[PW_1>70221] <- 1)
k=t[t$STATUS=="NOT CERTIFIED",]
x=t[t$STATUS=="CERTIFIED",]

ggplot(data = subset(x, !is.na(x$FULL_TIME_POSITION)),
        aes(x = FULL_TIME_POSITION, y = (..count..)*100/545661, fill = FULL_TIME_POSITION)) + 
     geom_bar() +
     labs(y = "Petitions Made(in percentage)",title="Full Time Position") +
     theme(legend.position = "none") 
     

ggplot(data = subset(m, !is.na(m$FULL_TIME_POSITION)),
       aes(x = STATUS, y = (..count..)*100/610666, fill = STATUS)) + 
  geom_bar() +
  labs(y = "Petitions Made(in percentage)", title="Full POsition = Y") +
  theme(legend.position = "none")


ggplot(data = subset(n, !is.na(n$FULL_TIME_POSITION)),
       aes(x = STATUS, y = (..count..)*100/13872, fill = STATUS)) + 
  geom_bar() +
  labs(y = "Petitions Made(in percentage)", title="Full POsition = N") +
  theme(legend.position = "none")

ggplot(data = subset(x, !is.na(x$LCA_CASE_EMPLOYMENT_END_DATE)),
       aes(x = LCA_CASE_EMPLOYMENT_END_DATE, y = (..count..)*100/545661, fill = LCA_CASE_EMPLOYMENT_END_DATE)) + 
     geom_bar() +
       labs(y = "Petitions Made(in percentage)", title="Employment End Date for Certified") +
       theme(legend.position = "none")

k=cleaned2017[cleaned2017$STATUS=="NOT CERTIFIED",]

ggplot(data = subset(k, !is.na(k$LCA_CASE_EMPLOYMENT_END_DATE)),
       aes(x = LCA_CASE_EMPLOYMENT_END_DATE, y = (..count..)*100/78877, fill = LCA_CASE_EMPLOYMENT_END_DATE)) + 
       geom_bar() +
       labs(y = "Petitions Made(in percentage)", title="Employment End Date for NOt Certified") +
       theme(legend.position = "none")
#Bringing a new variable with wage greater then median wage as 1 and rest as 0




ggplot(data = subset(x, !is.na(x$Wage200)),
       aes(x = Wage200, y = (..count..)*100/545661, fill = Wage200)) + 
       geom_bar() +
       labs(y = "Petitions Made(in percentage)",title="Wages above and below 70221 Certified ") +
       theme(legend.position = "none") 
prop.table(table(x$Wage200))

#0         1 
#0.4936142 0.5063858 

ggplot(data = subset(k, !is.na(k$Wage200)),
        aes(x = Wage200, y = (..count..)*100/78877, fill = Wage200)) + 
       geom_bar() +
       labs(y = "Petitions Made(in percentage)",title="Wages above and below 70221 Not Certified ") +
       theme(legend.position = "none") 

prop.table(table(k$Wage200))

#0         1 
#0.5614057 0.4385943 

ggplot(data = subset(t, !is.na(t$LCA_CASE_EMPLOYER_STATE)),
       aes(x = LCA_CASE_EMPLOYER_STATE, y = (..count..)*100/624538, fill = LCA_CASE_EMPLOYER_STATE)) 
       geom_bar() 
       labs(y = "Petitions Made(in percentage)",title="Number of application as a factor of Employer State ") 
       theme(legend.position = "none") + theme(axis.ticks.length=unit(0.5,"cm"))

ggplot(data = subset(x, !is.na(x$LCA_CASE_EMPLOYER_STATE)),
       aes(x = LCA_CASE_EMPLOYER_STATE, y = (..count..)*100/624538, fill = LCA_CASE_EMPLOYER_STATE))
       geom_bar() 
       labs(y = "Petitions Made(in percentage)",title="Number of application as a factor of Employer State Certified ") 
       theme(legend.position = "none") + theme(axis.ticks.length=unit(0.5,"cm"))
       
       
qplot(x=SUBMIT_DECISION,y=START_END, data=cleaned2017_REGIONS,color=STATUS)      
       
qplot(x=SUBMIT_START,y=SUBMIT_END, data=cleaned2017_REGIONS,color=STATUS) 

qplot(x=END_DECISION,y=START_DECISION, data = t,color=STATUS)
       
qplot(x=END_DECISION,y=START_DECISION, data = t,color=STATUS)     
theme(plot.title = element_text(hjust = 0.5))
library(dplyr)
regionwise=t %>% group_by(WORKLOC_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(WORKLOC_REGION, percent), y = percent, fill = WORKLOC_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(p)

regionwise=x %>% group_by(WORKLOC_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(x),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(WORKLOC_REGION, percent), y = percent, fill = WORKLOC_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region for Certified") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()+theme(plot.title = element_text(hjust = 0.5))

print(p)

regionwise=k %>% group_by(WORKLOC_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(k),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(WORKLOC_REGION, percent), y = percent, fill = WORKLOC_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region for Certified") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()+theme(plot.title = element_text(hjust = 0.5))

print(p)

regionwise=x %>% group_by(WORKLOC_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(k),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(EMPLOYER_REGION, percent), y = percent, fill = EMPLOYER_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region Based on Employer Region") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(p)

regionwise=t %>% group_by(EMPLOYER_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(EMPLOYER_REGION, percent), y = percent, fill = EMPLOYER_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region Based on Employer Region") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(p)

regionwise=x %>% group_by(EMPLOYER_REGION) %>% summarise(count=n(),percent=round(count*100/nrow(x),5)) %>% arrange(desc(count))
p=ggplot(data = regionwise, aes(x = reorder(EMPLOYER_REGION, percent), y = percent, fill = EMPLOYER_REGION)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Region", y = "Percent", title = "Applications per region Based on Employer Region For Certified") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(p)

subplot(2,2,1)
b=t[t$EMPLOYER_REGION=="South",]

regionwise=b %>% group_by(STATUS) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
library(ggplot2)
l=ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for South") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(l)

s=t[t$EMPLOYER_REGION=="Northeast",]

regionwise=s %>% group_by(STATUS) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
library(ggplot2)
u=ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for Noertheast") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(u)

d=t[t$EMPLOYER_REGION=="West",]

regionwise=d %>% group_by(STATUS) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
library(ggplot2)
i=ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for West") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(i)

f=t[t$EMPLOYER_REGION=="Midwest",]

regionwise=f %>% group_by(STATUS) %>% summarise(count=n(),percent=round(count*100/nrow(t),5)) %>% arrange(desc(count))
o=ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for Midwest") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
print(o)
par(mfrow=c(2,2))
ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for South") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for Noertheast") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for West") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
ggplot(data = regionwise, aes(x = reorder(STATUS, percent), y = percent, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "STATUS", y = "Percent", title = "STATUS for Midwest") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()
#End: Exploratory Analysis 





#Start: KNN Code
# library(dummies)
# onehot_data2017= dummy.data.frame(data2017,names= c('LCA_CASE_SUBMIT'))

dummy_data2017 = as.data.frame(model.matrix(~data2017$LCA_CASE_SUBMIT,data2017$LCA_CASE_EMPLOYMENT_START_DATE)[,-1])
onehot_data2017 = cbind(data2017, dummy_data2017)
dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$LCA_CASE_EMPLOYMENT_START_DATE)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)
dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$LCA_CASE_EMPLOYMENT_END_DATE)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)
dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$DECISION_DATE)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)
dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$FULL_TIME_POSITION)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)

dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$PW_SOURCE_1)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)

onehot_data2017$PW_1=as.integer(onehot_data2017$PW_1)
onehot_data2017$LCA_CASE_WAGE_RATE_FROM=as.integer(onehot_data2017$LCA_CASE_WAGE_RATE_FROM)

dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$WORKLOC_REGION)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)

dummy_data2017 = as.data.frame(model.matrix(~onehot_data2017$EMPLOYER_REGION)[,-1])
onehot_data2017 = cbind(onehot_data2017, dummy_data2017)

# Dropping columns which are one hot encoded 
onehot_data2017=onehot_data2017[-c(2,3,4,5,6,7,9,11)]

write.csv(onehot_data2017,file='one_hot.csv')

# Dropping Employer state and workloc state columns since one hot encoding will cause lots of new dimensions
# onehot_data2017=onehot_data2017[-c(2,5)]

data_norm <- function(x) {((x-min(x))/(max(x)-min(x)))}
onehot_data <- as.data.frame(lapply(onehot_data2017[,-1],data_norm))

# Removing decision date related columns
onehot_data = onehot_data[-c(3,5,6,31)]
onehot_data$STATUS = onehot_data2017$STATUS

smp_size <- floor(1 * nrow(onehot_data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(onehot_data)), size = smp_size)

train_temp <- onehot_data[train_ind, ]
test_temp <- onehot_data[-train_ind, ]

smp_size1 <- floor(0.7 * nrow(train_temp))

## set the seed to make your partition reproductible
set.seed(123)
train_ind1 <- sample(seq_len(nrow(train_temp)), size = smp_size1)

train <- train_temp[train_ind1, ]
test <- train_temp[-train_ind1, ]

status_train <- train$STATUS

library(class)
pred_knn <-knn(train[,-39],test[,-39],status_train,k=40)
pred_knn
table(pred_knn,test$STATUS)
mean(pred_knn==test$STATUS)

write.csv(pred_knn,'KNN_predictions_nodecision.csv')
#End: KNN Code

#Start: Random Forests Code
library(randomForest)
library(caret)
library(e1071)
train<-read.csv("train.csv")
test<-read.csv("test.csv")
train_rf<-randomForest(STATUS~., data = train)
preds<-predict(train_rf, test)
confusionMatrix(preds, test$STATUS)
#End: Random Forests Code

#Start: XGBoost Code
library(Matrix)
require(xgboost)

one_hot<-read.csv("502 proj/one_hot.csv")
one_hot<-one_hot[-c(1,5,7,8)]
one_hot<-one_hot[-c(29)]
one_hot<-one_hot[-c(2)]

#random sampling and splitting into train and test 70:30
smp_size <- floor(0.7 * nrow(one_hot))

set.seed(123)
train_ind <- sample(seq_len(nrow(one_hot)), size = smp_size)

train <- one_hot[train_ind, ]
test <- one_hot[-train_ind, ]

#removing rows with null values
train=train[complete.cases(train),]
test=test[complete.cases(test),]

#converting the whole dataset to matrix
M1<-matrix(as.numeric(unlist(one_hot)),nrow=nrow(one_hot))

#removing STATUS and storing it in another variable
train_Y<-train$STATUS
test_Y<-test$STATUS

train<-train[-c(1)]
test<-test[-c(1)]

#converting the training and testing dataset to matrix
M1_train<-matrix(as.numeric(unlist(train)),nrow=nrow(train))
M1_train <- as(M1_train, "dgCMatrix")

M1_test<-matrix(as.numeric(unlist(test)),nrow=nrow(test))
M1_test <- as(M1_test, "dgCMatrix")

#converting target variable to 0 and 1
train_Y<-as.numeric(train_Y)
test_Y<-as.numeric(test_Y)

train_Y<-replace(train_Y, train_Y==1, 0)
train_Y<-replace(train_Y, train_Y==2, 1)

test_Y<-replace(test_Y, test_Y==1, 0)
test_Y<-replace(test_Y, test_Y==2, 1)

#creating model
bstSparse <- xgboost(data = M1_train, label = train_Y, max.depth = 15, eta = 0.5, nthread = 5, nrounds = 19, objective = "binary:logistic")

#making predictions
pred <- predict(bstSparse, M1_test)

#finding the test error
err <- mean(as.numeric(pred > 0.5) != test_Y)
print(paste("test-error=", err))

#creating the importance plot
importance<-xgb.importance(feature_names = colnames(train), model = bstSparse)
xgb.ggplot.importance(importance_matrix = importance)
#End: XGBoost Code

#Start: Logistic Regression Code
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

#End: Logistic Regression Code