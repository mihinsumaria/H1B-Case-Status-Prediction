library(readr)
H_1B_FY14_Q4 <- read_csv("D:/Dropbox/WPI/DS 502/Project/H-1B_FY14_Q4.csv")
h1b=H_1B_FY14_Q4
h1b=h1b[-c(1,5,9,12,13,17,20,26:35)]
h1b=h1b[complete.cases(h1b),]

h1b$LCA_CASE_SUBMIT=as.Date(h1b$LCA_CASE_SUBMIT,"%d-%m-%Y")
h1b$DECISION_DATE=as.Date(h1b$DECISION_DATE,"%d-%m-%Y")
h1b$LCA_CASE_EMPLOYMENT_START_DATE=as.Date(h1b$LCA_CASE_EMPLOYMENT_START_DATE,"%d-%m-%Y")
h1b$LCA_CASE_EMPLOYMENT_END_DATE=as.Date(h1b$LCA_CASE_EMPLOYMENT_END_DATE,"%d-%m-%Y")

h1b$SUBMIT_DECISION=h1b$DECISION_DATE-h1b$LCA_CASE_SUBMIT
h1b$START_END=h1b$LCA_CASE_EMPLOYMENT_START_DATE-h1b$LCA_CASE_EMPLOYMENT_END_DATE
h1b$START_DECISION=h1b$LCA_CASE_EMPLOYMENT_START_DATE-h1b$DECISION_DATE
h1b$END_DECISION=h1b$LCA_CASE_EMPLOYMENT_END_DATE-h1b$DECISION_DATE
h1b$SUBMIT_END=h1b$LCA_CASE_SUBMIT-h1b$LCA_CASE_EMPLOYMENT_END_DATE
h1b$SUBMIT_START=h1b$LCA_CASE_SUBMIT-h1b$LCA_CASE_EMPLOYMENT_END_DATE

h1b$LCA_CASE_SUBMIT=format(as.Date(h1b$LCA_CASE_SUBMIT,"%d-%m-%Y"),"%Y")
h1b$DECISION_DATE=format(as.Date(h1b$DECISION_DATE,"%d-%m-%Y"),"%Y")
h1b$LCA_CASE_EMPLOYMENT_START_DATE=format(as.Date(h1b$LCA_CASE_EMPLOYMENT_START_DATE,"%d-%m-%Y"),"%Y")
h1b$LCA_CASE_EMPLOYMENT_END_DATE=format(as.Date(h1b$LCA_CASE_EMPLOYMENT_END_DATE,"%d-%m-%Y"),"%Y")

h1b=h1b[which(h1b$STATUS=='CERTIFIED' | h1b$STATUS=='CERTIFIED-WITHDRAWN' | h1b$STATUS=='DENIED' | h1b$STATUS=='WITHDRAWN'),]

#Removing Employer Name
h1b=h1b[-c(6)]
h1b=h1b[which(h1b$LCA_CASE_WAGE_RATE_UNIT=='Year' & h1b$PW_UNIT_1=='Year'),]

#Removing Work Loc City and Employer City
h1b=h1b[-c(6,12)]

h1b$SUBMIT_DECISION=as.integer(h1b$SUBMIT_DECISION)
h1b$START_END=as.integer(h1b$START_END)
h1b$START_DECISION=as.integer(h1b$START_DECISION)
h1b$END_DECISION=as.integer(h1b$END_DECISION)
h1b$SUBMIT_END=as.integer(h1b$SUBMIT_END)
h1b$SUBMIT_START=as.integer(h1b$SUBMIT_START)

h1b=h1b[h1b$START_END<0,]

h1b$STATUS=as.factor(h1b$STATUS)
h1b$DECISION_DATE=as.factor(h1b$DECISION_DATE)
h1b$LCA_CASE_SUBMIT=as.factor(h1b$LCA_CASE_SUBMIT)
h1b$LCA_CASE_EMPLOYMENT_START_DATE=as.factor(h1b$LCA_CASE_EMPLOYMENT_START_DATE)
h1b$LCA_CASE_EMPLOYMENT_END_DATE=as.factor(h1b$LCA_CASE_EMPLOYMENT_END_DATE)
h1b$LCA_CASE_EMPLOYER_STATE=as.factor(h1b$LCA_CASE_EMPLOYER_STATE)
h1b$LCA_CASE_SOC_NAME=as.factor(h1b$LCA_CASE_SOC_NAME)
h1b$LCA_CASE_WORKLOC1_CITY=as.factor(h1b$LCA_CASE_WORKLOC1_CITY)
h1b$LCA_CASE_WORKLOC1_STATE=as.factor(h1b$LCA_CASE_WORKLOC1_STATE)
h1b$PW_SOURCE_1=as.factor(h1b$PW_SOURCE_1)
h1b=h1b[-c(14,10,8)]
h1bonehot=h1b[-c(1)]
#install.packages('mltools')
library(mltools)
one_hot(h1bonehot)
names(h1b)
#set.seed(1)
#spec=c(train=0.5,validation=0.5)
#g=sample(cut(seq(nrow(h1b)),nrow(h1b)*cumsum(c(0,spec)),labels=names(spec)))
#res=split(h1b,g)
#nrow(h1b)
#nrow(res$train)
#nrow(res$validation)
#library(MASS)
#qda.fit=qda(STATUS~.,data=res$train)
#qda.class=predict(qda.fit,res$validation)$class
#table(qda.class,res$validation$STATUS)