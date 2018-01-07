
h1b_17 = H_1B_Disclosure_Data_FY17

h1b_cleaning = h1b_17
names(h1b_cleaning)
h1b_cleaning=h1b_cleaning[-c(1,5,8,9,10,11,13,14,15,16,17,18,19,20,21,23,25,26:32,36,38,39,41,43:49,51,52)]

# h1b_cleaning = h1b_cleaning[-c(4)]


library(data.table)
renamed <-  h1b_cleaning
setnames(renamed,old = ('CASE_STATUS'),new= ('STATUS'))
setnames(renamed,old = ('CASE_SUBMITTED'),new= ('LCA_CASE_SUBMIT'))
setnames(renamed,old = ('EMPLOYMENT_START_DATE'),new= ('LCA_CASE_EMPLOYMENT_START_DATE'))
setnames(renamed,old = ('EMPLOYMENT_END_DATE'),new= ('LCA_CASE_EMPLOYMENT_END_DATE'))

setnames(renamed,old = ('EMPLOYER_STATE'),new= ('LCA_CASE_EMPLOYER_STATE'))
setnames(renamed,old = ('JOB_TITLE'),new= ('LCA_CASE_JOB_TITLE'))
setnames(renamed,old = ('SOC_NAME'),new= ('LCA_CASE_SOC_NAME'))
setnames(renamed,old = ('PREVAILING_WAGE'),new= ('PW_1'))

setnames(renamed,old = ('PW_UNIT_OF_PAY'),new= ('PW_UNIT_1'))
setnames(renamed,old = ('PW_SOURCE'),new= ('PW_SOURCE_1'))
setnames(renamed,old = ('WAGE_RATE_OF_PAY_FROM'),new= ('LCA_CASE_WAGE_RATE_FROM'))
setnames(renamed,old = ('WAGE_UNIT_OF_PAY'),new= ('LCA_CASE_WAGE_RATE_UNIT'))
setnames(renamed,old = ('WORKSITE_STATE'),new= ('LCA_CASE_WORKLOC1_STATE'))


h1b=renamed
h1b=h1b[complete.cases(h1b),]

h1b$LCA_CASE_SUBMIT=as.Date(h1b$LCA_CASE_SUBMIT,"%m/%d/%Y")
h1b$DECISION_DATE=as.Date(h1b$DECISION_DATE,"%m/%d/%Y")
h1b$LCA_CASE_EMPLOYMENT_START_DATE=as.Date(h1b$LCA_CASE_EMPLOYMENT_START_DATE,"%m/%d/%Y")
h1b$LCA_CASE_EMPLOYMENT_END_DATE=as.Date(h1b$LCA_CASE_EMPLOYMENT_END_DATE,"%m/%d/%Y")

h1b$SUBMIT_DECISION=abs(h1b$DECISION_DATE-h1b$LCA_CASE_SUBMIT)
h1b$START_END=abs(h1b$LCA_CASE_EMPLOYMENT_START_DATE-h1b$LCA_CASE_EMPLOYMENT_END_DATE)
h1b$START_DECISION=abs(h1b$LCA_CASE_EMPLOYMENT_START_DATE-h1b$DECISION_DATE)
h1b$END_DECISION=abs(h1b$LCA_CASE_EMPLOYMENT_END_DATE-h1b$DECISION_DATE)
h1b$SUBMIT_END=abs(h1b$LCA_CASE_SUBMIT-h1b$LCA_CASE_EMPLOYMENT_END_DATE)
h1b$SUBMIT_START=abs(h1b$LCA_CASE_SUBMIT-h1b$LCA_CASE_EMPLOYMENT_START_DATE)

h1b$LCA_CASE_SUBMIT=format(as.Date(h1b$LCA_CASE_SUBMIT,"%m/%d/%Y"),"%Y")
h1b$DECISION_DATE=format(as.Date(h1b$DECISION_DATE,"%m/%d/%Y"),"%Y")
h1b$LCA_CASE_EMPLOYMENT_START_DATE=format(as.Date(h1b$LCA_CASE_EMPLOYMENT_START_DATE,"%m/%d/%Y"),"%Y")
h1b$LCA_CASE_EMPLOYMENT_END_DATE=format(as.Date(h1b$LCA_CASE_EMPLOYMENT_END_DATE,"%m/%d/%Y"),"%Y")

summary(h1b)
unique(h1b$STATUS)

# h1b=h1b[which(h1b$STATUS=='CERTIFIED' | h1b$STATUS=='CERTIFIED-WITHDRAWN' | h1b$STATUS=='DENIED' | h1b$STATUS=='WITHDRAWN'),]
#h1b=h1b[which(h1b$LCA_CASE_WAGE_RATE_UNIT=='Year' & h1b$PW_UNIT_1=='Year'),]


h1b$SUBMIT_DECISION=as.integer(h1b$SUBMIT_DECISION)
h1b$START_END=as.integer(h1b$START_END)
h1b$START_DECISION=as.integer(h1b$START_DECISION)
h1b$END_DECISION=as.integer(h1b$END_DECISION)
h1b$SUBMIT_END=as.integer(h1b$SUBMIT_END)
h1b$SUBMIT_START=as.integer(h1b$SUBMIT_START)

names(h1b)

# Remove Jobtitle (7), PW_UNIT_1 (11), LCA_CASE_WAGE_RATE_UNIT(14)
h1b=h1b[-c(7,11,14)]

h1b$LCA_CASE_SOC_NAME_1 = NA  
h1b <- within(h1b, LCA_CASE_SOC_NAME_1[LCA_CASE_SOC_NAME=="Computer Systems Analysts" | LCA_CASE_SOC_NAME=="Computer Programmers" | grepl("Engineers", LCA_CASE_SOC_NAME) | LCA_CASE_SOC_NAME=="Software Developers, Applications"] <- 1)
h1b$LCA_CASE_SOC_NAME_1[is.na(h1b$LCA_CASE_SOC_NAME_1)] <- 0


h1b_trial = h1b


smp_size <- floor(0.6 * nrow(h1b_trial))

## set the seed to make your partition reproductible
set.seed(123)
h1b_trial_ind1 <- sample(seq_len(nrow(h1b_trial)), size = smp_size)

trian.1 <- h1b_trial[h1b_trial_ind1, ]
test.1 <- h1b_trial[-h1b_trial_ind1, ]


trian.1$LCA_CASE_SUBMIT<-as.factor(trian.1$LCA_CASE_SUBMIT)
trian.1$DECISION_DATE<-as.factor(trian.1$DECISION_DATE)
trian.1$LCA_CASE_EMPLOYMENT_START_DATE<-as.factor(trian.1$LCA_CASE_EMPLOYMENT_START_DATE)
trian.1$LCA_CASE_EMPLOYMENT_END_DATE<-as.factor(trian.1$LCA_CASE_EMPLOYMENT_END_DATE)

trian.1<-trian.1[!(trian.1$LCA_CASE_EMPLOYER_STATE == "" | trian.1$LCA_CASE_EMPLOYER_STATE == "FM" | trian.1$LCA_CASE_EMPLOYER_STATE == "MP" | trian.1$LCA_CASE_EMPLOYER_STATE == "FM" | trian.1$LCA_CASE_EMPLOYER_STATE == "VI" | trian.1$LCA_CASE_EMPLOYER_STATE == "AS" | trian.1$LCA_CASE_EMPLOYER_STATE == "PW"), ]
trian.1<-trian.1[!(trian.1$LCA_CASE_WORKLOC1_STATE == "" | trian.1$LCA_CASE_WORKLOC1_STATE == "FM" | trian.1$LCA_CASE_WORKLOC1_STATE == "MP" | trian.1$LCA_CASE_WORKLOC1_STATE == "FM" | trian.1$LCA_CASE_WORKLOC1_STATE == "VI" | trian.1$LCA_CASE_WORKLOC1_STATE == "AS" | trian.1$LCA_CASE_WORKLOC1_STATE == "PW"), ]
test.1<-test.1[!(test.1$LCA_CASE_WORKLOC1_STATE == "" | test.1$LCA_CASE_WORKLOC1_STATE == "FM" | test.1$LCA_CASE_WORKLOC1_STATE == "MP" | test.1$LCA_CASE_WORKLOC1_STATE == "FM" | test.1$LCA_CASE_WORKLOC1_STATE == "VI" | test.1$LCA_CASE_WORKLOC1_STATE == "AS" | test.1$LCA_CASE_WORKLOC1_STATE == "PW"), ]
test.1<-test.1[!(test.1$LCA_CASE_EMPLOYER_STATE == "" | test.1$LCA_CASE_EMPLOYER_STATE == "FM" | test.1$LCA_CASE_EMPLOYER_STATE == "MP" | test.1$LCA_CASE_EMPLOYER_STATE == "FM" | test.1$LCA_CASE_EMPLOYER_STATE == "VI" | test.1$LCA_CASE_EMPLOYER_STATE == "AS" | test.1$LCA_CASE_EMPLOYER_STATE == "PW"), ]


trian.1<-trian.1[-c(7)]
test.1<-test.1[-c(7)]
trian.1$PW_1<-as.numeric(trian.1$PW_1)
test.1$PW_1<-as.numeric(test.1$PW_1)
levels(test.1$LCA_CASE_EMPLOYMENT_END_DATE)<- levels(trian.1$LCA_CASE_EMPLOYMENT_END_DATE)
levels(test.1$LCA_CASE_EMPLOYER_STATE)<- levels(trian.1$LCA_CASE_EMPLOYER_STATE)
levels(test.1$LCA_CASE_WORKLOC1_STATE)<- levels(trian.1$LCA_CASE_WORKLOC1_STATE)

trian.1$LCA_CASE_EMPLOYER_STATE <- as.factor(trian.1$LCA_CASE_EMPLOYER_STATE)
trian.1$LCA_CASE_EMPLOYER_STATE<-droplevels(trian.1$LCA_CASE_EMPLOYER_STATE)
trian.1$LCA_CASE_WORKLOC1_STATE <- as.factor(trian.1$LCA_CASE_WORKLOC1_STATE)
trian.1$LCA_CASE_WORKLOC1_STATE<-droplevels(trian.1$LCA_CASE_WORKLOC1_STATE)
trian.1$STATUS <- as.factor(trian.1$STATUS)
trian.1$STATUS<-droplevels(trian.1$STATUS)

trian.1=trian.1[complete.cases(trian.1),]

table(trian.1$LCA_CASE_EMPLOYER_STATE)
table(trian.1$LCA_CASE_WORKLOC1_STATE)
length(unique(trian.1$LCA_CASE_EMPLOYER_STATE))
length(unique(trian.1$LCA_CASE_WORKLOC1_STATE))

library(randomForest)
trian.rf<-randomForest(STATUS~., data = trian.1)
preds= predict(train.rf,test)
confusionMatrix(preds,test$STATUS)