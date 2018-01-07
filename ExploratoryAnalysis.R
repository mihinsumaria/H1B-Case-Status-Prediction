library(dplyr)
library(ggplot2)
visa=H_1B_FY14_Q4
case.status=visa %>% filter(!is.na(STATUS)) %>% group_by(STATUS) %>% summarise(PROPORTION=round(n()*100/nrow(visa),1))


print(ggplot(data = case.status, aes(x = reorder(STATUS, PROPORTION), y = PROPORTION, fill = STATUS)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(PROPORTION,"%")), vjust=0,hjust = 1) + labs(x = "Case Status", y = "Percent", title = "Status of petitioned applications") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip())

employers=visa %>% group_by(LCA_CASE_EMPLOYER_NAME) %>% summarise(count=n(),percent=round(count*100/nrow(visa),5)) %>% arrange(desc(count))
           
p=ggplot(data = employers[1:10,], aes(x = reorder(LCA_CASE_EMPLOYER_NAME, percent), y = percent, fill = LCA_CASE_EMPLOYER_NAME)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Company", y = "Percent", title = "Applications per company") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()

p=p+theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=5,byrow=TRUE))
print(p)

jobs=visa %>% group_by(LCA_CASE_JOB_TITLE) %>% summarise(count=n(),percent=round(count*100/nrow(visa),5)) %>% arrange(desc(count))

p=ggplot(data = jobs[1:10,], aes(x = reorder(LCA_CASE_JOB_TITLE, percent), y = percent, fill = LCA_CASE_JOB_TITLE)) +geom_bar(stat = "identity") + geom_text(aes(label = paste(percent,"%")), vjust=0,hjust = 1) + labs(x = "Job-title", y = "Percent", title = "Jobs") + scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()

p=p+theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=5,byrow=TRUE))
print(p)

fulltimeprop=prop.table(table(visa$FULL_TIME_POS))

nrow(visa[which(visa$STATUS=="DENIED" | visa$STATUS=="CERTIFIED-WITHDRAWN" | visa$STATUS=="DENIED" | visa$STATUS=="WITHDRAWN" | visa$STATUS=="REJECTED" | visa$STATUS=="INVALIDATED" ),])
