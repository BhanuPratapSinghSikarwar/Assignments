setwd("D://R//job Assign")
getwd()

# Read the source file
file<-read.csv("Dataset_1.csv")


#file[] <- lapply(file, as.character)

library(dplyr)
library(plyr)
library(reshape2)

# ordering by date
file<-ddply(file, .(merch_id), mutate, id = order(activity_date))

# choosing only three date
file<-ddply(file,.(merch_id),function(x)head(x,3))

# writing output
out<-dcast(file,merch_id~id,value.var=c("activity_date"),fill=0)
colnames(out)<-(c("merch_id","First.Date","Second.Date","Third.Date"))
head(out)
