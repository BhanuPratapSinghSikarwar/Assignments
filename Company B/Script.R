setwd("D://R//job Assign//ilink")
getwd()

## Libraries
library(readr)
library(plyr)
library(dplyr)

# Read a file
coltypes <-  list(trade_date = col_datetime("%m-%d-%Y"))
file<-read_csv(file="sample_for_R_code.csv") 

# 1.	How many distinct stock id present over their?
# Ans. 7 [3IINFOTECH.NS, ORIENTCEM.NS, MRF.NS, MRPL.NS,MCLEODRUSS.NS,KIRIINDUS.NS, KILITCH.NS
result<-as.data.frame(unique(file[,8]))
result<-result[order(result$stock_id),]
result<-as.data.frame(result)
###########################################################

# 2.	What's the count of each distinct ID's?
# Ans.
# Id   stock_id   Freq
# 1 3IINFOTECH.NS  200
# 2    KILITCH.NS  145
# 3  KIRIINDUS.NS  123
# 4 MCLEODRUSS.NS  185
# 5        MRF.NS  330
# 6       MRPL.NS   50
# 7  ORIENTCEM.NS  110

# Create frequency table of each distinct ID's 

StockTable <- table(file$stock_id)
StockTable <- as.data.frame(StockTable)
names(StockTable)<-c("stock_id","count")
StockTable<-StockTable[order(StockTable$stock_id),]
result<-cbind(result,StockTable)

#######################################################

# 3.	What is the average adj_close for each ID?
# Ans.
#     stock_id         Avg
# 1 3IINFOTECH.NS     7.26100
# 2    KILITCH.NS    35.31966
# 3  KIRIINDUS.NS   120.61585
# 4 MCLEODRUSS.NS   233.58492
# 5        MRF.NS 28006.78903
# 6       MRPL.NS    62.72800
# 7  ORIENTCEM.NS   131.01509

## customer average time spend on call
adj_closeAvg<-ddply(file, .(file$stock_id), function(x) mean(x$adj_close))
names(adj_closeAvg) <- c("stock_id","Avg")

adj_closeAvg<-adj_closeAvg[order(adj_closeAvg$stock_id),]
result<-cbind(result,adj_closeAvg)

########################################################

# 4.	What is the mean and SD of Volume for each ID?
# Ans.
#     stock_id      Volume    VSD[, 2]
# 1 3IINFOTECH.NS 2135853.000 3262643.770
# 2    KILITCH.NS    9482.069   16292.835
# 3  KIRIINDUS.NS  162656.911  372165.294
# 4 MCLEODRUSS.NS  427333.514  386681.297
# 5        MRF.NS   11740.303    9960.386
# 6       MRPL.NS  975364.000 1831869.157
# 7  ORIENTCEM.NS  177889.091  234161.127

VAvg<-ddply(file, .(file$stock_id), function(x) mean(x$volume))
names(VAvg) <- c("stock_id","Volume")

VAvg<-VAvg[order(VAvg$stock_id),]

VSD<-ddply(file, .(file$stock_id), function(x) sd(x$volume))
names(VSD) <- c("stock_id","SD")
VSD<-VSD[order(VSD$stock_id),]
result<-cbind(result,VAvg,VSD[,2])

# 5.	In which date the adj_close was maximum for each id and what's the value?
# Ans.
# file$stock_id    adj_close trade_date
# 1 3IINFOTECH.NS     20.30 2012-02-17
# 2    KILITCH.NS     69.70 2012-08-30
# 3  KIRIINDUS.NS    381.80 2016-06-28
# 4 MCLEODRUSS.NS    349.19 2013-02-19
# 5        MRF.NS  68794.22 2017-04-26
# 6       MRPL.NS     90.30 2016-10-28
# 7  ORIENTCEM.NS    222.05 2016-10-06

MaxClose<-ddply(file, .(file$stock_id), function(x) x[which.max(x$adj_close),])
MaxClose<- MaxClose[,c(1,7,8)]
names(MaxClose)<-c("stock_id","adj_close","trade_date")
MaxClose<-MaxClose[order(MaxClose$stock_id),]
result<-cbind(result,MaxClose)

# 6.	For how many days day_open < day_close for each stock id?
# Ans.
# Var1            Freq
# 1 3IINFOTECH.NS   64
# 2    KILITCH.NS   52
# 3  KIRIINDUS.NS   51
# 4 MCLEODRUSS.NS   76
# 5        MRF.NS  163
# 6       MRPL.NS   21
# 7  ORIENTCEM.NS   42

days<-subset(file,day_open < day_close)
dayTable <- table(days$stock_id)
dayTable <- as.data.frame(dayTable)
dayTable
names(dayTable)<-c("stock_id","freq")
dayTable<-dayTable[order(dayTable$stock_id),]
result<-cbind(result,dayTable)

#output
result<-result[,-c(1,4,6,9,12)]
names(result)<-c("stock_id",	"1.count",	"2.Avg_adj_close",	"3.Mean_volume",	"4.SD_volume",	"5a.Max_date_for_max_adj_close",	"5b.Max_adj_close",	"6.Dayopen_lt_dayclose")
result
write.table(result, "filename1.csv", col.names=TRUE, sep=",")

