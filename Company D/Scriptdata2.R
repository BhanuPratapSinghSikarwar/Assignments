#setwd("D://R//job Assign")
#getwd()

# Library

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(data.table) 
library(scales)
library(tseries)



# Read a data from file
# change date to date format
coltypes <-  list(called_on = col_datetime("%Y-%m-%d %H:%M:%S"))

# Read the source file

file<-read_csv(file="Dataset_2.csv", col_types=coltypes) 

# column description
# merch_id - Merchant's Unique ID
# outlet_id - Merchant may have multiple outlets.
# customer_id - Merchants(Restaurant) Customer Calling on their Unique Number
# call_duration - in Seconds
# call_status
#     0 - Call Disconnected By User
#     1 - Call Not picked by outlet
#     2 - Connected Calls
#     3 - Voicemails
# Year- 2017  Month - 1-12  Day- 0-31 Hour- 0-23  Minute- 0-59 Second- 0-59 dayDate - DATE
# DayParting Day time divided into interval of three hours.
#   Late Night      - 0:3
#   Early Morning   - 3:6
#   Morning         - 6:12
#   AfterNoon       - 12:15
#   Evening         - 15:18
#   Night           - 18:21
#   Mid Night       - 21:24
# DayOfWeek: week days

file <-
  file %>%
    mutate(merch_id =factor(merch_id),
              outlet_id =factor(outlet_id),
              customer_id =factor(customer_id),
              call_statusold =call_status, #, levels=c("CallDisconnected","CallNotpicked","ConnectedCalls","Voicemails")),
              call_status =factor(ifelse(call_status < 1, "CallDisconnected", ifelse(call_status < 2, "CallNotpicked", ifelse(call_status < 3, "ConnectedCalls",  "Voicemails")))),
              Year  = factor(year(called_on), levels=2017),
              Month = factor(month(called_on), levels=1:12),
              Day   = day(called_on),
              Hour  = factor(hour(called_on), levels=0:23),
              Minute  = factor(minute(called_on), levels=0:59),
              Second  = factor(second(called_on), levels=0:59),
              dayDate = as.POSIXct(round(called_on, units = "days")),
              DayOfWeek = weekdays(as.Date(dayDate,'%Y-%m-%d')),
              DayParting = cut(as.numeric(as.character(factor(hour(called_on), levels=0:23))), 
                          breaks = c(0 , 3, 6, 12, 15, 18, 21, 24),
                          labels = c("Late Night", "Early Morning","Morning", "AfterNoon", "Evening", "Night","Mid Night"),
                          right = FALSE)
           )


####################################################################

  # Create frequency table of call Status #### 
  
  CallStatusTable <- table(file$call_status)
  CallStatusTable <- as.data.frame(CallStatusTable)
  names(CallStatusTable)[1] <- "CallStatus"
  names(CallStatusTable)[2] <- "Count"
  print(CallStatusTable)
  
  #     CallStatus  Count
  # 1          0      4570
  # 2          1    109316
  # 3          2    215018  most of the call recieved :) 
  # 4          3    7795
  
  # call Status Bar graph
  ggplot(
    data = file, 
    aes(x = call_status)) + 
    geom_bar(fill="light blue") + #geomatric object
    #coord_flip() +
    xlab("Call Status")
    ggtitle("Count of Call Status")
  
  
  # Create a pie chart of call Status
  ggplot(
    data = file, 
    aes(x = "", fill = call_status)) +# no variable to x axis and fill call Status
    geom_bar() +
    coord_polar(theta = "y") +  # maps the data in circle
    ggtitle("Proportion of calls by call Status")
  
  
  
  # Peak hours : time of day when the calls are high
  ggplot( data = file, 
          aes(x = DayParting)) + 
    geom_bar(fill="light blue") + 
    #coord_flip() +
    ggtitle("Count of calls by DayParting")
  
  # During Night - 6:9pm call are high folowed by AfterNoon 12:3pm, Mid Night - 9:12am
  
  # Peak hours : Hours when the calls are high
  ggplot(# + is to adding layer
    data = file, 
    aes(x = Hour)) + 
    geom_bar(fill="light blue") + 
    coord_flip() +
    ggtitle("Count of calls by Hour")
  
  # Busy Day : Day when the calls are high
  ggplot(# + is to adding layer
    data = file, 
    aes(x = DayOfWeek)) + 
    geom_bar(fill="light blue") + 
    #coord_flip() +
    xlab("Day of week")+
    ggtitle("Call count by days")
  
  # unique merch_id, customer_id, and outlet_id
  str(unique(file$merch_id))  #593
  str(unique(file$customer_id)) # 165450
  MerchTable<-ddply(file, .(file$merch_id, file$outlet_id), nrow) #1155 combine merch id and outlet id
  names(MerchTable) <- c("merch_id", "outlet_id", "Freq")
  
  # combination of all three merch_id, customer_id, and outlet_id
  merchOutletCustFreq <- ddply(file, .(file$merch_id, file$outlet_id,file$customer_id), nrow)
  names(merchOutletCustFreq) <- c("merch_id", "outlet_id", "customer_id","Freq")
  merchOutletCustFreq
  
  # Dialy call rate
  dailycall<-ddply(file, .(file$dayDate), nrow)
  names(dailycall) <- c("DayDate", "cnt")
  dailycall$DayDate = as.Date(dailycall$DayDate)
  
  ## clean process * data is already clean will produce same data
  count_ts = ts(dailycall[, c('cnt')])
  dailycall$clean_cnt = tsclean(count_ts)
  
  ## Plot timeseries graph  
  ggplot() +
    geom_line(data = dailycall, aes(x = DayDate, y = clean_cnt),color="orange") +
    scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day",date_labels = "%d-%b") +
    xlab("Dates") +
    ylab('Call recieved Count')+
    ggtitle("TimeSeries Of calls")


# Dialy call rate on the basis of category 
  Cdailycall<-ddply(file, .(file$call_status,file$dayDate), nrow)
  names(Cdailycall) <- c("call_status","dayDate", "cnt")
  Cdailycall$dayDate = as.Date(Cdailycall$dayDate)
  Cdailycall$call_status<-as.factor(Cdailycall$call_status)
  
  # time series graph by category
  ggplot(Cdailycall,aes(x=dayDate,y=cnt,colour=call_status)) + geom_line()+
    scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day",date_labels = "%d-%b")+
    xlab("Dates") +
    ylab('Call recieved Count')+
    ggtitle("TimeSeries Of call Status")
  
  # remove connected call 
  rmConnected<- Cdailycall[Cdailycall$call_status!= "ConnectedCalls",]
  
  # plot
  ggplot(rmConnected,aes(x=dayDate,y=cnt,colour=call_status)) + geom_line()+
    scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day",date_labels = "%d-%b")+
    xlab("Dates") +
    ylab('Call recieved Count')+
    ggtitle("TimeSeries Of calls")
  # again remove CallNotpicked
  
  rmCallNotpicked<- rmConnected[rmConnected$call_status!= "CallNotpicked",]
  
  # plot
  ggplot(rmCallNotpicked,aes(x=dayDate,y=cnt,colour=call_status)) + geom_line()+
    scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day",date_labels = "%d-%b")+
    xlab("Dates") +
    ylab('Call recieved Count')+
    ggtitle("TimeSeries Of calls")
  
  ## customer average time spend on call
  customerAvg<-ddply(file, .(file$customer_id), function(x) mean(x$call_duration))
  names(customerAvg) <- c("customer_id","Avg")
  #temp<-customerAvg
  qplot(customerAvg$Avg, geom="histogram") 
  
  customerAvg$customer_id<-as.numeric(customerAvg$customer_id)
  
  # Create box plot of recieved call
  ggplot(
    data = customerAvg, 
    aes(x = Avg, y = Avg)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle("Distribution of call recieved times") +
    xlab("") +
    ylab("recieved (second)") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
  
  
  
  # categorical distribution of avg time spend by the customer.
  avgTimeSpnd<- 
    customerAvg %>%
    mutate(AvgMin =factor(ifelse(Avg == 0, "A NotAttended"
                                 , ifelse(Avg <60, "B lessOneMin"
                                          , ifelse(Avg < 180, "C lessThreeMin"
                                                   ,  ifelse(Avg < 300, "D lessSixMin","E moreSixMin"))))))
  
  
  
  # Amount of time spent by the customer
  
  ggplot(# 
    data = avgTimeSpnd, 
    aes(x = AvgMin)) + 
    geom_bar(fill="light blue") + 
    xlab("Time Spend")+
    ggtitle("Count of Avg min spend by a customer") 
  
  
#  time taken by customer to call again  
  custTimeStamp<-file[,c(3,5)]

# order the customerid and timestamp for calculation
  custTimeStamp<-custTimeStamp[with(custTimeStamp, order(customer_id,called_on)), ]
  custTimeStamp$customer_id<-as.integer(custTimeStamp$customer_id)
  
  # Function defination
  padded.diff = function(x) c(0L, diff(x)) 
  

  setDT(custTimeStamp)[, date.diff := padded.diff(as.POSIXct(called_on)), by = customer_id]
  
# Amount of time spent by the customer to come back
# categorical distribution of avg time spend by the customer.
  custTimeStamp<- 
    custTimeStamp %>%
    mutate(AvgMin =factor(ifelse(date.diff == 0, "Never"
                                 , ifelse(date.diff <60, "lessOneMin"
                                          , ifelse(date.diff < 600, "lessTenMin"
                                                   ,  ifelse(date.diff < 3600, "lessThanAHour"
                                                             ,ifelse(date.diff < 3600, "lessThanAHour"
                                                                     ,ifelse(date.diff < 86400, "lessThanADay"
                                                                             ,ifelse(date.diff < 604800, "lessThanAweek","lessThanAMonth")))))))))
  
# Amount of time spent by the customer to call back
    ggplot(# 
        data = custTimeStamp, 
        aes(x = AvgMin)) + 
        geom_bar(fill="light blue") + 
        xlab("Time spend")+
        coord_flip()+
        ylab("Count")+
        ggtitle("Category of customer on the basis of avg time spend to come back") 
  
  
# Create a pie chart 
   ggplot(
    data = custTimeStamp, 
    aes(x = "", fill = AvgMin)) +# no variable to x axis and fill call Status
    geom_bar() +
    coord_polar(theta = "y") +  # maps the data in circle
    ggtitle("User call back time") +
    ylab("")
