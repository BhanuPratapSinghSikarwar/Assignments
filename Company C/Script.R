  setwd("D://R//job Assign//install//data_for_question_uninstall//interview")
  getwd()

# Library
  library(lattice)
  library(plyr)
  library(dplyr)
  library(ggplot2)#
  library(readr)
  library(lubridate)
  library(data.table)
  library(lattice)
  library(ggplot2)
  library(reshape)
  library(scales)#
  library(data.table)#
  library(lubridate)
  library(readr)
  
 # change date to date format
 # coltypes <-  list(event_timestamp= col_datetime("%d-%m-%Y %H:%M:%S"),properties.timeStamp=col_datetime("%d-%m-%Y %H:%M:%S"))
 # Read A file
 # file<-read_csv("Formatted Data.csv", col_types=coltypes)
 # saveRDS(file,"rowfile.rds")
 
  eventlogfile<-readRDS("rowfile.rds")
  
   
#  Reading second file
  # coltypes <-  list(creation_date= col_datetime("%Y-%m-%d %H:%M:%S"))
  # 
  # uifile<-read_csv("uicycles.csv", col_types=coltypes)
  # saveRDS( uifile,"uifile.rds")
  # uifile<-readRDS("uifile.rds")
  
  
# Year- 2017  Month - 1-12  Day- 0-31 Hour- 0-23  Minute- 0-59 Second- 0-59 dayDate - DATE
# DayParting Day time divided into interval of three hours.
#   Late Night      - 0:3
#   Early Morning   - 3:6
#   Morning         - 6:12
#   AfterNoon       - 12:15
#   Evening         - 15:18
#   Night           - 18:21
#   Mid Night       - 21:24
#   DayOfWeek: week days
  
# check user unique user info in both file
  str(unique(uifile$uuid)) #118242
  str(unique(eventlogfile$uuid)) #72805
  
#### Transform
  uifile <-
    uifile %>%
    mutate(uuid =factor(uuid),
           os =factor(os),
           event_type =factor(event_type),
           uiYear  = factor(year(creation_date), levels=2015:2017),
           uiMonth = factor(month(creation_date), levels=1:12),
           uiDay   = day(creation_date),
           uiHour  = factor(hour(creation_date), levels=0:23),
           uiMinute  = factor(minute(creation_date), levels=0:59),
           uiSecond  = factor(second(creation_date), levels=0:59),
           uidayDate = as.POSIXct(round(creation_date, units = "days")),
           uiDayOfWeek = weekdays(as.Date(uidayDate)),
           uiDayParting = cut(as.numeric(as.character(factor(hour(creation_date), levels=0:23))), 
                            breaks = c(0 , 3, 6, 9,12, 15, 18, 21, 24),
                            labels = c("Late Night", "Early Morning","Morning","Late Morning", "AfterNoon", "Evening", "Night","Mid Night"),
                            right = FALSE)
    )
  
  
  saveRDS( uifile,"uifile.rds")

  
  
  eventlogfile <-
    eventlogfile %>%
    mutate(uuid =factor(uuid),
           event =factor(event),
           elfYear  = factor(year(event_timestamp), levels=2015:2017),
           elfMonth = factor(month(event_timestamp), levels=1:12),
           elfDay   = day(event_timestamp),
           elfHour  = factor(hour(event_timestamp), levels=0:23),
           elfMinute  = factor(minute(event_timestamp), levels=0:59),
           elfSecond  = factor(second(event_timestamp), levels=0:59),
           elfdayDate = as.POSIXct(round(event_timestamp, units = "days")),
           elfDayOfWeek = weekdays(as.Date(elfdayDate)),
           elfDayParting = cut(as.numeric(as.character(factor(hour(event_timestamp), levels=0:23))), 
                              breaks = c(0 , 3, 6,9, 12, 15, 18, 21, 24),
                              labels = c("Late Night", "Early Morning","Morning","Late Morning", "AfterNoon", "Evening", "Night","Mid Night"),
                              right = FALSE)
    )
  
  # saveRDS(eventlogfile,"rowfile.rds")

##################
    
  eventlogfile<-readRDS("rowfile.rds")
  uifile<-readRDS("uifile.rds") 
####################################################################
  
# Customer retention trends
  
# Stack bar Graph  
  retentionInstall<-data.table(uifile)
  retentionInstall<- retentionInstall[ , list(ActivityCount = sum(.N)), by = list(event_type,uidayDate)]

  # Stack bar graph of daily event install, reinstall, uninstall  
  ggplot(retentionInstall, aes(x = uidayDate, y = ActivityCount, fill = event_type)) + 
    geom_bar(stat = "identity")+
    #scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day",date_labels = "%d-%b") +
    scale_y_continuous(label=comma)+
    xlab(" Date") +
    ylab("Activity Count") +
    ggtitle("Daily Activity Chart ")
  
# Time series of event type  
  ggplot(data=retentionInstall, aes(x = uidayDate, y = ActivityCount, group = event_type, colour = event_type)) +
    geom_line() +
    geom_point( size=4)+
    xlab(" Date") +
    ylab("Activity Count") +
    ggtitle("Daily Activity Time Series ")

  # Customer retention trends from their install-uninstall cycles 
  # [frequency chart or histogram plot]   
  
  dt<-data.table(uifile)
  # using setkey or setkeyv to set the key
  setkeyv(dt, c('uidayDate','event_type'))
  
  # converting date to week number to find weekly activity 
  dt$weekNumber<-week(dt$uidayDate)
  dt<-transform(dt, weekNumber = factor(weekNumber, 
                                        levels = c("31", "32", "33", "34", "35","36"),
                                        labels = c("1 FirstWeek", "2 Second Week", "3 Third Week",
                                                   "4 Forth Week", "5 Fifth Week", "6 Sixth Week")))
  
  weeklyFile<-dt[, .(NumberofDistinctCoustomer = length(unique(uuid))), by =list(weekNumber,event_type)]
  
  # Weekly Activity
  ggplot(weeklyFile, aes(x = weekNumber, y = NumberofDistinctCoustomer, fill = event_type)) + 
    geom_bar(stat = "identity")+
    scale_y_continuous(label=comma)+
    xlab(" Weeks") +
    ylab("Activity Count") +
    ggtitle("Weekly Activity ")
  
  # line graph
  ggplot(data=weeklyFile, aes(x=weekNumber, y=NumberofDistinctCoustomer, group = event_type, colour = event_type)) +
    geom_line() +
    geom_point( size=4)+
    xlab(" Weeks") +
    ylab("Activity Count") +
    ggtitle("Weekly Activity ")
  
  # Transpose to colomn install, reinstall, uninstall
  weeklyFile<-cast(weeklyFile,weekNumber~event_type)
  # Add re-install install
  weeklyFile$Install_ReInstall<-rowSums(weeklyFile[,2:3])
  # Summary report
  View(weeklyFile)
  rm(dt,weeklyFile,retentionInstall)  
 
  ###################################################################
  ##  Find out the time of day when the customers are most active in the day 
  ##  [use your own discretion for time of day bucketing 
  # Year- 2017  Month - 1-12  Day- 0-31 Hour- 0-23  
  # Minute- 0-59 Second- 0-59 dayDate - DATE
  # DayParting Day time divided into interval of three hours.
  # Late Night      - 0:3
  # Early Morning   - 3:6
  # Morning         - 6:12
  # AfterNoon       - 12:15
  # Evening         - 15:18
  # Night           - 18:21
  # Mid Night       - 21:24
  # DayOfWeek: week days
  
  # - Find out the time of day when the customers are most active in the day 
  # [use your own discretion for time of day bucketing]
  # Customer Activity on the basis of day parting  
  
  eventlogfile <-
    eventlogfile %>%
    mutate(elfDayParting = cut(as.numeric(as.character(factor(hour(event_timestamp), levels=0:23))), 
                     breaks = c(0 , 3, 6, 9,12, 15, 18, 21, 24),
                     labels = c("Late Night", "Early Morning","Morning","Late Morning", "AfterNoon", "Evening", "Night","Mid Night"),
                     right = FALSE))
  
  ggplot(
    data = eventlogfile, 
    aes(x = elfDayParting)) + 
    geom_bar(fill="light blue") +  
    xlab(" Day Parting") +
    ylab("User Activity Count") +
    scale_y_continuous(label=comma)+
    coord_flip() +
    ggtitle("Dayparting ~ User Activity ")
  
  
  # Purchase value buckets 
  # [find purchase events from event logs and parse the 'properties' column to get 
  # money/cart value associated and generate a simple bucketed frequency chart/histogram plot]
  
  # convert to numeric
  eventlogfile$`properties.Cart Value`<-as.numeric(eventlogfile$`properties.Cart Value`)
  # Histogram and frequency chart  
  ggplot(eventlogfile, aes(`properties.Cart Value`)) + # removing na values
    geom_histogram(binwidth = 500,fill="sky blue")+
    geom_freqpoly(binwidth =500, color = "dark blue")+
    xlab(" Cart Value( in Rupee)") +
    ylab("No of users") +
    ggtitle("Cart Value Histogram ")
  
  #custom width
  temp<-as.numeric(eventlogfile$`properties.Cart Value`)
  temp<-as.data.frame(temp)
  names(temp)<-c("properties.Cart Value")
  temp<-na.omit(temp) # removed the NA value
  
  temp <-
    temp %>%
    mutate(customBin=cut( `properties.Cart Value`, 
                          breaks = c(0 , 1000, 2000, 4000, 8000, 16000, 32000, 64000),
                          labels = c("0-1000", "1000-2000","2000-4000", "4000-8000", "8000-16000",
                                     "16000-32000","32000-64000"),
                          right = FALSE)
    )
  
  ggplot(temp, aes(customBin)) + # removing na values
    geom_histogram(stat = "count",fill="sky blue")+
    xlab(" Cart Value( in Rupee)") +
    ylab("No of Users") +
    ggtitle("Cart Value Histogram ")
  
  # remove temp
  rm(temp)
  
  # Filter Checkout activity   
  checkoutevent<-  c("Checkout is completed by PG",
                     "Checkout is completed by null",
                     "Checkout is completed by Credit Cards / Debit Cards / Net Banking",
                     "Checkout is completed by Cash On Delivery","Guest checkout initiated",
                     "Checkout is completed by Paid using zCoins")
  # filtered value has empty cart value
  checkoutFilter<-filter(eventlogfile, grepl(paste(checkoutevent, collapse="|"), event))  
  ## this have no information about cart value
  
  # Histogram and frequency chart  
  # ggplot(checkoutFilter, aes(`properties.Cart Value`)) + # removing na values
  #   geom_histogram(binwidth = 500,fill="sky blue")+
  #   geom_freqpoly(binwidth =500, color = "dark blue")+
  #   xlab(" Cart Value( in Rupee)") +
  #   ylab("Count") +
  #   ggtitle("Cart Value Histogram ")
  
  rm(checkoutevent,checkoutFilter)
 
  ## Behavior of purchasing and non-purchasing customers 
  ##[something along the lines of their in-app event path in a given install-uninstall cycle]  
  
  ## User file
  PNCustomer<-uifile
  
  # order by creation date
  PNCustomer<-PNCustomer %>%
    group_by(uuid) %>%
    arrange(creation_date)
  
  ## Create summary report of activity
  PNCustomer<-PNCustomer %>%
    group_by(uuid) %>%  
    summarise(eventSeries = paste(event_type, collapse = ", ")
              ,eventdate = paste(as.character(creation_date), collapse = ", "))
  
  View(PNCustomer)
  
  # table have user information on his activity and frequecy count
  PNCustomertable<-(table(PNCustomer$eventSeries))
  PNCustomertable<-as.data.frame(PNCustomertable)
  
  # file has information about diffrent pattern of install reinstall uninstall cycle
  View(PNCustomertable)
  
  ## Graph for coustomer cycle of install and uninstall
  GraphPNCustomertable<-PNCustomertable
  GraphPNCustomertable$CyclicEvent<-paste(sub('\\s*,.*', '', as.character(GraphPNCustomertable$Var1))
                 ,sub('.*,\\s*', '', as.character(GraphPNCustomertable$Var1)), sep = ",")
  
  GraphPNCustomertable<-data.table(GraphPNCustomertable)
  GraphPNCustomertable<- 
    GraphPNCustomertable[ , list(ActivityCount = sum(as.numeric(Freq))), by = CyclicEvent]
  
  GraphPNCustomertable<-GraphPNCustomertable[order(ActivityCount),]
  #GraphPNCustomertable: Similar Activity merge into install reinstall uninstall cycle by just keeping first and last activity.
  View(GraphPNCustomertable)  
  
  # Bar graph  coustomer cycle of install and uninstall
  ggplot(GraphPNCustomertable, aes(x = CyclicEvent, y = ActivityCount)) + 
    geom_bar(stat = "identity")+
    scale_y_continuous(label=comma)+
    coord_flip()+
    xlab(" Cyclic Event") +
    ylab("Customer Activity Count") +
    ggtitle("Customer cycle of install and uninstall ")
  
  #rm(PNCustomertable,GraphPNCustomertable)
  
  # on Event file
  efltemp<-eventlogfile[,1:3]
  
  ## removing uio_push_token
  efltemp<-efltemp[- grep("uio_push_token", efltemp$event),]
  
  # order by event_timestamp 
  efltemp<-efltemp %>%
    group_by(uuid) %>%
    arrange(event_timestamp)
  
  ## Create summary report of activity
  efltemp<-efltemp %>%
    group_by(uuid) %>%  
    summarise(eventSeries = paste(event, collapse = ", ")
              ,eventdate = paste(as.character(event_timestamp), collapse = ", "))
  
  # file has information about diffrent Activity of customer
  View(efltemp) 
  
  # join data have information of all the activity he has done over a period of time and 
  # install reinstall uninstall cycle.
  
  CombineActivityInfo<-full_join(efltemp, PNCustomer,by = "uuid")
  View(CombineActivityInfo)
  
  
  # How are their purchases distributed post install?  
  # Coustome behaviour
  # uuid
  # first Activity
  # last activity
  # number of order
  # total amount of purchase
  
  # use data table for aggregation because of large data
  dt<-data.table(eventlogfile)
  
  a<- dt[ , list(FirstActivity = min(event_timestamp)), by = uuid]
  b<- dt[ , list(LastActivity = max(event_timestamp)), by = uuid]
  c<- dt[ , list(TotalCartValue = sum(`properties.Cart Value`,na.rm = TRUE)), by = uuid]
  d<- dt[ , list(productCount = sum(as.numeric(`properties.No Of Products`),na.rm = TRUE)), by = uuid]
  
  data_raw <- cbind(a,b,c,d)
  
  # Customer Activity Summary
  data_raw <-data_raw[,c(1,2,4,6,8)]
  
  # ProductCount ~ TotalCartValue
  ggplot(data_raw,aes(productCount,TotalCartValue))+
    geom_point(color="blue")+
    geom_smooth(method=lm)+
    scale_y_continuous(label=comma)+
    xlab(" Product purchase Count") +
    ylab("Total Amount Spend") +
    ggtitle("User Purchase Summary ")
  
  
  # ProductCount ~ TotalNSCartValue -- droping this information as this is not mactching with product count
  # ggplot(data_raw,aes(productCount,TotalNSCartValue))+
  #   geom_point(color="blue")+
  #   geom_smooth(method=lm)+
  #   scale_y_continuous(label=comma)+
  #   xlab(" Product purchase Count") +
  #   ylab("Total Amount Spend") +
  #   ggtitle("User Purchase Summary ")
  
  data_raw <-data_raw[,c(1,2,3,4,6)]
  
  # Coustome behaviour from his activity (Event log file)
  View(data_raw)
  rm(data_raw)
  
  # Do they perform purchases in the 2nd,3rd etc weeks post install?
  # Filter user who have installed and reinstalled the app.
  uisubset<-subset(uifile, event_type == "install" | event_type == "re-install")
  uisubset<-as.data.frame(uisubset[,1])
  
  # filter data from event log file
  elfsubfile<-eventlogfile[
    ,c("uuid","event","event_timestamp","properties.No Of Products", "properties.Cart Value","properties.category","properties.ns_cart_amount","elfdayDate")]
  
  ## removing uio_push_token
  elfsubfile<-elfsubfile[- grep("uio_push_token", elfsubfile$event),]
  
  # converting date to week number to find weekly activity 
  elfsubfile$weekNumber<-week(elfsubfile$elfdayDate)
  elfsubfile<-transform(elfsubfile, weekNumber = factor(weekNumber, 
                                                        levels = c("34", "35","36"),
                                                        labels = c("1 FirstWeek", "2 Second Week","3 Third Week")))
  
  # join with event log to find out user's activity
  EventJoinData<-inner_join(x=elfsubfile,y=uisubset,by="uuid")
  
  
  dt<-data.table(EventJoinData)
  # using setkey or setkeyv to set the key
  setkeyv(dt, c('uuid', 'weekNumber'))
  # self 
  
  a<- dt[ , list(FirstActivity = min(event_timestamp)), list(uuid, weekNumber)]
  b<- dt[ , list(LastActivity = max(event_timestamp)), list(uuid, weekNumber)]
  c<- dt[ , list(TotalCartValue = sum(as.numeric(`properties.Cart.Value`),na.rm = TRUE)), by = list(uuid, weekNumber)]
  d<- dt[ , list(TotalCartValue = sum(as.numeric(`properties.ns_cart_amount`),na.rm = TRUE)), by = list(uuid, weekNumber)]
  e<- dt[ , list(productCount = sum(as.numeric(`properties.No.Of.Products`),na.rm = TRUE)), by = list(uuid, weekNumber)]
  f<- dt[ , list(ActivityCount = sum(.N)), by = list(uuid, weekNumber)]
  g<- dt[ , .(eventTrack = paste(event, collapse=",")), by = list(uuid, weekNumber)]
  h<- dt[ , .(dayDateTrack =  paste(elfdayDate, collapse=",")), by = list(uuid, weekNumber)]
  
  
  data_raw <- cbind(a,b,c,d,e,f,g,h)
  data_raw<-data_raw[, which(duplicated(names(data_raw))) := NULL]
  View(data_raw)
  
  rm(a,b,c,d,e,f,g,h)
  
  
  # - Is there a steady inflow of revenue as a customer's retention increases? 
  #   [growth can decline but is it still a positive gradient?]
  
  # large volume of data handle better in data.table
  dt<-data.table(uifile)
  # using setkey or setkeyv to set the key
  setkeyv(dt, c('uidayDate','event_type'))
  
  # converting date to week number to find weekly activity 
  dt$weekNumber<-week(dt$uidayDate)
  dt<-transform(dt, weekNumber = factor(weekNumber, 
                                        levels = c("31", "32", "33", "34", "35","36"),
                                        labels = c("1 FirstWeek", "2 Second Week", "3 Third Week",
                                                   "4 Forth Week", "5 Fifth Week", "6 Sixth Week")))
  
  weeklyFile<-dt[, .(NumberofDistinctCoustomer = length(unique(uuid))), by =list(weekNumber,event_type)]
  
  # Weekly Activity
  ggplot(weeklyFile, aes(x = weekNumber, y = NumberofDistinctCoustomer, fill = event_type)) + 
    geom_bar(stat = "identity")+
    scale_y_continuous(label=comma)+
    xlab(" Week") +
    ylab("Customer Activity") +
    ggtitle("Weekly Activity ")
  
  # line graph
  ggplot(data=weeklyFile, aes(x=weekNumber, y=NumberofDistinctCoustomer, group = event_type, colour = event_type)) +
    geom_line() +
    geom_point( size=4)+
    xlab(" Week") +
    ylab("Customer Activity") +
    ggtitle("Weekly Activity ")
  
  
  # Transpose to colomn install, reinstall, uninstall
  weeklyFile<-cast(weeklyFile,weekNumber~event_type)
  
  # Add re-install install
  weeklyFile$Install_New<-rowSums(weeklyFile[,2:3])
  View(weeklyFile)
  
# rm(checkoutFilter,checkoutevent,data_raw)
# rm(CombineActivityInfo,dt,efltemp,elfsubfile,EventJoinData)
  
## Customer Age
  
  retention<-uifile[,c(1,3,4)]
  retention<-data.table(retention)
  retention<-retention[order(uuid,creation_date),]
  
  retentionInstall<-subset(retention, event_type == "install" | event_type == "re-install")
  retentionUninstall<-subset(retention, event_type == "uninstall")
  
  a<- retentionInstall[ , list(FirstActivity = min(creation_date)), by=uuid]
  b<- retentionUninstall[ , list(LastActivity = max(creation_date)), by=uuid]
  
  retention<-full_join(retentionInstall,retentionUninstall,by="uuid")
  names(retention)<-c("uuid","Install","InstallDate" ,"Uninstall","UninstallDate")
  
  retention<-setorder(retention, uuid,InstallDate,UninstallDate)
  retention<-retention[3:121723,]
  
  minimum<-min(retention$InstallDate,na.rm = TRUE)
  minimum<-as.POSIXct(minimum, "%m-%d-%Y-%X")
  
  maximum<-max(retention$UninstallDate,na.rm = TRUE)
  maximum<-as.POSIXct(maximum, "%m-%d-%Y-%X")
  
  # replacing na with lower date and maximum date
  retention$InstallDate<- ymd_hms(ifelse(is.na(retention$InstallDate),paste(minimum),paste(retention$InstallDate)))
  retention$UninstallDate<- ymd_hms(ifelse(is.na(retention$UninstallDate),paste(maximum),paste(retention$UninstallDate)))  
  
  
  retention$AgeInDay<-as.Date(as.character(retention$UninstallDate), format="%Y-%m-%d %H:%M:%S")-
    as.Date(as.character(retention$InstallDate), format="%Y-%m-%d %H:%M:%S")
  
  retention<-retention[,c(1,3,5,6)]
  
  # if Age is  -ve it maen he have not uninstalled the app yet
  retention$AgeInDay<-ifelse(retention$AgeInDay<0,30,retention$AgeInDay)
  # Create box plot of Customer Age
  ggplot(
    data = retention, 
    aes(x = AgeInDay, y = AgeInDay)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle("Distribution of Customer Age") +
    xlab("") +
    ylab("Customer Age(Days)") +
    scale_y_continuous(minor_breaks = seq(1, 30, 1))+
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
  
  summary(retention$AgeInDay)
  