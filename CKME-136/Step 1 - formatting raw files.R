#1 - Load libraries, raw file and format the raw data 
library(lubridate)  #used for formatting month function
rm(list=ls()) 
daily<-read.csv("C:/Users/Chatty Cat Zen/Documents/GitHub/CKME-136/gsp.csv",header=TRUE, stringsAsFactors = F)


#check for nulls and clean up
temp<-daily[which(daily$Adj.Close=="null"),]
#daily<-daily[-which(daily$Adj.Close=="null"),]
temp1<-daily[which(daily$Open=="null"),]
temp2<-daily[which(daily$Close=="null"),]
temp3<-daily[which(daily$High=="null"),]
temp4<-daily[which(daily$Low=="null"),]
temp5<-daily[which(daily$Volume=="null"),]
temp6<-daily[which(daily$Date=="null"),]

rm(temp,temp1,temp2,temp3,temp4,temp5,temp6)

#Check Close, Open, High, Low, Volume, Date

str(daily,digits=8) # check Date column format and other columns are numeric

daily$Date<-as.Date(daily$Date) 
daily$Open<-as.numeric(daily$Open)
daily$Close<-as.numeric(daily$Close)
daily$Low<-as.numeric(daily$Low)
daily$High<-as.numeric(daily$High)
daily$Adj.Close<-as.numeric(daily$Adj.Close)
daily$Volume<-as.numeric(daily$Volume)
#**********************************************************************************************************
#2 - Identify the end of week, and build interim daily formuals


daily$wkday<-as.POSIXlt(daily$Date)$wday
for(i in 1:nrow(daily)) ifelse (daily$wkday[i]>daily$wkday[i+1],
                                daily$eow[i]<-"eow", daily$eow[i]<-"")
daily$eow[nrow(daily)]<-"eow"  

daily$highofweek<-0
daily$highofweek[1]<-daily$High[1]
for(i in 2:nrow(daily)) ifelse (daily$eow[i-1]=="eow",
                                daily$highofweek[i]<-daily$High[i],
                                ifelse(daily$High[i]>daily$highofweek[i-1],
                                       daily$highofweek[i]<-daily$High[i],
                                       daily$highofweek[i]<-daily$highofweek[i-1]))
daily$lowofweek<-0
daily$lowofweek[1]<-daily$Low[1]
for(i in 2:nrow(daily)) ifelse (daily$eow[i-1]=="eow",
                                daily$lowofweek[i]<-daily$Low[i],
                                ifelse(daily$Low[i]<daily$lowofweek[i-1],
                                       daily$lowofweek[i]<-daily$Low[i],
                                       daily$lowofweek[i]<-daily$lowofweek[i-1]))

daily$wk_pr_range<-(daily$highofweek-daily$lowofweek)/daily$lowofweek

daily$sumofvol<-0
daily$sumofvol[1]<-daily$Volume[1]
for(i in 2:nrow(daily)) ifelse (daily$eow[i-1]=="eow",
                                daily$sumofvol[i]<-daily$Volume[i],
                                daily$sumofvol[i]<-daily$sumofvol[i-1]+daily$Volume[i])
daily$numberofdays<-0
daily$numberofdays[1]<-1
for(i in 2:nrow(daily)) ifelse (daily$eow[i-1]=="eow",
                                daily$numberofdays[i]<-1,
                                daily$numberofdays[i]<-daily$numberofdays[i-1]+1)
daily$avg.vol<-daily$sumofvol/daily$numberofdays

head(daily,20) #check
tail(daily)  #check

#****************************************************************************************************************************
#3. Convert the daily file to a weekly file and clean up the interim colums we won't be using

weekly<-daily[which(daily$eow=="eow"),]
weekly<-subset(weekly, select=c(Date,Adj.Close,wk_pr_range, avg.vol)) #keep only Date, Adj.Close, wk.pr.range, avgvolume


#****************************************************************************************************************************
#4. Build the new weekly variables

#fill in the columns of the data: month, wk_ret, w.1, w.2, w.3, w.4, w.5, w.6, w.7, w.8, w.9, w.10
weekly$month <- month(weekly$Date)
weekly$wk_ret<-0
for(i in 2:nrow(weekly)) {weekly$wk_ret[i]<-(weekly$Adj.Close[i]/weekly$Adj.Close[i-1]-1)}
weekly$w.1<-0
for(i in 2:nrow(weekly)) {weekly$w.1[i]<-weekly$wk_ret[i-1]}
weekly$w.2<-0
for(i in 3:nrow(weekly)) {weekly$w.2[i]<-weekly$wk_ret[i-2]}
weekly$w.3<-0
for(i in 4:nrow(weekly)) {weekly$w.3[i]<-weekly$wk_ret[i-3]}
weekly$w.4<-0
for(i in 5:nrow(weekly)) {weekly$w.4[i]<-weekly$wk_ret[i-4]}
weekly$w.5<-0
for(i in 6:nrow(weekly)) {weekly$w.5[i]<-weekly$wk_ret[i-5]}
weekly$w.6<-0
for(i in 7:nrow(weekly)) {weekly$w.6[i]<-weekly$wk_ret[i-6]}
weekly$w.7<-0
for(i in 8:nrow(weekly)) {weekly$w.7[i]<-weekly$wk_ret[i-7]}
weekly$w.8<-0
for(i in 9:nrow(weekly)) {weekly$w.8[i]<-weekly$wk_ret[i-8]}
weekly$w.9<-0
for(i in 10:nrow(weekly)) {weekly$w.9[i]<-weekly$wk_ret[i-9]}
weekly$w.10<-0
for(i in 11:nrow(weekly)) {weekly$w.10[i]<-weekly$wk_ret[i-10]}

#fill in columns for lagged cumulative values: c2, c3, c4, c13, c26, c39, c52

weekly$c2<-0
for(i in 4:nrow(weekly)) {weekly$c2[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-3]-1} 
weekly$c3<-0
for(i in 5:nrow(weekly)) {weekly$c3[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-4]-1} 
weekly$c4<-0
for(i in 6:nrow(weekly)) {weekly$c4[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-5]-1} 
weekly$c13<-0
for(i in 15:nrow(weekly)) {weekly$c13[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-14]-1} 
weekly$c26<-0
for(i in 28:nrow(weekly)) {weekly$c26[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-27]-1} 
weekly$c39<-0
for(i in 41:nrow(weekly)) {weekly$c39[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-40]-1} 
weekly$c52<-0
for(i in 54:nrow(weekly)) {weekly$c52[i]<-weekly$Adj.Close[i-1]/weekly$Adj.Close[i-53]-1} 

#fill in lagged weekly price ranges

weekly$wk_pr_range.1<-0
for(i in 2:nrow(weekly)) {weekly$wk_pr_range.1[i]<-weekly$wk_pr_range[i-1]}
weekly$wk_pr_range.2<-0
for(i in 3:nrow(weekly)) {weekly$wk_pr_range.2[i]<-weekly$wk_pr_range[i-2]}
weekly$wk_pr_range.3<-0
for(i in 4:nrow(weekly)) {weekly$wk_pr_range.3[i]<-weekly$wk_pr_range[i-3]}
weekly$wk_pr_range.4<-0
for(i in 5:nrow(weekly)) {weekly$wk_pr_range.4[i]<-weekly$wk_pr_range[i-4]}

weekly$avg.vol.delta<-0
weekly$avg.vol.delta[1]<-0
for(i in 2:nrow(weekly)) {weekly$avg.vol.delta[i]<-weekly$avg.vol[i]/weekly$avg.vol[i-1]-1}

weekly$vol.delta.1<-0
for(i in 2:nrow(weekly)) {weekly$vol.delta.1[i]<-weekly$avg.vol.delta[i-1]}
weekly$vol.delta.2<-0
for(i in 3:nrow(weekly)) {weekly$vol.delta.2[i]<-weekly$avg.vol.delta[i-2]}
weekly$vol.delta.3<-0
for(i in 4:nrow(weekly)) {weekly$vol.delta.3[i]<-weekly$avg.vol.delta[i-3]}
weekly$vol.delta.4<-0
for(i in 5:nrow(weekly)) {weekly$vol.delta.4[i]<-weekly$avg.vol.delta[i-4]}
tail(weekly)

#****************************************************************************************************************************
#5 - clean up the file - remove interim data, remove rows that don't contain data; export results to csv

#str(weekly)
#names(weekly)
weekly<-subset(weekly,select=-c(wk_pr_range,avg.vol,avg.vol.delta))  #remove interim columns 
temp<-weekly[which(weekly$c52==0),]
weekly<-weekly[-which(weekly$c52==0),]
write.table(weekly,"C:/Users/Chatty Cat Zen/Documents/GitHub/CKME-136/test.csv",sep=",")#,append=T,col.names = F)
