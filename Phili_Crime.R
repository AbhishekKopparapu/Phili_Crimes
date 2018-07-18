library(dplyr)
library(ggplot2)

# Overall trend in crimes for the whole period of time in the dataset. The
# granularity should be at the Day level.
# 2. Which are the most and the least dangerous hours in Philadelphia?
#   3. Is there any seasonality in the crime rate?
#   4. What are the top 10 crimes crime types?
#   5. Which police HQ is in the most need of strengthening?

myData <- read.csv('CrimeData2.csv')

myData$dateTime <- as.POSIXct(myData$Dispatch_Date_Time,format = "%Y-%m-%d %H:%M:%S", tz='EST')
myData$date <- as.Date(myData$dateTime, tz = 'EST')

bydate <- myData %>% group_by(myData$date) %>% summarise(date =length(date))
colnames(bydate) <- c('Date','Number_of_Crimes_commited')
ggplot(bydate, aes(Date,Number_of_Crimes_commited,color = Date)) + geom_line()

myData$Hour <- strftime(myData$dateTime, format = '%H', tz='EST')
byhour <- myData %>% group_by(myData$Hour) %>% summarise(Hour =length(Hour))
colnames(byhour) <- c("Hour", "Total")
byhour$Hour <- as.integer(byhour$Hour)
ggplot(byhour, aes(Hour,Total)) + geom_line(color='red') + ggtitle('Hourly Trend') + xlab('Hour')+ylab('Total')

myData$Month <- strftime(myData$dateTime, format = '%m', tx = 'EST')
bymonth<- myData %>% group_by(myData$Month) %>% summarise(Month = length(Month))
colnames(bymonth) <- c('Month', 'Total')
ggplot(bymonth, aes(Month,Total))+ geom_bar(fill = "Maroon", stat = 'Identity')+ggtitle('Monthly Trend')+xlab('Months')+ylab('Total')

bytype <- myData %>% group_by(myData$Text_General_Code) %>% summarise(code = length(Text_General_Code))
colnames(bytype) <- c('Type', 'Total')
bytype_sorted <- bytype[order(bytype$Type,decreasing=TRUE),]
top10crimes <- bytype_sorted[1:10,]
colnames(bytype) <- c('Type', 'Total')
ggplot(top10crimes, aes(x=reorder(Type,Total),y=Total))+geom_bar(aes(fill=Type), stat = 'Identity' )+ggtitle('Category Trend')+xlab('Type')+ylab('Total')+coord_flip()

byhq<- myData %>% group_by(myData$Dc_Dist) %>% summarise(hq = length(Dc_Dist))
colnames(byhq)<- c('HQ','Total')
ggplot(byhq, aes(x=reorder(HQ,-Total),y=Total))+geom_bar(aes(fill=HQ),stat = "Identity")+ggtitle('Head Quarters Trend')+ xlab('Head Quarters')+ ylab('Total')
