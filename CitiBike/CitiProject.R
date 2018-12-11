citi1<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201601-citibike-tripdata.csv')
citi2<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201602-citibike-tripdata.csv')
citi3<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201603-citibike-tripdata.csv')
citi4<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201604-citibike-tripdata.csv')
citi5<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201605-citibike-tripdata.csv')
citi6<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201606-citibike-tripdata.csv')
citi7<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201607-citibike-tripdata.csv')
citi8<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201608-citibike-tripdata.csv')
citi9<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201609-citibike-tripdata.csv')
citi10<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201610-citibike-tripdata.csv')
citi11<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201611-citibike-tripdata.csv')
citi12<- read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Citi/201612-citibike-tripdata.csv')
total<-nrow(citi1)+nrow(citi2)+nrow(citi3)+nrow(citi4)+nrow(citi5)+nrow(citi6)+nrow(citi7)+nrow(citi8)+nrow(citi9)+nrow(citi10)+nrow(citi11)+nrow(citi12)
total


#Dividing into quarterly data frames
citiq1<-rbind(citi1,citi2,citi3)
write.csv(citiq1,file="C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ1.csv")
citiq2<-rbind(citi4,citi5,citi6)
write.csv(citiq2,file="C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ2.csv")
citiq3<-rbind(citi7,citi8,citi9)
write.csv(citiq3,file="C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ3.csv")
citiq4<-rbind(citi10,citi11,citi12)
write.csv(citiq4,file="C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ4.csv")

#Reading Quarterly data sets
citiQuarter1<-read.csv("C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ1.csv")
citiQuarter2<-read.csv("C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ2.csv")
citiQuarter3<-read.csv("C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ3.csv")
citiQuarter4<-read.csv("C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQ4.csv")
nrow(citiQuarter1)
nrow(citiQuarter2)
nrow(citiQuarter3)
nrow(citiQuarter4)
head(citiQuarter1)
head(citiQuarter2)
head(citiQuarter3)
head(citiQuarter4)
str(citiQuarter1)


#Visualizations
summary(citiQuarter1)
summary(citiQuarter2)
summary(citiQuarter3)
summary(citiQuarter4)
boxplot(citiQuarter1$tripduration,main="Tripduration Quarter 1")
boxplot(citiQuarter2$tripduration,main="Tripduration Quarter 2")
boxplot(citiQuarter3$tripduration,main="Tripduration Quarter 3")
boxplot(citiQuarter4$Trip.Duration,main="Tripduration Quarter 4")
hist(citiQuarter1$tripduration)
hist(citiQuarter2$tripduration)
hist(citiQuarter3$tripduration)
hist(citiQuarter4$Trip.Duration)
length(unique(citiQuarter1$start.station.name))

library(ggmap)
qmap('New York City,New York')
coords<-cbind(citiQuarter1$start.station.longitude,citiQuarter1$start.station.latitude)
library(sp)
place.pts <- SpatialPointsDataFrame(coords, citiQuarter1[, -(7:8)], proj4string = CRS("+init=epsg:4326"))
plot(place.pts, pch = ".", col = "darkred")
map<-qmap('New York City,New York',zoom=13)
map + geom_point(data = citiQuarter1, aes(x =start.station.longitude , y = start.station.latitude), color="dark red", size=3, alpha=0.5)
map + geom_point(aes(x = start.station.longitude, y = start.station.latitude, size=0.2,color=citiQuarter1$usertype,alpha=0.5),data=citiQuarter1)


#Computing Age
citiQuarter1$Age<-2017
citiQuarter1$Age<-citiQuarter1$Age-citiQuarter1$birth.year
citiQuarter2$Age<-2017
citiQuarter2$Age<-citiQuarter2$Age-citiQuarter2$birth.year
citiQuarter3$Age<-2017
citiQuarter3$Age<-citiQuarter3$Age-citiQuarter3$birth.year
citiQuarter4$Age<-2017
citiQuarter4$Age<-citiQuarter4$Age-citiQuarter4$Birth.Year

par(mfrow=c(1,2))
boxplot(citiQuarter1$Age,main="Age Q1")
hist(citiQuarter1$Age)
boxplot(citiQuarter2$Age,main="Age Q2")
boxplot(citiQuarter3$Age,main="Age Q3")
boxplot(citiQuarter4$Age,main="Age Q4")
hist(citiQuarter1$Age)
hist(citiQuarter2$Age)
hist(citiQuarter3$Age)
hist(citiQuarter4$Age)
nrow(citiQuarter1)
nrow(citiQuarter2)
nrow(citiQuarter3)
nrow(citiQuarter4)
length(unique(citiQuarter$bikeid))
length(unique(citiQuarter2$bikeid))
length(unique(citiQuarter3$bikeid))
length(unique(citiQuarter4$Bike.ID))
length(unique(citiQuarter1$start.station.id))

#Extracting weekdays
citiQuarter1$startweekday<-weekdays(as.Date(citiQuarter1$starttime,"%m/%d/%Y"))
citiQuarter1$stopweekday<-weekdays(as.Date(citiQuarter1$stoptime,"%m/%d/%Y"))
citiQuarter1$startweekday<-as.factor(citiQuarter1$startweekday)
citiQuarter1$stopweekday<-as.factor(citiQuarter1$stopweekday)
citiQuarter2$startweekday<-as.factor(weekdays(as.Date(citiQuarter2$starttime,"%m/%d/%Y")))
citiQuarter2$stopweekday<-as.factor(weekdays(as.Date(citiQuarter2$stoptime,"%m/%d/%Y")))
citiQuarter3$startweekday<-as.factor(weekdays(as.Date(citiQuarter3$starttime,"%m/%d/%Y")))
citiQuarter3$stopweekday<-as.factor(weekdays(as.Date(citiQuarter3$stoptime,"%m/%d/%Y")))
citiQuarter4$startweekday<-as.factor(weekdays(as.Date(citiQuarter4$Start.Time)))
citiQuarter4$stopweekday<-as.factor(weekdays(as.Date(citiQuarter4$Stop.Time)))

head(citiQuarter4)
head(citiQuarter1)

plot(citiQuarter1$startweekday)
plot(citiQuarter1$stopweekday)
plot(citiQuarter2$startweekday)
plot(citiQuarter2$stopweekday)
plot(citiQuarter3$startweekday)
plot(citiQuarter3$stopweekday)
plot(citiQuarter4$startweekday)
plot(citiQuarter4$stopweekday)

#Extracting time
rows_quarters<-c(nrow(citiQuarter1),nrow(citiQuarter2),nrow(citiQuarter3),nrow(citiQuarter4))
barplot(rows_quarters,main="Number of observations",names.arg = c("Q1","Q2","Q3","Q4"))
citiQuarter1$X<-NULL
citiQuarter1$Time<-NULL
class(citiQuarter1$starttime)
head(citiQuarter1)
library(anytime)
citiQuarter1$starttime<- as.POSIXct(as.character(citiQuarter1$starttime), format="%Y-%m-%d %H:%M:%S")
citiQuarter1$starthour<-format(citiQuarter1$starttime,"%H")
citiQuarter1$stoptime<- as.POSIXct(as.character(citiQuarter1$stoptime), format="%Y-%m-%d %H:%M:%S")
citiQuarter1$stophour<-format(citiQuarter1$stoptime,"%H")
head(citiQuarter1)

#Calculating distance from longitude and latitude values
library(geosphere)
start_lat<-as.vector(citiQuarter1$start.station.latitude)
start_lon<-as.vector(citiQuarter1$start.station.longitude)
end_lat<-as.vector(citiQuarter1$end.station.latitude)
end_lon<-as.vector(citiQuarter1$end.station.longitude)
citiQuarter1$dist<-distHaversine(citiQuarter1[,8:7],citiQuarter1[,12:11])
citiQuarter1$dist<-citiQuarter1$dist/1000
nrow(citiQuarter1)

#Checking if weekday or not
library(car)
citiQuarter1$isweekday<-NULL
citiQuarter1$startweekday<-as.character(citiQuarter1$startweekday)
citiQuarter1$isstartweekday<-recode(citiQuarter1$startweekday,"'Friday'=1;'Monday'=1;'Tuesday'=1;'Wednesday'=1;'Thursday'=1;'Saturday'=0;'Sunday'=0")
citiQuarter1$isstartweekday<-as.factor(citiQuarter1$isstartweekday)
str(citiQuarter1$isstartweekday)
citiQuarter1$startweekday<-as.factor(citiQuarter1$startweekday)
citiQuarter1$stopweekday<-as.character(citiQuarter1$stopweekday)
citiQuarter1$isstopweekday<-recode(citiQuarter1$stopweekday,"'Friday'=1;'Monday'=1;'Tuesday'=1;'Wednesday'=1;'Thursday'=1;'Saturday'=0;'Sunday'=0")
citiQuarter1$isstopweekday<-as.factor(citiQuarter1$isstopweekday)
str(citiQuarter1$isstopweekday)
citiQuarter1$stopweekday<-as.factor(citiQuarter1$stopweekday)

#Dropping start station ID and end station id
citiQuarter1$start.station.id<-NULL
citiQuarter1$end.station.id<-NULL
nrow(citiQuarter1)
head(citiQuarter1)

#Reading the climate dataset
climate_1<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-1-1.csv')
climate_2<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-2-1.csv')
climate_3<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-3-1.csv')
climate_4<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-4-1.csv')
climate_5<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-5-1.csv')
climate_6<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-6-1.csv')
climate_7<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-7-1.csv')
climate_8<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-8-1.csv')
climate_9<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-9-1.csv')
climate_10<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-10-1.csv')
climate_11<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-11-1.csv')
climate_12<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/Climate data/GHCND-USW00094728_2016-12-1.csv')
climateq1<-rbind(climate_1,climate_2,climate_3)
climateq2<-rbind(climate_4,climate_5,climate_6)
climateq3<-rbind(climate_7,climate_8,climate_9)
climateq4<-rbind(climate_10,climate_11,climate_12)
citiQuarter1$Date<-as.Date(citiQuarter1$starttime,"%m/%d/%Y")
head(citiQuarter1)
head(climateq1)
climateq1$Date<-as.Date(climateq1$Date,"%m/%d/%Y")
head(climateq1)

#Munging climate data with bike data
citiQuarter1_ex<-merge(citiQuarter1,climateq1,by.x="Date",by.y="Date")
head(citiQuarter1_ex)
citiQuarter1_ex$Year<-NULL
citiQuarter1_ex$Month<-NULL
citiQuarter1_ex$Day<-NULL
citiQuarter1<-citiQuarter1_ex
citiQuarter1_ex<-NULL
write.csv(citiQuarter1,'C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQuarter1.csv')
citiQuarter1<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQuarter1.csv')
head(citiQuarter1)
#Dividing into weekend and weekday data
weekday_q1<-citiQuarter1[which(citiQuarter1$isstartweekday==1),]
#26 outliers found having 0 as end station longitude and latitude. Hence removed.
outliers_q1<-citiQuarter1[which(citiQuarter1$Age>100),]
nrow(outliers_q1)
citiQuarter1<-citiQuarter1[which(citiQuarter1$dist<8000),]
citiQuarter1<-citiQuarter1[which(citiQuarter1$Age<100),]
weekday_q1<-weekday_q1[which(weekday_q1$dist<8000),]
weekend_q1<-citiQuarter1[which(citiQuarter1$isstartweekday==0),]
weekend_q1<-weekend_q1[which(weekend_q1$dist<8000),]
#Calculating average temperature 
weekday_q1$AvgTemp<-(weekday_q1$Max+weekday_q1$Min)/2
weekend_q1$AvgTemp<-(weekend_q1$Max+weekend_q1$Min)/2
citiQuarter1$AvgTemp<-(citiQuarter1$Max+citiQuarter1$Min)/2
nrow(weekday_q1)
nrow(weekend_q1)

#Visulaizations for distance traveled
boxplot(weekday_q1$dist,main="Distance on weekdays")
hist(weekday_q1$dist,main="Weekdays distance travelled")
hist(weekend_q1$dist,main="Weekend distance travelled")
boxplot(weekend_q1$dist,main="Distance on weekends")

#Visualizations for max and min temperatures
boxplot(weekday_q1$AvgTemp,main="Avg weekday temperature - Q1")
boxplot(weekend_q1$AvgTemp,main="Avg weekend temperature - Q1")
plot(weekday_q1$AvgTemp,weekday_q1$dist)
lines(weekday_q1$AvgTemp,weekday_q1$dist)


plot(weekday_q1$dist~weekday_q1$startweekday)
require(ggplot2)
g<-ggplot( data = citiQuarter1, aes( stophour, dist  )) 
g + geom_raster(aes(fill=gender))
g + geom_bar(stat="identity") 
g + geom_point()
plot(citiQuarter1$Age,citiQuarter1$dist)
g<-ggplot( data = weekday_q1, aes( gender, dist  )) 
head(citiQuarter1)
citiQuarter1$starthour<-factor(citiQuarter1$starthour)
citiQuarter1$stophour<-factor(citiQuarter1$stophour)
str(citiQuarter1)
citiQuarter1$speed<-citiQuarter1$dist/(citiQuarter1$tripduration/3600)
head(citiQuarter1)
citiQuarter1$count<-1
#Total number of trips
aggdata_trips<-aggregate(citiQuarter1$count ~ citiQuarter1$starthour, citiQuarter1, sum)
install.packages("ggplot2", dependencies = TRUE)
plot(aggdata_trips)
lines(aggdata_trips)
X.3<-NULL                 
X.2<-NULL              
X.1<-NULL
#To calculate speed
aggdata_speed<-aggregate(citiQuarter1$speed ~ citiQuarter1$bikeid, citiQuarter1, mean)
plot(aggdata_speed$`citiQuarter1$speed`)
head(aggdata_speed)
colnames(aggdata_speed)<-c("bikeid","speed")
#To get the avg speed for all bike ids
citiQuarter1_ex<-merge(citiQuarter1,aggdata_speed,by.x="bikeid",by.y="bikeid")
head(citiQuarter1_ex)
str(citiQuarter1_ex)
citiQuarter1_ex$gender<-as.factor(citiQuarter1_ex$gender)
qplot(x=starthour,y=speed.y,data=citiQuarter1_ex,color=citiQuarter1_ex$gender)
qplot(x=Age,y=speed.y,data=citiQuarter1_ex,color=citiQuarter1_ex$gender)
head(citiQuarter1_ex)
nrow(citiQuarter1_ex)
citiQuarter1_ex$Trips <- with(citiQuarter1_ex, ave(count, starthour, FUN=cumsum))
write.csv(citiQuarter1_ex,'C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQuarter1_ex.csv')
g<-ggplot( data = citiQuarter1_ex, aes( starthour, Trips  )) 

head(citiQuarter1_ex)

citiQuarter1_rush_hour<-citiQuarter1_ex[which((citiQuarter1_ex$starthour==7)|(citiQuarter1_ex$starthour==8)|(citiQuarter1_ex$starthour==9)|(citiQuarter1_ex$starthour==10)|(citiQuarter1_ex$starthour==16)|(citiQuarter1_ex$starthour==17)|(citiQuarter1_ex$starthour==18)|(citiQuarter1_ex$starthour==19)),]
citiQuarter1_rush_hour<-citiQuarter1_rush_hour[which(citiQuarter1_rush_hour$gender!=0),]
citiQuarter1_rush_hour<-citiQuarter1_rush_hour[which(citiQuarter1_rush_hour$isstartweekday==1),]

library(data.table)
citiQuarter1_rush_hour<-data.table(citiQuarter1_rush_hour)
citiQuarter1_rush_hour_table<-citiQuarter1_rush_hour[,sum(count),by=list(Date,starthour)]
citiQuarter1_rush_hour_table<-data.frame(citiQuarter1_rush_hour_table)
citiQuarter1_rush_hour<-merge(citiQuarter1_rush_hour,citiQuarter1_rush_hour_table,by=c("Date","starthour"))
head(citiQuarter1_rush_hour)
plot(citiQuarter1_rush_hour$V1)
ncol(citiQuarter1_rush_hour)
colnames(citiQuarter1_rush_hour)[33]<-"TripsPerHour"
write.csv(citiQuarter1_rush_hour,'C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQuarter1RushHour_1.csv')
citiQuarter1_rush_hour<-read.csv('C:/Users/USER/Documents/MS Stuff/RPI/Spring 2017/Data Analytics/Project/CitiQuarter1RushHour_ex.csv')
#Linear reg
str(citiQuarter1_ex)
lm1<-lm(citiQuarter1_ex$Trips~citiQuarter1_ex$tripduration+citiQuarter1_ex$gender+citiQuarter1_ex$Age+citiQuarter1_ex$AvgTemp+citiQuarter1_ex$starthour+citiQuarter1_ex$isstartweekday+citiQuarter1_ex$dist)
summary(lm1)
plot(lm1)
lm1<-lm(citiQuarter1_ex$Trips~citiQuarter1_ex$tripduration+citiQuarter1_ex$gender+citiQuarter1_ex$Age+citiQuarter1_ex$AvgTemp:citiQuarter1_ex$dist+citiQuarter1_ex$AvgTemp+citiQuarter1_ex$isstartweekday+citiQuarter1_ex$dist)

nrow(citi_train)
nrow(citi_test)

#Converting into factor and extracting month
citiQuarter1_rush_hour$tripduration<-citiQuarter1_rush_hour$tripduration/3600
citiQuarter1_rush_hour$gender<-as.factor(citiQuarter1_rush_hour$gender)
citiQuarter1_rush_hour$starthour<-as.factor(citiQuarter1_rush_hour$starthour)
citiQuarter1_rush_hour$Date<-as.Date(citiQuarter1_rush_hour$Date)
citiQuarter1_rush_hour$week<-as.numeric(format(citiQuarter1_rush_hour$Date,"%d"))
citiQuarter1_rush_hour$week<-(citiQuarter1_rush_hour$week/7)
citiQuarter1_rush_hour$week<-as.integer(citiQuarter1_rush_hour$week)
citiQuarter1_rush_hour$week<-as.factor(citiQuarter1_rush_hour$week)
citiQuarter1_rush_hour$month<-as.factor(format(citiQuarter1_rush_hour$Date,"%m"))
head(citiQuarter1_rush_hour)
str(citiQuarter1_rush_hour)
citiQuarter1_rush_hour<-data.table(citiQuarter1_rush_hour)
citiQuarter1_rush_hour_table<-citiQuarter1_rush_hour[,sum(count),by=list(bikeid,startweekday)]
citiQuarter1_rush_hour_table<-data.frame(citiQuarter1_rush_hour_table)
citiQuarter1_rush_hour<-merge(citiQuarter1_rush_hour,citiQuarter1_rush_hour_table,by=c("bikeid","startweekday"))
colnames(citiQuarter1_rush_hour)[38]<-"TripsPerBikeID"

ind <- sample(2, nrow(citiQuarter1_rush_hour), replace=TRUE, prob=c(0.7, 0.3))
citi_train <- citiQuarter1_rush_hour[ind==1,]
citi_test<-citiQuarter1_rush_hour[ind==2,]

#Modeling
#Simple linear model - R squared 0.775
lm1<-lm(citiQuarter1_rush_hour$TripsPerHour~
          citiQuarter1_rush_hour$gender+
          citiQuarter1_rush_hour$Max+
          citiQuarter1_rush_hour$dist+
          citiQuarter1_rush_hour$startweekday+
          citiQuarter1_rush_hour$Min+
          citiQuarter1_rush_hour$starthour+
          citiQuarter1_rush_hour$month+
          citiQuarter1_rush_hour$week+
          citiQuarter1_rush_hour$TripsPerBikeID
          )
summary(lm1)

lm1_1<-lm(TripsPerHour~
          gender+
          Max+
          dist+
          startweekday+
          Min+
          starthour+
          month+
          week+
          TripsPerBikeID, data=citi_train
)
summary(lm1_1)
head(citi_test)
citi_test$TripsPerHour<-NULL
#predicted<-predict(lm1_1,citi_test)
#sqrt(mean((predicted - y_citi_test)^2))


#PCR

library(pls)
fit<-pcr(citiQuarter1_rush_hour$TripsPerHour~
           citiQuarter1_rush_hour$gender+
           citiQuarter1_rush_hour$Max+
           citiQuarter1_rush_hour$dist+
           citiQuarter1_rush_hour$startweekday+
           citiQuarter1_rush_hour$Min+
           citiQuarter1_rush_hour$starthour+
           citiQuarter1_rush_hour$month+
           citiQuarter1_rush_hour$week+
           citiQuarter1_rush_hour$TripsPerBikeID,
           validation="CV")
summary(fit)
#RMSE
validationplot(fit)
validationplot(pcr_model)
#Cross validation MSE
validationplot(fit,val.type = "MSEP")
#R-squared
validationplot(fit, val.type = "R2")
#Predicted vs Measured values
predplot(fit)
trips_q1<-citiQuarter1_rush_hour[which(citiQuarter1_rush_hour$TripsPerHour<2000),]
nrow(trips_q1)
#Nearly 1/3rd of the data has TripsPerHour <2000, lesser training data more error
#Regression coefficients
coefplot(fit)

#Reordering columns
ncol(citi_test)
citiQuarter1_rush_hour_final<- citiQuarter1_rush_hour[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36,37,38,35)]
head(citiQuarter1_rush_hour_final)
ind <- sample(2, nrow(citiQuarter1_rush_hour_final), replace=TRUE, prob=c(0.8, 0.2))
citi_train <- citiQuarter1_rush_hour_final[ind==1,]
citi_test<-citiQuarter1_rush_hour_final[ind==2,1:37]
y_citi_test<-citiQuarter1_rush_hour_final[ind==2,38]
citi_test_2<-citiQuarter1_rush_hour_final[ind==2,]

#Linear model with interactions

lm2<-lm(citiQuarter1_rush_hour$TripsPerHour~
          citiQuarter1_rush_hour$gender+
          citiQuarter1_rush_hour$Max+
          citiQuarter1_rush_hour$dist+
          citiQuarter1_rush_hour$tripduration+
          citiQuarter1_rush_hour$startweekday+
          citiQuarter1_rush_hour$Min+
          citiQuarter1_rush_hour$starthour:citiQuarter1_rush_hour$month+
          citiQuarter1_rush_hour$week+
          citiQuarter1_rush_hour$TripsPerBikeID
)
summary(lm2)
cor(citiQuarter1_rush_hour$TripsPerHour,lm2$fitted.values)
plot(x=lm2$fitted.values,y=citiQuarter1_rush_hour$TripsPerHour,xlab="Fitted",ylab="Predicted")
abline(lm(citiQuarter1_rush_hour$TripsPerHour~lm2$fitted.values),col="red")
log_TripsPerHour<-log(citiQuarter1_rush_hour$TripsPerHour)
plot(log_TripsPerHour)
qqnorm(lm2$residuals)
qqplot(lm2$fitted.values,lm2$residuals)


#Extension Trips originating from start station

citiQuarter1_rush_hour<-data.table(citiQuarter1_rush_hour)
citiQuarter1_rush_hour_table<-citiQuarter1_rush_hour[,sum(count),by=list(start.station.name,starthour,end.station.name)]
citiQuarter1_rush_hour_table<-data.frame(citiQuarter1_rush_hour_table)
citiQuarter1_rush_hour<-merge(citiQuarter1_rush_hour,citiQuarter1_rush_hour_table,by=c("start.station.name","starthour","end.station.name"))
ncol(citiQuarter1_rush_hour)
colnames(citiQuarter1_rush_hour)[40]<-"TripsOriginatingfromStationsPair"
str(citiQuarter1_rush_hour)
lm1_1<-lm(TripsOriginatingfromStationsPair~
            tripduration+
            Age+
            gender+
            Max+
            dist+
            startweekday+
            starthour+
            month+
            week+
            TripsPerHour+
            TripsPerBikeID+
            TripsOriginatingfromStations,
            data=citiQuarter1_rush_hour
)
summary(lm1_1)
qqnorm(lm1_1$residuals)
qqplot(lm1_1$fitted.values,lm1_1$residuals)

#RandomForest 
rforest<-randomForest(citiQuarter1_rush_hour$TripsPerHour~
                        citiQuarter1_rush_hour$gender+
                        citiQuarter1_rush_hour$Max+
                        citiQuarter1_rush_hour$dist+
                        citiQuarter1_rush_hour$tripduration+
                        citiQuarter1_rush_hour$startweekday+
                        citiQuarter1_rush_hour$Min+
                        citiQuarter1_rush_hour$starthour:citiQuarter1_rush_hour$month+
                        citiQuarter1_rush_hour$week+
                        citiQuarter1_rush_hour$TripsPerBikeID)
