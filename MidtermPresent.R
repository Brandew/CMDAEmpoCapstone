visits <- read.csv(file = "~/Downloads/emporium_visits.csv",header = T, sep = ",")
library(lubridate)


day2 <- strptime(visits$checkin_at, format= "%d/%m/%Y")
day1 <- format(day2, format="%Y-%m-%d")
day <- weekdays(as.Date(day))
day <- data.frame(day)
day2 <- data.frame(day2)
monl <- length(which(day=="Monday"))
tuel <- length(which(day=="Tuesday"))
wedl <- length(which(day=="Wednesday"))
thurl <- length(which(day=="Thursday"))
fril <- length(which(day=="Friday"))
satl <- length(which(day=="Saturday"))
sunl <- length(which(day=="Sunday"))
Day = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
Count = c(monl,tuel,wedl,thurl,fril,satl,sunl)
tmp <- data.frame(Count, Day)
tmp$Day <- factor(tmp$Day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(data=tmp, aes(x=Day,y=Count)) +
  geom_bar(stat="identity")
year <- strtrim(day2, 4)
y2014 <- length(which(year=="2014"))
y2015 <- length(which(year=="2015"))
y2016 <- length(which(year=="2016"))
y2017 <- length(which(year=="2017"))
y2018 <- length(which(year=="2018"))
yearcount <- c(2*y2014,y2015,y2016,y2017,y2018)
years <- c("2014","2015","2016","2017","2018")
tmp2 <- data.frame(yearcount,years)
tmp2$years<-factor(tmp2$years, levels = c("2014","2015","2016","2017","2018"))

yearplot <- ggplot(data=tmp2, aes(x=years,y=yearcount)) +
  geom_bar(stat="identity",fill="steelblue")+
  scale_y_continuous(name="Visitations", labels = scales::comma) +
  scale_x_discrete(name="Year", labels = scales::comma)

yearplot + ggtitle("Math Emporium Visitations by Year")


## by semester

require(data.table)

day3 <- strptime(visits$checkin_at, format= "%d/%m/%Y")
day3 <- format(day3, format="%Y-%m-%d")

date_num <- data.frame(day3)
date_num$day3 <- as.Date(date_num$day3)

f14s <- as.Date("2014-8-25")
f14e <- as.Date("2014-12-18")

s15s <- as.Date("2015-1-20")
s15e <- as.Date("2015-5-13")

f15s <- as.Date("2015-8-24")
f15e <- as.Date("2015-12-17")

s16s <- as.Date("2016-1-19")
s16e <- as.Date("2016-5-11")

f16s <- as.Date("2016-8-22")
f16e <- as.Date("2016-12-15")

s17s <- as.Date("2017-1-17")
s17e <- as.Date("2017-5-10")

f17s <- as.Date("2017-8-28")
f17e <- as.Date("2017-12-20")

s18s <- as.Date("2018-1-16")
s18e <- as.Date("2018-5-9")

fall2014 <- subset(date_num, day3 >= f14s & day3 <= f14e)
fall2014$day3 <- "Fall 2014"
fall2014$day3 <- as.factor(fall2014$day3)

spring2015 <- subset(date_num, day3 >= s15s & day3 <= s15e)
spring2015$day3 <- "Spring 2015"
spring2015$day3 <- as.factor(spring2015$day3)

fall2015 <- subset(date_num, day3 >= f15s & day3 <= f15e)
fall2015$day3 <- "Fall 2015"
fall2015$day3 <- as.factor(fall2015$day3)

spring2016 <- subset(date_num, day3 >= s16s & day3 <= s16e)
spring2016$day3 <- "Spring 2016"
spring2016$day3 <-as.factor(spring2016$day3)

fall2016 <- subset(date_num, day3 >= f16s & day3 <= f16e)
fall2016$day3 <- "Fall 2016"
fall2016$day3 <- as.factor(fall2016$day3)

spring2017 <- subset(date_num, day3 >= s17s & day3 <= s17e)
spring2017$day3 <- "Spring 2017"
spring2017$day3 <- as.factor(spring2017$day3)

fall2017 <- subset(date_num, day3 >= f17s & day3 <= f17e)
fall2017$day3 <- "Fall 2017"
fall2017$day3 <- as.factor(fall2017$day3)


spring2018 <- subset(date_num, day3 >= s18s & day3 <= s18e)
spring2018$day3 <- "Spring 2018"
spring2018$day3 <- as.factor(spring2018$day3)

semester <- rbind(fall2014, spring2015, fall2015, spring2016, fall2016, spring2017, fall2017, spring2018)

ggplot(semester, aes(x=semester$day3)) +
  labs(x="Semesters", y="Count", title = "Student Foot Traffic by Semester") + 
  geom_bar(fill = "steelblue")
