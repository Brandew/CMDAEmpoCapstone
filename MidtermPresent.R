visits <- read.csv(file = "~/Downloads/emporium_visits.csv",header = T, sep = ",")
library(lubridate)


day2 <- strptime(visits$checkin_at, format= "%d/%m/%Y")
day <- format(day2, format="%Y-%m-%d")
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
