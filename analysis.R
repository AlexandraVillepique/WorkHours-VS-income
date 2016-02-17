data=read.csv("RMW_16022016200632658.csv")
#looking at the data:
head(data)
summary(data)
tail(data)
str(data)

#getting data from 2010- present
test <- subset(data, data$TIME >= 2010)
myVar<-c("COUNTRY", "Country", "TIME", "Time", "Value")
minSalary <- test[myVar]

plot(minSalary$COUNTRY,minSalary$Value)


data=read.csv("ANHRS_16022016200952408.csv")
#looking at the data:
head(data)
summary(data)
tail(data)
str(data)

#getting data from 2010- present
test <- subset(data, data$TIME >= 2010)
myVar<-c("COUNTRY", "Country", "TIME", "Time", "Value")
workHours <- test[myVar]

plot(workHours$COUNTRY, workHours$Value)

MS2010<-subset(minSalary, minSalary$TIME==2010)
plot(MS2010$COUNTRY, MS2010$Value, xlab="Countries", ylab="Min. Income")
summary(MS2010)
head(MS2010)

WH2010<-subset(workHours, workHours$TIME==2010)
plot(WH2010$COUNTRY, WH2010$Value, xlab="Countries", ylab="Work hours")
summary(WH2010)
head(WH2010)

test<-unique(MS2010$COUNTRY)
test2<-subset(WH2010, WH2010$COUNTRY %in% test)
test3 <- merge(test2, MS2010, by.x="COUNTRY", by.y="COUNTRY")
d2010<- subset(test3, test3$Value.y >=20)

plot(d2010$Value.x, d2010$Value.y, xlab="Working Hours", ylab="Min. Wage", title="2010")
abline(lm(d2010$Value.y~d2010$Value.x))

d2010.lm<-lm(d2010$Value.y~d2010$Value.x)
summary(d2010.lm)$r.squared
summary(d2010.lm)
cor(d2010$Value.y,d2010$Value.x)

#2011
MS2011<-subset(minSalary, minSalary$TIME==2011)
plot(MS2011$COUNTRY, MS2011$Value)
summary(MS2011)
head(MS2011)

WH2011<-subset(workHours, workHours$TIME==2011)
plot(WH2011$COUNTRY, WH2011$Value)
summary(WH2011)
head(WH2011)

test<-unique(MS2011$COUNTRY)
test2<-subset(WH2011, WH2011$COUNTRY %in% test)
test3 <- merge(test2, MS2011, by.x="COUNTRY", by.y="COUNTRY")
d2011<- subset(test3, test3$Value.y >=20)

plot(d2011$Value.x, d2011$Value.y, xlab="Working Hours", ylab="Min. Wage")
abline(lm(d2011$Value.y~d2011$Value.x))

d2011.lm<-lm(d2011$Value.y~d2011$Value.x)
summary(d2011.lm)$r.squared
summary(d2011.lm)
cor(d2011$Value.y,d2011$Value.x)

#2012

MS2012<-subset(minSalary, minSalary$TIME==2012)
plot(MS2012$COUNTRY, MS2012$Value)
summary(MS2012)

WH2012<-subset(workHours, workHours$TIME==2012)
plot(WH2012$COUNTRY, WH2012$Value)
summary(WH2012)

test<-unique(MS2012$COUNTRY)
test2<-subset(WH2012, WH2012$COUNTRY %in% test)
test3 <- merge(test2, MS2012, by.x="COUNTRY", by.y="COUNTRY")
d2012<- subset(test3, test3$Value.y >=20)

plot(d2012$Value.x, d2012$Value.y, xlab="Working Hours", ylab="Min. Wage")
abline(lm(d2012$Value.y~d2012$Value.x))

d2012.lm<-lm(d2012$Value.y~d2012$Value.x)
summary(d2012.lm)$r.squared
summary(d2012.lm)
cor(d2012$Value.y,d2012$Value.x)

#2013

MS2013<-subset(minSalary, minSalary$TIME==2013)
plot(MS2013$COUNTRY, MS2013$Value)
summary(MS2013)

WH2013<-subset(workHours, workHours$TIME==2013)
plot(WH2013$COUNTRY, WH2013$Value)
summary(WH2013)

test<-unique(MS2013$COUNTRY)
test2<-subset(WH2013, WH2013$COUNTRY %in% test)
test3 <- merge(test2, MS2013, by.x="COUNTRY", by.y="COUNTRY")
d2013<- subset(test3, test3$Value.y >=20)

plot(d2013$Value.x, d2013$Value.y, xlab="Working Hours", ylab="Min. Wage")
abline(lm(d2013$Value.y~d2013$Value.x))

d2013.lm<-lm(d2013$Value.y~d2013$Value.x)
summary(d2013.lm)$r.squared
summary(d2013.lm)
cor(d2013$Value.y,d2013$Value.x)

#2014

MS2014<-subset(minSalary, minSalary$TIME==2014)
plot(MS2014$COUNTRY, MS2014$Value)
summary(MS2014)

WH2014<-subset(workHours, workHours$TIME==2014)
plot(WH2014$COUNTRY, WH2014$Value)
summary(WH2014)

test<-unique(MS2014$COUNTRY)
test2<-subset(WH2014, WH2014$COUNTRY %in% test)
test3 <- merge(test2, MS2014, by.x="COUNTRY", by.y="COUNTRY")
d2014<- subset(test3, test3$Value.y >=20)

plot(d2014$Value.x, d2014$Value.y, xlab="Working Hours", ylab="Min. Wage")
abline(lm(d2014$Value.y~d2014$Value.x))

d2014.lm<-lm(d2014$Value.y~d2014$Value.x)
summary(d2014.lm)$r.squared
summary(d2014.lm)
cor(d2014$Value.y,d2014$Value.x)



