# An example is a data set of the number of births per month in New York city, from January 1946 to December 1959 
dat <- read.table("http://robjhyndman.com/tsdldata/data/nybirths.dat", 
           header=F)


mnth <- seq(as.Date("1946/1/1"), as.Date("1959/12/1"), by = "month")


head(dat)
dat$mnth <- mnth 

plot.ts(dat$V1)
str(dat)

dat1 <- subset(dat, mnth > as.Date("1956/1/1")) 

plot.ts(dat1$V1)

m1 <- lm(V1 ~ mnth, data = dat1)
summary(m1)
