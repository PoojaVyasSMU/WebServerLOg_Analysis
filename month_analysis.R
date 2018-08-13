#month data analysis for web server logs
setwd("~/Spring'18/Project")
getwd()
load("month.rda")

#initial data cleaning
df = read.table("access_log_Jul95", fill= TRUE)
head(df)
summary(df)
sapply(df, class)
colnames(df)=c('host','http','httpreply','timestamp','bytes','request','reply','bytesgiven')
keeps <-c('host','timestamp','request','reply','bytesgiven')
df_clean <-df[ , keeps, drop = FALSE]
summary(df_clean)
sapply(df_clean, class)

#remove '[' from date
levels(df_clean$timestamp) <- gsub("\\[","",levels(df_clean$timestamp))
summary(df_clean)

#convert factors to data types
x <- df_clean$timestamp
#add date
df_clean$date <- substr(x,0,11)
head(df_clean)
#add time
df_clean$time <-substr(x,13,20)
head(df_clean)

#convert date
y<-df_clean$date
head(y)
df_clean$date <- format(strptime(y,format="%d/%b/%Y"),"%d/%m/%Y")
head(df_clean$date)
tail(df_clean$date)
df_clean$date <- as.Date(df_clean$date, "%d/%m/%Y")
sapply(df_clean, class)

#1 month of data
df_month <- subset(df_clean, df_clean$date >= as.Date("1995-07-01")& df_clean$date <= as.Date("1995-08-31"))
summary(df_month)
nrow(df_2months)
head(df_2months)
tail(df_2months)
View(df_2months)

#daily hits over a month
agg_userm <-aggregate(data.frame(count = df_month$host), list(value = df_month$date), length)
head(agg_userm)
View(agg_userm)
library(sqldf)
#ditibct hosts for a month
summary(df_month)
df_month$date <- as.character(df_month$date)
sqldf("select count (distinct(host)) from df_month group by date");

#monthly error count
df_month$date <- as.character(df_month$date)
df_month$reply <-as.character(df_month$reply)
sqldf("select count (reply) from df_month WHERE reply in ('404','500','403','501') group by date");

#daily hits over time
sqldf("select count (host) from df_2weeks WHERE date ='1995-07-01'");
#implement G-O model
SRGMs100.csv <-read.csv("GO_input.csv", header=TRUE)
x<-SRGMs100.csv$Day
y <- SRGMs100.csv$Cumulative.errors
dat<-data.frame(x,y)
head(dat)
summary(dat)

head(dat)
sapply(dat,class)
nrow(dat)

plot(y ~ x,pch="*",col="blue",ylab= "Cumulative Errors", xlab="CalenderTime")
FGO <- function(x,n,b) {n * (1-exp(-b * x))}
funcGO<-nls(y~FGO(x,n,b),data= dat, start= c(n=165,b=1.1e-2),trace=TRUE)
coGO<-coef(funcGO)
coGO

fMO<-function(a,b,x){a*log2((b*x)+1)}
fS<-function(x,n,b) {n * (1-((1 + (b *x)) * exp(-b * x)))}

funMO<-nls(y~fMO(a,b,x), data=dat, start=c(a=238,b=2.7e-9),trace=TRUE)

funS<-nls(y~ fS(x,n,b),data= dat, start= c(n=100,  b=5e-2),trace=TRUE)

coGO<-coef(funcGO)
coS<-coef(funS)
coMO<-coef(funMO) 

coGO
coS
coMO
plot(y ~ x,pch="*",col="blue", ylab= "Cumulative Errors", xlab="Calendar time")

curve(fS(x,n=coS[1],b=coS[2]),lty=4,lwd=2, col = 51, add = TRUE)
curve(FGO(x, n=coGO[1],b=coGO[2]), pch=1,lty=3,lwd=4, col = "red", add = TRUE)

curve(fMO(a=coMO[1],b=coMO[2],x),type="p",lwd=3,lty=3, col = "black", pch="+",add = TRUE)

legend("bottomright", c(" Acctual","  GO", "  S-Shaped"),
       pch=c("*","-","."),lty=c(3,3,3,6),
       lwd=c(1,4,1,1),bty="n",col=c("blue","red","green"))



