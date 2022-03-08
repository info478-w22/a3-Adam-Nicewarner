

library(data.table)
library(mgcv)
library(dlnm)
library(pracma)

###############################################################################################################
### Influenza cases in Taiwan 
###############################################################################################################

dir<-"H:/repos/iSchool/disease_modelling/"
data<-fread(paste0(dir,"severe_complicated_influ.csv"))
colnames(data)<-c("Onset","Excluded","Pending","Confirmed")
data[,date:=strptime(paste0(Onset,"1"),"%Y%U%u")]
data[,trend:=as.numeric(seq(1:nrow(data)))]
data[,month:=as.numeric(format(date,"%m"))]
data[,year:=as.numeric(format(date,"%Y"))]


### Explore time series
plot(data$date,data$Confirmed, xlab="",ylab="Cases",type="l",ylim=c(0,300))
par(new=TRUE)
plot(data$date,data$Excluded, xlab="",ylab="",type="l",ylim=c(0,300),col="gray",lty=2)

### Moving averages
plot(data$Confirmed, type = "l", col = 1, ylim = c(1, 300),
     xlab = "", ylab = "Cases")
y <- movavg(data$Confirmed, 25, "w"); lines(y, col = 2)


### Fit a loess smoother 
lo.con <- loess(data$Confirmed ~ data$trend, span=0.1, degree=2)
p.con<-predict(lo.con, data.frame(trend = seq(1, nrow(data), 1)), se = TRUE)

plot(data$date,data$Confirmed, xlab="",ylab="Cases",type="p",ylim=c(0,300),cex=0.5)
par(new=TRUE)
plot(data$date,p.con$fit, xlab="",ylab="",type="l",ylim=c(0,300),col="red",lty=1,axes=F)

### fit a time series model using a Generalized Additive Model (GAM)
g<-gam(Confirmed~s(trend,k=37,fx=TRUE), data=data)
plot(g)
summary(g)

df<-data.frame(cbind(trend = data$year,month = data$month))
g.con<-as.data.frame(predict(g, data.frame(trend = seq(1, nrow(data), 1)), se = TRUE))

plot(data$date,data$Confirmed, xlab="",ylab="Cases",type="p",ylim=c(0,300),cex=0.5)
par(new=TRUE)
plot(data$date,g.con$fit, xlab="",ylab="",type="l",ylim=c(0,300),col="red",lty=1,axes=F)

plot(residuals(g))
pacf(residuals(g))

### Test for correlation
cor.test(data$Confirmed,data$Excluded)

### Barplot
barplot(data$Confirmed, axes=FALSE,xlab = "",ylab = "",col="blue")
axis(2)
mtext("Cases",2,line=2.5)

###############################################################################################################
#### Unemployment trend 
###############################################################################################################

unemployment <- as.data.table(read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv", sep=","))
str(unemployment)

unemployment[,month:=substr(Period,2,3)]
unemployment[,date:=strptime(paste0("15-",month,"-",Year),"%d-%m-%Y")]
unemployment[,trend:=seq(1,nrow(unemployment),1)]

plot(unemployment$date,unemployment$Value,type="l")


###### Linear model 
LM<-lm(Value~trend,data=unemployment)
TREND<-predict(LM,data.frame(trend=seq(1,nrow(unemployment),1)),se=TRUE)
summary(LM)

###### Generalized additive model (GAM)
GAM<-gam(Value~trend+as.factor(as.numeric(month)),data=unemployment)
summary(GAM)

##### LOESS model
LO.unem<-lo.con <- loess(unemployment$Value ~ unemployment$trend, span=0.25, degree=2)
P.CON<-predict(LO.unem, data.frame(trend = seq(1, nrow(unemployment), 1)), se = TRUE)

plot(unemployment$date,unemployment$Value, xlab="",ylab="Unemployment rate",type="p",cex=0.5,ylim=c(0,14))
par(new=TRUE)
plot(unemployment$date,P.CON$fit, xlab="",ylab="",type="l",cex=0.5,ylim=c(0,14),col="red")
par(new=TRUE)
plot(unemployment$date,TREND$fit,xlab="",ylab="",type="l",ylim=c(0,14),cex=0.5,lty=2)


##### GAM model
GAM.unem<-gam(Value ~ s(trend), data=unemployment)
GAM.CON<-predict(GAM.unem, data.frame(trend = seq(1, nrow(unemployment), 1)), se = TRUE)

plot(unemployment$date,unemployment$Value, xlab="",ylab="Unemployment rate",type="p",cex=0.5,ylim=c(0,14))
par(new=TRUE)
plot(unemployment$date,GAM.CON$fit, xlab="",ylab="",type="l",cex=0.5,ylim=c(0,14),col="red")
par(new=TRUE)
plot(unemployment$date,TREND$fit,xlab="",ylab="",type="l",ylim=c(0,14),cex=0.5,lty=2)
