
# Distribution plot for age  when people become vulnerable:
png(file="/Users/raghulsekar/Desktop/ Disseritation /Disseritation Project/Images/Distribution plot for age  when people become vulnerable1.png",width=600, height=350)
x <- cox_fd$age_v
mycol <- t_col("Red", perc = 50, name = "lt.red")
h<-hist(x, breaks=60, col=mycol, xlab="Age",
        main="Distribution for age when people become vulnerable")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
abline(v=mean(x),col="#66FF33",lwd=2)
legend(x = "topright", # location of legend within plot area
       c( "Mean"),
       col = c("#66FF33"),
       lwd = c(2))
dev.off()

# Kernel Density Plot for age when people become vulnerable

d <- density(cox_fd$age_v)
plot(d, main="Kernel Density for age when people become vulnerable",xlab="Age")
polygon(d, col=mycol, border="blue")


#preparing data for age and wealth when people become frail
cox_fd_wona <- cox_fd[!is.na(cox_fd$age_f),]
cox_fd_wona

###############Distribution plot for age when people become frail#############
x <- cox_fd_wona$age_f
mycol1 <- t_col("yellow", perc = 50, name = "lt.red")
h<-hist(x, breaks=60, col=mycol1, xlab="Age",
        main="Distribution for age when people become frail")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
abline(v=mean(x),col="#66FF33",lwd=2)
legend(x = "topright", # location of legend within plot area
       c( "Mean"),
       col = c("#66FF33"),
       lwd = c(2))

################## Kernel Density Plot for age when people become frail#############
d <- density(cox_fd_wona$age_f)
plot(d, main="Kernel Density for age when people become frail",xlab="Age")
polygon(d, col=mycol1, border="blue")

################ Distribution plot for wealth when people become vulnerable:###############
x <- cox_fd$wealth_v
h<-hist(x, breaks=100, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
        main="Distribution for wealth when people become vulnerable")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
abline(v=mean(x),col="#66FF33",lwd=2)
legend(x = "topright", # location of legend within plot area
       c( "Mean"),
       col = c("#66FF33"),
       lwd = c(2))


################# Kernel Density Plot for wealth when people become vulnerable:###############
d <- density(cox_fd$wealth_v)
plot(d, main="Kernel Density for wealth when people become vulnerable",xlab="Total wealth", xlim = c(-200000, 1500000))
polygon(d, col=mycol, border="blue")

################### Distribution plot for wealth when people become frail:###############
x <- cox_fd_wona$wealth_f
h<-hist(x, breaks=100, col=mycol1, xlab="Total wealth", xlim = c(-200000, 1500000),
        main="Distribution for wealth when people become frail")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
abline(v=mean(x),col="#66FF33",lwd=2)
legend(x = "topright", # location of legend within plot area
       c( "Mean"),
       col = c("#66FF33"),
       lwd = c(2))

############## Kernel Density Plot for wealth when people become frail:##############
d <- density(cox_fd_wona$wealth_f)
plot(d, main="Kernel Density for wealth when people become frail",xlab="Total wealth", xlim = c(-200000, 1500000))
polygon(d, col=mycol1, border="blue")




#-------- ------ ------- -------------------- -------

#preparing data for plotting both sex
library(tidyverse)

cox_fd_m <- cox_fd %>% filter(ragender == 1)
cox_fd_f <- cox_fd %>% filter(ragender == 2)

########## Distribution plot of age  when people become vulnerable for both sex:############
x <- cox_fd_m$age_v
y <- cox_fd_f$age_v

h<-hist(x, breaks=60, col="red", xlab="Age",
        main="Distribution for age when people become vulnerable for male")
h1<-hist(y, breaks=60, col="red", xlab="Age",
         main="Distribution for age when people become vulnerable for female")

plot(h, col=rgb(0,0,1,1/4),ylim=c(0,200),main = "Distribution of age when people become vulnerable for both sex", xlab="Age")
plot(h1, col=rgb(1,0,0,1/4) ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))
#plot save size 750,550

########### Distribution plot of wealth when people become vulnerable for both sex:##############
x <- cox_fd_m$wealth_v
y <- cox_fd_f$wealth_v

h<-hist(x, breaks=160, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
        main="Distribution of wealth when people become vulnerable for male")
h1<-hist(y, breaks=160, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
         main="Distribution of wealth when people become vulnerable for female")

plot(h, col=rgb(0,0,1,1/4),ylim=c(0,950), xlim = c(-200000, 1500000), main = "Distribution of wealth when people become vulnerable for both sex", xlab="Total wealth")
plot(h1, col=rgb(1,0,0,1/4) ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)))
#plot save size 750,550

#preparing data for frial
library(tidyverse)

cox_fd_wona_m <- cox_fd_wona %>% filter(ragender == 1)
cox_fd_wona_f <- cox_fd_wona %>% filter(ragender == 2)

################## Distribution plot of age  when people become frail for both sex:###############
x <- cox_fd_wona_m$age_f
y <- cox_fd_wona_f$age_f

h<-hist(x, breaks=60, col="red", xlab="Age",
        main="Distribution of age when people become frail for male")
h1<-hist(y, breaks=60, col="red", xlab="Age",
         main="Distribution of age when people become frail for female")
mycol1 <- t_col("Yellow", perc = 50, name = "lt.red")
mycol2 <- t_col("Pink", perc = 50, name = "lt.red")
plot(h, col=mycol1,ylim=c(0,80),main = "Distribution of age when people become frail for both sex", xlab="Age")
plot(h1, col=mycol2 ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(mycol1,mycol2))
#plot save size 750,550


################# Distribution plot of wealth when people become frail for both sex:#############
x <- cox_fd_wona_m$wealth_f
y <- cox_fd_wona_f$wealth_f

h<-hist(x, breaks=100, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
        main="Distribution of wealth when people become frail for male")
h1<-hist(y, breaks=100, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
         main="Distribution of wealth when people become frail for female")
mycol1 <- t_col("Yellow", perc = 50, name = "lt.red")
mycol2 <- t_col("Pink", perc = 50, name = "lt.red")
plot(h, col=mycol1 ,ylim=c(0,400), xlim = c(-200000, 1500000), main = "Distribution of wealth when people become frail for both sex", xlab="Total wealth")
plot(h1, col=mycol2 ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(mycol1,mycol2))
#plot save size 750,550

#--------- ------ ------- ------

###########3# Distribution plot of wealth when people become frail for both sex:#############
x <- cox_fd_wona_m$wealth_f
y <- cox_fd_wona_f$wealth_f

h <- density(cox_fd_wona_m$wealth_f)
h1 <- density(cox_fd_wona_f$wealth_f)

plot(h, main="Kernel Density for age when people become frail",xlab="Age")
plot(h1, main="Kernel Density for age when people become frail",xlab="Age")

polygon(d, col=mycol1, border="blue")


h<-hist(x, breaks=100, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
        main="Distribution of wealth when people become frail for male")
h1<-hist(y, breaks=100, col=mycol, xlab="Total wealth", xlim = c(-200000, 1500000),
         main="Distribution of wealth when people become frail for female")
mycol1 <- t_col("Yellow", perc = 50, name = "lt.red")
mycol2 <- t_col("Pink", perc = 50, name = "lt.red")
plot(h, col=mycol1 ,ylim=c(0,400), xlim = c(-200000, 1500000), main = "Distribution of wealth when people become frail for both sex", xlab="Total wealth")
plot(h1, col=mycol2 ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(mycol1,mycol2))
#plot save size 750,550

#---- ------- ----
cox_fd_c <- cox_fd %>% filter(status == 1)
cox_fd_uc <- cox_fd %>% filter(status == 2)

############## Distribution plot of age  when people become frail for both sex:#################
x <- cox_fd_c$time_days
y <- cox_fd_uc$time_days

h<-hist(x, col="red", xlab="Age",
        main="Distribution of age when people become frail for male")
h1<-hist(y, col="red", xlab="Age",
         main="Distribution of age when people become frail for female")
mycol1 <- t_col("Blue", perc = 50, name = "lt.red")
mycol2 <- t_col("Red", perc = 50, name = "lt.red")
plot(h, col=mycol1,main = "Distribution of days when people become frail for both sex", xlab="Days", ylab="Count")
plot(h1, col=mycol2 ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Male", "Female"),
       fill = c(mycol1,mycol2))
#plot save size 750,550



#distribution comparing between frailty at wave1 and wave 7:

gen_min <- data[,c("idauniq","ragender","rabyear","r1agey","r2agey","r3agey","r4agey",
                   "r5agey","r6agey","r7agey","fraill1","fraill2","fraill3","fraill4",
                   "fraill5","fraill6","fraill7","h1atotb","h2atotb","h3atotb","h4atotb",
                   "h5atotb","h6atotb","h7atotb")]
gen_min1 <- gen_min[!is.na(gen_min$fraill1),]
gen_min2 <- gen_min[!is.na(gen_min$fraill7),]
###
x <- gen_min1$fraill1
y <- gen_min2$fraill7

h<-hist(x,breaks=30, col="red", xlab="Age",
        main="Distribution of Frailty Index in wave 1")
h1<-hist(y,breaks=30,  col="red", xlab="Age",
         main="Distribution of Frailty Index in wave 7")
mycol1 <- t_col("Blue", perc = 50, name = "lt.red")
mycol2 <- t_col("Red", perc = 50, name = "lt.red")
plot(h, col=mycol1,main = "Distribution of Frailty Index in wave 1 and wave 7", xlab="Frailty Index", ylab="People")
plot(h1, col=mycol2 ,add = T)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h1$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)

legend(x = "topright", 
       legend = c("Wave 1", "Wave 7"),
       fill = c(mycol1,mycol2))
#plot save size 750,550



