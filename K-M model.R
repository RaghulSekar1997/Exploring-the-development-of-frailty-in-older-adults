#To Start:
library("survival")
library("survminer")

summary(cox_fd)
str(cox_fd)

###########################K-M model: without x################################
km.model <- surv_fit(Surv(time,status) ~ 1, data = cox_fd)
#ask for summaries of the model
km.model
summary(km.model)
plot(km.model, conf.int = T, mark.time = T)
plot(km.model, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model without x varibles",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)

#plot size 750, 650

###########################K-M model: with ragender################################
km.model1 <- surv_fit(Surv(time,status) ~ ragender, data = cox_fd)
#ask for summaries of the model
km.model1
summary(km.model1)
plot(km.model1, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Gender",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)
legend(x = "topright", legend = c("Male","Female"), 
       lty = 1, lwd = 2, col = c("red","blue"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ ragender,data = cox_fd)
# It's fails to reject null hypothesis

###########################K-M model with age_vc variable:###################
km.model2 <- surv_fit(Surv(time,status) ~ age_vc, data = cox_fd)
#ask for summaries of the model
km.model2
summary(km.model2)
plot(km.model2, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Age",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)
legend(x = "topright", legend = c("age below 65","age between 65 - 75","age above 75"), 
       lty = 1, lwd = 2, col = c("red","blue","green"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on age_vc


#########################K-M model with wealth_vc variable:#####################
km.model3 <- surv_fit(Surv(time,status) ~ wealth_vc, data = cox_fd)
#ask for summaries of the model
km.model3
summary(km.model3)
plot(km.model3, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Wealth",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)
legend(x = "topright", legend = c("lower class","middle class","upper class"), 
       lty = 1, lwd = 2, col = c("red","blue","green"),bty = "",cex = 0.6)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on wealth_vc


#########################K-M model with age_vc + wealth_vc variable:#####################
km.model4 <- surv_fit(Surv(time,status) ~ age_vc + wealth_vc , data = cox_fd)
#ask for summaries of the model
km.model4
summary(km.model4)
plot(km.model4, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Age and Wealth",
      col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"), las = 1, lwd = 2,mark.time = T )
legend(x = "bottomleft", legend = c("A-1,W-1","A-1,W-2","A-1,W-3","A-2,W-1","A-2,W-2","A-2,W-3","A-3,W-1","A-3,W-2","A-3,W-3"), 
       lty = 1, lwd = 2, col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc + wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc


#########################K-M model with ragender + age_vc + wealth_vc variable:#####################
km.model5 <- surv_fit(Surv(time,status) ~ ragender + age_vc + wealth_vc , data = cox_fd)
#ask for summaries of the model
km.model5
summary(km.model5)
plot(km.model5, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Gender, Age, Wealth",
     col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"), las = 1, lwd = 2,mark.time = T )
legend(x = "bottomleft", legend = c("A-1,W-1","A-1,W-2","A-1,W-3","A-2,W-1","A-2,W-2","A-2,W-3","A-3,W-1","A-3,W-2","A-3,W-3"), 
       lty = 1, lwd = 2, col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ ragender + age_vc + wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc

#########  Dividing data-set into two male and female  because volutes the assumption#######
male_dataset <- subset(cox_fd,ragender %in% c(1))
female_dataset <- subset(cox_fd,ragender %in% c(2))

###########################K-M model: without x for both male and female  ################################
km.model6.m <- surv_fit(Surv(time,status) ~ 1, data = male_dataset)
#ask for summaries of the model
km.model6.m
summary(km.model6.m)
plot(km.model6.m, conf.int = T, mark.time = T)
plot(km.model6.m, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model without x varibles for Male",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)

km.model6.f <- surv_fit(Surv(time,status) ~ 1, data = female_dataset)
#ask for summaries of the model
km.model6.f
summary(km.model6.f)
plot(km.model6.f, conf.int = T, mark.time = T)
plot(km.model6.f, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model without x varibles for Female",
     col = c("red","blue","green"), las = 1, lwd = 2 ,mark.time = T)

#########################K-M model with age_vc + wealth_vc variable for both male and female:#####################
km.model7.m <- surv_fit(Surv(time,status) ~ age_vc + wealth_vc , data = male_dataset)
#ask for summaries of the model
km.model7.m
summary(km.model7.m)
plot(km.model7.m, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Age and Wealth for Male",
     col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"), las = 1, lwd = 2,mark.time = T )
legend(x = "bottomleft", legend = c("A-1,W-1","A-1,W-2","A-1,W-3","A-2,W-1","A-2,W-2","A-2,W-3","A-3,W-1","A-3,W-2","A-3,W-3"), 
       lty = 1, lwd = 2, col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc + wealth_vc,data = male_dataset)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc

km.model7.f <- surv_fit(Surv(time,status) ~ age_vc + wealth_vc , data = female_dataset)
#ask for summaries of the model
km.model7.f
summary(km.model7.f)
plot(km.model7.f, conf.int = F, xlab = "Time (years)", ylab = "% Alive = S(t)", main = "KM-Model for Age and Wealth for Female",
     col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"), las = 1, lwd = 2,mark.time = T )
legend(x = "bottomleft", legend = c("A-1,W-1","A-1,W-2","A-1,W-3","A-2,W-1","A-2,W-2","A-2,W-3","A-3,W-1","A-3,W-2","A-3,W-3"), 
       lty = 1, lwd = 2, col = c("red", "green","orange","pink","blue","grey","yellow","gold","black"),bty = "",cex = 0.6)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc + wealth_vc,data = female_dataset)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc


anova(km.model3,km.model4, test = "LRT")


######  final model #######
km.model51 <- surv_fit(Surv(time,status) ~ ragender + age_vcd + wealth_vcd , data = cox_fd)

survdiff(Surv(time,status) ~ ragender + age_vcd + wealth_vcd,data = cox_fd)

ggsurv <- ggsurvplot(km.model51,  conf.int = F,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
   theme (legend.position = "right")+
   facet_grid(age_vcd ~ wealth_vcd)


############### .------- Important point ------------. ######

# Only  "K-M model: with ragender" is failed in assumption
# So, i divided into male and female




