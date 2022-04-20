#To Start:
library("survival")
library("survminer")

summary(cox_fd)
str(cox_fd)

# Checking each variable:
############## Cox model for ragender ###########

cox.mod1 <- coxph(Surv(time,status) ~ ragender, data = cox_fd)
summary(cox.mod1)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod1)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

############## Cox model for age_vc ###########
cox.mod2 <- coxph(Surv(time,status) ~ age_vc, data = cox_fd)
summary(cox.mod2)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL


cox.zph(cox.mod2)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

#ggsurvplot(surv_fit(cox.mod2, data = cox_fd), data = cox_fd,
 #          ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv"
  #         , title = "Cox model for age_vc:", 
   #        xlab = "Time (Year)", legend.title = "Age")

############## Cox model for wealth_vc ********************* perfect###########
cox.mod3 <- coxph(Surv(time,status) ~ wealth_vc, data = cox_fd)
summary(cox.mod3)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod3)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. so it is not violating

# ggsurvplot(surv_fit(cox.mod3, data = cox_fd), data = cox_fd,
#           ggtheme = theme_minimal(), conf.int = TRUE , title = "Cox model for wealth")

ggsurvplot(surv_fit(cox.mod3, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE , title = "Cox model for wealth:", 
           xlab = "Time (Year)", risk.table = TRUE,surv.median.line = "hv")


############## Cox model for ragender + age_vc ###########
cox.mod4 <- coxph(Surv(time,status) ~ ragender + age_vc, data = cox_fd)
summary(cox.mod4)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod4)
# here p value is less .. so it not PROPORTIONAL .. so it is violating


############## Cox model for ragender + wealth_vc ###########
cox.mod5 <- coxph(Surv(time,status) ~ ragender + wealth_vc, data = cox_fd)
summary(cox.mod5)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod5)
# here p value is less .. so it not PROPORTIONAL .. so it is violating


############## Cox model for age_vc + wealth_vc ###########
cox.mod6 <- coxph(Surv(time,status) ~ age_vc + wealth_vc, data = cox_fd)
summary(cox.mod6)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod6)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

############## Cox model for age_vc + wealth_vc + ragender ###########
cox.mod7 <- coxph(Surv(time,status) ~ age_vc + wealth_vc + ragender, data = cox_fd)
summary(cox.mod7)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod7)
# here p value is less .. so it not PROPORTIONAL .. so it is violating



############## Cox model for age_v numerical ************************ Perfect ########### 

#x varibles as numerical
cox.mod8 <- coxph(Surv(time,status) ~ age_v , data = cox_fd)
summary(cox.mod8)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod8), residuals(cox.mod8,type = "martingale"), xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod8), residuals(cox.mod8,type = "martingale")), col="red")

#check for LINEARITY using deviance residuals:
plot(predict(cox.mod8), residuals(cox.mod8,type = "deviance"), xlab = "fitting values", 
     ylab = "deviance residuals", main = "Residual Plot", las = 1)
lines(smooth.spline(predict(cox.mod8), residuals(cox.mod8,type = "deviance")), col="red")
# It shows LINEARITY .. so it is good

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL

# I Think we don't need to check PROPORTIONAL.  because it is numerical and no two curves
#cox.zph(cox.mod8)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

ggsurvplot(surv_fit(cox.mod8, data = cox_fd), data = cox_fd,
          ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
          title = "Cox model for Age Numerical:", xlab = "Time (Year)", legend.title = "Age")



############## Cox model for wealth_v numerical ###########
cox.mod9 <- coxph(Surv(time,status) ~ wealth_v, data = cox_fd)
summary(cox.mod9)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod9), residuals(cox.mod9,type = "martingale"), xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod9), residuals(cox.mod9,type = "martingale")), col="red")

#check for LINEARITY using deviance residuals:
plot(predict(cox.mod9), residuals(cox.mod9,type = "deviance"), xlab = "fitting values", 
     ylab = "deviance residuals", main = "Residual Plot", las = 1)
lines(smooth.spline(predict(cox.mod9), residuals(cox.mod9,type = "deviance")), col="red")
# It shows Non - LINEARITY .. so it is violating
#So that we can use catalog variable (wealth_vc) 

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod9)
# here p value is less .. so it not PROPORTIONAL .. so it is violating




############## Cox model for age_v + wealth_v numerical ###########
cox.mod10 <- coxph(Surv(time,status) ~ age_v + wealth_v, data = cox_fd)
summary(cox.mod10)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod10), residuals(cox.mod10,type = "martingale"), xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod10), residuals(cox.mod10,type = "martingale")), col="red")

#check for LINEARITY using deviance residuals:
plot(predict(cox.mod10), residuals(cox.mod10,type = "deviance"), xlab = "fitting values", 
     ylab = "deviance residuals", main = "Residual Plot", las = 1)
lines(smooth.spline(predict(cox.mod10), residuals(cox.mod10,type = "deviance")), col="red")
# It shows LINEARITY .. so it is good

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod10)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

############## Cox model for strata(ragender)   ----  Clearing violation of Assumption  ^^^^  no output for cox.zph ###########

cox.mod11 <- coxph(Surv(time,status) ~ strata(ragender), data = cox_fd)
summary(cox.mod11)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod11)
# no output for cox.zph
ggsurvplot(surv_fit(cox.mod11, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv"
           ,risk.table.col = "strata", title = "Cox model for Gender:", 
           xlab = "Time (Year)", legend.title = "Sex",
           legend.labs = c("Male", "Female"))




############## Cox model for strata(age_vc)   ----  Clearing violation of Assumption  ^^^^  no output for cox.zph ###########
cox.mod12 <- coxph(Surv(time,status) ~ strata(age_vc), data = cox_fd)
summary(cox.mod12)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod12)
# here p value is less .. so it not PROPORTIONAL .. so it is violating
ggsurvplot(surv_fit(cox.mod12, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Age:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))


############## Cox model for strata(age_vc) + wealth_vc   ----  Clearing violation of Assumption  ****  perfect ###########
cox.mod13 <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc, data = cox_fd)
summary(cox.mod13)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod13)
# here p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod13, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Age and Wealth:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

############## Cox model for strata(ragender) + wealth_vc   ----  Clearing violation of Assumption  *****  perfect ###########
cox.mod14 <- coxph(Surv(time,status) ~ strata(ragender) + wealth_vc, data = cox_fd)
summary(cox.mod14)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod14)

# here p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod14, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Gender and Wealth:", 
           xlab = "Time (Year)", legend.title = "Sex",
           legend.labs = c("Male","Female"))

############## Cox model for ragender + strata(age_vc) + wealth_vc  ----  Clearing violation of Assumption **********  globally perfect###########
cox.mod15 <- coxph(Surv(time,status) ~ ragender + strata(age_vc) + wealth_vc , data = cox_fd)
summary(cox.mod15)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod15)
# here global p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod15, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Gender, Age and Wealth:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

############## Cox model for strata(ragender) + strata(age_vc) + wealth_vc  ----  Clearing violation of Assumption **********  perfect###########
cox.mod16 <- coxph(Surv(time,status) ~ strata(ragender) + strata(age_vc) + wealth_vc , data = cox_fd)
summary(cox.mod16)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod16)
# here global p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod16, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = F,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Gender, Age and Wealth:", 
           xlab = "Time (Year)", 
           legend.labs = c("G-1,A-1","G-1,A-2","G-1,A-3","G-2,A-1","G-2,A-2","G-2,A-3"))

############## Cox model for strata(ragender) + strata(age_vc)  ----  Clearing violation of Assumption **********  perfect###########
cox.mod20 <- coxph(Surv(time,status) ~ strata(ragender) + strata(age_vc) , data = cox_fd)
summary(cox.mod20)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod20)
# here global p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod20, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = T,risk.table = TRUE,
           surv.median.line = "hv",risk.table.col = "strata")

############## Cox model for strata(wealth_vc)  ----  Clearing violation of Assumption **********  perfect###########
cox.mod21 <- coxph(Surv(time,status) ~ strata(wealth_vc) , data = cox_fd)
summary(cox.mod21)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod21)
# here global p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod21, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = F,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Wealth Strata", 
           xlab = "Time (Year)", 
           legend.labs = c("below 60k ","between 60k-300k","over 300k"))

#########  Dividing data-set into two male and female  #######
male_dataset <- subset(cox_fd,ragender %in% c(1))
female_dataset <- subset(cox_fd,ragender %in% c(2))

############## Cox model for MALE and FEMALE     + strata(age_vc)    ^^^^^^^    no output for cox.zph###########
cox.mod17.m <- coxph(Surv(time,status) ~  strata(age_vc), data = male_dataset)
summary(cox.mod17.m)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod17.m)
## no output
ggsurvplot(surv_fit(cox.mod17.m, data = male_dataset), data = male_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age on Male:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

cox.mod17.f <- coxph(Surv(time,status) ~  strata(age_vc), data = female_dataset)
summary(cox.mod17.f)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod17.f)
## no output
ggsurvplot(surv_fit(cox.mod17.f, data = female_dataset), data = female_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age on Female:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

############## Cox model for MALE and FEMALE     + wealth_vc    ********************* perfect ###########
cox.mod18.m <- coxph(Surv(time,status) ~ wealth_vc, data = male_dataset)
summary(cox.mod18.m)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod18.m)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. so it is not violating
ggsurvplot(surv_fit(cox.mod18.m, data = male_dataset), data = male_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           title = "Cox model Wealth on Male:", 
           xlab = "Time (Year)")

cox.mod18.f <- coxph(Surv(time,status) ~ wealth_vc, data = female_dataset)
summary(cox.mod18.f)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod18.f)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. so it is not violating
ggsurvplot(surv_fit(cox.mod18.f, data = female_dataset), data = female_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           title = "Cox model Wealth on Female:", 
           xlab = "Time (Year)")

############## Cox model for MALE and FEMALE     + strata(age_vc) + wealth_vc   ********************* perfect ###########

cox.mod19.m <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc , data = male_dataset)
summary(cox.mod19.m)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod19.m)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. so it is not violating
ggsurvplot(surv_fit(cox.mod19.m, data = male_dataset), data = male_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age, Wealth on Male:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

cox.mod19.f <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc , data = female_dataset)
summary(cox.mod19.f)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod19.f)

# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. so it is not violating
ggsurvplot(surv_fit(cox.mod19.f, data = female_dataset), data = female_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age, Wealth on Female:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))




################################### ** ####################################
########### ***************** TESTING ****************  ##############
################################## ** ####################################

###### Likehood test  #####

#######  **** Test for wealth *** ########

# cox.mod12 ---- strata(age_vc)   
# cox.mod13 ---- strata(age_vc) + wealth_vc
anova(cox.mod12,cox.mod13, test = "LRT")
# Here p-value is small ..so its better to keep wealth_vc 

# cox.mod11 ---- strata(ragender)  
# cox.mod14 ---- strata(ragender) + wealth_vc
anova(cox.mod11,cox.mod14, test = "LRT")
# Here p-value is small ..so its better to keep wealth_vc 

# cox.mod20 ---- strata(ragender) + strata(age_vc)
# cox.mod16 ---- strata(ragender) + strata(age_vc) + wealth_vc
anova(cox.mod20,cox.mod16, test = "LRT")
# Here p-value is small ..so its better to keep wealth_vc 

#######  **** Test for comparing other models *** ########

# cox.mod11 ---- strata(ragender) 
# cox.mod20 ---- strata(ragender) + strata(age_vc)
anova(cox.mod11,cox.mod20, test = "LRT")
# Here p-value is small ..so its better to keep strata(age_vc) 

# cox.mod12 ---- strata(age_vc)
# cox.mod20 ---- strata(ragender) + strata(age_vc)
anova(cox.mod12,cox.mod20, test = "LRT")
# Here p-value is small ..so its better to keep strata(age_vc) 

# cox.mod13 ---- strata(age_vc) + wealth_vc
# cox.mod16 ---- strata(ragender) + strata(age_vc) + wealth_vc
anova(cox.mod13,cox.mod16, test = "LRT")
# Here p-value is small ..so its better to keep sstrata(ragender)

# cox.mod14 ---- strata(ragender) + wealth_vc
# cox.mod16 ---- strata(ragender) + strata(age_vc) + wealth_vc
anova(cox.mod14,cox.mod16, test = "LRT")
# Here p-value is small ..so its better to keep strata(age_vc)



################################## AIC test ####################################

library("MASS")

AIC(cox.mod1)
AIC(cox.mod2)
AIC(cox.mod3)
AIC(cox.mod4)
AIC(cox.mod5)
AIC(cox.mod6)
AIC(cox.mod7)
AIC(cox.mod8)
AIC(cox.mod9)
AIC(cox.mod10)
AIC(cox.mod11)
AIC(cox.mod12)
AIC(cox.mod13)
AIC(cox.mod14)
AIC(cox.mod15)
AIC(cox.mod16)
AIC(cox.mod17.m)
AIC(cox.mod17.f)
AIC(cox.mod18.m)
AIC(cox.mod18.f)
AIC(cox.mod19.m)
AIC(cox.mod19.f)
AIC(cox.mod20)
AIC(cox.mod21)

# From above AIC test
# For overall dataset --- model 16 is the best among all because it as low AIC score
# For male dataset --- model-19.m is the best because it as low AIC score
# For female dataset --- model-19.f is the best because it as low AIC score








