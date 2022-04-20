# Now lets start with Cox P-H model.

# Two main assumption of cox model are 
#  1) linearity in numerical x variable
#  2) PROPORTIONAL assumption

# Let's start with simple model

#####. ---------------    Let's try with individual X variables ---------------- #######

# $$$$$$$$$$$$$$$$$$$$$$$$ First Gender: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

############## Cox model for ragender ########

cox.mod1 <- coxph(Surv(time,status) ~ ragender, data = cox_fd)
summary(cox.mod1)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod1)
# here p value is less .. so it not PROPORTIONAL .. so it is violating
# so, it can be strata

cox.mod1.2 <- coxph(Surv(time,status) ~ strata(ragender), data = cox_fd)
summary(cox.mod1.2)


# This can be perfect cox model for gender

ggsurvplot(surv_fit(cox.mod1.2, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv"
           ,risk.table.col = "strata", title = "Cox model for Gender:", 
           xlab = "Time (Year)", legend.title = "Sex",
           legend.labs = c("Male", "Female"))

# from above plot we can see there is a effect of gender but it was more subtle 

# $$$$$$$$$$$$$$$$$$$$$$$$ Age: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

############## Cox model for Age  ########### 

#x varibles as numerical
cox.mod8 <- coxph(Surv(time,status) ~ age_v , data = cox_fd)
summary(cox.mod8)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod8), residuals(cox.mod8,type = "martingale"), 
     xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod8), residuals(cox.mod8,type = "martingale")),
      col="red")


# It can see little non-LINEARITY .. so lets transform the X variable

cox.mod8.1 <- coxph(Surv(time,status) ~ I(age_v^2)  , data = cox_fd)
summary(cox.mod8.1)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod8.1), residuals(cox.mod8.1,type = "martingale"), 
     xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod8.1),
                    residuals(cox.mod8.1,type = "martingale"))
      , col="red")
# Now we can see this model meets LINEARITY 

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL


cox.zph(cox.mod8.1)
# here p value is less .. so it not PROPORTIONAL .. so it is violating

# So, I'm taking age catalog X

cox.mod8.2 <- coxph(Surv(time,status) ~ age_vc , data = cox_fd)
# we don't need to check linearity for catlog variable

cox.zph(cox.mod8.2)
# here p value is less .. so it not PROPORTIONAL .. so it is violating
# let's strata

cox.mod8.3 <- coxph(Surv(time,status) ~ strata(age_vc) , data = cox_fd)


ggsurvplot(surv_fit(cox.mod8.3, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Age:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

# Lets try with 4 group of age to see whether model it improves the model

cox.mod8.4 <- coxph(Surv(time,status) ~ strata(age_vc4) , data = cox_fd)


ggsurvplot(surv_fit(cox.mod8.4, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Age:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 60", "60-70","70-80","over 80"))

# AIC test: to find the best model. 
#model which ever has the low AIC score is the best model

library("MASS")

AIC(cox.mod8.3)
AIC(cox.mod8.4)

# we can see cox.mod8.4 has less score. So cox.mod8.4 is the best model.
# That means as we split age into more group, model gets improving
# we can see that effect of age is more strong 


# $$$$$$$$$$$$$$$$$$$$$$$$ Wealth: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

############## Cox model for wealth ###########
cox.mod9 <- coxph(Surv(time,status) ~ wealth_v, data = cox_fd)
summary(cox.mod9)
###    CHECKING LINEARITY 
#check for LINEARITY using Martingale
plot(predict(cox.mod9), residuals(cox.mod9,type = "martingale"), 
     xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod9), residuals(cox.mod9,type = "martingale")),
      col="red")

# Here we can see NON-LINEARITY in the model,
#so we can try transformation of wealth_v  
#Or we can use catalog variable (wealth_vc), 
#if we use catalog variable we don't need to check LINEARITY

cox.mod9.1 <- coxph(Surv(time,status) ~ wealth_vc, data = cox_fd)
summary(cox.mod9.1)
###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod9.1)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. 
#so it is not violating

ggsurvplot(surv_fit(cox.mod9.1, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE , 
           title = "Cox model for wealth:", 
           xlab = "Time (Year)", risk.table = TRUE,surv.median.line = "hv")

# lets divide wealth into more groups 
#and check wealth it improves the model (wealth_vc4)
cox.mod9.2 <- coxph(Surv(time,status) ~ wealth_vc4, data = cox_fd)

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod9.2)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. 
#so it is not violating

# AIC test: to find the best model. 
#model which ever has the low AIC score is the best model

AIC(cox.mod9.1)
AIC(cox.mod9.2)

# AIC score of cox.mod9.1 is less. 
#this means splitting more groups in wealth does not improve the model

############## Cox model for strata(wealth_vc)  
cox.mod21 <- coxph(Surv(time,status) ~ strata(wealth_vc) , data = cox_fd)
summary(cox.mod21)

ggsurvplot(surv_fit(cox.mod21, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = F,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Wealth Strata", 
           xlab = "Time (Year)", 
           legend.labs = c("below 60k ","between 60k-300k","over 300k"))

# This plot shows the effect of wealth is strong in the model


#####. Let's check adding wealth in X variable improves the model: -- #######



############## Cox model for strata(ragender) + wealth_vc 
cox.mod14 <- coxph(Surv(time,status) ~ strata(ragender) + wealth_vc,
                   data = cox_fd)
summary(cox.mod14)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod14)

# here p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod14, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata",
           title = "Cox model for Gender and Wealth:", 
           xlab = "Time (Year)", legend.title = "Sex",
           legend.labs = c("Male","Female"))

###### Likehood test  

# cox.mod1.2 ---- strata(ragender)  
# cox.mod14 ---- strata(ragender) + wealth_vc
anova(cox.mod1.2,cox.mod14, test = "LRT")
# Here p-value is small ..so its better to keep wealth_vc 
# Because of small P-value, model 2 is the best. 
#So, model with wealth performs well than model without wealth.


############## Cox model for strata(age_vc) + wealth_vc   
cox.mod13 <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc,
                   data = cox_fd)
summary(cox.mod13)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod13)
# here p value greater than 0.5.  H0 if failed to reject .. so it PROPORTIONAL 
ggsurvplot(surv_fit(cox.mod13, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model for Age and Wealth:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

###### Likehood test  

# cox.mod8.3 ---- strata(age_vc)   
# cox.mod13 ---- strata(age_vc) + wealth_vc
anova(cox.mod8.3,cox.mod13, test = "LRT")
# Here p-value is small ..so its better to keep wealth_vc 
# Because of small P-value, model 2 is the best. 
#So, model with wealth performs well than model without wealth.


####From both likehood test we can find adding wealth clearly improves the model


#####. ---------------   Now let find the perfect fit model: ---------- #######

############## Cox model for ragende + age_vc + wealth_vc 
cox.mod16 <- coxph(Surv(time,status) ~ ragender + age_vc + wealth_vc ,
                   data = cox_fd)
summary(cox.mod16)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod16)
# From the output we can see ragender and age_vc has small p-value 
#so it not proportional 
# also global p value is also small, Model is not proportional, 
#So lets strata ragender and age_vc

cox.mod16.1 <- coxph(Surv(time,status) ~ strata(ragender) + strata(age_vc) 
                     + wealth_vc , data = cox_fd)
summary(cox.mod16.1)

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod16.1)
# here global p value greater than 0.5.  H0 if failed to reject .. 
#so it PROPORTIONAL 

ggsurvplot(surv_fit(cox.mod16.1, data = cox_fd), data = cox_fd,
           ggtheme = theme_minimal(), conf.int = F,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", 
           title = "Cox model for Gender, Age and Wealth:", 
           xlab = "Time (Year)", 
           legend.labs = c("G-1,A-1","G-1,A-2","G-1,A-3",
                           "G-2,A-1","G-2,A-2","G-2,A-3"))


# As we seen in cox.mod8 that incressing the groups of age improves the fit of the model. 
# lets try here the same 

cox.mod16.2 <- coxph(Surv(time,status) ~ strata(ragender) + strata(age_vc4) 
                     + wealth_vc , data = cox_fd)
summary(cox.mod16.2)

# Let's compare the cox.mod16.1 and cox.mod16.2 to find whether it deveops in model

### AIC test is used to compare

AIC(cox.mod16.1)
AIC(cox.mod16.2)

# from output we can see the output of cox.mod16.2 is small 
#this show there is a improvement in model

# So lets strata the whole age

cox.mod16.3 <- coxph(Surv(time,status) ~ strata(ragender) 
                     + strata(age_v) + wealth_vc , data = cox_fd)

summary(cox.mod16.3)
### AIC test is used to compare

AIC(cox.mod16.1)
AIC(cox.mod16.2)
AIC(cox.mod16.3)


# cox.mod16.3 has the low AIC score 
# So this will be the perfect fit for the model

# lets try different:
cox.mod16.4 <- coxph(Surv(time,status) ~ strata(ragender) 
                     + strata(age_v) +  wealth_vc , data = cox_fd)

cox.zph(cox.mod16.4)

plot(predict(cox.mod16.4), residuals(cox.mod16.4,type = "martingale"), 
     xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.mod16.4),
                    residuals(cox.mod16.4,type = "martingale")), col="red")

AIC(cox.mod16.4)


#####. ----------   Lets try to compare gender with different model -- #######

# Now lets seperate the dataset into male and female to 
#compare between male and female

#########  Dividing data-set into two male and female  
male_dataset <- subset(cox_fd,ragender %in% c(1))
female_dataset <- subset(cox_fd,ragender %in% c(2))

############## Cox model for MALE and FEMALE + age_vc + wealth_vc   

cox.mod19.m <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc ,
                     data = male_dataset)
summary(cox.mod19.m)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod19.m)
# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. 
#so it is not violating
ggsurvplot(surv_fit(cox.mod19.m, data = male_dataset), data = male_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age, Wealth on Male:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))

cox.mod19.f <- coxph(Surv(time,status) ~ strata(age_vc) + wealth_vc , 
                     data = female_dataset)
summary(cox.mod19.f)

###    CHECKING LINEARITY 
# don't want to check linearity because it is catalog

###   CHECKING PROPORTIONAL HAZARDS ASSUMPTION
# H0 : HAZARDS are PROPORTIONAL   Ha : HAZARDS are not PROPORTIONAL
cox.zph(cox.mod19.f)

# here p value is large ..so it fails to reject H0, so it PROPORTIONAL .. 
#so it is not violating
ggsurvplot(surv_fit(cox.mod19.f, data = female_dataset), data = female_dataset,
           ggtheme = theme_minimal(), conf.int = TRUE,risk.table = TRUE,
           surv.median.line = "hv",
           risk.table.col = "strata", title = "Cox model Age, Wealth on Female:", 
           xlab = "Time (Year)", legend.title = "Age",
           legend.labs = c("below 65", "65-75","over 75"))


-----
  
cox.modt <- coxph(Surv(time,status) ~ strata(ragender) + I(age_v^2) 
                  + wealth_v, data = cox_fd)
summary(cox.modt)

plot(predict(cox.modt), residuals(cox.modt,type = "martingale"),
     xlab = "fitting values", 
     ylab = "Martingale residuals", main = "Residual Plot", las = 1)
abline(h=0)
lines(smooth.spline(predict(cox.modt), residuals(cox.modt,type = "martingale"))
      , col="red")











