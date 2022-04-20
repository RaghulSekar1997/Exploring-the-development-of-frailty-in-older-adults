# K-M model only include few catalog X's not numerical

# http://www.sthda.com/english/wiki/survival-analysis-basics  ------ more use full to explain output

# Let's start with simple model

###########################K-M model: without x################################
km.model <- surv_fit(Surv(time,status) ~ 1, data = cox_fd)
#ask for summaries of the model
km.model
summary(km.model)


ggsurvplot(km.model,
           pval = F, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv",title = "KM-Model without x varibles", 
           xlab = "Time (Year)",break.time.by=2) # Specify median survival)

#plot size 750, 650

# Let's try with individual X variables.

# $$$$$$$$$$$$$$$$$$$$$$$$ First Gender: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

###########################K-M model: with ragender#############################
km.model1 <- surv_fit(Surv(time,status) ~ ragender, data = cox_fd)
#ask for summaries of the model
km.model1
summary(km.model1)
summary(km.model1)$table
head(surv_summary(km.model1))



ggsurvplot(km.model1,
           pval = F, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           palette = c("red","blue"),
           legend.labs = c("male", "female"),title = "KM-Model for Gender", 
           xlab = "Time (Year)",break.time.by=2)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ ragender,data = cox_fd)
# It's fails to reject null hypothesis


# $$$$$$$$$$$$$$$$$$$$$$$$ Age: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###########################K-M model with age_vc variable:###################

km.model2 <- surv_fit(Surv(time,status) ~ age_vc, data = cox_fd)
#ask for summaries of the model
km.model2
summary(km.model2)
summary(km.model2)$table
head(surv_summary(km.model2))


ggsurvplot(km.model2,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           palette = c("red","blue","green"),
           legend.labs = c("below 65","between 65 - 75","above 75"),
           title = "KM-Model for Age", 
           xlab = "Time (Year)",break.time.by=2)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  
#we have evidence to believe that 
# survival is not same depending on age_vc

# $$$$$$$$$$$$$$$$$$$$$$$$ Age4: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Lets Split age into 4 groups to get more evidence 
cox_fd$age_vc4 <- cut(cox_d_fpy$age_v, breaks = c(0,60,70,80,100), 
                      labels = c(1,2,3,4))
cox_fd$age_vc4 <- as.factor(cox_fd$age_vc4)

###########################K-M model with age_vc4 variable:###################
km.model2_4 <- surv_fit(Surv(time,status) ~ age_vc4, data = cox_fd)
#ask for summaries of the model
km.model2_4
summary(km.model2_4)
summary(km.model2_4)$table
head(surv_summary(km.model2_4))

ggsurvplot(km.model2_4,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           palette = c("red","blue","green","pink"),
           legend.labs = c("below60", "60-70", "70-80", "above80"),title = "KM-Model for Age four groups", 
           xlab = "Time (Year)",break.time.by=2)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc4,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis. 
#we have evidence to believe that 
# survival is not same depending on age_vc

#now we can see clearly that age impact survival probability

# $$$$$$$$$$$$$$$$$$$$$$$$ Wealth: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#########################K-M model with wealth_vc variable:#####################
km.model3 <- surv_fit(Surv(time,status) ~ wealth_vc, data = cox_fd)
#ask for summaries of the model
km.model3
summary(km.model3)
summary(km.model3)$table
head(surv_summary(km.model3))


ggsurvplot(km.model3,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           palette = c("red","blue","green"),
           legend.labs = c("below6ok","60k-300k","above300k"),
           title = "KM-Model for Wealth", 
           xlab = "Time (Year)",break.time.by=2)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  we have evidence to believe that 
# survival is not same depending on wealth_vc

# $$$$$$$$$$$$$$$$$$$$$$$$ Wealth4: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

cox_fd$wealth_vc4 <- cut(cox_d_fpy$wealth_v,
                         breaks = c(-10460004,40000,200000,500000,10460004), 
                         labels = c(1,2,3,4))
cox_fd$wealth_vc4 <- as.factor(cox_fd$wealth_vc4)

#########################K-M model with wealth_vc4 variable:#####################
km.model3_4 <- surv_fit(Surv(time,status) ~ wealth_vc4, data = cox_fd)
#ask for summaries of the model
km.model3_4
summary(km.model3_4)
summary(km.model3_4)$table
head(surv_summary(km.model3_4))

ggsurvplot(km.model3_4,
           pval = TRUE, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           palette = c("red","blue","green","pink"),
           legend.labs = c("below4oK", "40k-200k", "200k-500k","above500K"),
           title = "KM-Model for Wealth four groups", 
           xlab = "Time (Year)",break.time.by=2)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis. 
#we have evidence to believe that 
# survival is not same depending on wealth_vc

# in wealth also we can see a clear impact on survival

# from above output we can see, effect of wealth and age are more strong, 
#where effect of gender is not so good.
# So, we are taking both age and wealth as a X in a model

# $$$$$$$$$$$$$$$$$$$$$$$$ Age and Wealth: $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#########################K-M model with age_vc + wealth_vc variable:########### 
km.model4 <- surv_fit(Surv(time,status) ~ age_vc + wealth_vc , data = cox_fd)
#ask for summaries of the model
km.model4
summary(km.model4)
summary(km.model4)$table
head(surv_summary(km.model4))

ggsurvplot(km.model4,
           pval = T, conf.int = F,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           palette = c("red", "green","orange","pink","blue","grey","yellow",
                       "gold","black"),
           legend.labs = c("A-1,W-1","A-1,W-2","A-1,W-3","A-2,W-1","A-2,W-2",
                           "A-2,W-3","A-3,W-1","A-3,W-2","A-3,W-3"),
           title = "KM-Model for Age and Wealth", 
           xlab = "Time (Year)",break.time.by=2)

cox_fd$age_vcd <- cut(cox_fd$age_v, breaks = c(0,65,75,100), 
                      labels = c("below65","65-75","above75"))
cox_fd$wealth_vcd <- cut(cox_fd$wealth_v, 
                         breaks = c(-10460004,60000,300000,10460004), 
                         labels = c("below60K","60k-300k","above300k"))


cox_fd$age_vcd <- as.factor(cox_fd$age_vcd)
cox_fd$wealth_vcd <- as.factor(cox_fd$wealth_vcd)

## to see output more clearly 
km.model4_s <- surv_fit(Surv(time,status) ~ age_vcd + wealth_vcd, data = cox_fd)

ggsurv1 <- ggsurvplot(km.model4_s,  conf.int = F,xlab = "Time (Year)",
                     ggtheme = theme_bw(),break.time.by=2)

ggsurv1$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(age_vcd ~ wealth_vcd)

#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ age_vc + wealth_vc,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis. 
#we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc


# Now lets Fix the model with Gender also..
#That will show us the a clear virtuvalation, 
#how age and wealth affect in different gender

# $$$$$$$$$$$$$$$$$$$$$$$$ Effect of Age and Wealth on gender: $$$$$$$$$$$$$$$$

#########################K-M model with ragender + age_vcd + wealth_vcd variable:
km.model5 <- surv_fit(Surv(time,status) ~ ragender + age_vcd + wealth_vcd ,
                      data = cox_fd)
#ask for summaries of the model
km.model5
summary(km.model5)
summary(km.model5)$table
head(surv_summary(km.model5))

## to see output more clearly 

ggsurv2 <- ggsurvplot(km.model5,  conf.int = F,xlab = "Time (Year)",
                     ggtheme = theme_bw(),break.time.by=2)

ggsurv2$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(age_vcd ~ wealth_vcd)
#test: LOG-RANK-TEST
#   H0 : survival in two groups is same
#   Ha : surv not
survdiff(Surv(time,status) ~ ragender + age_vcd + wealth_vcd,data = cox_fd)
# Based on the small P-value we can reject our null hypothesis.  
#we have evidence to believe that 
# survival is not same depending on age_vc and wealth_vc


############### .------- Important point ------------. ######

# Only  "K-M model: with ragender" is failed in assumption
