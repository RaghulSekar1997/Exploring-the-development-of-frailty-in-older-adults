library(tidyverse)
library(haven)
library("tidyr")
library("survival")
library("survminer")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

data <- read_dta(file = "/Users/raghulsekar/Desktop/ Disseritation /Dataset/MSc_Raghul_data.dta")
write.csv(data, file = "MSc_Raghul_data.csv")

gen_min <- data[,c("idauniq","ragender","rabyear","r1agey","r2agey","r3agey","r4agey","r5agey","r6agey","r7agey","fraill1","fraill2","fraill3","fraill4","fraill5","fraill6","fraill7","h1atotb","h2atotb","h3atotb","h4atotb","h5atotb","h6atotb","h7atotb")]

str(gen_min)
colnames(gen_min)

sapply(gen_min, function(x) sum(is.na(x)))


# preparing data for Cox
cox_d <- gen_min[,c("idauniq","ragender")]

#adding age
cox_d$r1agey <- 2002 - gen_min$rabyear
cox_d$r2agey <- 2004 - gen_min$rabyear
cox_d$r3agey <- 2006 - gen_min$rabyear
cox_d$r4agey <- 2008 - gen_min$rabyear
cox_d$r5agey <- 2010 - gen_min$rabyear
cox_d$r6agey <- 2012 - gen_min$rabyear
cox_d$r7agey <- 2014 - gen_min$rabyear

#to add survaial time and status i'm using pandas

write.csv(gen_min,'gen_min_py.csv')
write.csv(cox_d,'cox_d_py.csv')

#reading the dataset that prepared in Py
cox_d_fpy <- read_csv(file = "cox_d_fpy.csv")

#converting age and wealth into catalog:
cox_d_fpy$age_vc <- cut(cox_d_fpy$age_v, breaks = c(0,65,75,100), labels = c(1,2,3))
cox_d_fpy$wealth_vc <- cut(cox_d_fpy$wealth_v, breaks = c(-10460004,60000,300000,10460004), labels = c(1,2,3))


colnames(cox_d_fpy)

#preparing final dataset for Cox
 cox_fd <- cox_d_fpy[,c("idauniq","ragender","status","time","age_v",
                        "wealth_v","age_f","wealth_f","age_vc","wealth_vc")]
 
 colnames(cox_fd)
 
 

 str(cox_fd)

 cox_fd$ragender <- as.factor(cox_fd$ragender)
 #cox_fd$status <- as.factor(cox_fd$status)
 cox_fd$age_vc <- as.factor(cox_fd$age_vc)
 cox_fd$wealth_vc <- as.factor(cox_fd$wealth_vc)
 
 str(cox_fd)
 
 View(cox_fd)
 
 ##saving csv to send it to py and add new columns male, female, below 65, between 65 -75, above 75
 write.csv(cox_fd,'cox_fd.csv')
 
 #reading the dataset that prepared in Py
 cox_d_fpy <- read_csv(file = "cox_fd_py.csv")

 colnames(cox_d_fpy)
 
 cox_fd <- cox_d_fpy[,c("idauniq","ragender","status","time","age_v",
                        "wealth_v","age_f","wealth_f","age_vc","wealth_vc", "male","female",
                        "below_65","between_65_75", "over_75")]
 
 str(cox_fd)
 
 #correlation matrix
 cor(cox_fd)
 
 cox_fd$ragender <- as.factor(cox_fd$ragender)
 #cox_fd$status <- as.factor(cox_fd$status)
 cox_fd$age_vc <- as.factor(cox_fd$age_vc)
 cox_fd$wealth_vc <- as.factor(cox_fd$wealth_vc)
 cox_fd$male <- as.factor(cox_fd$male)
 cox_fd$female <- as.factor(cox_fd$female)
 cox_fd$below_65 <- as.factor(cox_fd$below_65)
 cox_fd$between_65_75 <- as.factor(cox_fd$between_65_75)
 cox_fd$over_75 <- as.factor(cox_fd$over_75)
 
 cox_fd_d <- cox_d_fpy[,c("idauniq","ragender","status","time","age_v",
                        "wealth_v","age_vc","wealth_vc")]
 cox_fd_d$ragender <- as.factor(cox_fd$ragender)
 #cox_fd$status <- as.factor(cox_fd$status)
 cox_fd_d$age_vc <- as.factor(cox_fd$age_vc)
 cox_fd_d$wealth_vc <- as.factor(cox_fd$wealth_vc)
 
 scatterplotMatrix(cox_fd_d)
 
 #
 describe(cox_fd$age_v)
 
 cox_fd$over_66 <- cut(cox_d_fpy$age_v, breaks = c(0,66,100), labels = c(0,1))
 
 
 #########
 
 
 
 
 
 
 
  