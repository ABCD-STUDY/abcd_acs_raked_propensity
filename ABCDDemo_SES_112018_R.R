# PB translate SAS to R for Step 1 of Weight Process 
# Read in November data, work done on 11dec2018 using October R script files modifed for November data
library(survey)
library(haven)
library(mice)

# set working directory
setwd( "L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump")
getwd()

# read in demo data from November 2018 SAS data set, then save for Wes T. as RDS file 
demo <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump/Nov2018_demo.sas7bdat")
names(demo) 
summary(demo) 
str(demo) #n=11875 

# restrict to Baseline data only, already done in SAS steps 
newdemo <- subset(demo, redcap_event_name =="baseline_year_1_arm_1")
nrow(newdemo) 

# descriptive for age in months 
summary(newdemo$tlfb_age_calc_inmonths_l) 

# divide by 12  
newdemo$age1 <- (newdemo$tlfb_age_calc_inmonths_l)/12
summary(newdemo$age1) 

# as integer 
newdemo$age <- as.integer(newdemo$age1) 
table(newdemo$age, exclude=NULL) 
 
# set those > 10 to 10 and 8 to 9 for integer age 
newdemo$age=newdemo$age
newdemo$age [newdemo$age > 10] <- 10
newdemo$age [newdemo$age ==8] <- 9
table(newdemo$age, exclude=NULL) 

# recode sex and intersex  
newdemo$sex <- 1
newdemo$sex [newdemo$demo_sex_v2b==2 | newdemo$demo_sex_v2b==4] <- 2
newdemo$sex [newdemo$demo_sex_v2b==6] <- NA
table(newdemo$sex, exclude= NULL) 

# race/ethnicity 
# create dummys first 
newdemo$white <- 0 
newdemo$white [newdemo$demo_race_a_p___10==1] <- 1
table(newdemo$white, newdemo$demo_race_a_p___10, exclude=NULL) 

# black 
newdemo$blk <- 0 
newdemo$blk [newdemo$demo_race_a_p___11==1] <- 1
table(newdemo$blk) 

# asian 
newdemo$asian <- 0 
newdemo$asian [newdemo$demo_race_a_p___18==1 | newdemo$demo_race_a_p___19==1 | 
newdemo$demo_race_a_p___20==1 | newdemo$demo_race_a_p___21==1 | newdemo$demo_race_a_p___22==1 
| newdemo$demo_race_a_p___23==1 | newdemo$demo_race_a_p___24==1  ] <- 1
table(newdemo$asian) 

# aian 
newdemo$aian <- 0 
newdemo$aian [newdemo$demo_race_a_p___12==1 | newdemo$demo_race_a_p___13==1] <- 1
table(newdemo$aian) 

# nhpi 
newdemo$nhpi <- 0 
newdemo$nhpi [newdemo$demo_race_a_p___14==1 | newdemo$demo_race_a_p___15==1 
| newdemo$demo_race_a_p___16==1 | newdemo$demo_race_a_p___17==1] <- 1
table(newdemo$nhpi) 

#other 
newdemo$oth <- 0 
newdemo$oth [newdemo$demo_race_a_p___25==1 ] <-1 
table(newdemo$oth) 

# race counts with certain groups set to 1 only (asian aian nhpi)
newdemo$racecount2 <- (newdemo$white + newdemo$blk + newdemo$asian + newdemo$aian + newdemo$nhpi+ newdemo$oth)  
table(newdemo$racecount2) 

# indicator of mixed race
newdemo$mixed1 <- NA
newdemo$mixed1 [newdemo$racecount2 <=1]  <- 0 
newdemo$mixed1 [newdemo$racecount2 > 1] <- 1 
table (newdemo$mixed1) 
 
# use coding as in SAS, assign in this order to match SH  
newdemo$race_eth [newdemo$demo_race_a_p___10==1 ] <- 1
newdemo$race_eth [newdemo$demo_race_a_p___11==1 ] <- 2
newdemo$race_eth [newdemo$demo_race_a_p___12==1 ] <- 5
newdemo$race_eth [newdemo$demo_race_a_p___13==1 ] <- 5
newdemo$race_eth [newdemo$demo_race_a_p___14==1 ] <- 6
newdemo$race_eth [newdemo$demo_race_a_p___15==1 ] <- 6 
newdemo$race_eth [newdemo$demo_race_a_p___16==1 ] <- 6
newdemo$race_eth [newdemo$demo_race_a_p___17==1 ] <- 6
newdemo$race_eth [newdemo$demo_race_a_p___18==1 ] <- 4 
newdemo$race_eth [newdemo$demo_race_a_p___19==1 ] <- 4
newdemo$race_eth [newdemo$demo_race_a_p___20==1 ] <- 4 
newdemo$race_eth [newdemo$demo_race_a_p___21==1 ] <- 4
newdemo$race_eth [newdemo$demo_race_a_p___22==1 ] <- 4
newdemo$race_eth [newdemo$demo_race_a_p___23==1 ] <- 4
newdemo$race_eth [newdemo$demo_race_a_p___24==1 ] <- 4
newdemo$race_eth [newdemo$demo_race_a_p___25==1 ] <- 8
newdemo$race_eth [newdemo$demo_race_a_p___99==1 | newdemo$demo_race_a_p___77==1] <- NA
newdemo$race_eth [newdemo$mixed1==1]         <- 9
newdemo$race_eth [newdemo$demo_ethn_v2b==1] <- 3 

table(newdemo$race_eth,exclude=NULL)

# create race pattern variables
# full counts 
newdemo$whitecount <- (newdemo$demo_race_a_p___10) 
newdemo$blackcount <- (newdemo$demo_race_a_p___11)
newdemo$aiancount <- (newdemo$demo_race_a_p___12 + newdemo$demo_race_a_p___13)
newdemo$nhpicount <- (newdemo$demo_race_a_p___14 +newdemo$demo_race_a_p___15 + newdemo$demo_race_a_p___16
+ newdemo$demo_race_a_p___17)
newdemo$asiancount <- (newdemo$demo_race_a_p___18 + newdemo$demo_race_a_p___19 + newdemo$demo_race_a_p___20 +
newdemo$demo_race_a_p___21 + newdemo$demo_race_a_p___22 + newdemo$demo_race_a_p___23 + newdemo$demo_race_a_p___24)
newdemo$othercount <- newdemo$demo_race_a_p___25 
newdemo$racecount <- (newdemo$whitecount+  newdemo$blackcount + newdemo$aiancount + newdemo$nhpicount + 
  newdemo$asiancount + newdemo$othercount)  
table(newdemo$racecount, exclude=NULL) 
 
# count variables for pattern #1
newdemo$race_eth_pat <- (100000*newdemo$whitecount + 10000*newdemo$blackcount + 1000*newdemo$asiancount + 100*newdemo$aiancount + 
  10*newdemo$nhpicount + newdemo$othercount)
table(newdemo$race_eth_pat, exclude=NULL) 

# dummy variables for pattern #2
newdemo$race_eth_pat2 <- (100000*newdemo$white + 10000*newdemo$blk + 1000*newdemo$asian + 100*newdemo$aian + 
  10*newdemo$nhpi + newdemo$oth)
table(newdemo$race_eth_pat2, exclude=NULL) 

# family income 
newdemo$faminc <- NA
newdemo$faminc [newdemo$demo_comb_income_v2b %in% 1:4] <- 1
newdemo$faminc [newdemo$demo_comb_income_v2b %in% 5:6] <- 2
newdemo$faminc [newdemo$demo_comb_income_v2b %in% (7)] <- 3
newdemo$faminc [newdemo$demo_comb_income_v2b %in% (8)] <- 4
newdemo$faminc [newdemo$demo_comb_income_v2b %in% (9)] <- 5
newdemo$faminc [newdemo$demo_comb_income_v2b %in% (10)] <- 6
table(newdemo$faminc, exclude= NULL) 

# family type 
newdemo$famtype <- NA
newdemo$famtype [newdemo$demo_prnt_marital_v2b==1 | newdemo$demo_prnt_marital_v2b==6] <- 1
newdemo$famtype [newdemo$demo_prnt_marital_v2b %in% 2:5] <- 2 
table (newdemo$famtype, exclude = NULL) 

# HH size 
newdemo$hhsize <- NA
newdemo$hhsize [newdemo$demo_roster_v2 %in% c(0,1,2,3)] <- 1
newdemo$hhsize [newdemo$demo_roster_v2 %in% c(4)] <- 4
newdemo$hhsize [newdemo$demo_roster_v2 %in% c(5)] <- 5
newdemo$hhsize [newdemo$demo_roster_v2 %in% c(6)] <- 6
newdemo$hhsize [newdemo$demo_roster_v2 > 6] <- 7
table (newdemo$hhsize, exclude = NULL) 

# Region 
newdemo$region <- 3 
newdemo$region [newdemo$site_name %in% c("ROC","UPMC","UVM","YALE")] <- 1
newdemo$region [newdemo$site_name %in% c("UMICH","UMN","UWM","WUSTL")] <- 2
newdemo$region [newdemo$site_name %in% c("CHLA","CUB","OHSU","SRI","UCLA","UCSD","UTAH")] <- 4
table (newdemo$region, exclude = NULL) 

# save first part of program
#save (newdemo, file="L:/projects/ABCD/02 Sampling/PB ABCD Analysis/temp1.RData")
#str(newdemo)
#names(newdemo)

######################################################################################### 
# read in the November2018 ACS data set 

acs <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump/Nov2018_ACS.sas7bdat")
names(acs) 

# keep only key variables 
acs_s <- acs[c("id_redcap", "rel_family_id", "rel_group_id", "rel_relationship", "rel_same_sex", "site_name")]
names(acs_s) 
summary(acs_s) 

# allmult variable (multiple births) 
acs_s$allmult <- 0 
acs_s$allmult [acs_s$rel_relationship %in% c(2,3)] <- 1 
table(acs_s$allmult) 
 
# twin site variable, note corrected to VCU assignment as twin site (SH) 
acs_s$sitetwin <- 0 
acs_s$sitetwin [acs_s$site_name %in% c("UMN","CUB","VCU","WUSTL") & acs_s$allmult==1] <- 1 
table(acs_s$sitetwin)
str(acs_s)
# sort by id_redcap (may not be needed but here just in case) 
#acs_ss <- acs_s [order("id_redcap"),] 
#names(acs_ss)

############################################################################
# merge temp1 saved above and acs_s into tmpmerge 
tmpmerge <- merge(newdemo, acs_s, by="id_redcap") 
summary(tmpmerge) 
names(tmpmerge)
table(tmpmerge$age, exclude=NULL)

# tables of key variables: 1. one way tables and 2. by single and multiple birth status 
# factor vars to add variable labels 

# Allmult factor  
tmpmerge$allmultc <- factor(tmpmerge$allmult, levels = c(0,1), labels = c("Single Birth", "Multiple Birth"))
table(tmpmerge$allmultc, exclude=NULL) 

# Race_Eth Factor 
tmpmerge$race_ethc <- factor(tmpmerge$race_eth, levels = c(1,2,3,4,5,6,8,9),
 labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian" ,"AIAN", "NHPI", "OTHER" ,"MULTIPLE")) 
table (tmpmerge$race_ethc, exclude=NULL) 
table (tmpmerge$race_ethc, tmpmerge$allmultc, exclude=NULL) 

# Sex Factor 
tmpmerge$sexc <- factor(tmpmerge$sex, levels = c(1,2), labels = c("Male", "Female")) 
table (tmpmerge$sexc, exclude=NULL) 
table (tmpmerge$sexc, tmpmerge$allmultc, exclude=NULL) 

# Family income 
tmpmerge$famincc <- factor(tmpmerge$faminc, levels = c(1:6), 
 labels = c("<25k", "25k-49k", "50k-74k", "75k-99k", "100k-199k", "200k+")) 
table(tmpmerge$famincc, exclude=NULL) 
table(tmpmerge$famincc, tmpmerge$allmultc, exclude=NULL) 

# Family type 
tmpmerge$famtypec  <- factor(tmpmerge$famtype, levels = c(1:2), labels = c("Married", "Other Family")) 
table (tmpmerge$famtypec , exclude=NULL) 
table (tmpmerge$famtypec, tmpmerge$allmultc, exclude=NULL) 

# Site Twin 
tmpmerge$sitetwinc <- factor(tmpmerge$sitetwin, levels = c(0:1), labels = c("General Cohort", "Site Twin Cohort")) 
table (tmpmerge$sitetwinc, exclude=NULL) 
table (tmpmerge$sitetwinc, tmpmerge$allmultc, exclude=NULL) 

# Age  
table(tmpmerge$age, exclude=NULL) 
table (tmpmerge$age, tmpmerge$allmultc, exclude=NULL) 

# Region
table(tmpmerge$region, exclude=NULL) 
table (tmpmerge$region, tmpmerge$allmultc, exclude=NULL) 

# HH size 
table(tmpmerge$hhsize, exclude=NULL) 
table (tmpmerge$hhsize, tmpmerge$allmultc, exclude=NULL) 

# Site Twin 
table(tmpmerge$sitetwin, exclude=NULL) 
table (tmpmerge$sitetwin, tmpmerge$allmultc, exclude=NULL) 
# All matches SAS except we have 9 NA on age here but none in SAS output? 

###############################################################################

# Use site name from first data set site_name.x 
tmpmerge$site_name <- tmpmerge$site_name.x 
tmpmerge$site_name 
names(tmpmerge) 

# keep only key variables for imputation
# use names to select 
tmpimp <- tmpmerge[c("id_redcap", "age", "sex", "allmult", "famtype", "faminc", "sitetwin",
 "hhsize", "race_eth", "rel_relationship", "site_name", "demo_prnt_empl_v2b","demo_prtnr_empl_v2b",
 "demo_prnt_gender_id_v2b", "region")]
names(tmpimp)

# set some vars to factor for imputation
tmpimp$age=as.factor(tmpimp$age)
tmpimp$hhsize=as.factor(tmpimp$hhsize)
tmpimp$rel_relationship=as.factor(tmpimp$rel_relationship)
tmpimp$region=as.factor(tmpimp$region)
tmpimp$sex=as.factor(tmpimp$sex) 
tmpimp$allmult=as.factor(tmpimp$allmult)
tmpimp$famtype=as.factor(tmpimp$famtype)
tmpimp$faminc=as.factor(tmpimp$faminc)
tmpimp$race_eth=as.factor(tmpimp$race_eth) 
tmpimp$sitetwin=as.factor(tmpimp$sitetwin)

# set 777 and 999 to NA 
tmpimp$demo_prnt_empl_v2b [tmpimp$demo_prnt_empl_v2b == 777 | tmpimp$demo_prnt_empl_v2b==999]  <- NA 
table(tmpimp$demo_prnt_empl_v2b,exclude=NULL) 

tmpimp$demo_prtnr_empl_v2b [tmpimp$demo_prtnr_empl_v2b==777 | tmpimp$demo_prtnr_empl_v2b==999] <- NA 
table(tmpimp$demo_prtnr_empl_v2b , exclude=NULL) 

tmpimp$demo_prnt_gender_id_v2b [tmpimp$demo_prnt_gender_id_v2b==777 | tmpimp$demo_prnt_gender_id_v2b==999] <- NA 
table(tmpimp$demo_prnt_gender_id_v2b , exclude=NULL) 

tmpimp$demo_prnt_empl_v2b=as.factor(tmpimp$demo_prnt_empl_v2b)
tmpimp$demo_prtnr_empl_v2b=as.factor(tmpimp$demo_prtnr_empl_v2b)
tmpimp$demo_prnt_gender_id_v2b=as.factor(tmpimp$demo_prnt_gender_id_v2b)
str(tmpimp)

# begin imputation steps ##############################################################################
# missing data pattern for tmpimp 
md.pattern(tmpimp)
summary(tmpimp)

# impute missing data on ager famtype faminc hhsize race_eth demo_prnt_empl_v2b demo_prtnr_empl_v2b demo_prnt_gender_id_v2b 
init = mice(tmpimp, maxit=0) 
meth = init$method
meth

# specify imputation model type, would be ok as is but for clarity
meth[c("faminc", "hhsize", "race_eth", "demo_prnt_empl_v2b", 
 "demo_prtnr_empl_v2b", "demo_prnt_gender_id_v2b")]="polyreg" 
meth[c("age","famtype")]="logreg" 
meth

tmpimpm1 <- mice(tmpimp, m=1, seed=0270, method=meth) 
summary(tmpimpm1)

#extract the first imputed data set 
longm1 <- complete(tmpimpm1, 1) 
summary(longm1) 
table(longm1$race_eth, exclude=NULL)

# create fes variable using long imputed data set 
longm1$fesabcd [longm1$famtype==1 & longm1$demo_prnt_empl_v2b %in% c(1,2,3,9,10,11) & longm1$demo_prtnr_empl_v2b %in% c(1,2,3,9,10,11)] <- 1
longm1$fesabcd [longm1$famtype==1 & longm1$demo_prnt_empl_v2b %in% c(1,2,3,9,10,11) & longm1$demo_prtnr_empl_v2b %in% c(4,5,6,7,8,12,13,NA)] <- 2
longm1$fesabcd [longm1$famtype==1 & longm1$demo_prnt_empl_v2b %in% c(4,5,6,7,8,12,13,NA) & longm1$demo_prtnr_empl_v2b %in% c(1,2,3,9,10,11)] <- 2
longm1$fesabcd [longm1$famtype==1 & longm1$demo_prnt_empl_v2b %in% c(4,5,6,7,8,12,13,NA) & longm1$demo_prtnr_empl_v2b %in% c(4,5,6,7,8,12,13,NA)] <- 4

longm1$fesabcd [longm1$famtype %in% c(NA,2) & longm1$demo_prnt_empl_v2b %in% c(1,2,3,9,10,11) & longm1$demo_prnt_gender_id_v2b==1 ] <- 5
longm1$fesabcd [longm1$famtype %in% c(NA,2) & longm1$demo_prnt_empl_v2b %in% c(4,5,6,7,8,12,NA) & longm1$demo_prnt_gender_id_v2b==1 ] <- 6
longm1$fesabcd [longm1$famtype %in% c(NA,2) & longm1$demo_prnt_empl_v2b %in% c(1,2,3,9,10,11) & longm1$demo_prnt_gender_id_v2b %in% c(2,3,4,5,6,7,NA) ] <- 7
longm1$fesabcd [longm1$famtype %in% c(NA,2) & longm1$demo_prnt_empl_v2b %in% c(4,5,6,7,8,12,NA) & longm1$demo_prnt_gender_id_v2b %in% c(2,3,4,5,6,7,NA) ] <- 8
table(longm1$fesabcd, exclude=NULL)

##################################################################################################################
# Begin here with cross check with SAS 
# tables of key variables using imputed data set longm1  (1. one way tables and 2. by single and multiple birth status) 
# factor vars to add variable labels 
# Fes 
longm1$fesc <- factor(longm1$fes, levels = c(1,2,4,5,6,7,8),
 labels = c("Married, 2 in LF", "Married, 1 in LF", "Married, 0 in LF", "Single HH, M, LF", 
"Single HH, M, NLF","Single HH, F, LF", "Single HH, F, NLF")) 

table(longm1$fesc, exclude=NULL) 
table(longm1$fesc, longm1$famtype, exclude=NULL) 
table(longm1$fesc, longm1$demo_prnt_empl_v2b, exclude=NULL) 
table(longm1$fesc, longm1$demo_prtnr_empl_v2b, exclude=NULL) 
table(longm1$fesc, longm1$demo_prnt_gender_id_v2b, exclude=NULL) 

# check imputed variables using factor and labels
# Multiple Birth
longm1$allmultc  <- factor(longm1$allmult, levels = c(0:1), labels = c("Single Birth", "Multiple Birth")) 
table (longm1$allmultc, exclude=NULL) 

# Race_Eth Factor 
longm1$race_ethc <- factor(longm1$race_eth, levels = c(1,2,3,4,5,6,8,9),
 labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian" ,"AIAN", "NHPI", "OTHER" ,"MULTIPLE")) 
table (longm1$race_ethc, exclude=NULL) 
table (longm1$race_ethc, longm1$allmultc, exclude=NULL) 
  
# Sex Factor 
longm1$sexc <- factor(longm1$sex, levels = c(1,2), labels = c("Male", "Female")) 
table (longm1$sexc, exclude=NULL) 
table (longm1$sexc, longm1$allmultc, exclude=NULL) 

# family income 
longm1$famincc <- factor(longm1$faminc, levels = c(1:6), 
 labels = c("<25k", "25k-49k", "50k-74k", "75k-99k", "100k-199k", "200k+")) 
table (longm1$famincc , exclude=NULL) 
table (longm1$famincc, longm1$allmultc, exclude=NULL) 

# family type 
longm1$famtypec  <- factor(longm1$famtype, levels = c(1:2), labels = c("Married", "Other Family")) 
table (longm1$famtypec , exclude=NULL) 
table (longm1$famtypec, longm1$allmultc, exclude=NULL) 

# fesabcd 
table(longm1$fesc, exclude=NULL)
table(longm1$fesc, longm1$allmultc, exclude=NULL)
table (longm1$fesc, longm1$famtypec, exclude=NULL) 

# age  
table (longm1$age, exclude=NULL) 
table (longm1$age, longm1$allmultc, exclude=NULL) 

# HH size 
longm1$hhsizec  <- factor(longm1$hhsize, levels = c(1,4,5,6,7), 
  labels = c( "2-3 Persons", "4 Persons" ,"5 Persons", "6 Persons", "7 or More Persons")) 
table (longm1$hhsizec, exclude=NULL) 
table (longm1$hhsizec, longm1$allmultc, exclude=NULL) 

# site twin 
longm1$sitetwinc <- factor(longm1$sitetwin, levels = c(0:1), labels = c("General Cohort", "Site Twin Cohort")) 
table (longm1$sitetwinc, exclude=NULL) 
table (longm1$sitetwinc, longm1$allmultc, exclude=NULL) 

# rel_relationship  
longm1$rel_relationshipc <- factor(longm1$rel_relationship, levels = c(0,1,2,3), labels = c("Singleton", "Sibling", "Twin","Triplet")) 
table (longm1$rel_relationshipc, exclude=NULL) 
table (longm1$rel_relationshipc, longm1$allmultc, exclude=NULL)

# create a acsflag =0 variable 
longm1$acsflag <- 0 
summary(longm1) 

# save imputed data to a RDS file 
saveRDS(longm1, file = "nov2018abcd_i.Rds")
new_test <- readRDS("Nov2018abcd_i.Rds")
names(new_test) 
summary(new_test)





