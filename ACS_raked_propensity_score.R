# Read in data
#################################################################################################### 
require(openxlsx)
my.path="/Users/rongzablocki/Desktop/ABCD/Downloads/";


dat.1=read.xlsx(paste0(my.path,"NDA2_Jan_29_2019_Demo.xlsx"),sheet=1)#Demo
dat.demo=dat.1[dat.1$redcap_event_name == "baseline_year_1_arm_1",]
dim(dat.demo)

dat.2 = read.xlsx(paste0(my.path,"NDA2_Jan_29_2019_ACS.xlsx"),sheet=1) #ACS
dat.acs = dat.2[dat.2$redcap_event_name == "baseline_year_1_arm_1",]
nrow(dat.acs) 

dat.2011_15=readRDS("ACS2011_15_I.Rds") #from Steve
nrow(dat.2011_15) #n=376370 

####################################################################################################

library(survey)
library(haven)
library(mice)

#Demographic
selected.vars=c("tlfb_age_calc_inmonths_l","demo_sex_v2b","demo_race_a_p___10","demo_race_a_p___11","demo_race_a_p___12","demo_race_a_p___13","demo_race_a_p___14","demo_race_a_p___15","demo_race_a_p___16","demo_race_a_p___17","demo_race_a_p___18","demo_race_a_p___19","demo_race_a_p___20","demo_race_a_p___21","demo_race_a_p___22","demo_race_a_p___23","demo_race_a_p___24","demo_race_a_p___25","demo_comb_income_v2b","demo_prnt_marital_v2b","demo_roster_v2","demo_prnt_empl_v2b","demo_prtnr_empl_v2b","demo_prnt_gender_id_v2b","demo_ethn_v2b","demo_race_a_p___99","demo_race_a_p___77")
newdemo=dat.demo[,c("id_redcap","site_name",selected.vars)]
nrow(newdemo)  

# set 777 and 999 to NA, a number of selected variables have these missing data codes  
newdemo[newdemo==999|newdemo==777]=NA
newdemo[,selected.vars]=sapply(newdemo[,selected.vars],as.numeric)

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
newdemo$race_eth<-NA;
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

######################################################################################### 
# ACS data set 
acs=dat.acs

# keep only key variables 
acs_s <- acs[,c("id_redcap", "rel_family_id", "rel_group_id", "rel_relationship", "rel_same_sex", "site_name")]
acs_s[,2:5]=sapply(acs_s[,2:5],as.numeric)
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

# fescabcd 
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

saveRDS(longm1, file = "Jan_2019abcd_i.Rds")
new_test <- readRDS("Jan_2019abcd_i.Rds")
names(new_test) 
summary(new_test)

#################################
# ACS_ABCD_Poolweight

library(haven)
acs11_15_i  <-dat.2011_15

# add 2 new variables to match abcd_oct18_i 
acs11_15_i$demo_prtnr_empl_v2b <- NA
acs11_15_i$rel_relationship <- NA
acs11_15_i$sitetwin <- NA

names(acs11_15_i) 
summary(acs11_15_i) 
str(acs11_15_i)

abcd_nov18_i  <- readRDS("Jan_2019abcd_i.Rds")
summary(abcd_nov18_i) 

# add PWGTP to abcd_nov18_i and subset to variables in ACS data for concatenation 
abcd_nov18_i$PWGTP <- NA

abcd_nov18_i_s <- abcd_nov18_i[c("PWGTP", "age", "sex", "region", "race_eth","faminc", "famtype", 
"hhsize","allmult","id_redcap", "site_name", "fesabcd", "acsflag", "demo_prtnr_empl_v2b", "rel_relationship", "sitetwin")] 
names(abcd_nov18_i_s) 
summary(abcd_nov18_i_s) 
str(abcd_nov18_i_s)

# stack ACS and ABCD data sets using rbind (row bind)  
nov2018_acs_abcd_i <- rbind(acs11_15_i, abcd_nov18_i_s) 
names(nov2018_acs_abcd_i) 
head(nov2018_acs_abcd_i)
summary(nov2018_acs_abcd_i)

# recode vars for logistic models
# age 
table(nov2018_acs_abcd_i$age, exclude=NULL) 
table(nov2018_acs_abcd_i$age, nov2018_acs_abcd_i$acsflag, exclude=NULL)

# fesabcd 
nov2018_acs_abcd_i$c_fesabcd <- nov2018_acs_abcd_i$fesabcd
nov2018_acs_abcd_i$c_fesabcd [nov2018_acs_abcd_i$fesabcd %in% c(5,7)] <- 5
nov2018_acs_abcd_i$c_fesabcd [nov2018_acs_abcd_i$fesabcd %in% c(6,8)] <- 6
table(nov2018_acs_abcd_i$c_fesabcd, nov2018_acs_abcd_i$acsflag, exclude =NULL) 

# c_race_eth 
nov2018_acs_abcd_i$c_race_eth <- nov2018_acs_abcd_i$race_eth
nov2018_acs_abcd_i$c_race_eth [nov2018_acs_abcd_i$race_eth %in% c(4,5,6,8)] <- 4
table(nov2018_acs_abcd_i$c_race_eth,nov2018_acs_abcd_i$acsflag, exclude =NULL) 
 
# weight for method 1
nov2018_acs_abcd_i$meth1wgt = nov2018_acs_abcd_i$PWGTP 
nov2018_acs_abcd_i$meth1wgt [nov2018_acs_abcd_i$acsflag==0] <- 1
summary(nov2018_acs_abcd_i$meth1wgt, exclude = NULL)

# create factor versions of variables 
# Multiple Birth
nov2018_acs_abcd_i$allmultc  <- factor(nov2018_acs_abcd_i$allmult, levels = c(0:1), labels = c("Single Birth", "Multiple Birth")) 
table (nov2018_acs_abcd_i$allmultc, exclude=NULL) 

# Race_Eth Factor 
nov2018_acs_abcd_i$race_ethc <- factor(nov2018_acs_abcd_i$race_eth, levels = c(1,2,3,4,5,6,8,9),
 labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian" ,"AIAN", "NHPI", "OTHER" ,"MULTIPLE")) 
table (nov2018_acs_abcd_i$race_ethc, exclude=NULL) 

# Collapsed Race Eth 
nov2018_acs_abcd_i$c_race_ethc <- factor(nov2018_acs_abcd_i$c_race_eth, levels = c(1,2,3,4,9),
 labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian AIAN NHPI OTHER" ,"MULTIPLE")) 
table (nov2018_acs_abcd_i$c_race_ethc, exclude=NULL) 
  
# Sex Factor 
nov2018_acs_abcd_i$sexc <- factor(nov2018_acs_abcd_i$sex, levels = c(1,2), labels = c("Male", "Female")) 
table (nov2018_acs_abcd_i$sexc, exclude=NULL) 

# Family income 
nov2018_acs_abcd_i$famincc <- factor(nov2018_acs_abcd_i$faminc, levels = c(1:6), 
 labels = c("<25k", "25k-49k", "50k-74k", "75k-99k", "100k-199k", "200k+")) 
table (nov2018_acs_abcd_i$famincc , exclude=NULL) 

# Family type 
nov2018_acs_abcd_i$famtypec  <- factor(nov2018_acs_abcd_i$famtype, levels = c(1:2), labels = c("Married", "Other Family")) 
table (nov2018_acs_abcd_i$famtypec , exclude=NULL) 

# age
nov2018_acs_abcd_i$age= as.factor(nov2018_acs_abcd_i$age)   
table (nov2018_acs_abcd_i$age, exclude=NULL) 

# HH size 
nov2018_acs_abcd_i$hhsizec  <- factor(nov2018_acs_abcd_i$hhsize, levels = c(1,4,5,6,7), 
  labels = c( "2-3 Persons", "4 Persons" ,"5 Persons", "6 Persons", "7 or More Persons")) 
table (nov2018_acs_abcd_i$hhsizec, exclude=NULL) 

# site twin 
nov2018_acs_abcd_i$sitetwinc <- factor(nov2018_acs_abcd_i$sitetwin, levels = c(0:1), 
 labels = c("General Cohort", "Site Twin Cohort")) 
table (nov2018_acs_abcd_i$sitetwinc, exclude=NULL) 

# rel_relationship  
nov2018_acs_abcd_i$rel_relationshipc <- factor(nov2018_acs_abcd_i$rel_relationship, levels = c(0,1,2,3), labels = c("Singleton", "Sibling", "Twin","Triplet")) 
table (nov2018_acs_abcd_i$rel_relationshipc, exclude=NULL) 

# fesabcd   
nov2018_acs_abcd_i$c_fesabcdc <- factor(nov2018_acs_abcd_i$c_fesabcd, levels = c(1,2,4,5,6), 
 labels = c("Married,2 in LF", "Married, 1 in LF", "Married, 0 in LF","Single HH, LF", "Single HH, NLF")) 
table (nov2018_acs_abcd_i$c_fesabcdc, exclude=NULL) 

# region  
nov2018_acs_abcd_i$regionc <- factor(nov2018_acs_abcd_i$region, levels = c(1,2,3,4), 
 labels = c("Northeast" ,"MidWest", "South", "West")) 
table (nov2018_acs_abcd_i$regionc, exclude=NULL) 

# tables of key vars by acsflag, with labels  
table (nov2018_acs_abcd_i$age, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$famincc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$famtypec, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$c_fesabcdc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$hhsizec, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$race_ethc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$c_race_ethc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$regionc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$sexc, nov2018_acs_abcd_i$acsflag, exclude=NULL) 
table (nov2018_acs_abcd_i$site_name, nov2018_acs_abcd_i$acsflag, exclude=NULL) 

##########################################################################################
# Models for weight preparation 

# Model 1 Propensity for Method 1 Weight

m1fit1 <- glm(relevel(factor(acsflag),ref='1') ~ I(age==9) 
 + relevel(factor(faminc),ref='6')
 + relevel(factor(famtype),ref='2')
 + relevel(factor(c_fesabcd),ref='6')
 + relevel(factor(hhsize),ref='7')
 + relevel(factor(c_race_eth),ref='9')
 + relevel(factor(region),ref='4')
 + relevel(factor(sex),ref='2'), 
 data=nov2018_acs_abcd_i, family=quasibinomial(), weights=meth1wgt)
summary(m1fit1)

# create new variables 
nov2018_acs_abcd_i$propmeth1 = predict(m1fit1, type="response") 
names(nov2018_acs_abcd_i)
summary(nov2018_acs_abcd_i$propmeth1) 

# assign different weight values depending on acsflag status 
nov2018_acs_abcd_i$pwgtmeth1=1/nov2018_acs_abcd_i$propmeth1
summary(nov2018_acs_abcd_i$pwgtmeth1) 

nov2018_acs_abcd_i$pwgtmeth1 <- ifelse(nov2018_acs_abcd_i$acsflag %in% (1), 
  nov2018_acs_abcd_i$PWGTP, nov2018_acs_abcd_i$pwgtmeth1) 
summary(nov2018_acs_abcd_i$pwgtmeth1)

# load psych package for use of descriptive tools 
library(psych)

describeBy(nov2018_acs_abcd_i$pwgtmeth1, nov2018_acs_abcd_i$acsflag)
quantile(nov2018_acs_abcd_i$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))

# by group of acsflag 
# acsflag==1
acssub <- subset(nov2018_acs_abcd_i,acsflag==1)
quantile(acssub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))

# acsflag==0 
abcdsub <- subset(nov2018_acs_abcd_i,acsflag==0)
quantile(abcdsub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))

# Note: the values for 2% and 98% should be 188 and 1653 for the NDA2 data, these would change with different data or models  

# Trim weight at .02 and .98 percentiles for acsflag==0 only, note these are manual entry from quantiles above
nov2018_acs_abcd_i$pwgtmeth1 [nov2018_acs_abcd_i$acsflag==0 & nov2018_acs_abcd_i$pwgtmeth1 < 188] <- 188
nov2018_acs_abcd_i$pwgtmeth1 [nov2018_acs_abcd_i$acsflag==0 & nov2018_acs_abcd_i$pwgtmeth1 > 1653] <- 1653

# Trimmed weights 
describeBy(nov2018_acs_abcd_i$pwgtmeth1, nov2018_acs_abcd_i$acsflag)

# Quantiles by acsflag 
acssub <- subset(nov2018_acs_abcd_i,acsflag==1)
quantile(acssub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))

abcdsub <- subset(nov2018_acs_abcd_i,acsflag==0)
quantile(abcdsub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))
# obtain sum of weight for abcd 
sum(abcdsub$pwgtmeth1)
 
# sum of weight for acs 
sum(acssub$pwgtmeth1) 

# Adjust to control total, note that the control value should be changed with each change in data (denominator)  

nov2018_acs_abcd_i$pwgtmeth1 <- ifelse(nov2018_acs_abcd_i$acsflag %in% c(0), 
(nov2018_acs_abcd_i$pwgtmeth1*8211605/8072664), nov2018_acs_abcd_i$pwgtmeth1) 

describeBy(nov2018_acs_abcd_i$pwgtmeth1, nov2018_acs_abcd_i$acsflag) 
 
################################################################################################

# Obtain weighted comparisons of pooled ABCD and weighted ACS Socio-demographics 
# use Survey package for weighted tables with svyby/svymean 
 
library(survey)

# set survey design with single psu for each respondent and weight, no strata or FPC 
svy <- svydesign(id=~1, weights=nov2018_acs_abcd_i$pwgtmeth1, data=nov2018_acs_abcd_i)

# tables of key vars by acsflag, with labels using trimmed and adjusted weight 
# weighted two way crosstab from Survey package using svyby and svymean
svyby(~age, ~acsflag, design=svy, svymean)
svyby(~famincc, ~acsflag, design=svy, svymean) 
svyby(~famtypec, ~acsflag, design=svy, svymean) 
svyby(~c_fesabcdc, ~acsflag, design=svy, svymean) 
svyby(~hhsizec, ~acsflag, design=svy, svymean) 
svyby(~c_race_ethc, ~acsflag, design=svy, svymean) 
svyby(~regionc, ~acsflag, design=svy, svymean) 
svyby(~sexc, ~acsflag, design=svy, svymean) 

# Regress weight on the propensity model covariates by acsflag

# for acs subset first 
wtfit_acs <- glm(pwgtmeth1 ~ I(age==9) 
 + relevel(factor(faminc),ref='6')
 + relevel(factor(famtype),ref='2')
 + relevel(factor(c_fesabcd),ref='6')
 + relevel(factor(hhsize),ref='7')
 + relevel(factor(c_race_eth),ref='9')
 + relevel(factor(region),ref='4')
 + relevel(factor(sex),ref='2'), 
 data=acssub)
summary(wtfit_acs)

# for abcd subset 
wtfit_abcd <- glm(pwgtmeth1 ~ I(age==9) 
 + relevel(factor(faminc),ref='6')
 + relevel(factor(famtype),ref='2')
 + relevel(factor(c_fesabcd),ref='6')
 + relevel(factor(hhsize),ref='7')
 + relevel(factor(c_race_eth),ref='9')
 + relevel(factor(region),ref='4')
 + relevel(factor(sex),ref='2'), 
 data=abcdsub)
summary(wtfit_abcd)

######################################################################################################
# create population total marginal data sets for rake of weight variable, age sex and collaped race 
# rake is done for ABCD weights using ACS population marginal totals as controls for age sex collapsed race/eth

library(Hmisc)  
# population data sets for raking 
pop.age <- data.frame(age=c(9,10),Freq=c(4074807,4136798)) 
pop.sex <- data.frame(sex=c(1,2),Freq=c(4205925,4005680))
pop.race <- data.frame(c_race_eth=c(1,2,3,4,9), Freq=c(4305552, 1101297, 1973827, 487673, 343256)) 
pop.age
pop.sex
pop.race

# survey design data for acbd only, set psu =1 and no strata, with weight 
svyabcd1 <- svydesign(id=~1, strata=NULL, weights=~pwgtmeth1, data=abcdsub)

# rake using population data above 
svyabcd_test_3  <- rake(svyabcd1, list(~age, ~sex, ~c_race_eth), list(pop.age,pop.sex,pop.race ))

# extract weight 
svyabcd_test_3$rpwgtmeth1 <-(weights(svyabcd_test_3))

# add to abcdsub data frame 
abcdsub$rpwgtmeth1 <- svyabcd_test_3$rpwgtmeth1 
names(abcdsub)

# check raked totals 
svytable (~age, svyabcd_test_3) 
svytable (~sex, svyabcd_test_3)
svytable (~c_race_eth, svyabcd_test_3)

# check with raked weight rpwgtmeth1 
wtd.table(abcdsub$age, weights=abcdsub$rpwgtmeth1) 
wtd.table(abcdsub$sex, weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$c_race_eth, weights=abcdsub$rpwgtmeth1)

# evaluate raked propensity method 1 weight by regressing on propensity model covariates
wtfit_rake <- glm(rpwgtmeth1 ~ pwgtmeth1 
 + I(age==9) 
 + relevel(factor(faminc),ref='6')
 + relevel(factor(famtype),ref='2')
 + relevel(factor(c_fesabcd),ref='6')
 + relevel(factor(hhsize),ref='7') 
 + relevel(factor(c_race_eth),ref='9')
 + relevel(factor(region),ref='4')
 + relevel(factor(sex),ref='2'), 
 data=abcdsub)
summary(wtfit_rake)
 
# weighted estimates of ABCD variables using raked propensity weight rpwgtmeth1 
wtd.table(abcdsub$age,weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$famincc,weights=abcdsub$rpwgtmeth1) 
wtd.table(abcdsub$famtypec,weights=abcdsub$rpwgtmeth1) 
wtd.table(abcdsub$c_fesabcdc,weights=abcdsub$rpwgtmeth1) 
wtd.table(abcdsub$hhsizec,weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$c_race_ethc,weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$regionc,weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$sexc,weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$site_name,weights=abcdsub$rpwgtmeth1)

# save data for future use
# save 24jan2019 data with revised weights including trim values and sum of weight in denominator for controls 
saveRDS(abcdsub, file= "Jan_2019abcd_R.Rds")

###################################################
check_abcd_r <- readRDS("Jan_2019abcd_R.Rds")

# Use saved data set for weighted tables after final raked weight is prepared 
# Set svydesign data 
svyr <- svydesign(data=check_abcd_r, id=~1, strata=NULL, weights=check_abcd_r$rpwgtmeth1) 

# weighted with raked weight 
svymean(~age, design=svyr)
svytotal(~age, svyr) 

svymean(~famincc, design=svyr) 
svytotal(~famincc, svyr) 

svymean(~famtypec, design=svyr) 
svytotal(~famtypec, svyr) 

svymean(~c_fesabcdc, design=svyr) 
svytotal(~c_fesabcdc, svyr) 

svymean(~hhsizec, design=svyr) 
svytotal(~hhsize, svyr) 

svymean(~c_race_ethc, design=svyr) 
svytotal(~c_race_ethc, svyr) 

svymean(~regionc, design=svyr) 
svytotal(~regionc, svyr) 

svymean(~sexc, design=svyr) 
svytotal(~sexc, svyr) 

svymean(~site_name, design=svyr) 
svytotal(~site_name, svyr) 

svymean(~sitetwin, design=svyr) 
svytotal(~sitetwin, svyr) 

# summary for final raked weight, 24jan2019 PB  
describeBy(check_abcd_r$rpwgtmeth1, check_abcd_r$acsflag)

summary(check_abcd_r$rpwgtmeth1) 



