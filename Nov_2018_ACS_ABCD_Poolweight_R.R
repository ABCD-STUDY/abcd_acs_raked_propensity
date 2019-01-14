# PB, 10dec2018: translate SAS to R for Step 2 of Weight Process, Weights Using Methods outlined Code 
# Read in November 2018 imputed data produced by Step 1 of process

library(haven)
# read in ACS2011_15_I data prepared by SH from SAS   
acs11_15_i  <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/ACS2011_15_I.sas7bdat")
# add 2 new variables to match abcd_oct18_i 
acs11_15_i$demo_prtnr_empl_v2b <- NA
acs11_15_i$rel_relationship <- NA
acs11_15_i$sitetwin <- NA

names(acs11_15_i) 
summary(acs11_15_i) 
str(acs11_15_i)

# read in Nov2018ABCD_I.Rds file prepared in Step 1 R Script  
abcd_nov18_i  <- readRDS("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump/Nov2018ABCD_I.Rds")
summary(abcd_nov18_i) 

# add PWGTP to abcd_oct18_i and subset to variables in ACS data for concatenation 
abcd_nov18_i$PWGTP <- NA

abcd_nov18_i_s <- abcd_nov18_i[c("PWGTP", "age", "sex", "region", "race_eth","faminc", "famtype", 
"hhsize","allmult","id_redcap", "site_name", "fesabcd", "acsflag", "demo_prtnr_empl_v2b", "rel_relationship", "sitetwin")] 
names(abcd_nov18_i_s) 
summary(abcd_nov18_i_s) 
str(abcd_nov18_i_s)

# stack ACS and ABCD data sets using rbind (row bind) for equivalent to set statement in SAS 
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
# HHsize as factor variable (continuous in SAS)  
m1fit <- glm(acsflag ~ factor(age) + factor(faminc) + factor(famtype) + factor(c_fesabcd) + 
 factor(hhsize) + factor(c_race_eth) + factor(region) + factor(sex), 
 data=nov2018_acs_abcd_i, family=quasibinomial(), weights=meth1wgt )
m1fit

# Use hhsize as factor variable, treated as continuous in SAS)
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

# Trim weight at .02 and .98 percentiles for acsflag==0 only, note these are manual entry from quantiles above 
nov2018_acs_abcd_i$pwgtmeth1 [nov2018_acs_abcd_i$acsflag==0 & nov2018_acs_abcd_i$pwgtmeth1 < 189] <- 189
nov2018_acs_abcd_i$pwgtmeth1 [nov2018_acs_abcd_i$acsflag==0 & nov2018_acs_abcd_i$pwgtmeth1 > 1660] <- 1660

# Trimmed weights 
describe.By(nov2018_acs_abcd_i$pwgtmeth1, nov2018_acs_abcd_i$acsflag)

# Quantiles by acsflag 
acssub <- subset(nov2018_acs_abcd_i,acsflag==1)
quantile(acssub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))

abcdsub <- subset(nov2018_acs_abcd_i,acsflag==0)
quantile(abcdsub$pwgtmeth1, c(0,.01,.02,.05,.25,.50,.75, .95,.98,.99,1.0))
# obtain sum of weight for abcd 
sum(abcdsub$pwgtmeth1) 

# Adjust to control total 
nov2018_acs_abcd_i$pwgtmeth1 <- ifelse(nov2018_acs_abcd_i$acsflag %in% c(0), 
(nov2018_acs_abcd_i$pwgtmeth1*8211605/8074034), nov2018_acs_abcd_i$pwgtmeth1) 

describeBy(nov2018_acs_abcd_i$pwgtmeth1, nov2018_acs_abcd_i$acsflag)
# Good, similar to results from SAS 13dec2018  

################################################################################################

# Obtain weighted comparisons of pooled ABCD and weighted ACS Socio-demographics 
# use Survey package for weighted tables with svyby/svymean 
 
library(survey)
# set survey design with single psu for each respondent and weight, no strata or FPC 
svy <- svydesign(id=~1, weights=nov2018_acs_abcd_i$pwgtmeth1, data=nov2018_acs_abcd_i)
summary(svy)

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

# ok matches SAS output 6dec2018 similar to SAS but small differences in ABCD since weight is not exactly the same plus the
# imputation of key variables is slightly different 

# Regress weight on the propensity model covariates by acsflag, note that SAS treats 
# hhsize as a continuous variable so not an exact match 

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
#note: slightly different results as from SAS due to NA for c_fesabcd category 4

######################################################################################################
# create population total marginal data sets for rake of weight variable, age sex and collaped race 
# rake is done for ABCD weights using ACS population marginal totals as controls for age sex collapsed race/eth
# have a subset of ABCD cases called abcdsub

library(Hmisc)  
# tables of abcd age, sex and collapsed race
wtd.table(abcdsub$age, weights=abcdsub$pwgtmeth1) 
wtd.table(abcdsub$sex, weights=abcdsub$pwgtmeth1) 
wtd.table(abcdsub$c_race_eth, weights=abcdsub$pwgtmeth1)

# population data sets for raking 
pop.age <- data.frame(age=c(9,10),Freq=c(4074807,4136798)) 
pop.sex <- data.frame(sex=c(1,2),Freq=c(4205925,4005680))
pop.race <- data.frame(c_race_eth=c(1,2,3,4,9), Freq=c(4305552, 1101297, 1973827, 487673, 343256)) 
pop.age
pop.sex
pop.race

# survey design data for acbd only, set psu =1 and no strata, with weight 
svyabcd1 <- svydesign(id=~1, strata=NULL, weights=~pwgtmeth1, data=abcdsub)

# totals before rake 
svytable (~age, svyabcd1) 
svytable (~sex, svyabcd1)
svytable (~c_race_eth, svyabcd1)  

# rake age sex c_race_eth individually 
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

# double check with pwgtmeth1 and then rpwgtmeth1 
wtd.table(abcdsub$age, weights=abcdsub$pwgtmeth1) 
wtd.table(abcdsub$sex, weights=abcdsub$pwgtmeth1)
wtd.table(abcdsub$c_race_eth, weights=abcdsub$pwgtmeth1)

wtd.table(abcdsub$age, weights=abcdsub$rpwgtmeth1) 
wtd.table(abcdsub$sex, weights=abcdsub$rpwgtmeth1)
wtd.table(abcdsub$c_race_eth, weights=abcdsub$rpwgtmeth1)

# create a difference between the raked and original versions of pwgtmeth1 
abcdsub$rpwgtdiff <- (abcdsub$rpwgtmeth1-abcdsub$pwgtmeth1)
summary(abcdsub$rpwgtmeth1) 
summary(abcdsub$rpwgtdiff) 

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
# note slight differences due to way R handles some covariates that are singular 

# unweighted estimates of ABCD variables 
table(abcdsub$age)
table(abcdsub$famincc) 
table(abcdsub$famtypec) 
table(abcdsub$c_fesabcdc) 
table(abcdsub$hhsizec) 
table(abcdsub$c_race_ethc)
table(abcdsub$regionc)
table(abcdsub$sexc)
table(abcdsub$site_name)
 
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
saveRDS(abcdsub, file = "Nov2018abcd_R.Rds")
check_abcd_r <- readRDS("Nov2018abcd_R.Rds")
names(check_abcd_r) 
summary(check_abcd_r)

# Use saved data set for weighted tables after final raked weight is prepared 
library(survey)
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










