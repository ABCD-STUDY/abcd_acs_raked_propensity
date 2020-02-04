library(survey)
library(gamm4)

dat = readRDS("/path/to/nda2.0.1.Rds")
dat = dat[dat$eventname=="baseline_year_1_arm_1",]
#omit missing propensity scores
dat = dat[!is.na(dat$acs_raked_propensity_score),]

# set survey design with single psu for each respondent and weight, no strata or FPC
svyr <- svydesign(id=~1, weights=dat$acs_raked_propensity_score, data=dat, na.rm=T)

#example using propensity scores to obtain adjusted means and SEs
svymean(~ nihtbx_picvocab_uncorrected+nihtbx_totalcomp_uncorrected , design=svyr, na.rm=TRUE)

#example using propensity scores in gamm4
m = gamm4(nihtbx_totalcomp_uncorrected ~ nihtbx_picvocab_uncorrected +sex+race.4level, 
          random=~(1|site_id_l/rel_family_id), weights = dat$acs_raked_propensity_score,
          data = dat)

# plot(dat$acs_raked_propensity_score,c(svyr$allprob)$weights)





