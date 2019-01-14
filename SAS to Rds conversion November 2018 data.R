# Convert SAS files for November 2018 data to Rds data for input files used in weights process
# PB 14dec2018 

library(haven)

# read in ACS2011_15_I data prepared by SH from SAS   
acs11_15_i  <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/ACS2011_15_I.sas7bdat")
# save data in Rds format 
saveRDS(acs11_15_i, file = "ACS2011_15_I.Rds")
check1 <- readRDS("ACS2011_15_I.Rds")
names(check1) 
summary(check1)

# read in demo data from November 2018 SAS data set, then save for Wes T. as Rds file 
demo <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump/Nov2018_demo.sas7bdat")
str(demo) 
names(demo) 
summary(demo)

saveRDS(demo, file = "Nov2018_demo.Rds")
check2 <- readRDS("Nov2018_demo.Rds")
names(check2) 
str(check2)

# read in acs data from November 2018 SAS data set, then save for Wes as Rds file 
acs1 <- read_sas ("L:/projects/ABCD/02 Sampling/PB ABCD Analysis/November 2018 Wgts/Nov_2018_DataDump/Nov2018_ACS.sas7bdat")
names(acs1) 
summary(acs1) 

saveRDS(acs1, file = "Nov2018_ACS.Rds")
check3 <- readRDS("Nov2018_ACS.Rds")
names(check3) 
summary(check3) 
