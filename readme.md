Propensity-based Population Weights for the ABCD Study

Author: Steven Heeringa and Patricia Berglund 

Date: 	 February 11, 2019	

Overview

The NDA2 release of the Adolescent Brain Cognitive Development (ABCD) baseline data includes a propensity-based weight that analysts can employ to compute descriptive estimates for the U.S. population of children age 9 and 10.  The methodology used to develop this weight variable is described in Elliott, M.R. and Richard L. Valliant. 2017 “Inference for nonprobability samples.” Statistical Science, 32(2):249-264.

Included in this repository are R script files and supporting data sets for the computation of the final propensity-based population weights for the ABCD Study baseline subjects.  

Key Input Files 

-ACS2011_15_I.Rds - This is the ACS 2011-2015 data set with the demographic (demo) and socio-economic (SES) variables that are used as the benchmark for the propensity weight estimation. The "I" suffix indicates that the missing data in this input file are already imputed with a single multivariate imputation. This file will be used as the input for the Step 2 program below. It serves as our permanent benchmark/reference for the population.
The data file, "ACS2011_15_I.Rds" is created directly from the American Community Survey 2011-2015 Public Use Microsample File. Some variables were recoded to make them consistent with ABCD demographic and SES constructs but the ACS data is in the public domain. The link to the ACS user guide and best citation is: https://www2.census.gov/programssurveys/acs/tech_docs/pums/ACS2011_2015_PUMS_README.pdf. In addition to some variable recoding, missing data in the original ACS 2011-2015 PUMS file was singly imputed using the SAS V9.4 MI procedure. 

-Demo.xlsx (not provided) - This is the baseline demographic data from ABCD study. If you have the access to ABCD report data dump, it is in the "ABCD 2 release Mental Health", "P Demographic" sheet. If you don't have access to ABCD report, you will have to go to: https://ndar.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL 
to find the required input variables.   

-ACS.xlsx (not provided) - This is a second data table from the ABCD study. If you have the access to ABCD report data dump, it is in the "ABCD 2 release Mental Health", "ACS" sheet. If you don't have access to ABCD report, you will have to go to:  https://ndar.nih.gov/data_dictionary.html?source=ABCD%2BRelease%2B2.0&submission=ALL 
to find variables required. This data is used only to identify the twin/multiple birth status of a child. In our pooled propensity weight, single births and twins will not be differentiated in the weighting process. 
 


R Script Files

ABCDPropWeight_V1_R.R

(Step 1):  Used to integrate selected variables from the demo.xlsx and ACS.xlsx.  Once data management is complete, the output data set contains a selected set of demographic and SES constructs that will be merged in Step 2 with the identical set from the ACS2011_15_I.Rds benchmark.  

(Step 2): Merges the 2011-2015 ACS, ACS2011_15_I.Rds, and the output from the Step 1 (with missing data imputed), estimates the propensity model, computes and scales/trims the propensity weights and finally rakes the scaled weights to final ACS control totals by age, sex and race/ethnicity. The final output file contains the final raked, propensity weight, labeled as "rpwgtmeth1" and subsequently renamed to “acs_raked_propensity_score” in the NDA2 release.

Note from authors

The intent is to share code that replicates the process (with some level of stochastic variance in the outcome). It is NOT intended to exactly replicate the weight provided in NDA2 release due to a few reasons:
-ACS2011_15_I.Rds (2011-2015 American Community Survey data) serves as a fixed population reference.  Anyone using the Github code will use this input.  However, replication by another user will require them to process the data from the ABCD data sets that they are provided access to.  

The critical features in the exact replication of the propensity-based population weighting by a future analyst are: 
1) missing data imputation replication will require the same order of case input, identical variables and coding and a fixed starting seed. The number of iterations and convergence criteria (e.g. logistic model) should also be fixed.
2) for propensity model estimation, the convergence criteria for the ML estimates should be fixed.  If not, there may be very minor differences in the final parameter estimates and the predicted propensities that are used to derive the weights.


