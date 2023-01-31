# ME-CFS-Research

A sample compilation of scripts written during 2022 summer internship with ME/CFS Research Team at DePaul University's Center for Community Research. Please note that this repository does not contain completed projects, only select scripts meant to showcase technical skills. 

# DESCRIPTION

## dsq_functions.R

Functions for working with DePaul Symptom Questionnaire data. Includes functions for: data cleaning, pulling DSQ variables from data sets, calculating SF-36 scores, assessing whether respondent meets criteria for 4 major case definitions for ME/CFS based on DSQ responses.

For more information on the DSQ, please visit [this webpage](https://csh.depaul.edu/about/centers-and-institutes/ccr/myalgic-encephalomyelitis-cfs/Pages/measures.aspx).

## MECFS_diagnostics.R

Contains only functions from dsq_functions.R for assessing ME/CFS case definition criteria. 

## eRm_Rasch.R

Full script for running Rasch analysis on DePaul Symptom Questionnaire data. Includes sections for generating dichotomous and polytomous Rasch models, running item and person fit tests, assessing unidimensionality, local dependence, and separation reliability. 

## rasch_domains.R

Contains supplemental functions for eRm_Rasch.R for pulling DSQ variables for symptom domains and for automating stepwise elimination of questionnaire items based on item fit, unidimensionality, and local dependence. 

## Mono Study Script.Rmd

Full script for automating and processing REDCap data for ongoing study (as of Jan 2023) by Center for Community Research at DePaul University. Project consisted of consolidating old method of data processing (which consisted of 4 scripts in R and 4 in Visual Basic) into one concise script that could be used by those unfamiliar with coding. 

# DEPENDENCIES

+ [readr](https://www.rdocumentation.org/packages/readr/versions/2.1.3)
+ [magrittr](https://www.rdocumentation.org/packages/magrittr/versions/2.0.3)
+ [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/1.0.10)
+ [eRm](https://www.rdocumentation.org/packages/eRm/versions/1.0-2)
+ [WrightMap](https://www.rdocumentation.org/packages/WrightMap/versions/1.3)
+ [psych](https://www.rdocumentation.org/packages/psych/versions/2.2.9)
+ [data.table](https://www.rdocumentation.org/packages/data.table/versions/1.14.6)
+ [lubridate](https://www.rdocumentation.org/packages/lubridate/versions/1.9.0)
+ [tidyverse](https://www.rdocumentation.org/packages/tidyverse/versions/1.3.2)
+ [openxlsx](https://www.rdocumentation.org/packages/openxlsx/versions/4.2.5.1)

# LICENSE

This repository is made public for the sole purpose of showcasing previous work. None of the content herein may be used without explicit permission from the author (contact: jnmaciuch@gmail.com).  

