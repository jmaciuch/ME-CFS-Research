# ---------------------------------------------
# Name:        eRm_rasch.R
# Author:      Jess Maciuch
# Date:        2022-06-24
# Description: Rasch model for DSQ data using eRm package
# ---------------------------------------------

#install.packages("eRm")
#install.packages("WrightMap")
#install.packages("readr")
#install.packages("psych")

require("eRm")
require("WrightMap")
require("readr")
require("psych")

#-------------------------------- Notes ---------------------------------#
# Martin-Loef test takes an extremely long time to process

# PCM() throws warning if not all possible responses are included
# for each variable (i.e. no one in the sample answered "4" for heavy14f)

# For some reason, difficulty scores for c1 of first item on list 
# is always excluded. Item is included on easiness scores and plot, though.
# Might be a problem for item fit tests. 

# Reference provided by:
# "Rasch Measurement Theory Analysis in R: Illustrations and 
# Practical Guidance for Researchers and Practitioners"

# By Stefanie Wind & Cheng Hua

# https://bookdown.org/chua/new_rasch_demo2/

#-------------------------- Data Preparation ----------------------------#

source("dsq_functions.R") # Pull useful DSQ functions
source("rasch_domains.R") # Pull functions for a priori domains and Rasch analysis

csv <- "Aggregate 6.22.20 test.csv"
data <- read_csv(csv)

# Filter only ME/CFS respondents(data$DATA groups 1-11)
data %<>% filter(data$DATA %in% c(1:11))

# Evaluate if respondent meets CCC case definition for ME/CFS (Takes a while to run)
CCC <- vector(mode = "logical")

for (i in 1:nrow(data)) {
  response <- data[i,]
  
  meets.case <- diagnose_CCC(response)
  
  CCC %<>% 
    append(., meets.case, after = length(.))
}

data %<>% cbind(CCC)

# Filter only ME/CFS cases 
data %<>% filter(data$CCC == TRUE)

# Clear environment
rm(meets.case, CCC, i, response)

# Un-comment one of the following options: 

  #dsq.data <- pull_dsq(data) # Pull all DSQ items
  #dsq.data <- neuro_domain(data) # Neurocognitive domain
  dsq.data <- PEM_domain(data) # PEM domain
  #dsq.data <- sleep_domain(data) # Sleep domain
  #dsq.data <- pain_domain(data) # Pain domain
  #dsq.data <- urinary_domain(data) #Genitourinary domain
  #dsq.data <- temp_domain(data) # Temperature regulation domain
  #dsq.data <- gastro_domain(data) # Gastrointestinal domain
  #dsq.data <- immune_domain(data) # Immune domain
  #dsq.data <- ortho_domain(data) # Orthostatic intolerance domain
  
# Determine item response rates (as percentage) 
# Note: Might be skewed because aggregate dataset contains both DSQ and DSQ-2
# respondents, and domains might contain DSQ-2 questions
print(response_rate(dsq.data))

## Run either the Partial Credit Model section or the Dichotomous Rasch Model
## section. Don't run both one after the other. 

#------------------------ Partial Credit Model -------------------------#
# If separating frequency and severity scores, un-comment on of the following:
  # Pull only frequency items
  dsq.data <- dsq.data[c(seq(from = 1, to = ncol(dsq.data), by = 2))]
  
  # Pull only severity items
  #dsq.data <- dsq.data[c(seq(from = 2, to = ncol(dsq.data), by = 2))]
  
# Remove rows with missing values (required for NPtest() function)
pre.filter <- nrow(dsq.data)

dsq.data %<>%   
  filter(!if_any(1:ncol(dsq.data), is.na))

cat("Before NA removal: n=", pre.filter, "\n", 
    "After NA removal: n=", nrow(dsq.data), "\n", sep = '')

rm(pre.filter)

# Reduce sample to 500 if needed
if (nrow(dsq.data) > 500) {
  dsq.data %<>% slice_sample(n = 500)
}

# Run the Partial Credit Model
dsq.model <- PCM(dsq.data)

# Check the result
summary(dsq.model)

# Plot person-item (i.e. Wright) map
plotPImap(dsq.model)

#---------------------- Dichotomous Rasch Model ------------------------#
# Remove rows with missing values (required for NPtest() function)
pre.filter <- nrow(dsq.data)

dsq.data %<>%   
  filter(!if_any(1:ncol(dsq.data), is.na))

cat("Before NA removal: n=", pre.filter, "\n", 
    "After NA removal: n=", nrow(dsq.data), "\n", sep = '')

rm(pre.filter)

# Reduce sample to 500 if needed
if (nrow(dsq.data) > 500) {
  dsq.data %<>% slice_sample(n = 500)
}

# Creating dichotomous variables

# Symptom marked as present ('1') or not present ('0') based on whether 
# respondent meets 2x2 threshold for frequency and severity

dsq.data <- symptom_present(dsq.data)

# Run the Rasch Model 
dsq.model <- RM(dsq.data)

# Check the result
summary(dsq.model)

# Plot person-item (i.e. Wright) map
plotPImap(dsq.model)

#---------------------------- Fit tests -------------------------------#

## Calculate item fit

  ppt.parameters <- person.parameter(dsq.model)
  item.fit <- itemfit(ppt.parameters)
  print(item.fit)
  
  # Stepwise item elimination
    # Checks for items with Outfit MSQ not in 0.7-1.3 range, tests if items can 
    # be brought into range by removing very unexpected responses. If not, removes items
    # and repeats until all items in acceptable range. 
  
  item.elimination <- stepwise_itemfit(dsq.model)
  
  # Save amended data after stepwise elimination
  dsq.data <- item.elimination$Data
  dsq.model <- item.elimination$Model

## Calculate person fit
  
  #Misfitting persons = Outfit & Infit z-score outside of -2 to 2 range
  #ppt.parameters <- person.parameter(dsq.model)
  person.fit <- personfit(ppt.parameters)
  print(person.fit)
  
  person.elimination <- stepwise_personfit(dsq.model)
  
  # Save amended data after stepwise elimination
  dsq.data <- person.elimination$Data
  dsq.model <- person.elimination$Model

## RASCH RELIABILITY (i.e. residual analysis)
  
  residual_analysis(dsq.model)
  
## PCA FOR UNIDIMENSIONALITY
  
  pca.resids <- PCA_residuals(dsq.model, sim.num = 200)
  
# MARTIN-LOEF TEST FOR UNIDIMENSIONALITY
  # Note: Function requires extremely long processing time. 
  # Other methods for testing unidimensionality preferred.
  
  #mloef <- NPtest(dsq.model$X01, method = "MLoef") 
  #print(mloef)
  
  # Clear environment
  #rm(mloef)
  
## LOCAL DEPENDENCE 
  # Q3 test
  # Checks for local dependence by detecting an increased correlation of 
  # inter-item residuals. 
  
  Q3.test <- Q3(dsq.model)
  
  # Prompting to drop items
  local_dep <- test_local_dep(dsq.model)
  
  dsq.data <- local_dep$Data
  dsq.model <- local_dep$Model

  # Explanation of T11 and T1 methods in Koller, I. & Hatzinger, R. (2013). 

  # T11 test
    # Global test for local dependence. The statistic calculates the sum of 
    # absolute deviations between the observed inter-item correlations and the 
    # expected correlations.
    
    # Check for p value < 0.01 (or different p value if justification exists)
    T11 <- NPtest(dsq.model$X01, method = "T11")
    print(T11)

  # Paired local dependence tests
  
    # Specify item pair inside list below
    locdep_items <- c('drained18', 'weakness33')
    locdep_data <- dsq.model$X[,c(locdep_items)] # Grabs data columns
  
    #T1 test
    # Checks for local dependence via increased inter-item correlations. 
    # For all item pairs, cases are counted with equal responses on both items.
    
    T1 <- NPtest(locdep_data, method = "T1") # Runs test
    print(T1)
    
    # Explanation of Q3l and Q3h in Debelak, R. & Koller, I. (2020).
    
    #Q3h test
    #Checks for [positive] local dependence by detecting an increased correlation of inter-item residuals. 
    #Low p-values correspond to a high ("h") correlation between two items.
    Q3h <- NPtest(locdep_data, method = "Q3h")
    print(Q3h)
    
    #Q3l test
    #Checks for [negative] local dependence by detecting a decreased correlation of inter-item residuals. 
    #Low p-values correspond to a low ("l") correlation between two items.
    Q3l <- NPtest(locdep_data, method = "Q3l")
    print(Q3l)

  
## ITEM CHARACTERISTIC CURVES (Polytomous models only!)
  plotICC(dsq.model)
  
## EMPIRICAL ITEM RESPONSE FUNCTIONS (Dichotomous models only!)
  ppt.parameters <- person.parameter(dsq.model)
  item.fit <- itemfit(ppt.parameters)
  std.resids <- item.fit$st.res
  
  for(item.number in 1:ncol(std.resids)){
    plotICC(dsq.model, item.subset = item.number, empICC = list("raw"), empCI = list())
  }
  
## ITEM SEPARATION RELIABILITY

  item.scores <- colSums(dsq.data, na.rm = TRUE)
  item.SD <- apply(dsq.data, 2, sd, na.rm = TRUE)
  item.SE <- item.SD/sqrt(length(item.SD))
  
  # Compute observed variance
  item.var <- var(item.scores) 
  
  # Compute mean square measurement error 
  item.MSE <- sum((item.SE)^2) / length(item.SE)
  
  # Compute separation reliability
  item.sep.rel <- (item.var - item.MSE) / item.var
  print(item.sep.rel)
  
  # Clear environment
  rm(item.scores, item.SD, item.SE, item.var, item.MSE, item.sep.rel)

## PERSON SEPARATION RELIABILITY 

  person.scores <- rowSums(dsq.data, na.rm = TRUE)
  person.SD <- apply(dsq.data, 1, sd, na.rm = TRUE)
  person.SE <- person.SD / sqrt(length(person.SD))
  
  # Compute observed variance
  person.var <- var(person.scores) 
  
  # Compute mean square measurement error 
  person.MSE <- sum((person.SE)^2) / length(person.SE) 
  
  # Compute separation reliability
  person.sep.rel <- (person.var - person.MSE) / person.var
  print(person.sep.rel)

# Clear environment
  rm(person.scores, person.SD, person.SE, person.var, person.MSE, person.sep.rel)
  
