# ---------------------------------------------
# Name:        rasch_domains.R
# Author:      Jess Maciuch
# Date:        2022-07-23
# Description: Functions for pulling a priori symptom domains
# ---------------------------------------------

require(dplyr)
require(magrittr)
require(eRm)
require(psych)
source("dsq_functions.R")

#--------------------------------------------------------------------------#
## Function: neuro_domain

# Purpose: pull conceptual a priori neurocognitive domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

neuro_domain <- function(dataset) {
  names <- c("remember36f",
             "remember36s",
             "difficulty37f",
             "difficulty37s",
             "word38f",
             "word38s",
             "understanding39f",
             "understanding39s",
             "focus40f",
             "focus40s",
             "slowness43f",
             "slowness43s",
             "noise34f",
             "noise34s",
             "lights35f",
             "lights35s",
             "smells66f",
             "smells66s",
             "unable41fa_dsq2",
             "unable41sa_dsq2",
             "unable41fb_dsq2",
             "unable41sb_dsq2",
             "depth42f",
             "depth42s",
             "twitches32f",
             "twitches32s",
             "absent44f",
             "absent44s",
             "disoriented76f",
             "disoriented76s",
             "dyslexiaonset78f",
             "dyslexiaonset78s",
             "slowedspeech77f",
             "slowedspeech77s",
             "eyepain79f",
             "eyepain79s")
  
  output <- subset(dataset, select = c(colnames(dataset) %in% names))
  output <- clean_dsq(output)

  return(output)
}

#--------------------------------------------------------------------------#
## Function: PEM_domain

# Purpose: pull conceptual a priori post exertional malaise domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

PEM_domain <- function(dataset) {
  names <- c("heavy14f",
             "heavy14s",
             "soreness15f",
             "soreness15s",
             "mental16f",
             "mental16s",
             "minimum17f",
             "minimum17s",
             "drained18f",
             "drained18s",
             "fatigue13f",
             "fatigue13s",
             "musclepain25f",
             "musclepain25s",
             "weakness33f",
             "weakness33s",
             "musclefatigue73f",
             "musclefatigue73s",
             "worsemildphys74f",
             "worsemildphys74s",
             "worsemildmental75f",
             "worsemildmental75s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: sleep_domain

# Purpose: pull conceptual a priori sleep domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

sleep_domain <- function(dataset) {
  names <- c("unrefreshed19f",
             "unrefreshed19s",
             "nap20f",
             "nap20s",
             "falling21f",
             "falling21s",
             "staying22f",
             "staying22s",
             "early23f",
             "early23s",
             "allday24f",
             "allday24s",
             "daydrowsy82f",
             "daydrowsy82s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: pain_domain

# Purpose: pull conceptual a priori pain domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

pain_domain <- function(dataset) {
  names <- c("musclepain25f",
             "musclepain25s",
             "jointpain26f",
             "jointpain26s",
             "eyepain27f",
             "eyepain27s",
             "chestpain28f",
             "chestpain28s",
             "stomach30f",
             "stomach30s",
             "headaches31f",
             "headaches31s",
             "painsensitive80f",
             "painsensitive80s",
             "pressurepain81f",
             "pressurepain81s"
             )
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: urinary_domain

# Purpose: pull conceptual a priori genitourinary domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

urinary_domain <- function(dataset) {
  names <- c("bladder45f",
             "bladder45s",
             "urgenturin86f",
             "urgenturin86s",
             "nighturin87f",
             "nighturin87s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: temp_domain

# Purpose: pull conceptual a priori temperature regulation domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

temp_domain <- function(dataset) {
  names <- c("htemp59f",
             "htemp59s",
             "ltemp60f",
             "ltemp60s",
             "hot58f",
             "hot58s",
             "chills57f",
             "chills57s",
             "limbs56f",
             "limbs56s",
             "night55f",
             "night55s",
             "sweating54f",
             "sweating54s",
             "tempintolerance71f",
             "tempintolerance71s",
             "tempflux89f",
             "tempflux89s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: gastro_domain

# Purpose: pull conceptual a priori gastrointestinal domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

gastro_domain <- function(dataset) {
  names <- c("bloating29f",
             "bloating29s",
             "stomach30f",
             "stomach30s",
             "bowel46f",
             "bowel46s",
             "nausea47f",
             "nausea47s",
             "weight52fa__dsq2",
             "weight52sa__dsq2",
             "weight52fb__dsq2",
             "weight52sb__dsq2",
             "appetite53f",
             "appetite53f"
             )
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: immune_domain

# Purpose: pull conceptual a priori immune domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()
immune_domain <- function(dataset) {
  names <- c("sorethroat62f",
             "sorethroat62s",
             "lymphnodes63f",
             "lymphnodes63s",
             "fever64f",
             "fever64s",
             "flu65f",
             "flu65s",
             "viralrec72f",
             "viralrec72s",
             "sinusinfect85f",
             "sinusinfect85s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: ortho_domain

# Purpose: pull conceptual a priori orthostatic intolerance domain

# Input: "dataset" must be a data frame or matrix
# e.g. produced by read_csv()

ortho_domain <- function(dataset) {
  names <- c("unsteady48f",
             "unsteady48s",
             "shortness49f",
             "shortness49s",
             "dizz50f",
             "dizz50s",
             "irregular51f",
             "irregular51s",
             "heartbeat67f",
             "heartbeat67s",
             "orthovis68f",
             "orthovis68s",
             "orthomem69f",
             "orthomem69s",
             "disoriented76f",
             "disoriented76s",
             "poorcoord84f",
             "poorcoord84s",
             "uprighttolerations88f",
             "uprighttolerations88s")
  
  output <- names %>%
    all_of(.) %>%
    select(dataset, .) %>%
    clean_dsq
  
  return(output)
}

#--------------------------------------------------------------------------#
## Function: Q3

# Purpose: Run Q3 test for local dependence

# Input: "model" must be an eRm object (output from PCM() or RM())

# Code for calculating Q3 values taken from "Q3h" method of NPtest() function
# in the eRm package. 
# https://rdrr.io/cran/eRm/src/R/NPtest.R

Q3 <- function(model) {
  ppt.parameters <- person.parameter(model)
  std.resids <- residuals(ppt.parameters)
  
  #model.prob <- pmat(ppt.parameters)
  #residuals <- model$X01 - model.prob
  
  # Code for calculating Q3 values taken from "Q3h" method of NPtest() function
  # in the eRm package. 
  # https://rdrr.io/cran/eRm/src/R/NPtest.R
  
  calcQ3h.stat <- function(resids) { ## Calculates Q3h based on observed matrix and expected values
    # Calculates Q3
    i <- ncol(resids)
    mat <- resids
    res <- matrix(nrow=i,ncol=i)
    for(a in 1:(i-1)) {
      for(b in (a+1):i) {
        res[b,a] <- res[a,b] <- cor(mat[,a],mat[,b], method = "pearson", use = "na.or.complete")
        # Added "pearson" method specification -JM
        
        # Removed the negative sign before cor() because I have no idea why the Q3 statistic should be negative
        # Might have been a mistake -JM
      }
    }
    return(res)
  }
  
  # Calculate Q3 values for empirical data
  Q3.mat <- calcQ3h.stat(std.resids)
  
  # Put item names on Q3 matrix
  item.names <- colnames(std.resids)
  colnames(Q3.mat) <- item.names
  rownames(Q3.mat) <- item.names
  
  # Calculate critical value for Q3 (Q3_max - Q3_avg) and return locally dependent pairs
  # Based on Christensen K. B., Makransky G., Horton M. (2017)
  crit.val <- function(res) {
    all.Q3 <- res[which(upper.tri(res), arr.ind = TRUE)] # Pulls upper triangle of Q3 matrix
    Q3.avg <- mean(all.Q3)
    Q3.max <- all.Q3[which.max(all.Q3)]
    Q3.crit <- Q3.avg + 0.2
    #Q3.crit <- Q3.max - Q3.avg
    
    violations <- data.frame("item 1" = NA,
                             "item 2" = NA,
                             "Q3 value" = NA)
    
    for (i in 2:nrow(res)) {
      for (j in 1:(i-1)) {
        if (isTRUE(res[i, j] > Q3.crit)) {
          entry <- c(rownames(res)[i],
                     colnames(res)[j],
                     res[[i, j]]
          )
          
          violations[nrow(violations) + 1,] <- entry
        }
      }
    }
    
    violations <- violations[-c(1),] # Removes placeholder first row
    Q3.crit.val <- paste("Q3 critical value =", Q3.crit, sep = " ")
    
    if (nrow(violations) == 0) {
      result <- list(Q3.crit.val, "No locally dependent pairs found")
    } else {
      result <- list(Q3.crit.val = Q3.crit.val, violations = violations)
    }
    
    return(result)
  }
  
  dep_pairs <- crit.val(Q3.mat)
  print(dep_pairs)
  return(dep_pairs)
}

#--------------------------------------------------------------------------#
## Function: stepwise_itemfit

# Purpose: Test items that fall outside of acceptable item range, and remove from
# data set if removing unexpected responses does not fix item fit. 

# Input: "model" must be an eRm object (output from PCM() or RM())

stepwise_itemfit <- function(model) {
  
  # Returns list of items with Outfit MSQ score outside 0.7-1.3,
  # item fit statistics, and standardized residuals of misfitting items.
  check_itemfit <- function(model) {
    # Calculate person parameters
    ppt.parameters <- person.parameter(model)
    
    # Calculate item fit
    item.fit <- itemfit(ppt.parameters)
    
    # Check if MSQ scores within 0.7-1.3 range
    outfit.score <- item.fit$i.outfitMSQ
    drop.items <- outfit.score[which(!between(outfit.score, 0.7, 1.3))]
    drop.items <- names(drop.items)
    
    std.resids <- item.fit$st.res
    misfit.resids <- std.resids[, c(drop.items)]
    
    result <- list(drop.items, item.fit, std.resids, misfit.resids)
    names(result) <- c("drop.items", "itemfit", "std.resids", "misfit.resids")
    return(result)
  }
  
  item.stats <- check_itemfit(model)
  drop.items <- item.stats$drop.items
  item.fit <- item.stats$itemfit
  
  # Experiment with removing misfitting person responses.
    # For each misfitting item, grab participants with highest residuals 
    # (i.e. most abnormal response pattern for question).
    # Replace misfitting responses, see if item fit moves into acceptable range.
    # If not, remove item.
  # Repeat process until no more misfitting items remain
round <- 1  
  while (length(drop.items) > 0) {
    cat("\n", "##################################", "\n", 
        "Item Fit Testing", "\n", 
        "Round ", round, "\n", 
        "##################################", "\n", sep = "")
    
    # Grab standardized residuals for misfitting items
    std.resids <- item.stats$std.resids
    misfit.resids <- item.stats$misfit.resids
    
    # Make copy of data for item fit testing
    data <- as.data.frame(model$X)
    test.data <- data
    
    # Get all individual responses where abs value of z-residual > 2
    for (item in 1:length(drop.items)) {
      col <- drop.items[item]
      item.resids <- std.resids[, col]
      
      # Note: It seems like on the first loop ppt #'s begin with "P", but are only numbers on subsequent loops
      ppts <- c()
      for (resid in 1:length(item.resids)) {
        if (isTRUE(abs(item.resids[[resid]]) > 2)) {
          ppt.num <- names(item.resids)[resid]
          ppt.num <- gsub("P", "", ppt.num) # If there is a "P" in ppt.num (i.e. "P23"), remove it.
          ppts <- append(ppts, ppt.num)
        }
      }
      
      # Replace misfitting response with NA in test data set
      for (p in 1:length(ppts)) {
        row <- ppts[p]
        test.data[row, col] <- NA
      }
    }
    
    # Recalculate model to see effect on item fit
    if (model$model == "PCM") {
      test.model <- PCM(test.data)
    } else if (model$model == "RM") {
      test.model <- RM(test.data)
    }
    
    # Check for misfitting items
    test.item.stats <- check_itemfit(test.model)
    test.drop.items <- test.item.stats$drop.items
    test.item.fit <- test.item.stats$itemfit
    
    test.names <- c("Outfit MSQ", "Infit MSQ", "Outfit t", "Infit t")
    before.test <- cbind(item.fit$i.outfitMSQ[c(drop.items)],
                         item.fit$i.infitMSQ[c(drop.items)],
                         item.fit$i.outfitZ[c(drop.items)],
                         item.fit$i.infitZ[c(drop.items)])
    
    after.test <- cbind(test.item.fit$i.outfitMSQ[c(drop.items)],
                          test.item.fit$i.infitMSQ[c(drop.items)],
                          test.item.fit$i.outfitZ[c(drop.items)],
                          test.item.fit$i.infitZ[c(drop.items)])
      
    colnames(before.test) <- test.names
    colnames(after.test) <- test.names
    
    cat("\n", "Misfitting items:", "\n", drop.items, "\n", sep = " ")
    cat("\n", "Before removing misfitting responses:", "\n", sep = "")
    print(before.test)
    cat("\n", "After removing misfitting responses:", "\n", sep = "")
    print(after.test)
    
    # See which items were fixed by removing misfitting responses
    fixed.misfit <- which(!drop.items %in% test.drop.items)
    fixed.misfit <- drop.items[c(fixed.misfit)]
    
    # See which items are still misfitting
    still.misfit <- which(drop.items %in% test.drop.items)
    still.misfit <- drop.items[c(still.misfit)]
    
    # If fixed, replace data with altered data
    if (length(fixed.misfit) > 0) {
      for (fix in 1:length(fixed.misfit)) {
        new.data <- test.data[, fixed.misfit[fix]]
        data[fixed.misfit[fix]] <- new.data
      }
      cat("\n", "Items fixed by misfitting data removal:", fixed.misfit, "\n", sep = " ")
    }
    
    # If not fixed, remove misfitting items
    if (length(still.misfit) > 0) {
      data %<>% subset(select = !(names(data) %in% still.misfit))
      cat("\n", "Items dropped from data set:", still.misfit, "\n", sep = " ")
    }
    
    # Recalculate model with amended data
    if (model$model == "PCM") {
      model <- PCM(data)
    } else if (model$model == "RM") {
      model <- RM(data)
    }
    
    item.stats <- check_itemfit(model)
    drop.items <- item.stats$drop.items
    item.fit <- item.stats$itemfit
    
    round <- round + 1
  }
  
 result <- list(data, model)
 names(result) <- c("Data", "Model")
 return(result)
}

#--------------------------------------------------------------------------#
## Function: stepwise_personfit

# Purpose: Test persons that exhibit high z-scores (i.e. outside -2 to 2 range)
# by removing unexpected responses. If removing unexpected responses does not bring 
# person into acceptable range, remove person from data set. 

# Input: "model" must be an eRm object (output from PCM() or RM())

stepwise_personfit <- function(model) {
  
  # Returns list of persons with Outfit and Infit z-scores outside -2 to 2 range,
  # person fit statistics, and std. residuals of misfitting persons.
  check_personfit <- function(model) {
    # Calculate person parameters
    ppt.parameters <- suppressWarnings(person.parameter(model))
    
    # Calculate person fit
    person.fit <- personfit(ppt.parameters)
    
    # Check if Outfit and Infit z-scores within -2 to 2 range
    outfit.t <- person.fit$p.outfitZ
    infit.t <- person.fit$p.infitZ
    
    person.stats <- rbind(outfit.t, infit.t)
    drop.persons <- colnames(person.stats)[which(!between(outfit.t, -2, 2) | !between(infit.t, -2, 2))]
    misfit.person.stats <- person.stats[, c(drop.persons)]
    
    std.resids <- person.fit$st.res
    misfit.resids <- std.resids[c(drop.persons),]
    
    result <- list(drop.persons, person.fit, std.resids, misfit.resids)
    names(result) <- c("drop.persons", "personfit", "std.resids", "misfit.resids")
    return(result)
  }
  
  person.stats <- check_personfit(model)
  drop.persons <- person.stats$drop.persons
  person.fit <- person.stats$personfit
  
  # Experiment with removing misfitting person responses.
  # For each misfitting person, grab items with highest residuals 
  # (i.e. most abnormal response pattern for question).
  # Replace misfitting responses, see if person fit moves into acceptable range.
  # If not, remove person from data set. 
  # Repeat process until no more misfitting persons remain.
  
  round <- 1  
  while (length(drop.persons > 0)) {
    cat("\n", "##################################", "\n", 
        "Person Fit Testing", "\n", 
        "Round ", round, "\n", 
        "##################################", "\n", sep = "")
    
    cat("\n", length(drop.persons), "misfitting persons identified", "\n\n", sep = " ")
    
    # Grab standardized residuals for misfitting items
    std.resids <- person.stats$std.resids
    misfit.resids <- person.stats$misfit.resids
    
    # Make copy of data for item fit testing
    data <- as.data.frame(model$X)
    test.data <- data
    
    # Replace highly unexpected responses with NA
    tracking <- matrix(nrow = length(drop.persons), ncol = 1, 
                       byrow = TRUE, dimnames = list(c(drop.persons), c("# responses removed")))
    
    for (person in 1:length(drop.persons)) {
      row <- drop.persons[person]
      person.resids <- std.resids[row, ]
      
      col <- c()
      for (resid in 1:length(person.resids)) {
        if (isTRUE(abs(person.resids[[resid]]) > 3)) {
          col <- append(col, names(person.resids[resid]))
        }
      }
      
      tracking[row, 1] <- length(col)
      
      # Replace misfitting response with NA in test data set
      for (p in 1:length(col)) {
        test.data[row, col] <- NA
      }
    }
    print(tracking)
    
    # Recalculate model to see effect on person fit
    if (model$model == "PCM") {
      test.model <- suppressWarnings(PCM(test.data))
    } else if (model$model == "RM") {
      test.model <- suppressWarnings(RM(test.data))
    }
    
    # Check for misfitting persons in test data
    test.person.stats <- check_personfit(test.model)
    test.drop.persons <- test.person.stats$drop.persons
    test.person.fit <- test.person.stats$personfit
    
    # See which persons were fixed by removing misfitting responses
    fixed.misfit <- which(!drop.persons %in% test.drop.persons)
    fixed.misfit <- drop.persons[c(fixed.misfit)]
    
    # See which persons are still misfitting
    still.misfit <- which(drop.persons %in% test.drop.persons)
    still.misfit <- drop.persons[c(still.misfit)]
    
    # If fixed, replace data with altered data
    if (length(fixed.misfit) > 0) {
      for (fix in 1:length(fixed.misfit)) {
        new.data <- test.data[fixed.misfit[fix],]
        data[fixed.misfit[fix],] <- new.data
      }
      cat("\n", length(fixed.misfit), "persons fixed by misfitting data removal", "\n", sep = " ")
    }
    
    # If not fixed, remove misfitting items
    if (length(still.misfit) > 0) {
      keep <- which(!rownames(data) %in% still.misfit)
      data <- data[c(keep),]
      cat("\n", length(still.misfit), "persons dropped from data set", "\n", sep = " ")
    }
    
    # Recalculate model with amended data
    if (model$model == "PCM") {
      model <- suppressWarnings(PCM(data))
    } else if (model$model == "RM") {
      model <- suppressWarnings(RM(data))
    }
    
    person.stats <- check_personfit(model)
    drop.persons <- person.stats$drop.persons
    round <- round + 1
  }
  
  result <- list(data, model)
  names(result) <- c("Data", "Model")
  return(result)
}

#--------------------------------------------------------------------------#
## Function: residual_analysis

# Purpose: Conducting residual analysis to determine unidimensionality 

# Input: "model" must be an eRm object (output from PCM() or RM())

# NOTE: # 0.2 = minimum requirement for unidimensionality (cf. Reckase 1979)
# Interpretation: percentage of raw variance that can be explained by 
# person and item measures

residual_analysis <- function(model) {
  # Get probability of "correct" ppt responses based on model
  ppt.parameters <- person.parameter(model)
  model.prob <- pmat(ppt.parameters)
  
  data <- model$X
  
  # Check if any extreme scores, remove ppts and recalculate model + participant parameters
  if(!nrow(data) == nrow(ppt.parameters$X.ex)) {
    ind <- ppt.parameters$pers.ex # Indices of extreme person responses
    
    cat(length(ind), "participants dropped from data set for extreme response pattern.", "\n", sep = " ")
    
    data <- data[-c(ind),]
    
    if (model$model == "PCM") {
      model <- PCM(data)
    } else if (model$model == "RM") {
      model <- RM(data)
    }
    
    ppt.parameters <- person.parameter(model)
    item.fit <- itemfit(ppt.parameters)
    model.prob <- pmat(ppt.parameters)
  }
  
  # Calculate residuals of dichotomized data
  residuals <- model$X01 - model.prob
  
  # Residual-based fit analysis per Linacre (2003)
  observations <- as.vector(model$X01)
  VO <- var(observations, na.rm = TRUE)
  
  res.vec <- as.vector(residuals)
  VR <- var(res.vec, na.rm = TRUE)
  
  # Raw Variance explained by Rasch measures
  result <- (VO -VR)/VO
  cat("\n", "Guidelines:",
      "\n", "> 0.2 minimum requirement for unidimensionality",
      "\n", "> 0.4 considered 'good'", "\n", sep = " ")
  cat("\n", "Variance explained by Rasch measures:", "\n", sep = " ")
  print(result)
}

#--------------------------------------------------------------------------#
## Function: PCA_residuals

# Purpose: Conducting principal component of residuals to determine unidimensionality 

# Input: "model" must be an eRm object (output from PCM() or RM())

PCA_residuals <- function(model, sim.num) {
  # Get standardized residuals 
  ppt.parameters <- person.parameter(model)
  item.fit <- itemfit(ppt.parameters)
  std.resids <- item.fit$st.res
  
  # Run the principal component analysis
  pca <- pca(std.resids, rotate = "none")
  cat("Principal component analysis of residuals", "\n", sep = "")
  print(pca$loadings)
  
  # If number of simulations not specified, run 200
  if (!exists("sim.num", inherits = FALSE)) {
    sim.num <- 200
  }
  
  # Run specified number of simulations, store eigenvalues of first 5 components
  for (num in 1:sim.num) {
    sim.data <- sim.rasch(ncol(dsq.data), nrow(dsq.data))
    sim.data <- sim.data$items
    
    if (dsq.model$model == "PCM") {
      sim.model <- PCM(sim.data)
    } else if (dsq.model$model == "RM") {
      sim.model <- RM(sim.data)
    }
    
    sim.ppt.parameters <- person.parameter(sim.model)
    sim.item.fit <- itemfit(sim.ppt.parameters)
    sim.std.resids <- sim.item.fit$st.res
    
    sim.pca <- pca(sim.std.resids, rotate = "none")
    
    if (num == 1) {
      sim.eigen <- data.frame(sim.pca$values[1:5])
    } else {
      sim.eigen %<>% cbind(sim.pca$values[1:5])
    }
    colnames(sim.eigen)[num] <- num
  }
  
  # Calculate mean eigenvalues of all simulations
  mean.values <- vector()
  median.values <- vector()
  
  for (row in 1:nrow(sim.eigen)) {
    sim.mean <- mean(as.numeric(sim.eigen[row,]))
    mean.values %<>% c(sim.mean)
    
    sim.median <- median(as.numeric(sim.eigen[row,]))
    median.values %<>% c(sim.median)
  }
  
  # Compare values (Eigenvalue of empirical first contrast must be less than or 
  # equal to mean of simulated eigenvalue for same contrast
  # https://www.rasch.org/rmt/rmt191h.htm
  
  pca.comparison <- suppressWarnings(cbind(mean.values, median.values, pca$values))
  colnames(pca.comparison) <- c("Mean Sim Data", "Median Sim Data", "Empirical Data")
  
  cat("\n", "Empirical eigenvalues should be equivalent or less than mean simulated values.", "\n", sep = "")
  print(pca.comparison)
}

#--------------------------------------------------------------------------#
## Function: resolve_local_dep

# Purpose: Print comparison of model fit stats to facilitate resolving local dependence

# Input: "model" must be an eRm object (output from PCM() or RM())

resolve_local_dep <- function(model) {
  
  data <- model$X
  Q3.test <- Q3(model)
  print(Q3.test)
  
  dependent.pairs <- Q3.test$violations[, 1:2]
  
  if (nrow(dependent.pairs) < 1) {
    stop("No locally dependent items found in data set.")
  }
  
  drop.list <- c()
  
  for (p in 1:nrow(dependent.pairs)) {
    pair <- dependent.pairs[p, ]
    item1 <- pair[[1]]
    item2 <- pair[[2]]
    
    test.data.1 <- data %>% subset(select = !colnames(data) %in% item1)
    test.data.2 <- data %>% subset(select = !colnames(data) %in% item2)
    
    if (model$model == "PCM") {
      model.1 <- PCM(test.data.1)
      model.2 <- PCM(test.data.2)
      plotICC(model, item.subset = item1)
      plotICC(model, item.subset = item2)
    } else if (model$model == "RM") {
      model.1 <- RM(test.data.1)
      model.2 <- RM(test.data.2)
      plotICC(model, item.subset = item1, empICC = list("raw"), empCI = list())
      plotICC(model, item.subset = item2, empICC = list("raw"), empCI = list())
    }
    
    ppt.parameters.1 <- suppressWarnings(person.parameter(model.1))
    ppt.parameters.2 <- suppressWarnings(person.parameter(model.2))
    
    item.fit.1 <- itemfit(ppt.parameters.1)
    item.fit.2 <- itemfit(ppt.parameters.2)
    
    cat("\n", "##################################", "\n", 
        "Item Fit Comparison", "\n", 
        "##################################", "\n\n", sep = "")
    
    cat("Item fit statistics with {", item1, "} removed:", "\n", sep = " ")
    print(item.fit.1)
    
    cat("\n", "Item fit statistics with {", item2, "} removed:", "\n", sep = " ")
    print(item.fit.2)
    
    cat("\n", "##################################", "\n", 
        "Residual Analysis Comparison", "\n", 
        "##################################", "\n\n", sep = "")
    
    cat("\n", "Residual analysis with {", item1, "} removed:", "\n", sep = " ")
    residual_analysis(model.1)
    
    cat("\n", "Residual analysis with {", item2, "} removed:", "\n", sep = " ")
    residual_analysis(model.2)
    
    cat("\n", "##################################", "\n", 
        "PCA of Residuals Comparison", "\n", 
        "##################################", "\n\n", sep = "")
    
    cat("\n", "PCA of residuals with {", item1, "} removed:", "\n\n", sep = " ")
    pca1 <- PCA_residuals(model.1, sim.num = 200)
    
    cat("\n", "PCA of residuals with {", item2, "} removed:", "\n\n", sep = " ")
    pca2 <- PCA_residuals(model.2, sim.num = 200)
    
    cat("\n", "Drop {", item1, "} or {", item2, "}", "\n", sep = " ")
    drop <- readline(prompt = "Choose variable to drop: ")
    
    while (ncol(model$X) > ncol(model.1$X)) {
      if (drop == item1) {
        model <- model.1
      } else if(drop == item2) {
        model <- model.2
      } else {
        print("Entry does not match either item name. Please retype item name.")
        drop <- readline(prompt = "Choose variable to drop: ")
      }
      
      data %<>% subset(select = !colnames(data) %in% drop)
      drop.list <- c(drop.list, drop)
    }
    
    if (p < nrow(dependent.pairs)) {
      readline(prompt="Press [enter] to continue to next item pair.")
    }
  }
  result <- list(drop.list, data, model)
  names(result) <- c("X.items", "Data", "Model")
  
  return(result)
  
  cat("\n", "All items dropped:", "\n", sep = "")
  print(drop.list)
}

#--------------------------------------------------------------------------#
## Function: test_local_dep

# Purpose: Print comparison of model fit stats to facilitate resolving local dependence

# Input: "model" must be an eRm object (output from PCM() or RM())

test_local_dep <- function(model) {
  
  data <- model$X
  Q3.test <- Q3(model)
  
  dependent.pairs <- Q3.test$violations[, 1:2]
  
  if (nrow(dependent.pairs) < 1) {
    stop("No locally dependent items found in data set.")
  } else if (nrow(dependent.pairs) == 1) {
    pair <- dependent.pairs[1, ]
    all.combos <- rbind(pair[[1]], pair[[2]])
  } else {
    # Get item pairs as individual vectors for expand.grid()
    all.pairs <- list()
    for(i in 1:nrow(dependent.pairs)) {
      pair <- c(dependent.pairs[i, 1], dependent.pairs[i, 2])
      all.pairs[[i]] <- pair
    }
    # Grab all possible combinations of items within pairs
    all.combos <- expand.grid(all.pairs)
  }
  
  # Perform iterative item fit for all combos of items to drop
  for (m in 1:nrow(all.combos)) {
    
    # Coerce variable names into character list for subset()
    drop.items <- c()
    for (n in 1:ncol(all.combos)) {
      item <- as.character(all.combos[m, n])
      drop.items %<>% c(item)
    }
    
    # Make test data set dropping selected items
    drop.data <- data %>% subset(select = !colnames(data) %in% drop.items)
    
    # Recalculate model
    if (model$model == "PCM") {
      drop.model <- PCM(drop.data)
    } else if (model$model == "RM") {
      drop.model <- RM(drop.data)
    }
    
    cat("\n", "##################################", "\n", 
        "Iterative Item Fit Elimination {", m, "}", "\n", 
        "##################################", "\n\n", sep = "")
    drop.item.fit <- stepwise_itemfit(drop.model)
    
    cat("\n", "##################################", "\n", 
        "Local Dependence Comparison {", m, "}", "\n", 
        "##################################", "\n\n", sep = "")
    drop.Q3 <- Q3(drop.model)
  }
  
  print(all.combos)
  drop <- readline(prompt = "Please enter number of item combination to drop from data set: ")
  
  while (!drop %in% c(1:length(all.combos)) ) {
    cat("Entry did not match any item combination. Please retype number.", "\n", sep = '')
    print(all.combos)
    drop <- readline(prompt = "Please enter number of item combination to drop from data set: ")
  } 
  
  drop <- all.combos[as.numeric(drop), ]
  
  drop.items <- c()
  for (n in 1:ncol(drop)) {
    item <- as.character(drop[, n])
    drop.items %<>% c(item)
  }
  data <- data %>% subset(select = !colnames(data) %in% drop.items)
  
  # Recalculate model
  if (model$model == "PCM") {
    model <- PCM(data)
  } else if (model$model == "RM") {
    model <- RM(data)
  }
  
  result <- list(drop, data, model)
  names(result) <- c("X.items", "Data", "Model")
  
  return(result)
  
  cat("\n", "All items dropped:", "\n", sep = "")
  print(drop)
}
