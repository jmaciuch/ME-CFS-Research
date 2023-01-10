# ---------------------------------------------
# Name:        MECFS_diagnostics.R
# Author:      Jess Maciuch
# Date:        2022-06-22
# Description: Functions for operationalizing ME/CFS diagnostic criteria
# ---------------------------------------------


#--------------------------------------------------------------------------#

## Function: pull_SF-36

# Purpose: Pull SF36 questions from individual response rows of a dataset.

# Input: 'dataset' must be data frame or matrix.
# i.e. output from read_csv()

pull_SF36 <- function(dataset) {
  names <- c("mos1",
             "mos2",
             "mos3a",
             "mos3b",
             "mos3c",
             "mos3d",
             "mos3e",
             "mos3f",
             "mos3g",
             "mos3h",
             "mos3i",
             "mos3j",
             "mos4a",
             "mos4b",
             "mos4c",
             "mos4d",
             "mos5a",
             "mos5b",
             "mos5c",
             "mos6",
             "mos7",
             "mos8",
             "mos9a",
             "mos9b",
             "mos9c",
             "mos9d",
             "mos9e",
             "mos9f",
             "mos9g",
             "mos9h",
             "mos9i",
             "mos10",
             "mos11a",
             "mos11b",
             "mos11c",
             "mos11d")
  
  output <- select(dataset, all_of(names)) 
  
  # Data normalization 
  norm_vec <- list("0", "1", "2", "3", "4", "5", "6")
  
  for (j in 1:ncol(output)) {
    for (i in 1:nrow(output)) {
      # Convert values that are not 0-6 into NA (i.e. missing values in R)
      if (!output[i, j] %in% norm_vec) {
        output[i, j] <- NA
      } 
    }
  }
  
  # Convert all values to numeric
  output %<>% 
    mutate(across(1:ncol(output), as.numeric))
  
  # Rename items for easier scoring 
  item.num <- paste("mos", 1:36, sep = "")
  colnames(output) <- item.num
  
  return(output)
}

#--------------------------------------------------------------------------#

## Function: score_SF36

# Purpose: Calculate SF-36 Scores from datasets

# Input: "dataset" must be data frame or matrix.
# i.e. produced by read_csv
# Method also works on individual rows from dataset

# Notes: Scoring rules pulled from:
# https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html

score_SF36 <- function(dataset) {
  dataset %<>% pull_SF36
  
  # Recode individual responses
  recode_group_1 <- paste("mos", c(1, 2, 20, 22, 34, 26), sep = "")
  recode_group_2 <- paste("mos", c(3:12), sep = "")
  recode_group_3 <- paste("mos", c(13:19), sep = "")
  recode_group_4 <- paste("mos", c(21, 23, 26, 27, 30), sep = "")
  recode_group_5 <- paste("mos", c(24, 25, 28, 29, 31), sep = "")
  recode_group_6 <- paste("mos", c(32, 33, 35), sep = "")
  
  for (j in 1:ncol(dataset)) {
    if (colnames(dataset)[j] %in% recode_group_1) {
      old <- c(1:5)
      new <- c(100, 75, 50, 25, 0)
    } else if (colnames(dataset)[j] %in% recode_group_2) {
      old <- c(1:3)
      new <- c(0, 50, 100)
    } else if (colnames(dataset)[j] %in% recode_group_3) {
      old <- c(1, 2)
      new <- c(0, 100)
    } else if (colnames(dataset)[j] %in% recode_group_4) {
      old <- c(1:6)
      new <- c(100, 80, 60, 40, 20, 0)
    } else if (colnames(dataset)[j] %in% recode_group_5) {
      old <- c(1:6)
      new <- c(0, 20, 40, 60, 80, 100)
    } else if (colnames(dataset)[j] %in% recode_group_6) {
      old <- c(1:5)
      new <- c(0, 25, 50, 75, 100)
    }
    
    for (i in 1:nrow(dataset)) {
      ind <- match(dataset[[i,j]], old)
      dataset[i, j] <- new[ind]
    }
  }
  
  # Create subscale scores
  output <- data.frame()
  
  PF <- paste("mos", c(3:12), sep = "")
  RP <- paste("mos", c(13:16), sep = "")
  RE <- paste("mos", c(17:19), sep = "")
  VT <- paste("mos", c(23, 27, 29, 31), sep = "")
  MH <- paste("mos", c(24, 25, 26, 28, 30), sep = "")
  SF <- paste("mos", c(20, 32), sep = "")
  BP <- paste("mos", c(21, 22), sep = "")
  GH <- paste("mos", c(1, 33:36), sep = "")
  
  subscales <- list(PF, RP, RE, VT, MH, SF, BP, GH)
  subscale_names <- c("PF", "RP", "RE", "VT", "MH", "SF", "BP", "GH")
  names(subscales) <- subscale_names
  
  for (i in 1:nrow(dataset)) {
    for (scale in subscale_names) {
      items <- subscales[[scale]]
      vec <- as.numeric(select(dataset, all_of(items))[i,])
      output[i, scale] <- mean(vec, na.rm = TRUE)
    }
  }
  
  return(output)
}

#--------------------------------------------------------------------------#

## Function: sub_reduc

# Purpose: Determine whether respondent demonstrates substantial reduction
# in functioning.

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

sub_reduc <- function(response) {
  SF36.scores <- score_SF36(response)
  
  count <- 0
  if (isTRUE(SF36.scores[['RP']] <= 50)) {
    count <- count + 1
  } 
  
  if (isTRUE(SF36.scores[['SF']] <= 62.5)) {
    count <- count + 1
  } 
  
  if (isTRUE(SF36.scores[['VT']] <= 35)) {
    count <- count + 1
  }
  
  return(count >= 2) 
}

#--------------------------------------------------------------------------#

## Function: six_months

# Purpose: Determine whether respondent has experienced fatigue for over 6 
# months (excluding lifelong fatigue and fatigue due to overexertion)

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

six_months <- function(response) {
  names <- c('lifelongfatigue90',
             'begin92',
             'time100',
             'past4weeks112a',
             'past4weeks112d')
  
  q <- select(response, all_of(names))
  
  for (k in 1:ncol(q)) {
    if (isTRUE(q[[k]] == "#NULL!")) {
      q[k] <- NA
    } else {
      q[k] <- as.numeric(q[k])
    }
  }
  
  if (isTRUE((q[['lifelongfatigue90']] == 1)) & 
      isTRUE((q[['begin92']] == 5)) &
      isTRUE((q[['time100']] == 7))) {
    return(FALSE)
  } else if (isTRUE((q[['past4weeks112a']] + 
                     q[['past4weeks112d']]) >= 60)) {
    return(FALSE)
  } else if (q[['begin92']] == 2 | 3 | 4) {
    return(TRUE)
  } else {
    return(FALSE)
  }   
}

#--------------------------------------------------------------------------#

## Function: diagnose_IOM

# Purpose: Determine whether respondent meets IOM (2015) case definition

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

diagnose_IOM <- function(response) {
  ## Post-Exertional Malaise
  pem.domain <- FALSE
  
  pem <- c("heavy14f",
           "heavy14s",
           "soreness15f",
           "soreness15s",
           "mental16f",
           "mental16s",
           "minimum17f",
           "minimum17s",
           "drained18f",
           "drained18s")
  
  pem.count <- 0
  
  for (s in seq(from = 1, to = length(pem), by = 2)) {
    if (isTRUE((response[[pem[s]]] >= 2) & (response[[pem[s+1]]] >= 2))) {
      pem.count <- pem.count + 1
    }
  }
  
  if (pem.count >= 1) {
    pem.domain <- TRUE
  }
  
  ## Unrefreshing Sleep
  unrefreshing.domain <- FALSE
  
  if (isTRUE((response["unrefreshed19f"] >= 2) & (response["unrefreshed19s"] >= 2))) {
    unrefreshing.domain <- TRUE
  }
  
  # IOM Symptom Domains
  IOM.domain <- FALSE
  IOM.count <- 0
  
  # Cognitive Impairment
  cog.domain <- FALSE
  
  cog <- c("remember36f",
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
           "absent44f",
           "absent44s")
  
  cog.count <- 0
  
  for (s in seq(from = 1, to = length(cog), by = 2)) {
    if (isTRUE((response[[cog[s]]] >= 2) & (response[[cog[s+1]]] >= 2))) {
      cog.count <- cog.count + 1
    }
  }
  
  if (cog.count >= 1) {
    IOM.count <- IOM.count + 1
  }
  
  # Orthostatic Intolerance
  ortho.domain <- FALSE
  
  ortho <- c("unsteady48f",
             "unsteady48s",
             "shortness49f",
             "shortness49s",
             "dizz50f",
             "dizz50s",
             "irregular51f",
             "irregular51s")
  
  ortho.count <- 0
  
  for (s in seq(from = 1, to = length(ortho), by = 2)) {
    if (isTRUE((response[[ortho[s]]] >= 2) & (response[[ortho[s+1]]] >= 2))) {
      ortho.count <- ortho.count + 1
    }
  }
  
  if (ortho.count >= 1) {
    IOM.count <- IOM.count + 1
  }
  
  if (IOM.count >= 1) {
    IOM.domain <- TRUE
  }
  
  # Case definition assessment
  IOM <- FALSE
  
  if (sub_reduc(response) &
      six_months(response) &
      pem.domain &
      unrefreshing.domain &
      IOM.domain) {
    IOM <- TRUE
  }
  
  return(IOM)
}

#--------------------------------------------------------------------------#

## Function: diagnose_CCC

# Purpose: Determine whether respondent meets CCC (2003) case definition

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

diagnose_CCC <- function(response) {
  # Post-Exertional Malaise
  pem.domain <- FALSE
  
  pem <- c("heavy14f",
           "heavy14s",
           "soreness15f",
           "soreness15s",
           "mental16f",
           "mental16s",
           "minimum17f",
           "minimum17s",
           "drained18f",
           "drained18s"
  )
  
  pem.count <- 0
  
  for (s in seq(from = 1, to = length(pem), by = 2)) {
    if (isTRUE((response[[pem[s]]] >= 2) & (response[[pem[s+1]]] >= 2))) {
      pem.count <- pem.count + 1
    }
  }
  
  if (pem.count >= 1) {
    pem.domain <- TRUE
  }
  
  # Sleep Problems
  sleep.domain <- FALSE
  
  sleep <- c("unrefreshed19f",
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
             "allday24s")
  
  sleep.count <- 0
  
  for (s in seq(from = 1, to = length(sleep), by = 2)) {
    if (isTRUE((response[[sleep[s]]] >= 2) & (response[[sleep[s+1]]] >= 2))) {
      sleep.count <- sleep.count + 1
    }
  }
  
  if (sleep.count >= 1) {
    sleep.domain <- TRUE
  }
  
  # Pain
  pain.domain <- FALSE
  
  pain <- c("musclepain25f",
            "musclepain25s",
            "jointpain26f",
            "jointpain26s",
            "eyepain27f",
            "eyepain27s",
            "chestpain28f",
            "chestpain28s",
            "bloating29f",
            "bloating29s",
            "stomach30f",
            "stomach30s",
            "headaches31f",
            "headaches31s"
  )
  
  pain.count <- 0
  
  for (s in seq(from = 1, to = length(pain), by = 2)) {
    if (isTRUE((response[[pain[s]]] >= 2) & (response[[pain[s+1]]] >= 2))) {
      pain.count <- pain.count + 1
    }
  }
  
  if (pain.count >= 1) {
    pain.domain <- TRUE
  }
  
  # Neurological/cognitive problems
  neuro.domain <- FALSE
  
  neuro <- c("twitches32f",
             "twitches32s",
             "weakness33f",
             "weakness33s",
             "noise34f",
             "noise34s",
             "lights35f",
             "lights35s",
             "remember36f",
             "remember36s",
             "difficulty37f",
             "difficulty37s",
             "word38f",
             "word38s",
             "understanding39f",
             "understanding39s",
             "focus40f",
             "focus40s",
             "unable41f",
             "unable41s",
             "depth42f",
             "depth42s",
             "slowness43f",
             "slowness43s",
             "absent44f",
             "absent44s")
  
  neuro.count <- 0
  
  for (s in seq(from = 1, to = length(neuro), by = 2)) {
    if (isTRUE((response[[neuro[s]]] >= 2) & (response[[neuro[s+1]]] >= 2))) {
      neuro.count <- neuro.count + 1
    }
  }
  
  if (neuro.count >= 2) {
    neuro.domain <- TRUE
  }
  
  # From 2 of the 3 following areas
  ccc.domain <- FALSE
  ccc.count <- 0
  
  # Autonomic 
  auto.domain <- 0
  
  auto <- c("bladder45f",
            "bladder45s",
            "bowel46f",
            "bowel46s",
            "nausea47f",
            "nausea47s",
            "unsteady48f",
            "unsteady48s",
            "shortness49f",
            "shortness49s",
            "dizz50f",
            "dizz50s",
            "irregular51f",
            "irregular51s"
  )
  
  auto.count <- 0
  
  for (s in seq(from = 1, to = length(auto), by = 2)) {
    if (isTRUE((response[[auto[s]]] >= 2) & (response[[auto[s+1]]] >= 2))) {
      auto.count <- auto.count + 1
    }
  }
  
  if (auto.count >= 1) {
    ccc.count <- ccc.count + 1
  }
  
  # Neuroendocrine
  endo.domain <- 0
  
  endo <- c("weight52f",
            "weight52s",
            "appetite53f",
            "appetite53s",
            "sweating54f",
            "sweating54s",
            "night55f",
            "night55s",
            "limbs56f",
            "limbs56s",
            "chills57f",
            "chills57s",
            "hot58f",
            "hot58s",
            "htemp59f",
            "htemp59s",
            "ltemp60f",
            "ltemp60s",
            "alcohol61f",
            "alcohol61s"
  )
  
  endo.count <- 0
  
  for (s in seq(from = 1, to = length(endo), by = 2)) {
    if (isTRUE((response[[endo[s]]] >= 2) & (response[[endo[s+1]]] >= 2))) {
      endo.count <- endo.count + 1
    }
  }
  
  if (endo.count >= 1) {
    ccc.count <- ccc.count + 1
  }
  
  # Immune
  immune.domain <- 0
  
  immune <- c("sorethroat62f",
              "sorethroat62s",
              "lymphnodes63f",
              "lymphnodes63s",
              "fever64f",
              "fever64s",
              "flu65f",
              "flu65s",
              "smells66f",
              "smells66s"
  )
  
  immune.count <- 0
  
  for (s in seq(from = 1, to = length(immune), by = 2)) {
    if (isTRUE((response[[immune[s]]] >= 2) & (response[[immune[s+1]]] >= 2))) {
      immune.count <- immune.count + 1
    }
  }
  
  if (immune.count >= 1) {
    ccc.count <- ccc.count + 1
  }
  
  # ccc.domain criteria
  if (ccc.count >= 2) {
    ccc.domain <- TRUE
  } 
  
  # Case definition assessment
  CCC <- FALSE
  
  if (sub_reduc(response) &
      six_months(response) &
      pem.domain &
      sleep.domain &
      pain.domain &
      neuro.domain &
      ccc.domain
  ) {
    CCC <- TRUE
  }
  
  return(CCC)
}

#--------------------------------------------------------------------------#

## Function: diagnose_fukuda

# Purpose: Determine whether respondent meets Fukuda (1994) case definition

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

diagnose_fukuda <- function(response) {
  # Fukuda Symptom Domains
  fukuda.domains <- FALSE
  domain.count <- 0
  
  # Memory/concentration 
  mem.con <- c("remember36f",
               "remember36s",
               "difficulty37f",
               "difficulty37s",
               "word38f",
               "word38s",
               "understanding39f",
               "understanding39s",
               "slowness43f",
               "slowness43s",
               "absent44f",
               "absent44s")
  
  mem.count <- 0
  
  for (s in seq(from = 1, to = length(mem.con), by = 2)) {
    if (isTRUE((response[[mem.con[s]]] + response[[mem.con[s+1]]]) >= 2)) {
      mem.count <- mem.count + 1
    }
  }
  
  if (mem.count >= 1) {
    domain.count <- domain.count + 1
  }
  
  # Unrefreshing sleep
  if (isTRUE((response[["unrefreshed19f"]] + response[["unrefreshed19s"]]) >= 2)) {
    domain.count <- domain.count + 1
  }
  
  # Joint pain
  if (isTRUE((response[["jointpain26f"]] + response[["jointpain26s"]]) >= 2)) {
    domain.count <- domain.count + 1
  }
  
  # Tender/sore lymph nodes
  if (isTRUE((response[["lymphnodes63f"]] + response[["lymphnodes63s"]]) >= 2)) {
    domain.count <- domain.count + 1 
  }
  
  # Muscle aches
  if (isTRUE((response[["musclepain25f"]] + response[["musclepain25s"]]) >= 2)) {
    domain.count <- domain.count + 1
  }
  
  # Post-exertional malaise
  pem <- c("heavy14f",
           "heavy14s",
           "soreness15f",
           "soreness15s",
           "mental16f",
           "mental16s",
           "minimum17f",
           "minimum17s",
           "drained18f",
           "drained18s"
  )
  
  pem.count <- 0
  
  for (s in seq(from = 1, to = length(pem), by = 2)) {
    if (isTRUE((response[[pem[s]]] + response[[pem[s+1]]]) >= 2)) {
      pem.count <- pem.count + 1
    }
  }
  
  if (pem.count >= 1) {
    domain.count <- domain.count + 1
  }
  
  # Headaches
  if (isTRUE(((response[["headaches31f"]] + response[["headaches31s"]]) >= 2) &
             (response[["headaches91"]] == 1))) {
    domain.count <- domain.count + 1
  }
  
  # Sore Throat
  if (isTRUE((response[["sorethroat62f"]] + response[["sorethroat62s"]]) >= 2)) {
    domain.count <- domain.count + 1
  }
  
  if (domain.count >= 4) {
    fukuda.domains <- TRUE
  } 
  
  
  ## Case Definition Assessment
  fukuda <- FALSE
  
  for (r in 1:nrow(diagnostics)) {
    response <- diagnostics[r,]
    
    if (sub_reduc(response) &
        six_months(response) &
        fukuda.domains) {
      fukuda <- TRUE
    }
  }
  
  return(fukuda)
}

#--------------------------------------------------------------------------#

## Function: diagnose_ME-ICC

# Purpose: Determine whether respondent meets ME-ICC (2011) case definition

# Input: "response" must be 1 row of participant responses

# Notes: Working definition pulled from:
# https://www.leonardjason.com/wp-content/uploads/2018/11/Jason-Sunnquist-2018.-The-Development...Suppl-A-DSQ-1.pdf

diagnose_ME_ICC <- function(response) {
  ## 50% reduction in activity level
  reduction <- FALSE
  
  if (isTRUE(response[["reduction120"]] == 1)) {
    reduction <- TRUE
  }
  
  ## Post-Exertional Malaise
  pem.domain <- FALSE
  
  pem <- c("heavy14f",
           "heavy14s",
           "soreness15f",
           "soreness15s",
           "mental16f",
           "mental16s",
           "minimum17f",
           "minimum17s",
           "drained18f",
           "drained18s"
  )
  
  pem.count <- 0
  
  for (s in seq(from = 1, to = length(pem), by = 2)) {
    if (isTRUE((response[[pem[s]]] >= 2) & (response[[pem[s+1]]] >= 2))) {
      pem.count <- pem.count + 1
    }
  }
  
  if (pem.count >= 1) {
    pem.domain <- TRUE
  }
  
  ## Domain 1 
  domain1 <- FALSE
  d1.count <- 0
  
  # Neurocognitive
  neuro.count <- 0
  
  neuro <- c("remember36f",
             "remember36s",
             "difficulty37f",
             "difficulty37s",
             "word38f",
             "word38s",
             "understanding39f",
             "understanding39s",
             "focus40f",
             "focus40s",
             "unable41f",
             "unable41s",
             "depth42f",
             "depth42s",
             "slowness43f",
             "slowness43s",
             "absent44f",
             "absent44s")
  
  for (s in seq(from = 1, to = length(neuro), by = 2)) {
    if (isTRUE((response[[neuro[s]]] >= 2) & (response[[neuro[s+1]]] >= 2))) {
      neuro.count <- neuro.count + 1
    }
  }
  
  if (neuro.count >= 1) {
    d1.count <- d1.count + 1
  }
  
  # Pain
  pain.count <- 0
  
  pain <- c("musclepain25f",
            "musclepain25s",
            "jointpain26f",
            "jointpain26s",
            "eyepain27f",
            "eyepain27s",
            "chestpain28f",
            "chestpain28s",
            "headaches31f",
            "headaches31s")
  
  for (s in seq(from = 1, to = length(pain), by = 2)) {
    if (isTRUE((response[[pain[s]]] >= 2) & (response[[pain[s+1]]] >= 2))) {
      pain.count <- pain.count + 1
    }
  }
  
  if (pain.count >= 1) {
    d1.count <- d1.count + 1
  }
  
  # Sleep Disturbance
  sleep.count <- 0
  
  sleep <- c("unrefreshed19f",
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
             "allday24s")
  
  for (s in seq(from = 1, to = length(sleep), by = 2)) {
    if (isTRUE((response[[sleep[s]]] >= 2) & (response[[sleep[s+1]]] >= 2))) {
      sleep.count <- sleep.count + 1
    }
  }
  
  if (sleep.count >= 1) {
    d1.count <- d1.count + 1
  }
  
  # Neurosensory, Perceptual, and Motor Disturbance
  spmd.count <- 0
  
  spmd <- c("twitches32f",
            "twitches32s",
            "weakness33f",
            "weakness33s",
            "noise34f",
            "noise34s",
            "lights35f",
            "lights35s",
            "unsteady48f",
            "unsteady48s")
  
  for (s in seq(from = 1, to = length(spmd), by = 2)) {
    if (isTRUE((response[[spmd[s]]] >= 2) & (response[[spmd[s+1]]] >= 2))) {
      spmd.count <- spmd.count + 1
    }
  }
  
  if (spmd.count >= 1) {
    d1.count <- d1.count + 1
  }
  
  # Domain 1 total
  if(d1.count >= 3) {
    domain1 <- TRUE
  }
  
  ## Domain 2
  domain2 <- FALSE
  d2.count <- 0
  
  # Flu-like
  flu.count <- 0
  
  flu <- c("sorethroat62f",
           "sorethroat62s",
           "lymphnodes63f",
           "lymphnodes63s",
           "fever64f",
           "fever64s",
           "flu65f",
           "flu65s")
  
  for (s in seq(from = 1, to = length(flu), by = 2)) {
    if (isTRUE((response[[flu[s]]] >= 2) & (response[[flu[s+1]]] >= 2))) {
      flu.count <- flu.count + 1
    }
  }
  
  if (flu.count >= 1) {
    d2.count <- d2.count + 1
  }
  
  # Gastrointestinal
  gastro.count <- 0
  
  gastro <- c("bloating29f",
              "bloating29s",
              "stomach30f",
              "stomach30s",
              "bowel46f",
              "bowel46s",
              "nausea47f",
              "nausea47s")
  
  for (s in seq(from = 1, to = length(gastro), by = 2)) {
    if (isTRUE((response[[gastro[s]]] >= 2) & (response[[gastro[s+1]]] >= 2))) {
      gastro.count <- gastro.count + 1
    }
  }
  
  if (gastro.count >= 1) {
    d2.count <- d2.count + 1
  }
  
  # Genitourinary
  if (isTRUE((response[['bladder45f']] >= 2) & (response[['bladder45s']] >= 2))) {
    d2.count <- d2.count + 1
  }
  
  # Sensitivities
  sens.count <- 0
  
  sens <- c("alcohol61f",
            "alcohol61s",
            "smells66f",
            "smells66s")
  
  for (s in seq(from = 1, to = length(sens), by = 2)) {
    if (isTRUE((response[[sens[s]]] >= 2) & (response[[sens[s+1]]] >= 2))) {
      sens.count <- sens.count + 1
    }
  }
  
  if (sens.count >= 1) {
    d2.count <- d2.count + 1
  }
  
  # Susceptibility to Viral Infections
  if(isTRUE(response['viral121'] == 1)) {
    d2.count <- d2.count + 1
  }
  
  # Domain 2 total
  if(d2.count >= 3) {
    domain2 <- TRUE
  }
  
  ## Domain 3
  domain3 <- FALSE
  d3.count <- 0
  
  # Cardiovascular
  cardio.count <- 0
  
  cardio <- c("dizz50f",
              "dizz50s",
              "irregular51f",
              "irregular51s")
  
  for (s in seq(from = 1, to = length(cardio), by = 2)) {
    if (isTRUE((response[[cardio[s]]] >= 2) & (response[[cardio[s+1]]] >= 2))) {
      cardio.count <- cardio.count + 1
    }
  }
  
  if (cardio.count >= 1) {
    d3.count <- d3.count + 1
  }
  
  # Respiratory
  if(isTRUE((response["shortness49f"] >= 2) & (response["shortness49s"] >= 2))) {
    d3.count <- d3.count + 1
  }
  
  # Loss of thermostatic ability
  thermo.count <- 0
  
  thermo <- c("sweating54f",
              "sweating54s",
              "night55f",
              "night55s",
              "limbs56f",
              "limbs56s",
              "chills57f",
              "chills57s",
              "hot58f",
              "hot58s",
              "htemp59f",
              "htemp59s",
              "ltemp60f",
              "ltemp60s")
  
  for (s in seq(from = 1, to = length(thermo), by = 2)) {
    if (isTRUE((response[[thermo[s]]] >= 2) & (response[[thermo[s+1]]] >= 2))) {
      thermo.count <- thermo.count + 1
    }
  }
  
  if (thermo.count >= 1) {
    d3.count <- d3.count + 1
  }
  
  # Temperature intolerance
  if(isTRUE(response["intolerant122"] >= 2)) {
    d3.count <- d3.count + 1
  }
  
  # Domain 3 total
  if(d3.count >= 1) {
    domain3 <- TRUE
  }
  
  # Case definition assessment
  ME_ICC <- FALSE
  
  if (reduction &
      pem.domain &
      domain1 &
      domain2 &
      domain3) {
    ME_ICC <- TRUE
  }
  
  return(ME_ICC)
}

