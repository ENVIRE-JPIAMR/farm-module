source(here::here("farm-module/load_libraries.R"))
source(here::here("farm-module/load_inputs.R"))
source(here::here("farm-module/farm_module.R"))

## Function to simulate one production batch
## Arguments: farm_module        := farm module environment
##            total_feces_input  := Total incoming contaminated feces in thinning step; 
##                                  default NA
##            total_C_esbl_input := Total incoming CFUs of ESBL E.coli in thinning step;
##                                  default NA
##            phages             := To apply phages spray in litter (Poland protocol)
##                                  default FALSE 
## generates 7 outputs for all days (QoIs from Becker et al. (2022))
## 1) load                 (CFU)   := Total ESBL E. coli in the environment till
##                                      this day
## 2) prevalence           (prop.) := Proportion of infected broilers 
## 3) avg_esbl_feces       (CFU/g) := Average (over broilers) ESBL E.coli 
##                                      concentration in daily excreted feces 
## 4) total_feces          (g)     := Total amount of feces produced by all 
##                                      broilers till this day
## 5) avg_esbl_gut         (CFU)   := Average (over broilers) ESBL E. coli in the 
##                                      the broiler's gut
## 6) avg_esbl_env         (CFU)   := Average (over broilers) ESBL E. coli excreted
##                                      in the environment
## 7) avg_esbl_ingested    (CFU)   := Average (over broilers) ESBL E. coli ingested
##                                      by a broiler

batch_simulator <- function(farm_module = new.farm_module(), total_feces_input = NA, total_C_esbl_input = NA, phages = FALSE) {
  
  # initialization
  animals <- farm_module$initialize_df() 
  day_idx <- farm_module$params$day.min
  output  <- list(load = sum(animals$C_sum_esbl_env),
                  prevalence = sum(animals$B_infection_status)/nrow(animals),
                  avg_esbl_feces = 0,
                  total_feces = sum(animals$sum_feces_gut),
                  avg_esbl_gut = mean(animals$C_esbl_gut),
                  avg_esbl_env = mean(animals$C_esbl_excreted),
                  avg_esbl_ingested = 0)

  while (day_idx < farm_module$params$day.max) {
    
    # run farm module for day_idx and update animals dataframe
    animals <- farm_module$run(animals, day_idx, phages = phages)
    
    # thinning
    if (is.na(total_feces_input) == FALSE && day_idx == farm_module$params$thinning_day - 1) {
      
      # Select a random row
      random_rows <- sample(nrow(animals), round(farm_module$params$thinning_percentage*nrow(animals)))
      
      # target animals are removed
      total_feces_target <- sum(animals$sum_feces_env[random_rows])
      animals <- animals[-random_rows, ]
      
      # adding target animal feces (non-contaminated) to first broiler row
      animals$sum_feces_env[1] <- animals$sum_feces_env[1] + total_feces_target
      
      # adding contaminated feces & CFU to first broiler row
      animals$sum_feces_cont_env[1] <- total_feces_input
      animals$C_sum_esbl_env[1] <- total_C_esbl_input
    }
    
    # store daily outputs
    output$load              <- c(output$load, sum(animals$C_sum_esbl_env))
    output$prevalence        <- c(output$prevalence, sum(animals$B_infection_status)/nrow(animals))  
    output$avg_esbl_feces    <- c(output$avg_esbl_feces, mean(animals$C_esbl_excreted/animals$feces_gut))  
    output$total_feces       <- c(output$total_feces, sum(animals$sum_feces_gut))
    output$avg_esbl_gut      <- c(output$avg_esbl_gut, mean(animals$C_esbl_gut))
    output$avg_esbl_env      <- c(output$avg_esbl_env, mean(animals$C_esbl_excreted))
    output$avg_esbl_ingested <- c(output$avg_esbl_ingested, mean(animals$ingested_feces) * sum(animals$C_sum_esbl_env) / sum(animals$sum_feces_gut)) #TODO: faut corriger
    
    # update day index
    day_idx <- day_idx + 1
  } 
  
  return(output)
}

## Function to simulate one production batch (same arguments as before)
## generates the full animals dataframe at each iteration

batch_simulator_full <- function(farm_module = new.farm_module(), total_feces_input = NA, total_C_esbl_input = NA, phages = FALSE) {
  # initialization
  animals <- farm_module$initialize_df()
  day_idx <- farm_module$params$day.min
  animals_full <- list()
  
  # store the first day_idx values
  animals$day <- rep(day_idx, nrow(animals))
  animals_full[[day_idx]] <- animals
  
  while (day_idx < farm_module$params$day.max) {
    # run farm module & update animals dataframe of day_idx
    animals <- farm_module$run(animals, day_idx, phages = phages)
    
    # thinning
    if (is.na(total_feces_input) == FALSE && day_idx == farm_module$params$thinning_day - 1) {
      
      # Select a random row
      random_rows <- sample(nrow(animals), round(farm_module$params$thinning_percentage*nrow(animals)))
      
      # target animals are removed
      total_feces_target <- sum(animals$sum_feces_env[random_rows])
      animals <- animals[-random_rows, ]
      
      # adding target animal feces (non-contaminated) to first broiler row
      animals$sum_feces_env[1] <- animals$sum_feces_env[1] + total_feces_target
      
      # adding contaminated feces & CFU to first broiler row
      animals$sum_feces_cont_env[1] <- total_feces_input
      animals$C_sum_esbl_env[1] <- total_C_esbl_input
    }
    
    # update day index
    day_idx <- day_idx + 1
    
    # store the daily iteration
    animals$day <- rep(day_idx, nrow(animals))
    animals_full[[day_idx]] <- animals
    
  }
  
  return(list(animals_full))
}

## Function to simulate one production batch with thinning
## Arguments: full       := TRUE; to generate full dataframe output
##            prevalence := TRUE; to set initial prevalence to 0 (only applicable for thinning step)  
##            phages     := To apply phages spray in litter (Poland protocol)
##                          default FALSE 
## Outputs: same format as either one of the above two functions

batch_simulator_thinning <- function(farm_module = new.farm_module(), full = FALSE, prevalence = TRUE, phages = FALSE) {
  
  # run baseline scenario
  baseline_output <- batch_simulator() 
  
  # baseline variables
  farm_module$params$C_sum_esbl_baseline  <- baseline_output$load[farm_module$params$thinning_day] 
  farm_module$params$total_feces_baseline <- baseline_output$total_feces[farm_module$params$thinning_day] 
  
  feces_per_m2_baseline <-
    (farm_module$params$total_feces_baseline + (farm_module$params$litter_mass * farm_module$params$farm_size)) /
    farm_module$params$farm_size 
  feces_per_cm2_baseline <- feces_per_m2_baseline/10000
  
  C_esbl_per_g_baseline <-
    farm_module$params$C_sum_esbl_baseline / (
      farm_module$params$total_feces_baseline + (
        farm_module$params$litter_mass * farm_module$params$farm_size
      )
    )
  
  # catching step
  total_feces_input <-
    farm_module$params$sole_size * 
    farm_module$params$sole_retain_rate * 
    feces_per_cm2_baseline * 
    farm_module$params$sole_discharge_rate * 
    farm_module$params$n_legs *
    farm_module$params$n_catchers
  
  total_C_esbl_input <- total_feces_input * C_esbl_per_g_baseline 
  
  # change initial prevalence before actual scenario
  if(prevalence == TRUE){
    farm_module$params$prevalence <- 0  
  }
  
  # full animal df output with thinning
  if(full == FALSE){
    output <- batch_simulator(farm_module = farm_module, total_feces_input = total_feces_input, total_C_esbl_input = total_C_esbl_input, phages = phages)
  }else{
    output <- batch_simulator_full(farm_module = farm_module, total_feces_input = total_feces_input, total_C_esbl_input = total_C_esbl_input, phages = phages)
  }
  
  return(output)
  
}
