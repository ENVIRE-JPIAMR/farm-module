source(here::here("load_libraries.R"))
source(here::here("load_inputs.R"))
source(here::here("farm_module.R"))

## Function to simulate one production batch
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

batch_simulator <- function(farm_module = new.farm_module()) {
  
  # initialization
  animals <- farm_module$initialize_df() 
  day_idx <- farm_module$params$day.min
  output  <- list()
  
  while (day_idx < farm_module$params$day.max) {
    
    # run farm module for day_idx and update animals dataframe
    animals <- farm_module$run(animals, day_idx)
    
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

## Function to simulate one production batch
## generates the full animals dataframe at each iteration

batch_simulator_full <- function(farm_module = new.farm_module()) {
  
  # initialization
  animals <- farm_module$initialize_df() 
  day_idx <- farm_module$params$day.min
  animals_full <- list()
  
  while (day_idx < farm_module$params$day.max) {
    
    # run farm module for day_idx and update animals dataframe
    animals <- farm_module$run(animals, day_idx)
    
    # store the daily iteration
    animals$day <- rep(day_idx, nrow(animals))
    
    animals_full[[day_idx]] <- animals
    
    # update day index
    day_idx <- day_idx + 1
  } 
  
  return(list(animals_full))
}
