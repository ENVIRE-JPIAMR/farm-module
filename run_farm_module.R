source("load_libraries.R")
source("load_inputs.R")
source("farm_module.R")

## Function to simulate one production batch
## generates 4 outputs for all production days
## 1) prevalence     := Proportion of infected broilers 
## 2) load           := Total ESBL E. coli in the environment (CFU)
## 3) total_feces    := Total amount of feces in broiler's gut (g)
## 4) total_esbl_gut := Total ESBL E. coli in broiler's gut (CFU)
batch_simulator <- function(farm_module = new.farm_module()) {
  
  # initialization
  animals <- farm_module$initialize_df() 
  day_idx <- farm_module$params$day.min
  output  <- list()
  
  while (day_idx < farm_module$params$day.max) {
    
    # run farm module for day_idx and update animals dataframe
    animals <- farm_module$run(animals, day_idx)
    
    # store daily outputs
    output$prevalence     <- c(output$prevalence, sum(animals$B_infection_status)/nrow(animals))
    output$load           <- c(output$load, sum(animals$C_sum_esbl_env))
    output$total_feces    <- c(output$total_feces, sum(animals$sum_feces_gut))
    output$total_esbl_gut <- c(output$total_esbl_gut, sum(animals$C_esbl_gut))
    
    # update day index
    day_idx <- day_idx + 1
  } 
  
  return(output)
}
