## Farm module

new.farm_module <- function(input_list = load_inputs()){
  
  # initialize new farm module environment
  fm <- new.env() 
  # initialize farm module parameters
  fm$params <- input_list
  
  ## Initialize the animals df: n_row = num(animals)
  fm$initialize_df <- function() {
    
    healthy <- tribble(~ infection_duration, ~ age, -1, 1)
    sick    <- tribble(~ infection_duration, ~ age, 1, 1)
    
    # Number of animals depending on broiler density and farm size
    n_animals <- round(fm$params$farm_density/fm$params$target_weight*fm$params$farm_size)
    n_animals_infected <- rbinom(1, n_animals, fm$params$prevalence)
    n_animals_healthy  <- n_animals - n_animals_infected
    
    animals <- rbind(sick[rep(1, n_animals_infected), ],
                     healthy[rep(1, n_animals_healthy), ]) %>% mutate(
                       feces_gut = 0,
                       sum_feces_gut = 0,
                       sum_feces_env = 0,
                       sum_feces_cont_env = 0,
                       C_esbl_gut = ifelse(
                         infection_duration == -1,
                         fm$params$esbl.min,
                         fm$params$esbl.max
                       ),
                       C_sum_esbl_env = 0,
                       B_infection_status = infection_duration != -1,
                       ingested_feces = 0,
                       C_esbl_excreted = 0
                     )
    
    return(animals)
    
  }
  
  ## Bacteria logistic growth function inside broiler's gut
  fm$logistic_growth <- function(animals) {
    
    K <- fm$params$K * animals$feces_gut
    r <- 10 ^ runif(1, fm$params$r.min, fm$params$r.max)
    
    animals %>%
      mutate(C_esbl_gut = ifelse(infection_duration != -1,
                           K / (
                             1 + ((K - C_esbl_gut) / C_esbl_gut) * exp(-r)
                           ),
                           C_esbl_gut))
  }

  ## Transmission model (Dame-Korevaar et al. (2019)) based on bacteria 
  ## concentration in the environment
  fm$new_infected <- function(animals) {
    
    # compute ESBL E. coli concentration in contaminated feces
    esbl_conc_feces <-
      ifelse(sum(animals$sum_feces_cont_env) > 0,
             sum(animals$C_sum_esbl_env) / sum(animals$sum_feces_cont_env),
             0)
    
    # compute force of infection
    foi <- ifelse(esbl_conc_feces == 0,
                  0,
                  log10(esbl_conc_feces))
    
    # compute newly infected broilers
    N_susceptible <- sum(animals$infection_duration == -1)
    
    N_new_infected <-
      round(N_susceptible * (1 - exp(-foi * fm$params$Dt)))
    
    N_new_infected <- max(0, N_new_infected)
    
    
    if (N_new_infected >= N_susceptible) {
      N_new_infected <- N_susceptible
    }
    
    # update infection duration for newly infected broilers
    animals$infection_duration[sample(which(animals$infection_duration == -1),
                                        N_new_infected,
                                        replace = FALSE)] <- 0
    
    # Assumption: only newly infected broilers ingest infected feces.
    
    # total ESBL E. coli ingested by all broilers that gets newly infected
    total_esbl <-
      sum(esbl_conc_feces * animals$ingested_feces[which(animals$infection_duration == 0)])
    
    # total feces ingested by doner broilers
    total_doner_feces <- sum(animals$ingested_feces[which(animals$infection_duration > 0)])
    
    # Assumption: the ESBL E. coli ingested by newly infected broilers comes form the 
    #             infected feces in the environment. L114 distributes the source
    #             according to the ingestion proportion of previously infected broilers.
    
    # update variables for ESBL E. coli infected broilers
    animals <- animals %>%
      mutate(
        B_infection_status = infection_duration != -1,
        C_esbl_gut = ifelse(
          infection_duration == 0,
          C_esbl_gut + esbl_conc_feces * ingested_feces,
          C_esbl_gut),
        C_sum_esbl_env = ifelse(
          infection_duration > 0,
          C_sum_esbl_env - (total_esbl * ingested_feces) / total_doner_feces,
          C_sum_esbl_env)
      )
    
    return(animals)
  }

  ## Quantity of feces produced by a broiler per day
  fm$feces_function <- function(day, animals) {
    
    feces_amount <-
      runif(
        nrow(animals),
        min = fm$params$water_consum.min[day] ,
        max = fm$params$water_consum.max[day]
      ) * fm$params$water_reduction + fm$params$daily_intake[day] - fm$params$daily_gain[day]
    
    animals$feces_gut     <- feces_amount
    animals$sum_feces_gut <- animals$sum_feces_gut + feces_amount
    
    return(animals)
    
  }
    
  ## Amount of feces ingested per day
  fm$ingested_feces <- function(day, animals) {
    
    # ingested feces is not necessarily ESBL E. coli contaminated (see assumptions)
    animals$ingested_feces <-
      rep(fm$params$daily_intake[day] * fm$params$ingestion_rate,
          nrow(animals))
    
    return(animals)
  }
  
  ## Excretion function
  fm$excretion <- function(animals) {
    
    animals %>% mutate(
      C_esbl_excreted    = ifelse(infection_duration != -1, C_esbl_gut * fm$params$e_rate, 0),
      C_sum_esbl_env     = C_sum_esbl_env + C_esbl_excreted,
      C_esbl_gut         = C_esbl_gut - C_esbl_excreted,
      sum_feces_env      = sum_feces_gut,
      sum_feces_cont_env = ifelse(infection_duration != -1, sum_feces_env, 0)
    )
  }
  
  ## Bacteria environmental decay  
  fm$environmental_decay <- function(animals) {
    
    animals %>%
      mutate(C_sum_esbl_env = ifelse(
        infection_duration != -1,
        C_sum_esbl_env * (1 - fm$params$ed_rate),
        C_sum_esbl_env
      ))
  }
  
  ## Function to run farm module for a particular day
  fm$run <- function(animals, day){
    
    animals <- fm$feces_function(day,animals)
    animals <- fm$ingested_feces(day, animals)
    animals <- fm$excretion(animals)
    animals <- fm$logistic_growth(animals)
    animals <- fm$new_infected(animals)
    animals <- fm$environmental_decay(animals)
    
    animals$age <- animals$age + 1
    animals$infection_duration <-
      ifelse(animals$infection_duration != -1,
             animals$infection_duration + 1,
             -1)
    
    return(animals)
  }
  
  return(fm)
  
}











