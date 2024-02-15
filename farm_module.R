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
  
  ## Force of infection
  fm$force_of_infection <- function(animals) {
    
    sum_excretion_concentration <- animals %>%
      filter(infection_duration != -1) %>%
      summarise(
        environment = sum(C_sum_esbl_env),
        feces = sum(sum_feces_env),
        env_fec = ifelse(environment == 0, 0, log10(environment / feces))
      ) %>% pull(env_fec)

    foi <- fm$params$beta.mean * sum_excretion_concentration
    #in the study of dame korevaar the density was blabla and in this simulation...
    #100/8 m2, factor my density/density study 
    #TODO: What is this comment?
    return(foi)
  }

  ## Infection model 3, based on bacteria cfu in the environment
  fm$new_infected <- function(animals) {
    
    foi <- fm$force_of_infection(animals)
    
    num_negatives <- sum(animals$infection_duration == -1)
    number_new_infected <-
      round(num_negatives * (1 - exp(-foi * fm$params$Dt)))
    number_new_infected <- max(0, number_new_infected)
    
    
    if (number_new_infected >= num_negatives) {
      number_new_infected <- num_negatives
    }
    
    # update infection duration for new infected broilers
    animals$infection_duration[sample(which(animals$infection_duration == -1),
                                        number_new_infected,
                                        replace = FALSE)] <- 0
    
    # compute ESBL E. coli concentration in environmental feces
    esbl_conc_env <-
      ifelse(
        sum(animals$sum_feces_env) > 0,
        sum(animals$C_sum_esbl_env) / sum(animals$sum_feces_env),
        0
      )
    
    # total ESBL E. coli ingested by all broilers that gets newly infected
    total_esbl <-
      sum(esbl_conc_env * animals$ingested_feces[which(animals$infection_duration == 0)])
    
    # total feces ingested by doner broilers
    total_doner_feces <- sum(animals$ingested_feces[which(animals$infection_duration > 0)])
    
    # update variables for ESBL E. coli infected broilers
    animals <- animals %>%
      mutate(
        B_infection_status = infection_duration != -1,
        C_esbl_gut = ifelse(
          infection_duration == 0,
          C_esbl_gut + esbl_conc_env * ingested_feces,
          C_esbl_gut),
        C_sum_esbl_env = ifelse(
          infection_duration > 0,
          C_sum_esbl_env - ingested_feces * total_esbl / total_doner_feces,
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
    animals$feces_gut <- feces_amount
    
    animals$sum_feces_gut <- animals$sum_feces_gut + feces_amount
    
    return(animals)
    
  }
    
  ## Amount of feces ingested per day
  fm$ingested_feces <- function(animals) {
    
    # ingested feces is not necessarily ESBL E. coli contaminated
    # TODO: Becker et al. (2022) has a better approach
    # TODO: For day 1 it should be zero (detail)
    animals$ingested_feces <-
      rpert(
        nrow(animals),
        fm$params$ingested_feces.min ,
        fm$params$ingested_feces.mode,
        fm$params$ingested_feces.max
      ) #+ log(animals$age)
    
    return(animals)
  }
  
  ## Excretion function
  fm$excretion <- function(animals) {
    
    animals %>% mutate(
      C_esbl_excreted = ifelse(infection_duration != -1, C_esbl_gut * fm$params$e_rate, 0),
      C_sum_esbl_env  = C_sum_esbl_env + C_esbl_excreted,
      C_esbl_gut      = C_esbl_gut - C_esbl_excreted,
      sum_feces_env   = sum_feces_gut
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
    
    animals <- fm$ingested_feces(animals)
    animals <- fm$new_infected(animals)
    animals <- fm$feces_function(day,animals)
    animals <- fm$logistic_growth(animals)
    animals <- fm$excretion(animals)
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











