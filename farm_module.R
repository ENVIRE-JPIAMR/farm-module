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
    n_animals <- round(input_list$farm_density/input_list$target_weight*input_list$farm_size)
    n_animals_infected <- rbinom(1, n_animals, input_list$prevalence)
    n_animals_healthy  <- n_animals - n_animals_infected
    
    animals <- rbind(sick[rep(1, n_animals_infected), ],
                     healthy[rep(1, n_animals_healthy), ]) %>% mutate(
                       feces_gut = 0,
                       sum_feces_gut = 0,
                       C_esbl_gut = ifelse(
                         infection_duration == -1,
                         input_list$esbl.min,
                         input_list$esbl.max
                       ),
                       C_sum_esbl_env = 0,
                       B_infection_status = infection_duration != -1,
                       ingested_feces = 0,
                       C_esbl_env = 0
                     )
    
    return(animals)
    
  }
  
  ## Bacteria logistic growth function inside broiler's gut
  fm$logistic_growth <- function(animals) {
    
    K <- input_list$K * animals$feces_gut
    r <- 10 ^ runif(1, input_list$r.min, input_list$r.max)
    
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
        feces = sum(sum_feces_gut),
        env_fec = ifelse(environment == 0, 0, log10(environment / feces))
      ) %>% pull(env_fec)

    foi <- input_list$beta.mean * sum_excretion_concentration
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
      round(num_negatives * (1 - exp(-foi * input_list$Dt)))
    number_new_infected <- max(0, number_new_infected)
    
    
    if (number_new_infected >= num_negatives) {
      number_new_infected <- num_negatives
    }
    
    animals$infection_duration[sample(which(animals$infection_duration == -1),
                                        number_new_infected,
                                        replace = FALSE)] <- 0
    
    return(animals)
  }
  
  ## Update animals dataframe at end of day
  fm$update_df <- function(animals) {
    
    animals <- animals %>%
      mutate(
        age = age + 1,
        infection_duration =
          ifelse(infection_duration != -1, infection_duration + 1, -1),
        B_infection_status = infection_duration != -1,
        C_esbl_gut = ifelse(
          infection_duration == 1,
          sum(C_sum_esbl_env) / sum(sum_feces_gut) * ingested_feces,
          C_esbl_gut
        )
      )
    
    return(animals)
  }

  ## Quantity of feces produced by a broiler per day
  fm$feces_function <- function(day, animals) {
    
    feces_amount <-
      runif(
        nrow(animals),
        min = input_list$water_consum.min[day] ,
        max = input_list$water_consum.max[day]
      ) * input_list$water_reduction + input_list$daily_intake[day] - input_list$daily_gain[day]
    animals$feces_gut <- feces_amount
    
    animals$sum_feces_gut <- animals$sum_feces_gut + feces_amount
    
    return(animals)
    
  }
    
  ## Amount of feces ingested per day
  fm$ingested_feces <- function(animals) {
    #TODO: What happens in Day 1 ?
    ingested <-
      rpert(
        nrow(animals),
        input_list$ingested_feces.min ,
        input_list$ingested_feces.mode,
        input_list$ingested_feces.max
      ) #+ log(animals$age)
    animals$ingested_feces <- ingested
    
    return(animals)
  }
  
  ## Excretion function
  fm$excretion <- function(animals) {
    
    excretion_cfu <-
      animals %>% mutate(
        C_esbl_env = ifelse(infection_duration != -1, C_esbl_gut * input_list$e_rate, 0),
        C_sum_esbl_env = C_sum_esbl_env + C_esbl_env - (
          animals$ingested_feces * sum(C_sum_esbl_env) / sum(sum_feces_gut)
        ),
        C_esbl_gut = C_esbl_gut - C_esbl_env  + (
          animals$ingested_feces * sum(C_sum_esbl_env) / sum(sum_feces_gut)
        )
      )
  }
  
  ## Bacteria environmental decay  
  fm$environmental_decay <- function(animals) {
    
    animals %>%
      mutate(C_sum_esbl_env = ifelse(
        infection_duration != -1,
        C_sum_esbl_env * (1 - input_list$ed_rate),
        C_sum_esbl_env
      ))
  }
  
  #TODO: fm$run
  
  return(fm)
  
}











