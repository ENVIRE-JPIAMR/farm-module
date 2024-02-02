## Farm module

new.farm_module <- function(input_list = load_inputs()){
  
  # initialize new farm module environment
  fm <- new.env() 
  # initialize farm module parameters
  fm$params <- input_list
  
  ## Initialize the animals df: n_row = num(animals)
  fm$initialize_df <- function() {
    
    healthy <- tribble(~ days_since_infection, ~ age, -1, 1)
    sick    <- tribble(~ days_since_infection, ~ age, 1, 1)
    
    # Number of animals depending on broiler density and farm size
    n_animals <- input_list$farm_density/input_list$target_weight*input_list$farm_size
    
    animals <- rbind(sick[rep(1, round(n_animals * input_list$prevalence)), ],
                     healthy[rep(1, round(n_animals * (1 - input_list$prevalence))), ]) %>% mutate(
                       content = 0,
                       sum_feces = 0,
                       esbl = ifelse(
                         days_since_infection == -1,
                         input_list$esbl.min,
                         input_list$esbl.max
                       ),
                       sum_environment = 0,
                       density = input_list$farm_density,
                       infected = days_since_infection != -1,
                       ingested_feces = 0,
                       cfu_environment = 0
                     )
    
    return(animals)
    
  }
  
  ## Bacteria logistic growth function inside broiler's gut
  fm$logistic_growth <- function(animals) {
    
    K <- input_list$K * animals$content
    r <- 10 ^ runif(1, input_list$r.min, input_list$r.max)
    
    animals %>%
      mutate(esbl = ifelse(days_since_infection != -1,
                           K / (
                             1 + ((K - esbl) / esbl) * exp(-r * days_since_infection)
                           ),
                           esbl))
  }
  
  ## Force of infection
  fm$force_of_infection_model3 <- function(animals) {
    sum_excretion_concentration <- animals %>%
      filter(days_since_infection != -1) %>%
      summarise(
        environment = sum(sum_environment),
        feces = sum(sum_feces),
        env_fec = log10(environment / feces)
      ) %>% pull(env_fec)
    
    foi <- input_list$beta.mean * sum_excretion_concentration
    #in the study of dame korevaar the density was blabla and in this simulation...
    #100/8 m2, factor my density/density study
    return(foi)
  }

  ## Infection model 3, based on bacteria cfu in the environment
  fm$infection_animals2_model3 <- function(animals) {
    
    foi <- fm$force_of_infection_model3(animals)
    
    num_negatives <- sum(animals$days_since_infection == -1)
    number_new_infected <-
      round(num_negatives * (1 - exp(-foi * input_list$Dt)))
    number_new_infected <- max(0, number_new_infected)
    
    
    if (number_new_infected >= num_negatives) {
      number_new_infected <- num_negatives
    }
    
    animals$days_since_infection[sample(which(animals$days_since_infection == -1),
                                        number_new_infected,
                                        replace = FALSE)] <- 0
    
    animals$age <- animals$age + 1
    animals <- animals %>%
      mutate(
        days_since_infection =
          ifelse(days_since_infection != -1, days_since_infection + 1,-1),
        infected = days_since_infection != -1,
        esbl = ifelse(
          days_since_infection == 1,
          sum(sum_environment) / sum(sum_feces) * ingested_feces,
          esbl
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
    animals$content <- feces_amount
    
    animals$sum_feces <- animals$sum_feces + feces_amount
    
    return(animals)
    
  }
    
  ## Amount of feces ingested per day
  fm$ingested_feces <- function(animals) {
    
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
    
    content <- animals$content
    esbl <- animals$esbl
    
    excretion_cfu <-
      animals %>% mutate(
        cfu_environment = ifelse(days_since_infection != -1, esbl * input_list$e_rate, 0),
        sum_environment = sum_environment + cfu_environment - (
          animals$ingested_feces * sum(sum_environment) / sum(sum_feces)
        ),
        esbl = esbl - cfu_environment  + (
          animals$ingested_feces * sum(sum_environment) / sum(sum_feces)
        )
      )
  }
  
  ## Bacteria environmental decay  
  fm$environmental_decay <- function(animals) {
    
    animals %>%
      mutate(sum_environment = ifelse(
        days_since_infection != -1,
        sum_environment * (1 - input_list$ed_rate),
        sum_environment
      ))
  }
  
  return(fm)
  
}











