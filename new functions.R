initial_animals_density <- function(target_weight, density, size) {
  # Calculate the number of animals based on broiler density and farm size
  n_animals <- density / target_weight * size
  
  # Initialize all animals as healthy
  animals <- tibble(
    days_since_infection = rep(-1, n_animals),  # All animals start as healthy
    age = rep(1, n_animals),  # Assuming age starts at 1 for simplicity
    content = rep(0, n_animals),
    sum_feces = rep(0, n_animals),
    esbl = rep(0, n_animals),  # ESBL count starts at 0 for all
    sum_environment = rep(0, n_animals),
    density = rep(density, n_animals),
    infected = rep(FALSE, n_animals),  # No animals are infected initially
    ingested_feces = rep(0, n_animals),
    cfu_environment = rep(0, n_animals)
  )
  
  return(animals)
}


scenario_1_flock_infected <- function(animals, prevalence, cfu) {
  # Ensure prevalence is between 0 and 1
  if(prevalence < 0 || prevalence > 1) {
    stop("Prevalence must be between 0 and 1.")
  }
  
  # Calculate the number of animals to be marked as positive
  n_positive <- round(nrow(animals) * prevalence)
  
  # Randomly select animals to be marked as positive
  positive_indices <- sample(nrow(animals), n_positive)
  
  # Update the animals data frame for the selected animals
  animals$infected[positive_indices] <- TRUE
  animals$days_since_infection[positive_indices] <- 1  # Assuming day 1 is the day of infection
  
  # Check if cfu is a single value or a vector
  if(length(cfu) == 1) {
    # If cfu is a single value, assign it directly to all selected animals
    animals$esbl[positive_indices] <- cfu
  } else {
    # If cfu is a vector, randomly assign CFU counts from the specified range/set
    animals$esbl[positive_indices] <- sample(cfu, n_positive, replace = TRUE)
  }
  
  return(animals)
}

scenario_2_breach_biosecurity <- function(animals, probability, cfu_entering) {
  # Determine if a breach in biosecurity occurs (TRUE or FALSE)
  breach_occurs <- runif(1) < probability
  
  if(breach_occurs) {
    # Calculate the amount of cfu to be added to each animal's environment
    cfu_per_animal <- cfu_entering / nrow(animals)
    
    # Update sum_environment for each animal
    animals$sum_environment <- animals$sum_environment + cfu_per_animal
  }
  
  return(animals)
}

### in this scenario they do not excrete
scenario_3_resistance_selection <- function(animals) {
  # Generate a random selection probability for each animal (0.5% to 2%)
  selection_probabilities <- runif(nrow(animals), 0.005, 0.02)
  
  # Determine if resistance selection occurs for each animal (TRUE or FALSE)
  selection_occurs <- runif(nrow(animals)) < selection_probabilities
  
  # Apply ESBL increase for animals where selection occurs
  # Calculate random ESBL increase only for those where selection occurs
  esbl_increase <- runif(nrow(animals), min = 10, max = 1000) * selection_occurs
  
  # Update ESBL counts where selection occurs
  animals$esbl <- animals$esbl + esbl_increase
  
  # Update the 'infected' status for animals where selection occurs and esbl_increase is applied
  animals$infected[selection_occurs] <- TRUE
  
  return(animals)
}


apply_random_scenario <- function(animals) {
  # Randomly select a scenario
  scenario_number <- sample(1:3, 1)  # Adjust numbers based on available scenarios
  
  # Apply the selected scenario
  animals <- switch(as.character(scenario_number),
                    "1" = scenario_1_flock_infected(animals, prevalence = 0.01, cfu = 10:1000),
                    "2" = scenario_2_breach_biosecurity(animals, probability = 0.02, cfu_entering = 100000),  # Corrected to show example values
                    "3" = scenario_3_resistance_selection(animals)
  )
  
  
  # Return both the modified animals and the selected scenario number
  return(list(animals = animals, scenario_number = scenario_number))
}



#this function simulates a production day
simulate_day <- function(animals, day, until, selected_scenario = NULL) {
  if (day == 1) {
    # Apply a randomly selected scenario on the first day and get the selected scenario
    result <- apply_random_scenario(animals)
    animals <- result$animals
    selected_scenario <- result$scenario_number
    
    # Add a new column to indicate the selected scenario
    animals$scenario <- selected_scenario
    
  }
  
  # Scenario 2: Breach in biosecurity (if selected)
  if (!is.null(selected_scenario) && selected_scenario == 2) {
    animals <- scenario_2_breach_biosecurity(animals, probability = 0.02, cfu_entering = 100000)
  }
  
  # Apply daily logic for scenario 3 if it was selected
  if (!is.null(selected_scenario) && selected_scenario == 3) {
    animals <- scenario_3_resistance_selection(animals)  # Adjust function name if necessary
  }
  
  
  # Your daily functions
  animals <- feces_function(day, animals)
  animals <- ingested_feces(day, animals)
  animals <- excretion(animals, e_rate = 0.3)
  animals <- logistic_growth(animals, K = 10^6 * animals$content, r = 10^runif(1, 0, 5))
  
  animals <- infection_animals2_model3(animals, rnorm(1, bconcentration_est, bconcentration_sd), 1)
  
  animals <- environmental_decay(animals, 0.5)
  
  # Recursive call for the next day, passing along the selected scenario
  if (day < until) 
    return(c(list(animals), simulate_day(animals, day = day + 1, until = until, selected_scenario = selected_scenario)))
  else 
    return(list(animals))
}


#montecarlo but also including the initial dataframe "animals" as day 1
# the map function is used to run the simulation in parallel
montecarlo <- map(1:30, .progress = TRUE, function(x) {
  simulated_days <- simulate_day(animals = animals, day = 1, until = 36)
  c(list(initial_animals), simulated_days)
})
