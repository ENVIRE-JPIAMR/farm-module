source("load_libraries.R")
source("load_inputs.R")
source("farm_module.R")

set.seed(123)

## Load farm module
farm_module <- new.farm_module()

animals <- farm_module$initialize_df()   
initial_animals <- animals

## Function to simulate a production day
simulate_day <- function(animals, day, until) {
  
  animals <- farm_module$feces_function (day,animals)
  animals <- farm_module$ingested_feces(animals)
  animals <- farm_module$excretion(animals)
  animals <- farm_module$logistic_growth(animals)
  animals <- farm_module$new_infected(animals)
  animals <- farm_module$environmental_decay(animals)
  animals <- farm_module$update_df(animals)
  
  if (day < until) 
    c(list(animals), simulate_day(animals, day = day + 1, until = until)) 
  else return(list(animals))
}

## MC simulations
montecarlo <- map(1:farm_module$params$n_sim, .progress = TRUE, function(x) {
  simulated_days <-
    simulate_day(animals = animals,
                 day = farm_module$params$day.min,
                 until = farm_module$params$day.max)
  c(list(initial_animals), simulated_days)
})
 
## Data frame with the results of the Monte Carlo simulation
df_montecarlo <-
  montecarlo |> map(bind_rows, .id = "day",  .progress = TRUE) |> bind_rows(.id = "groups") |>  mutate(day = as.numeric(day)) |> group_by(groups)

## Summarize the results of the Monte Carlo simulation
result <- df_montecarlo |>
  group_by(day, groups) |>
  summarise(
    C_sum_esbl_env = sum(C_sum_esbl_env, na.rm = TRUE),
    sum_feces_gut = sum(sum_feces_gut),
    C_esbl_gut = sum(C_esbl_gut)
  )
