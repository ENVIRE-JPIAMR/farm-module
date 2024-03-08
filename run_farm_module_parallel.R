source(here::here("load_libraries.R"))
source(here::here("load_inputs.R"))
source(here::here("farm_module.R"))

## Function to simulate production batches in parallel
## generates a 3D array: days-1 x 7 outputs x n_sim 

batch_simulator_parallel <- function(farm_module = new.farm_module(), n_sim) {
  
  # Setup parallel backend to use many cores
  cores <- detectCores()
  cl <- makeCluster(cores[1]/2)  # Modify if necessary based on your system's resources
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(min = 0, max = n_sim, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Run simulations in parallel and collect results
  results <- foreach(i = 1:n_sim, .combine = 'c', .options.snow = opts, .multicombine = TRUE, .inorder = FALSE) %dopar% {
    source(here::here("run_farm_module.R"))
    batch_output <- batch_simulator(farm_module)
    
    # Return the full output including animals_daily list
    return(list(batch_output$animals_daily))
  }
  
  stopCluster(cl)
  close(pb)
  
  # Organize the results into a more structured list if needed
  # results will be a list of lists, each containing animals_daily dataframes
  
  return(results)
}
