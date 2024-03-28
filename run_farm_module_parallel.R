source(here::here("load_libraries.R"))
source(here::here("load_inputs.R"))
source(here::here("farm_module.R"))

## Function to simulate production batches in parallel
## arguments: full -> TRUE; to generate full animals dataframe
##                    FALSE, to generates a 3D array: days-1 x 7 outputs x n_sim 
##            thinning -> TRUE; to perform thinning
##            prevalence -> TRUE; to set prevalence to 0

batch_simulator_parallel <- function(farm_module = new.farm_module(), n_sim, full = FALSE, thinning = FALSE, prevalence = TRUE) {
  
  # custom bind function
  mybind <- function(matrix1, matrix2) {
      abind(matrix1, matrix2, along = 3)
    }

  # setup parallel backend to use many cores
  cores = detectCores()
  cl <- makeCluster(cores[1]/2)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(min = 0, max = n_sim, style = 3)
  progress <- function(n_sim)
    setTxtProgressBar(pb, n_sim)
  opts <- list(progress = progress)
  
  day_max <- farm_module$params$day.max
  
  if (full == FALSE) {
    # Initialize a 3D array to store results
    # TODO: remove hard coded dimension
    output <- array(0, dim = c(day_max - 1, n_sim, 7))
    
    output <-
      foreach(i = 1:n_sim,
              .combine = mybind,
              .options.snow = opts) %dopar% {
                source(here::here("run_farm_module.R"))
                if (thinning == TRUE){
                  batch_output <- do.call(cbind, batch_simulator_thinning(farm_module, full = full, prevalence = prevalence))
                } else {
                  batch_output <- do.call(cbind, batch_simulator(farm_module))
                }
                return(batch_output)
              }
  } else {
    output <-
      foreach(
        i = 1:n_sim,
        .combine = 'c',
        .options.snow = opts,
        .multicombine = TRUE,
        .inorder = FALSE
      ) %dopar% {
        source(here::here("run_farm_module.R"))
        if (thinning == TRUE){
          batch_output <- batch_simulator_thinning(farm_module, full = full, prevalence = prevalence)
        } else {
          batch_output <- batch_simulator_full(farm_module)
        }
        
        return(batch_output)
      }
  }

  return(output)
}
