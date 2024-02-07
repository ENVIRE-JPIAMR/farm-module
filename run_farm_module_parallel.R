source("load_libraries.R")
source("load_inputs.R")
source("farm_module.R")

## Function to simulate production batches in parallel
## generates a 3D array: days x 5 outputs x n_sim 
## 1) prevalence     := Proportion of infected broilers 
## 2) load           := Total ESBL E. coli in the environment (CFU)
## 3) total_feces    := Total amount of feces in broiler's gut (g)
## 4) concentration  := load/total_feces
## 5) total_esbl_gut := Total ESBL E. coli in broiler's gut (CFU)
batch_simulator_parallel <- function(farm_module = new.farm_module(), n_sim) {
  
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
  output <- array(0, dim = c(day_max - 1, n_sim, 5))  # Initialize a 3D array to store results
  
  output <- foreach(i = 1:n_sim, .combine = mybind, .options.snow = opts) %dopar% {
    source("run_farm_module.R")
    batch_output <- do.call(cbind, batch_simulator(farm_module))
    
    return(batch_output)
  }
  
  return(output)
}
