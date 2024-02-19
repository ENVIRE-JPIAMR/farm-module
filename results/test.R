## test script

source(here::here("load_libraries.R"))
source(here::here("load_inputs.R"))
source(here::here("farm_module.R"))

# initialization
fm = new.farm_module()
fm$params$farm_size  <- 1
fm$params$prevalence <- 0.5

animals <- fm$initialize_df() 
day <- fm$params$day.min

# step chronology
animals.ingested_feces      <- fm$ingested_feces(day, animals)
animals.new_infected        <- fm$new_infected(animals.ingested_feces)
animals.feces_function      <- fm$feces_function(day, animals.new_infected)
animals.logistic_growth     <- fm$logistic_growth(animals.feces_function)
animals.excretion           <- fm$excretion(animals.logistic_growth)
animals.environmental_decay <- fm$environmental_decay(animals.excretion)

# update
animals.environmental_decay$age <- animals.environmental_decay$age + 1
animals.environmental_decay$infection_duration <-
  ifelse(animals.environmental_decay$infection_duration != -1,
         animals.environmental_decay$infection_duration + 1,
         -1)

# next day
animals.ingested_feces      <- fm$ingested_feces(day, animals.environmental_decay)
animals.new_infected        <- fm$new_infected(animals.ingested_feces)
animals.feces_function      <- fm$feces_function(day, animals.new_infected)
animals.logistic_growth     <- fm$logistic_growth(animals.feces_function)
animals.excretion           <- fm$excretion(animals.logistic_growth)
animals.environmental_decay <- fm$environmental_decay(animals.excretion)

