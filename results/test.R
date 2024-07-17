## test script

source(here::here("farm-module/load_libraries.R"))
source(here::here("farm-module/load_inputs.R"))
source(here::here("farm-module/farm_module.R"))

# initialization
fm = new.farm_module()
fm$params$farm_size  <- 1
fm$params$prevalence <- 0.5

animals <- fm$initialize_df() 
day <- fm$params$day.min

# step chronology
animals.feces_production    <- fm$feces_production(day, animals)
animals.feces_ingestion     <- fm$feces_ingestion(day, animals.feces_production)
animals.excretion           <- fm$excretion(animals.feces_ingestion)
animals.logistic_growth     <- fm$logistic_growth(animals.excretion)
animals.transmission        <- fm$transmission(animals.logistic_growth)
animals.environmental_decay <- fm$environmental_decay(animals.transmission)

# update
animals.environmental_decay$age <- animals.environmental_decay$age + 1
animals.environmental_decay$infection_duration <-
  ifelse(animals.environmental_decay$infection_duration != -1,
         animals.environmental_decay$infection_duration + 1,
         -1)

# next day
animals.feces_production    <- fm$feces_production(day, animals.environmental_decay)
animals.feces_ingestion     <- fm$feces_ingestion(day, animals.feces_production)
animals.excretion           <- fm$excretion(animals.feces_ingestion)
animals.logistic_growth     <- fm$logistic_growth(animals.excretion)
animals.transmission        <- fm$transmission(animals.logistic_growth)
animals.environmental_decay <- fm$environmental_decay(animals.transmission)

