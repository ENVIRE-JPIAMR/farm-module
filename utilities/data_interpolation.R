source(here::here("farm-module/load_libraries.R"))
source(here::here("farm-module/load_inputs.R"))

interpolate_values <- function(){
  
  inputs <- load_inputs()
  
  # Example list of values indexed over time
  time_series <- list(time = 1:inputs$day.max,
                      water_consum.min = inputs$water_consum.min,
                      water_consum.max = inputs$water_consum.max,
                      daily_gain = inputs$daily_gain,
                      daily_intake = inputs$daily_intake)
  
  # New time indices to predict
  new_times <- (inputs$day.max+1):42
  
  df <- as.data.frame(time_series)
  model_wc.min <- lm(water_consum.min ~ time, data = df)
  model_wc.max <- lm(water_consum.max ~ time, data = df)
  model_dg <- lm(daily_gain ~ time, data = df)
  model_di <- lm(daily_intake ~ time, data = df)
  
  # Predict the values for the new time points
  predicted_values_wc.min <- predict(model_wc.min, newdata = data.frame(time = new_times))
  predicted_values_wc.max <- predict(model_wc.max, newdata = data.frame(time = new_times))
  predicted_dg <- predict(model_dg, newdata = data.frame(time = new_times))
  predicted_di <- predict(model_di, newdata = data.frame(time = new_times))
  
  # New manual input lists
  input_manual <- list(water_consum.min = c(inputs$water_consum.min, unname(predicted_values_wc.min)),
                       water_consum.max = c(inputs$water_consum.max, unname(predicted_values_wc.max)),
                       daily_gain = c(inputs$daily_gain, unname(predicted_dg)),
                       daily_intake = c(inputs$daily_intake, unname(predicted_di)))
  
  return(input_manual)
}
