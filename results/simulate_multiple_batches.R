## Compute QoIs for multiple batches

source(here::here("visualization.R"))
source(here::here("run_farm_module_parallel.R"))

inputs <- load_inputs()
n_sim  <- inputs$n_sim

parallel_output <- batch_simulator_parallel(n_sim = n_sim)
output_avg      <- apply(parallel_output, c(1, 2), mean)
output_std      <- apply(parallel_output, c(1, 2), sd)
err             <- qt(p = 0.975, df = (n_sim - 1)) * output_std / sqrt(n_sim)
ci.lower        <- output_avg - err
ci.upper        <- output_avg + err

## Plot for average Within flock prevalence
plot_qoi(
  data     = output_avg,
  qoi      = output_avg[, 2],
  ci_lower = ci.lower[, 2],
  ci_upper = ci.upper[, 2],
  title    = "Average flock prevalence",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "Proportion of infected broilers"
)

## Plot for average (over broilers + iterations) excreted bacteria 
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 3]),
  ci_lower = log10(ci.lower[, 3]),
  ci_upper = log10(ci.upper[, 3]),
  title    = "Average Bacterial concentration in excreted feces",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "ESBL E. coli (log10 CFU/g)"
)

## Plot for amount of litter + feces  
plot_qoi(
  data     = output_avg,
  qoi      = (output_avg[, 4]+inputs$litter_mass)/1000,
  ci_lower = NA,
  ci_upper = NA,
  title    = "Mass of litter + feces",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "pen mass (kg)"
)

## Plot for number of bacteria per sq. meter  
plot_qoi(
  data     = output_avg,
  qoi      = output_avg[, 1]/inputs$farm_size,
  ci_lower = NA,
  ci_upper = NA,
  title    = "Concentration in bacteria in farm",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "pen concentration (log10 CFU/m2)"
)

## Plot for number of bacteria per g of pen mass  
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 1]/(output_avg[, 4]+inputs$litter_mass)),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Concentration in bacteria in manure (litter + feces)",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "pen concentration (log10 CFU/m2)"
)

## Plot for average number of bacteria in the broiler's gut
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 5]),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Concentration in bacteria in broilers gut",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "gut concentration (log10 CFU)"
)

## Plot for average number of bacteria excreted
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 6]),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Concentration in bacteria excreted",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "excretion concentration (log10 CFU)"
)

## Plot for average number of bacteria ingested
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 7]),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Concentration in bacteria ingested",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "ingestion concentration (log10 CFU)"
)

#### Creates a dataframe with all the animals per each iteration
all_animals <- do.call(rbind, lapply(seq_along(parallel_output), function(iteration) {
  lapply(parallel_output[[iteration]], function(day_df) {
    day_df$iteration <- iteration
    return(day_df)
  })
})) %>% dplyr::bind_rows()
