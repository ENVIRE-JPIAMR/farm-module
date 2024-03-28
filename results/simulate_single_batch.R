## Compute QoIs for a single batch

source(here::here("visualization.R"))
source(here::here("run_farm_module.R"))

# different functions to simulate a single batch
batch_output <- batch_simulator()
batch_output_full <- batch_simulator_full()
batch_output_thinning <- batch_simulator_thinning(full = FALSE)

# bind the full output dataframes
batch_output_full_df <- bind_rows(batch_output_full)
batch_output_thinning_df <- bind_rows(batch_output_thinning)

## Plot for within flock prevalence
plot_qoi(
  data     = batch_output,
  qoi      = batch_output$prevalence,
  ci_lower = NA,
  ci_upper = NA,
  title    = "Flock prevalence",
  xlab     = "Days",
  ylab     = "Proportion of infected broilers"
)

## Plot for average (over broilers) excreted bacteria 
plot_qoi(
  data     = batch_output,
  qoi      = log10(batch_output$avg_esbl_feces),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Bacterial concentration in excreted feces",
  xlab     = "Days",
  ylab     = "ESBL E. coli (log10 CFU/g)"
)
