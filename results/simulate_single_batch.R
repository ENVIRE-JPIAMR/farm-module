## Compute QoIs for a single batch

source(here::here("visualization.R"))
source(here::here("run_farm_module.R"))

batch_output <- batch_simulator()

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
