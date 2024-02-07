## Script for visualization

## Reusable function for plotting
plot_qoi <- function(data, qoi, title, subtitle = NULL, xlab, ylab) {
  ggplot(data = data.frame(
    days = 2:(length(qoi) + 1),
    qoi = qoi
  )) +
    geom_point(aes(x = days, y = qoi)) +
    geom_line(aes(x = days, y = qoi)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    )
}

## Compute QoIs for a single batch

source("run_farm_module.R")

batch_output <- batch_simulator()
C_barn       <- log10(batch_output$load / batch_output$total_feces)

## Plot for Within flock prevalence
plot_qoi(
  data  = batch_output,
  qoi   = batch_output$prevalence,
  title = "Flock prevalence",
  xlab  = "Days",
  ylab  = "Proportion of infected broilers"
)

## Plot for Bacterial concentration in the barn
plot_qoi(
  data  = batch_output,
  qoi   = C_barn,
  title = "Bacterial concentration in feces",
  xlab  = "Days",
  ylab  = "C_barn (log10 CFU/g)"
)

## Compute QoIs for multiple batches

source("run_farm_module_parallel.R")
inputs <- load_inputs()

parallel_output <- batch_simulator_parallel(n_sim = inputs$n_sim)
output_avg      <- apply(parallel_output, c(1, 2), mean)

## Plot for Average Within flock prevalence
plot_qoi(
  data     = output_avg,
  qoi      = output_avg[, 1],
  title    = "Average flock prevalence",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "Proportion of infected broilers"
)

## Plot for Average Bacterial concentration in the barn
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 2] / output_avg[, 3]),
  title    = "Average Bacterial concentration in feces",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "C_barn (log10 CFU/g)"
)
