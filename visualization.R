## Script for visualization

## Reusable function for plotting with CI band
plot_qoi <- function(data, qoi, ci_lower, ci_upper, title, subtitle = NULL, xlab, ylab) {
  
  # Check if all values in ci_lower or ci_upper are NA
  if (all(is.na(ci_lower)) || all(is.na(ci_upper))) {
    gg <- ggplot(data = data.frame(
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
  } else {
    gg <- ggplot(data = data.frame(
      days = 2:(length(qoi) + 1),
      qoi = qoi,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )) +
      geom_point(aes(x = days, y = qoi)) +
      geom_line(aes(x = days, y = qoi)) +
      geom_ribbon(aes(x = days, ymin = ci_lower, ymax = ci_upper),
                  alpha = 0.2) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xlab,
        y = ylab
      )
  }
  
  return(gg)
}


## Compute QoIs for a single batch

source("run_farm_module.R")

batch_output <- batch_simulator()

## Plot for Within flock prevalence
plot_qoi(
  data     = batch_output,
  qoi      = batch_output$prevalence,
  ci_lower = NA,
  ci_upper = NA,
  title    = "Flock prevalence",
  xlab     = "Days",
  ylab     = "Proportion of infected broilers"
)

## Plot for Bacterial concentration in the barn
plot_qoi(
  data     = batch_output,
  qoi      = log10(batch_output$concentration),
  ci_lower = NA,
  ci_upper = NA,
  title    = "Bacterial concentration in feces",
  xlab     = "Days",
  ylab     = "C_barn (log10 CFU/g)"
)

## Compute QoIs for multiple batches

source("run_farm_module_parallel.R")
inputs <- load_inputs()
n_sim  <- inputs$n_sim

parallel_output <- batch_simulator_parallel(n_sim = n_sim)
output_avg      <- apply(parallel_output, c(1, 2), mean)
output_std      <- apply(parallel_output, c(1, 2), sd)
err             <- qt(p = 0.975, df = (n_sim - 1)) * output_std / sqrt(n_sim)
ci.lower        <- output_avg - err
ci.upper        <- output_avg + err

## Plot for Average Within flock prevalence
plot_qoi(
  data     = output_avg,
  qoi      = output_avg[, 1],
  ci_lower = ci.lower[, 1],
  ci_upper = ci.upper[, 1],
  title    = "Average flock prevalence",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "Proportion of infected broilers"
)

## Plot for Average Bacterial concentration in the barn
plot_qoi(
  data     = output_avg,
  qoi      = log10(output_avg[, 4]),
  ci_lower = log10(ci.lower[, 4]),
  ci_upper = log10(ci.upper[, 4]),
  title    = "Average Bacterial concentration in feces",
  subtitle = paste(dim(parallel_output)[3], "simulations"),
  xlab     = "Days",
  ylab     = "C_barn (log10 CFU/g)"
)
