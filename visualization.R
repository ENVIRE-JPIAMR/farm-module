## Script for visualization

## Compute QoIs for a single batch

source("run_farm_module.R")

# 1) Prev_wfp_col := within flock prevalence infected broilers/Total broilers
# 2) C_barn := concentration of ESBL E. coli in the environment (CFU/g of feces)

output <- batch_simulator()
C_barn <- log10(output$load/output$total_feces)

# Plot for Within flock prevalence
ggplot(data = data.frame(
  days = 2:(length(output$prevalence) + 1),
  prevalence = output$prevalence
  )) +
  geom_point(aes(x = days, y = prevalence)) +
  geom_line(aes(x = days, y = prevalence)) +
  labs(title = "Within flock prevalence",
       x = "Days",
       y = "Proportion of infected broilers")

# Plot for Bacterial concentration in the barn
ggplot(data = data.frame(
  days = 2:(length(output$prevalence) + 1), 
  C_barn = C_barn
  )) +
  geom_point(aes(x = days, y = C_barn)) +
  geom_line(aes(x = days, y = C_barn)) +
  labs(title = "Bacterial concentration in the barn",
       x = "Days",
       y = "C_barn (log10 CFU/g)")

