# Script to estimate reductions for vaccination intervention for Lithuania data

# Number of bootstrap samples
n_bootstraps <- 10000

# Function to generate bootstrap samples and compute the mean
bootstrap_means <- function(data, n_bootstraps) {
  boot_means <- numeric(n_bootstraps)
  n <- length(data)
  for (i in 1:n_bootstraps) {
    boot_sample <- sample(data, n, replace = TRUE)
    boot_means[i] <- mean(boot_sample)
  }
  return(boot_means)
}

# Original data from Lituania
control_42 <- c(10000, 87000, 8100,11000, 9100, 23000, 1100000, 900, 5700, 740000, 4700, 
             3600000, 29000, 2600, 1100, 6500, 1300000, 770000, 11000, 13000, 9100, 23000, 
             97000, 8100, 2600, 2100, 1600000, 790000, 1000000, 900, 5700, 720000, 4700, 
             3800000, 29000, 8500)

vaccinated_42 <- c(7400, 77000, 100000, 850000, 28000, 2700, 45000, 3400000, 25000, 930000, 30000, 
                1200, 20000, 100, 1100, 340000, 2500, 2000, 6200, 77000, 750000, 28000, 4700, 
                45000, 8400, 120000, 2500, 2000, 8200, 1200, 18000, 100, 3600000, 25000, 810000, 
                30000, 1100, 240000)

vaccinated_21 <- c(81000, 13000, 68000, 170000, 42000, 250000, 330000, 27000, 670000, 370000, 
                   1300000, 460000, 4100000, 9400, 9400, 570000, 150000, 46000, 52000, 910, 
                   71000, 23000, 78000, 160000, 42000, 170000, 320000, 37000, 640000, 910, 
                   350000, 9400, 52000, 370000, 1300000, 460000, 4100000, 8400, 570000, 46000)

control_21 <- c(1200000, 20000, 65000, 1200000, 130000, 170000, 120000, 98000, 98000, 
                690000, 28000, 920000, 270000, 1300000, 620000, 910000, 18000, 44000, 
                820000, 20000, 1100000, 170000, 120000, 98000, 98000, 1100000, 65000, 
                230000, 910000, 820000, 690000, 28000, 960000, 370000, 1100000, 420000, 
                18000, 48000)

control_6 <- c(500, 1600000, 930000, 1400000, 730000, 500, 1200000, 500, 
               930000, 430000, 500, 120000)

vaccinated_6 <-  c(2700, 400, 800, 18000, 100, 38000, 26000, 2700, 400, 38000, 800, 
                 18000, 100, 26000, 2700, 600, 900, 18000, 100, 58000, 36000, 
                 2700, 400, 58000, 800, 28000, 26000, 200)

control_5 <- c(900, 900, 1100000, 2400000, 6500, 900, 1000000, 2800000, 1100, 8500, 2200, 2000, 
               8500, 980000, 2400000, 1900, 1000, 950000, 9500, 2400000)

vaccinated_5 <- c(400, 2100, 600, 2400000, 2500, 66000, 1500000, 300, 2200, 700, 
                  3400000, 2500, 1500000, 2500, 66000, 1500000, 600, 66000, 600, 
                  2400000, 1900, 400, 66000, 2100, 600, 1500000, 1400000, 2500)

# Generate bootstrap samples and calculate means for both control and vaccinated groups
boot_means_control_42 <- bootstrap_means(control_42, n_bootstraps)
boot_means_vaccinated_42 <- bootstrap_means(vaccinated_42, n_bootstraps)

boot_means_control_21 <- bootstrap_means(control_21, n_bootstraps)
boot_means_vaccinated_21 <- bootstrap_means(vaccinated_21, n_bootstraps)

boot_means_control_6 <- bootstrap_means(control_6, n_bootstraps)
boot_means_vaccinated_6 <- bootstrap_means(vaccinated_6, n_bootstraps)

boot_means_control_5 <- bootstrap_means(control_5, n_bootstraps)
boot_means_vaccinated_5 <- bootstrap_means(vaccinated_5, n_bootstraps)

# Calculate reduction rates
boot_reduction_rates_42 <- 1 - boot_means_vaccinated_42 / boot_means_control_42
boot_reduction_rates_21 <- 1 - boot_means_vaccinated_21 / boot_means_control_21
boot_reduction_rates_6  <- 1 - boot_means_vaccinated_6 / boot_means_control_6
boot_reduction_rates_5  <- 1 - boot_means_vaccinated_5 / boot_means_control_5

# Calculate 95% confidence interval and sd using the percentile method
ci_42 <- quantile(boot_reduction_rates_42, c(0.025, 0.975))
sd_42 <- sd(boot_reduction_rates_42)

ci_21 <- quantile(boot_reduction_rates_21, c(0.025, 0.975))
sd_21 <- sd(boot_reduction_rates_21)

ci_6 <- quantile(boot_reduction_rates_6, c(0.025, 0.975))
sd_6 <- sd(boot_reduction_rates_6)

ci_5 <- quantile(boot_reduction_rates_5, c(0.025, 0.975))
sd_5 <- sd(boot_reduction_rates_5)

# Print 
cat(sprintf("Reduction Rate 42: %.2f%%\n", mean(boot_reduction_rates_42) * 100))
cat(sprintf("95%% CI 42: [%.2f%%, %.2f%%]\n", ci_42[1] * 100, ci_42[2] * 100))
cat(sprintf("sd 42: %.2f\n", sd_42))

cat(sprintf("Reduction Rate 21: %.2f%%\n", mean(boot_reduction_rates_21) * 100))
cat(sprintf("95%% CI 21: [%.2f%%, %.2f%%]\n", ci_21[1] * 100, ci_21[2] * 100))
cat(sprintf("sd 21: %.2f\n", sd_21))

cat(sprintf("Reduction Rate 6: %.2f%%\n", mean(boot_reduction_rates_6) * 100))
cat(sprintf("95%% CI 6: [%.2f%%, %.2f%%]\n", ci_6[1] * 100, ci_6[2] * 100))
cat(sprintf("sd 6: %.2f\n", sd_6))

cat(sprintf("Reduction Rate 5: %.2f%%\n", mean(boot_reduction_rates_5) * 100))
cat(sprintf("95%% CI 5: [%.2f%%, %.2f%%]\n", ci_5[1] * 100, ci_5[2] * 100))
cat(sprintf("sd 5: %.2f", sd_5))

# plot
library(ggplot2)

# Data: reduction rates, standard deviations, and day points
days <- c(5, 6, 21, 42)
reduction_rates <- c(mean(boot_reduction_rates_5), 
                     mean(boot_reduction_rates_6), 
                     mean(boot_reduction_rates_21), 
                     mean(boot_reduction_rates_42)) 
sd <- c(sd_5, sd_6, sd_21, sd_42)  

# Create a data frame
data <- data.frame(
  Day = days,
  ReductionRate = reduction_rates,
  SD = sd
)

# Plot the data with error bars
ggplot(data, aes(x = Day, y = ReductionRate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = ReductionRate - SD, ymax = ReductionRate + SD), width = 0.5, color = "red") +
  labs(title = "Reduction Rates Over Time",
       x = "Day",
       y = "Reduction Rate") +
  theme_minimal()

# Fit
# Create a data frame
data <- data.frame(
  Day = days,
  ReductionRate = reduction_rates,
  SD = sd
)

# Fit a polynomial regression model (2nd degree polynomial)
poly_fit <- lm(ReductionRate ~ poly(Day, 2, raw = TRUE), data = data, weights = 1 / SD^2)

# Predict values for each day from 5 to 42
predicted_days <- seq(5, 42, by = 1)
predicted_reduction_rates <- predict(poly_fit, newdata = data.frame(Day = predicted_days))

# Create a data frame for predicted values
predicted_data <- data.frame(
  Day = predicted_days,
  PredictedReductionRate = predicted_reduction_rates
)

# Plot the original data and the fitted curve
ggplot(data, aes(x = Day, y = ReductionRate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = ReductionRate - SD, ymax = ReductionRate + SD), width = 0.5, color = "red") +
  geom_line(data = predicted_data, aes(x = Day, y = PredictedReductionRate), color = "green") +
  labs(title = "Reduction Rates Over Time with Polynomial Fit",
       x = "Day",
       y = "Reduction Rate") +
  theme_minimal()
