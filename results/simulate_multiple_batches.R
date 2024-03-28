## Compute QoIs for multiple batches

source(here::here("visualization.R"))
source(here::here("run_farm_module_parallel.R"))

inputs <- load_inputs()
n_sim  <- inputs$n_sim

## Only selected results
parallel_output <- batch_simulator_parallel(n_sim = n_sim)
parallel_output_thinning <- batch_simulator_parallel(n_sim = n_sim, full = FALSE, thinning = TRUE, prevalence = TRUE)
parallel_output_both <- batch_simulator_parallel(n_sim = n_sim, full = FALSE, thinning = TRUE, prevalence = FALSE)

## post-process selected results
output_avg <- apply(parallel_output, c(1, 2), mean)
output_avg_thinning <- apply(parallel_output_thinning, c(1, 2), mean)
output_avg_both <- apply(parallel_output_both, c(1, 2), mean)

## Full results
parallel_output_full <- batch_simulator_parallel(n_sim = n_sim, full = TRUE)
parallel_output_full_thinning <- batch_simulator_parallel(n_sim = n_sim, full = TRUE, thinning = TRUE)

## post-process full results
post_process <- function(parallel_output_full){
  
  # output post-processing full = TRUE
  all_animals <-
    do.call(rbind, lapply(seq_along(parallel_output_full), function(iteration) {
      lapply(parallel_output_full[[iteration]], function(day_df) {
        day_df$iteration <- iteration
        return(day_df)
      })
    })) %>% dplyr::bind_rows()
  
  # summarize parallel results
  result <- all_animals |> 
    group_by(day, iteration) |>
    summarise(C_sum_esbl_env = sum(C_sum_esbl_env, na.rm = TRUE),
              sum_feces_env= sum(sum_feces_env),
                C_esbl_gut= sum(C_esbl_gut)
    )
  
  return(result)
}

## Make plots

# Concentration of ESBL E.coli in manure (CFU/g)
group1 <- output_avg[, 1]/(output_avg[, 4] + inputs$litter_mass)
group2 <- output_avg_thinning[, 1]/(output_avg_thinning[, 4] + inputs$litter_mass)
group3 <- output_avg_both[, 1]/(output_avg_both[, 4] + inputs$litter_mass)

# Total ESBL E.coli in environment (CFU)
group1 <- output_avg[, 1]
group2 <- output_avg_thinning[, 1]
group3 <- output_avg_both[, 1]

df <- data.frame(day = rep(1:inputs$day.max, 3),
                 value = c(group1, group2, group3),
                 scenario = rep(c("Baseline", "Thinning", "Baseline+Thinning"), each = dim(output_avg)[1]))

ggplot(df, aes(x = day, y = value, color = scenario)) +
  geom_line() +
  labs(title = "Effect of thinning (avg. over simulations)",
       x = "Day",
       y = "ESBL E.coli in manure (CFU/g)")
       #y = "Total ESBL E.coli in env. (CFU)")


## Make plots (like Nunzio)

df <- post_process(parallel_output_full)
df_thinning <- post_process(parallel_output_full_thinning)

# Plot the multiple lines
ggplot(df_thinning, aes(x = day, y = C_sum_esbl_env/(sum_feces_env+inputs$litter_mass), group = iteration)) +
  geom_line(alpha = 0.1) +  # Set alpha for transparency
  labs(x = "Day", y = "ESBL E.coli in manure (CFU/g)", 
       title = "Thinning with prevalence") +
  theme_minimal()

ggplot(df_thinning, aes(x = day, y = C_sum_esbl_env, group = iteration)) +
  geom_line(alpha = 0.1) +  # Set alpha for transparency
  labs(x = "Day", y = "Total ESBL E.coli in env. (CFU)", 
       title = "Thinning with prevalence") +
  theme_minimal()




