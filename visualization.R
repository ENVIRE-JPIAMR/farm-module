## Script for visualization

## Reusable function for plotting with CI band
plot_qoi <- function(qoi, ci_lower, ci_upper, title, subtitle = NULL, xlab, ylab) {
  
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

## Reusable function for plotting two QoIs with CI bands
library(ggplot2)

plot_qois <- function(data1, data2, title, subtitle = NULL, xlab, ylab) {
  
  # Combine data1 and data2 into a single data frame
  combined_data <- data.frame(
    days      = 2:(length(data1$qoi) + 1),
    qoi1      = data1$qoi,
    ci1_lower = data1$ci_lower,
    ci1_upper = data1$ci_upper,
    qoi2      = data2$qoi,
    ci2_lower = data2$ci_lower,
    ci2_upper = data2$ci_upper
  )
  
  # Melt the data for easier plotting
  combined_data_long <- data.frame(
    days = rep(2:(length(data1$qoi) + 1), 2),
    qoi = c(data1$qoi, data2$qoi),
    ci_lower = c(data1$ci_lower, data2$ci_lower),
    ci_upper = c(data1$ci_upper, data2$ci_upper),
    group = factor(rep(c("Control", "Phages"), each = length(data1$qoi)))
  )
  
  # Initialize ggplot
  gg <- ggplot(data = combined_data_long) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab,
      color = "Group",
      fill = "Group"
    )
  
  # Add QoI and its CI
  gg <- gg +
    geom_point(aes(x = days, y = qoi, color = group)) +
    geom_line(aes(x = days, y = qoi, color = group)) +
    geom_ribbon(aes(x = days, ymin = ci_lower, ymax = ci_upper, fill = group), alpha = 0.2) +
    scale_color_manual(values = c("Control" = "blue", "Phages" = "red")) +
    scale_fill_manual(values = c("Control" = "blue", "Phages" = "red"))
  
  return(gg)
}


