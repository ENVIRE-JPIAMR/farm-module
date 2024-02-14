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
