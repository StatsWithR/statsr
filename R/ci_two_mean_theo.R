ci_two_mean_theo <- function(y, x, conf_level, y_name, x_name,
                             show_var_types, show_summ_stats, show_res,
                             show_eda_plot, show_inf_plot){
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate y-bar1 and y-bar2
  y_bars <- by(y, x, mean)
  y_bar1 <- as.numeric(y_bars[1])
  y_bar2 <- as.numeric(y_bars[2])
  
  # calculate difference in y-bars
  y_bar_diff <- y_bar1 - y_bar2
  
  # calculate s1 and s2
  sds <- by(y, x, sd)
  s1 <- as.numeric(sds[1])
  s2 <- as.numeric(sds[2])

  # define degrees of freedom
  df <- min(n1 - 1, n2 - 1)
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  t_star <- qt(perc_crit_value, df)
  
  # calculate SE
  se <- sqrt((s1^2 / n1) + (s2^2 / n2))
  
  # calculate ME
  me <- t_star * se
  
  # calculate CI
  ci <- y_bar_diff + c(-1, 1) * me
  
  # print variable types
  if(show_var_types == TRUE){
    n_x_levels <- length(levels(x))
    cat(paste0("Response variable: numerical, Explanatory variable: categorical (", n_x_levels," levels)\n"))
  }
  
  # print summary statistics
  gr1 <- levels(x)[1]
  gr2 <- levels(x)[2]
  
  if(show_summ_stats == TRUE){
    cat(paste0("n_", gr1, " = ", n1, ", y_bar_", gr1, " = ", round(y_bar1, 4), ", s_", gr1, " = ", round(s1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", y_bar_", gr2, " = ", round(y_bar2, 4), ", s_", gr2, " = ", round(s2, 4), "\n"))
  }
  
  # print results
  if(show_res == TRUE){
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI (", gr1 ," - ", gr2,"): (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  d_means <- data.frame(y_bars = as.numeric(y_bars), x = levels(x))
  
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab(x_name) +
    ggtitle("Sample Distribution") +
    geom_vline(data = d_means, aes(xintercept = y_bars), col = "#1FBEC3", lwd = 1.5) +
    facet_grid(x ~ .)
    
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.") }
  
  # return
  return(list(df = df, SE = se, ME = me, CI = ci))
}