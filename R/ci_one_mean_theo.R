ci_single_mean_theo <- function(y, conf_level, y_name, 
                                show_var_types, show_summ_stats, show_res,
                                show_eda_plot, show_inf_plot){

  # calculate sample size
  n <- length(y) 

  # calculate x-bar
  y_bar <- mean(y)

  # define degrees of freedom
  df <- n - 1
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  t_star <- qt(perc_crit_value, df)
  
  # calculate s
  s <- sd(y)

  # calculate SE
  se <- s / sqrt(n)
  
  # calculate ME
  me <- t_star * se
  
  # calculate CI
  ci <- y_bar + c(-1, 1)* me

  # print variable types
  if(show_var_types == TRUE){
    cat("Single numerical variable\n")
  }

  # print summary statistics
  if(show_summ_stats == TRUE){
    cat(paste0("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(s, 4), "\n"))
  }

  # print results
  if(show_res == TRUE){
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI: (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }

  # eda_plot
  d_eda <- data.frame(y = y)
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    geom_vline(xintercept = y_bar, col = "#1FBEC3", lwd = 1.5)
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.", call. = FALSE) }

  # return
  return(list(df = df, SE = se, ME = me, CI = ci))
  
}