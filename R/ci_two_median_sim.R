ci_two_median_sim <- function(y, x, conf_level, y_name, x_name,
                              boot_method, nsim, seed, 
                              show_var_types, show_summ_stats, show_res,
                              show_eda_plot, show_inf_plot){
  
  # set seed
  if(!is.null(seed)){ set.seed(seed) }
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate y-bar1 and y-bar2
  y_meds <- by(y, x, median)
  y_med1 <- as.numeric(y_meds[1])
  y_med2 <- as.numeric(y_meds[2])
  
  # calculate difference in y-bars
  y_med_diff <- y_med1 - y_med2
  
  # create bootstrap distribution
  y1 <- y[x == levels(x)[1]]
  y2 <- y[x == levels(x)[2]]
  
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp1 <- sample(y1, size = n1, replace = TRUE)
    boot_samp2 <- sample(y2, size = n2, replace = TRUE)
    sim_dist[i] <- median(boot_samp1) - median(boot_samp2)
  }
  
  # for percentile method
  if(boot_method == "perc"){
    # calculate quantile cutoffs based on confidence level
    lower_quantile <- (1-conf_level) / 2
    upper_quantile <- conf_level + lower_quantile
    
    # calculate quantiles of the bootstrap distribution
    ci_lower <- as.numeric(quantile(sim_dist, lower_quantile))
    ci_upper <- as.numeric(quantile(sim_dist, upper_quantile))
    
    # put CI together
    ci <- c(ci_lower, ci_upper)
  }
  
  # for standard error method
  if(boot_method == "se"){
    # define degrees of freedom
    df <- min(n1 - 1, n2 - 1)
    
    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)
    
    # find critical value
    t_star <- qt(perc_crit_value, df)
    
    # calculate SE
    se <- sd(sim_dist)
    
    # calculate ME
    me <- t_star * se
    
    # calculate CI
    ci <- y_med_diff + c(-1, 1) * me
  }
  
  # print variable types
  if(show_var_types == TRUE){
    n_x_levels <- length(levels(x))
    cat(paste0("Response variable: numerical, Explanatory variable: categorical (", n_x_levels," levels)\n"))
  }
  
  # print summary statistics
  gr1 <- levels(x)[1]
  gr2 <- levels(x)[2]
  
  if(show_summ_stats == TRUE){
    iqrs <- by(y, x, IQR)
    iqr1 <- as.numeric(iqrs[1])
    iqr2 <- as.numeric(iqrs[2])
    cat(paste0("n_", gr1, " = ", n1, ", y_med_", gr1, " = ", round(y_med1, 4), ", IQR_", gr1, " = ", round(iqr1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", y_med_", gr2, " = ", round(y_med2, 4), ", IQR_", gr2, " = ", round(iqr2, 4), "\n"))
  }
  
  # print results
  if(show_res == TRUE){
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI (", gr1 ," - ", gr2,"): (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)

  eda_plot <- ggplot(data = d_eda, aes(x = x, y = y), environment = environment()) +
    geom_boxplot(color = "#1FBEC3", fill = "#8FDEE1", outlier.colour = "#1FBEC3") +
    xlab(x_name) +
    ylab(y_name) +
    ggtitle("Sample Distribution")

  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = ci[1], xmax = ci[2], ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("bootstrap differences in medians") +
    ylab("") +
    ggtitle("Bootstrap Distribution") +
    geom_vline(xintercept = ci, color = "#F57670", lwd = 1.5)
  
  # print plots
  if(show_eda_plot & !show_inf_plot){ 
    print(eda_plot)
  }
  if(!show_eda_plot & show_inf_plot){ 
    print(inf_plot)
  }
  if(show_eda_plot & show_inf_plot){
    grid.arrange(eda_plot, inf_plot, ncol = 2)
  }
  
  # return
  if(boot_method == "perc"){
    return(list(sim_dist = sim_dist, CI = ci))
  } else {
    return(list(sim_dist = sim_dist, SE = se, ME = me, CI = ci))
  }
}