ci_single_prop_sim <- function(y, success, conf_level, y_name,
                               boot_method, nsim, seed, 
                               show_var_types, show_summ_stats, show_res,
                               show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # calculate sample size
  n <- length(y) 
  
  # calculate p_hat
  p_hat <- sum(y == success) / n
  
  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- sum(boot_samp == success) / n
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
    
    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)
    
    # find critical value
    z_star <- qnorm(perc_crit_value)
    
    # calculate SE
    se <- sd(sim_dist)
    
    # calculate ME
    me <- z_star * se
    
    # calculate CI
    ci <- p_hat + c(-1, 1) * me
  }
  
  # print variable types
  if(show_var_types == TRUE){
    cat(paste0("Single categorical variable, success: ", success,"\n"))
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    cat(paste0("n = ", n, ", p-hat = ", round(p_hat, 4), "\n"))
  }
  
  # print results
  if(show_res == TRUE){
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI: (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }
  
  # eda_plot
  d_eda <- data.frame(y = y)
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_bar(fill = "#8FDEE1") +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution")
  
  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = ci[1], xmax = ci[2], ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("bootstrap means") +
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
    return(list(sim_dist = sim_dist, CI = round(ci, 4)))
  } else {
    return(list(sim_dist = sim_dist, SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  }
  
}