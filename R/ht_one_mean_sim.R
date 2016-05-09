ht_single_mean_sim <- function(y, null, alternative, y_name,
                               nsim, seed, 
                               show_var_types, show_summ_stats, show_res,
                               show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){ set.seed(seed) }
  
  # calculate sample size
  n <- length(y) 
  
  # calculate y-bar
  y_bar <- mean(y)
  
  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- mean(boot_samp)
  }
  
  # center bootstrap distribution at null
  sim_dist_temp <- sim_dist
  sim_dist <- sim_dist_temp - (mean(sim_dist_temp) - null)
  
  # shading cutoffs
  if(alternative == "greater"){ x_min = y_bar; x_max = Inf }
  if(alternative == "less"){ x_min = -Inf; x_max = y_bar }
  if(alternative == "twosided"){
    if(y_bar >= null){
      x_min = c(null - (y_bar - null), y_bar)
      x_max = c(-Inf, Inf)
    }
    if(y_bar <= null){
      x_min = c(y_bar, null + (null - y_bar))
      x_max = c(-Inf, Inf)
    }    
  }
  
  # calculate p-value
  if(alternative == "greater"){ p_value <- sum(sim_dist >= y_bar) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= y_bar) / nsim }
  if(alternative == "twosided"){
    if(y_bar > null){
      p_value <- min(2 * (sum(sim_dist >= y_bar) / nsim), 1)
    }
    if(y_bar < null){
      p_value <- min(2 * (sum(sim_dist <= y_bar) / nsim), 1)
    }     
    if(y_bar == null){ p_value <- 1 }
  }
  
  # print variable types
  if(show_var_types == TRUE){
    cat("Single numerical variable\n")
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    s <- sd(y)
    cat(paste0("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(s, 4), "\n"))
  }
  
  # print results
  if(show_res == TRUE){
    if(alternative == "greater"){
      alt_sign <- ">"
    } else if(alternative == "less"){
      alt_sign <- "<"
    } else {
      alt_sign <- "!="
    }
    cat(paste0("H0: mu = ", null, "\n"))
    cat(paste0("HA: mu ", alt_sign, " ", null, "\n"))
    p_val_to_print <- ifelse(round(p_value, 4) == 0, "< 0.0001", round(p_value, 4))
    cat(paste0("p_value = ", p_val_to_print))
  }

  # eda_plot
  d_eda <- data.frame(y = y)
  
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    geom_vline(xintercept = y_bar, col = "#1FBEC3", lwd = 1.5)
  
  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)
  
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("simulated means") +
    ylab("") +
    ggtitle("Null Distribution") +
    geom_vline(xintercept = y_bar, color = "#F57670", lwd = 1.5)
  
  # print plots
  if(show_eda_plot & !show_inf_plot){ 
    suppressWarnings(print(eda_plot))
  }
  if(!show_eda_plot & show_inf_plot){ 
    suppressWarnings(print(inf_plot))
  }
  if(show_eda_plot & show_inf_plot){
    suppressWarnings(grid.arrange(eda_plot, inf_plot, ncol = 2))
  }
  
  # return
  return(list(sim_dist = sim_dist, p_value = p_value))
  
}