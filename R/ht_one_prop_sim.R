ht_single_prop_sim <- function(y, success, null, alternative,
                               nsim, seed, y_name,
                               show_var_types, show_summ_stats,
                               show_eda_plot, show_inf_plot, show_res){
  
  # set seed
  if(!is.null(seed)){ set.seed(seed) }

  # calculate sample size
  n <- length(y) 
  
  # calculate p-hat
  p_hat <- sum(y == success) / n

  # create null distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    sim_samp <- sample(c(TRUE, FALSE), size = n, replace = TRUE, prob = c(null, 1 - null))
    sim_dist[i] <- sum(sim_samp) / n
  }
  
  # shading cutoffs
  if(alternative == "greater"){ x_min = p_hat; x_max = Inf }
  if(alternative == "less"){ x_min = -Inf; x_max = p_hat }
  if(alternative == "twosided"){
    if(p_hat >= null){
      x_min = c(null - (p_hat - null), p_hat)
      x_max = c(-Inf, Inf)
    }
    if(p_hat <= null){
      x_min = c(p_hat, null + (null - p_hat))
      x_max = c(-Inf, Inf)
    }    
  }

  # calculate p-value
  if(alternative == "greater"){ p_value <- sum(sim_dist >= p_hat) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= p_hat) / nsim }
  if(alternative == "twosided"){
    if(p_hat > null){
      p_value <- min(2 * (sum(sim_dist >= p_hat) / nsim), 1)
    }
    if(p_hat < null){
      p_value <- min(2 * (sum(sim_dist <= p_hat) / nsim), 1)
    }
    if(p_hat == null){ p_value <- 1 }
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
    if(alternative == "greater"){
      alt_sign <- ">"
    } else if(alternative == "less"){
      alt_sign <- "<"
    } else {
      alt_sign <- "!="
    }
    cat(paste0("H0: p = ", null, "\n"))
    cat(paste0("HA: p ", alt_sign, " ", null, "\n"))
    p_val_to_print <- ifelse(round(p_value, 4) == 0, "< 0.0001", round(p_value, 4))
    cat(paste0("p_value = ", p_val_to_print))
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
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("simulated proportions") +
    ylab("") +
    ggtitle("Null Distribution") +
    geom_vline(xintercept = p_hat, color = "#F57670", lwd = 1.5)
  
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
  return(list(sim_dist = sim_dist, p_value = p_value)) 
}