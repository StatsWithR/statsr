ht_many_prop_sim <- function(y, x, x_name, y_name, seed, nsim,
                              show_var_types, show_summ_stats, show_res,
                              show_eda_plot, show_inf_plot){

  length(x)
  length(y)
  
  # set seed
  if(!is.null(seed)){ set.seed(seed) }
  
  # chi-sq test of independence
  res <- chisq.test(x, y, correct = FALSE, simulate.p.value = TRUE, B = min(2000, nsim))
  stat <- res$statistic

  # print variable types
  if(show_var_types == TRUE){
    n_x_levels <- length(levels(x))
    n_y_levels <- length(levels(y))
    cat(paste0("Response variable: categorical (", n_y_levels, " levels) \n"))
    cat(paste0("Explanatory variable: categorical (", n_x_levels, " levels) \n"))
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    cat("Observed:\n")
    print(res$observed) 
    cat("\n")
    cat("Expected:\n")
    print(res$expected)
    cat("\n")
  }
  
  # print results
  if(show_res == TRUE){
    cat(paste0("H0: ", x_name, " and ", y_name, " are independent\n"))
    cat(paste0("HA: ", x_name, " and ", y_name, " are dependent\n"))
    cat(paste0("chi_sq = ", round(as.numeric(stat), 4), 
               ", p_value = ", round(res$p.value, 4), "\n"))
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  
  n_fill_values <- length(levels(y))
  fill_values <- colorRampPalette(c("#1FBEC3", "#C7EEF0"))( n_fill_values )

  eda_plot <- ggplot(data = d_eda, aes(x = x, fill = y), environment = environment()) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = fill_values) +
    xlab(x_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    guides(fill = guide_legend(title = y_name))
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.") }
  
  # return
  return(list(chi_sq = as.numeric(stat), p_value = res$p.value))
}