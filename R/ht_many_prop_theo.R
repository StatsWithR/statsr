ht_many_prop_theo <- function(y, x, x_name, y_name, 
                              show_var_types, show_summ_stats, show_res,
                              show_eda_plot, show_inf_plot){
  
  # chi-sq test of independence
  res <- chisq.test(x, y, correct = FALSE)
  stat <- res$statistic
  deg_fr <- res$parameter

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
    cat(paste0("chi_sq = ", round(as.numeric(stat), 4), ", df = ", as.numeric(deg_fr),
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
  
  # inf_plot
  x_max <- max(qchisq(0.99, df = deg_fr), stat*1.1)
  inf_plot <- ggplot(data.frame(x = c(0, x_max)), aes(x)) +
    stat_function(fun = dchisq, args = list(df = deg_fr), color = "#999999") +
    annotate("rect", xmin = stat, xmax = stat+Inf, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle(paste0("Chi-sq Distribution\n(df = ", deg_fr, ")")) +
    xlab("") +
    ylab("") +
    geom_vline(xintercept = stat, color = "#F57670", lwd = 1.5)
  
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
  return(list(chi_sq = as.numeric(stat), df = as.numeric(deg_fr), p_value = res$p.value))
}