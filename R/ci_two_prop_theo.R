ci_two_prop_theo <- function(y, x, success, conf_level, 
                             x_name, y_name,
                             show_var_types, show_summ_stats, show_res,
                             show_eda_plot, show_inf_plot){
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate p-hat1 and p-hat2
  p_hat1 <- sum(y[x == levels(x)[1]] == success) / n1
  p_hat2 <- sum(y[x == levels(x)[2]] == success) / n2
  
  # calculate difference in p-hats
  p_hat_diff <- p_hat1 - p_hat2
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  z_star <- qnorm(perc_crit_value)
  
  # calculate SE
  se <- sqrt((p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2))
  
  # calculate ME
  me <- z_star * se
  
  # calculate CI
  ci <- p_hat_diff + c(-1, 1) * me
  
  # print variable types
  if(show_var_types == TRUE){
    n_x_levels <- length(levels(x))
    n_y_levels <- length(levels(y))
    cat(paste0("Response variable: categorical (", n_x_levels, " levels, success: ", success, ")\n"))
    cat(paste0("Explanatory variable: categorical (", n_y_levels, " levels) \n"))
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    gr1 <- levels(x)[1]
    gr2 <- levels(x)[2]
    cat(paste0("n_", gr1, " = ", n1, ", p_hat_", gr1, " = ", round(p_hat1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", p_hat_", gr2, " = ", round(p_hat2, 4), "\n"))
  }
  
  # print results
  if(show_res == TRUE){
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI (", gr1 ," - ", gr2,"): (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  
  if(which(levels(y) == success) == 1){ 
    fill_values = c("#1FBEC3", "#8FDEE1") 
  } else {
      fill_values = c("#8FDEE1", "#1FBEC3") 
      }
  
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
  return(list(SE = se, ME = me, CI = ci))
}