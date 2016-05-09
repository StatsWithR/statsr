ht_single_mean_theo <- function(y, null, alternative, y_name,
                                show_var_types, show_summ_stats, show_res,
                                show_eda_plot, show_inf_plot){

  # calculate sample size
  n <- length(y) 

  # calculate x-bar
  y_bar <- mean(y)
  
  # calculate s
  s <- sd(y)
  
  # calculate SE
  se <- s / sqrt(n)
  
  # calculate test statistic
  t <- (y_bar - null) / se
  
  # define degrees of freedom
  deg_fr <- n - 1

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
  if(alternative == "greater"){ p_value <- pt(t, deg_fr, lower.tail = FALSE) }
  if(alternative == "less"){ p_value <- pt(t, deg_fr, lower.tail = TRUE) }
  if(alternative == "twosided"){
    p_value <- pt(abs(t), deg_fr, lower.tail = FALSE) * 2
  }

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
    if(alternative == "greater"){
      alt_sign <- ">"
    } else if(alternative == "less"){
      alt_sign <- "<"
    } else {
      alt_sign <- "!="
    }
    cat(paste0("H0: mu = ", null, "\n"))
    cat(paste0("HA: mu ", alt_sign, " ", null, "\n"))
    cat(paste0("t = ", round(t, 4), ", df = ", deg_fr, "\n"))
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
  
  # inf_plot ### TO DO: remove y axis ticks
  inf_plot <- ggplot(data.frame(x = c(null - 4*se, null + 4*se)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = null, sd = se), color = "#999999") +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle("Null Distribution") +
    xlab("") +
    ylab("") +
    geom_vline(xintercept = y_bar, color = "#F57670", lwd = 1.5)
  
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
  return(list(SE = se, t = t, df = deg_fr, p_value = p_value))  
}