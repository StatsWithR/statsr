
ht_two_mean_theo <- function(y, x, null, alternative, 
                             y_name, x_name, 
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
  
  # calculate SE
  se <- sqrt((s1^2 / n1) + (s2^2 / n2))

  # define degrees of freedom
  deg_fr <- min(n1 - 1, n2 - 1)
  
  # calculate t
  t <- (y_bar_diff - null) / se
  
  # shading cutoffs
  if(alternative == "greater"){ 
    x_min <- y_bar_diff
    x_max <- Inf 
    }
  if(alternative == "less"){ 
    x_min <- -Inf
    x_max <- y_bar_diff
    }
  if(alternative == "twosided"){
    if(y_bar_diff >= null){
      x_min <- c(null - (y_bar_diff - null), y_bar_diff)
      x_max <- c(-Inf, Inf)
    }
    if(y_bar_diff <= null){
      x_min <- c(y_bar_diff, null + (null - y_bar_diff))
      x_max <- c(-Inf, Inf)
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
    n_x_levels <- length(levels(x))
    cat(paste0("Response variable: numerical\n"))
    cat(paste0("Explanatory variable: categorical (", n_x_levels, " levels) \n"))
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    gr1 <- levels(x)[1]
    gr2 <- levels(x)[2]
    cat(paste0("n_", gr1, " = ", n1, ", y_bar_", gr1, " = ", round(y_bar1, 4), ", s_", gr1, " = ", round(s1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", y_bar_", gr2, " = ", round(y_bar2, 4), ", s_", gr2, " = ", round(s2, 4), "\n"))
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
    cat(paste0("H0: mu_", gr1, " =  mu_", gr2, "\n"))
    cat(paste0("HA: mu_", gr1, " ", alt_sign, " mu_", gr2, "\n"))
    cat(paste0("t = ", round(t, 4), ", df = ", deg_fr, "\n"))
    p_val_to_print <- ifelse(round(p_value, 4) == 0, "< 0.0001", round(p_value, 4))
    cat(paste0("p_value = ", p_val_to_print))
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
    

  # inf_plot
  inf_plot <- ggplot(data.frame(x = c(null - 4*se, null + 4*se)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = null, sd = se), color = "#999999") +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle("Null Distribution") +
    xlab("") +
    ylab("") +
    geom_vline(xintercept = y_bar_diff, color = "#F57670", lwd = 1.5)
  
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