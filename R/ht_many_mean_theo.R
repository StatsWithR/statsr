ht_many_mean_theo <- function(y, x, null, alternative, sig_level,
                              y_name, x_name, 
                              show_var_types, show_summ_stats, show_res,
                              show_eda_plot, show_inf_plot){
  # summary stats
  ns <- by(y, x, length)
  y_bars <- by(y, x, mean)
  sds <- by(y, x, sd)
  
  # anova
  res <- anova(lm(y ~ x))
  
  # anova pieces
  terms <- c(x_name, "Residuals", "Total")
  deg_frs <- res$Df
  ss <- res$`Sum Sq`
  ms <- res$`Mean Sq`
  stat <- res$`F value`[1]
  p_value <- res$`Pr(>F)`[1]
  
  # calculate totals
  ss_tot <- sum(ss)
  ss <- c(ss, ss_tot)
  df_tot <- sum(deg_frs)
  deg_frs <- c(deg_frs, df_tot)
  
  # ss format
  ss_format <- as.character(round(ss, 4))
  
  # ms format
  ms_format <- as.character(c(round(ms, 4), NA))
  
  # stat format
  stat_format <- as.character(c(round(stat, 4), NA, NA))
  
  # p-value format
  p_value_format <- as.character(c(ifelse(round(p_value, 4) == 0, "< 0.0001", round(p_value, 4)), NA, NA))
  
  # format output
  anova_output <- data.frame(
    df = deg_frs,
    Sum_Sq = ss_format,
    Mean_Sq = ms_format,
    F = stat_format,
    p_value = p_value_format, 
    row.names = terms
  )
  
  # print variable types
  if(show_var_types == TRUE){
    n_x_levels <- length(levels(x))
    cat(paste0("Response variable: numerical\n"))
    cat(paste0("Explanatory variable: categorical (", n_x_levels, " levels) \n"))
  }
  
  # print summary statistics
  if(show_summ_stats == TRUE){
    grs <- levels(x)
    ns <- by(y, x, length)
    ybars <- round(by(y, x, mean), 4)
    sds <- round(by(y, x, sd), 4)
    for(i in 1:n_x_levels){
      cat(paste0("n_", grs[i], " = ", ns[i], ", y_bar_", grs[i], " = ", round(ybars[i], 4),
                 ", s_", grs[i], " = ", sds[i] , "\n"))
    }
    cat("\n")
  }

  # print results
  if(show_res == TRUE){
    cat("ANOVA:\n")
    print(anova_output, na.print = "", digits = 4)
    
    # post-hoc tests (if ANOVA is significant)
    if(p_value < sig_level){
      cat("\nPairwise tests - ")
      pairwise <- pairwise.t.test(y, x, p.adjust.method = "none", pool.sd = TRUE)
      cat(paste0(pairwise$method, ":\n"))
      print(broom::tidy(pairwise), digits = 4)
    }
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  d_means <- data.frame(y_bars = as.numeric(y_bars), x = levels(x))
  
  eda_plot <- ggplot2::ggplot(data = d_eda, ggplot2::aes(x = y), environment = environment()) +
    ggplot2::geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    ggplot2::xlab(y_name) +
    ggplot2::ylab(x_name) +
    ggplot2::ggtitle("Sample Distribution") +
    ggplot2::geom_vline(data = d_means, ggplot2::aes(xintercept = y_bars), col = "#1FBEC3", lwd = 1.5) +
    ggplot2::facet_grid(x ~ .)
  
  # inf_plot
  x_max <- max(qf(0.99, df1 = deg_frs[1], df2 = deg_frs[2]), stat*1.1)
  inf_plot <- ggplot2::ggplot(data.frame(x = c(0, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = df, args = list(df1 = deg_frs[1], df2 = deg_frs[2]), color = "#999999") +
    ggplot2::annotate("rect", xmin = stat, xmax = stat+Inf, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggplot2::ggtitle(paste0("F Distribution\n(df_G = ", deg_frs[1], ", df_E = ", deg_frs[2], ")")) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_vline(xintercept = stat, color = "#F57670", lwd = 1.5)
  
  # print plots
  if(show_eda_plot & !show_inf_plot){ 
    print(eda_plot)
  }
  if(!show_eda_plot & show_inf_plot){ 
    print(inf_plot)
  }
  if(show_eda_plot & show_inf_plot){
    gridExtra::grid.arrange(eda_plot, inf_plot, ncol = 2)
  }
  
  # return
  return(list(F = stat, df1 = deg_frs[1], df2 = deg_frs[2], p_value = p_value))
}