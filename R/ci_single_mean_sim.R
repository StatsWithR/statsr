ci_single_mean_sim <- function(y, conf_level, y_name,
                               boot_method, nsim, seed,
                               show_var_types, show_summ_stats, show_res,
                               show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){ set.seed(seed) }

  # calculate sample size
  n <- length(y)

  # calculate x-bar
  y_bar <- mean(y)

  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- mean(boot_samp)
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
    df <- n - 1

    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)

    # find critical value
    t_star <- qt(perc_crit_value, df)

    # calculate SE
    se <- sd(sim_dist)

    # calculate ME
    me <- t_star * se

    # calculate CI
    ci <- y_bar + c(-1, 1)* me
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
    conf_level_perc = conf_level * 100
    cat(paste0(conf_level_perc, "% CI: (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
  }

  # eda_plot
  d_eda <- data.frame(y = y)

  eda_plot <- ggplot2::ggplot(data = d_eda, ggplot2::aes(x = y), environment = environment()) +
    ggplot2::geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    ggplot2::xlab(y_name) +
    ggplot2::ylab("") +
    ggplot2::ggtitle("Sample Distribution") +
    ggplot2::geom_vline(xintercept = y_bar, col = "#1FBEC3", lwd = 1.5)

  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)

  inf_plot <- ggplot2::ggplot(data = d_inf, ggplot2::aes(x = sim_dist), environment = environment()) +
              ggplot2::geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
              ggplot2::annotate("rect", xmin = ci[1], xmax = ci[2], ymin = 0, ymax = Inf,alpha = 0.3, fill = "#FABAB8") +
              ggplot2::xlab("bootstrap means") +
              ggplot2::ylab("") +
              ggplot2::ggtitle("Bootstrap Distribution") +
              ggplot2::geom_vline(xintercept = ci, color = "#F57670", lwd = 1.5)

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
  if(boot_method == "perc"){
    return(list(sim_dist = sim_dist, CI = ci))
  } else {
    return(list(sim_dist = sim_dist, SE = se, ME = me, CI = ci))
  }

}
