#' Hypothesis tests and confidence intervals
#' @param y Response variable, can be numerical or categorical
#' @param x Explanatory variable, categorical (optional)
#' @param data Name of data frame that y and x are in
#' @param statistic parameter to estimate: mean, median, or proportion
#' @param success which level of the categorical variable to call "success", i.e. do inference on
#' @param order when x is given, order of levels of x in which to subtract parameters
#' @param method of inference; "theoretical" (CLT based) or "simulation" (randomization/bootstrap)
#' @param type of inference; "ci" (confidence interval) or "ht" (hypothesis test)
#' @param null null value for a hypothesis test
#' @param alternative direction of the alternative hypothesis; "less","greater", or "twosided"
#' @param sig_level significance level, value between 0 and 1 (used only for ANOVA to determine if posttests are necessary)
#' @param conf_level confidence level, value between 0 and 1
#' @param boot_method bootstrap method; "perc" (percentile) or "se" (standard error)
#' @param nsim number of simulations
#' @param seed seed to be set, default is NULL
#' @param verbose whether output should be verbose or not, default is TRUE
#' @param show_var_types print variable types, set to verbose by default
#' @param show_summ_stats print summary stats, set to verbose by default
#' @param show_eda_plot print EDA plot, set to verbose by default
#' @param show_inf_plot print inference plot, set to verbose by default
#' @param show_res print results, set to verbose by default
#' @return Results of inference task performed
#' @export

inference <- function(y, x = NULL, data,
                      type = c("ci", "ht"),
                      statistic = c("mean", "median", "proportion"),
                      success = NULL, order = NULL,
                      method = c("theoretical", "simulation"),
                      null = NULL, alternative = c("less","greater","twosided"),
                      sig_level = 0.05,
                      conf_level = 0.95,
                      boot_method = c("perc", "se"),
                      nsim = 15000, seed = NULL,
                      verbose = TRUE,
                      show_var_types = verbose, show_summ_stats = verbose,
                      show_eda_plot = verbose, show_inf_plot = verbose, show_res = verbose){

  # if required packages are not installed, stop
  installed_packages <- names(utils::installed.packages()[,"Package"])
  required_packages <- c("ggplot2", "gridExtra", "broom")
  if(!all(required_packages %in% installed_packages)){
    missing_packages <- required_packages[which(!(required_packages %in% installed_packages))]
    stop(paste("The following required packages are not installed:", missing_packages,
               "Please install these packages before running the inference function."), call. = FALSE)
  }

#  # load packages if needed
#  suppressMessages(library(ggplot2, quietly = TRUE))
#  suppressMessages(library(gridExtra, quietly = TRUE))
#   suppressMessages(library(broom, quietly = TRUE))

  # save axis labels for use later
  y_name <- paste(substitute(y))
  x_name <- paste(substitute(x))

  # assign x and y
  x <- eval(substitute(x), data, parent.frame())
  y <- eval(substitute(y), data, parent.frame())

  # error: weird y
  if (length(y) == 1) {stop("Sample size of y is 1", call. = FALSE)}

  # error: y or x is character, make factor
  if (is.character(y)) {y = as.factor(y)}
  if (is.character(x)) {x = as.factor(x)}
  if (is.logical(y)) {y = as.factor(y)}
  if (is.logical(x)) {x = as.factor(x)}

  # set variable type for y: numerical or categorical
  y_type = "categorical"
  if (is.numeric(y)) {y_type = "numerical"}

  # set variable type for y: numerical or categorical
  y_type = "categorical"
  if (is.numeric(y)) {y_type = "numerical"}

  # set variable type for x: categorical, numerical (unused), or only1var
  if (!is.null(x)) {
    x_type = "categorical"
    if (is.numeric(x)) {x_type = "numerical"}
  }
  if (is.null(x)) {x_type = "only1var"}

  # warning: explanatory variable numerical, convert to categorical
  if(x_type == "numerical"){
    x = as.factor(x)
    x_type = "categorical"
    warning("Explanatory variable was numerical, it has been converted
              to categorical. In order to avoid this warning, first convert
              your explanatory variable to a categorical variable using the
              as.factor() function", call. = FALSE)
  }

  # error: explanatory variable only has one level
  if(x_type == "categorical"){
    if(length(levels(x)) == 1){
      stop("Explanatory variable has only one level, it should have at least two
             levels", call. = FALSE)
    }
  }

  # error: response variable is categorical, but only has one level
  if(y_type == "categorical"){
    if(length(levels(y)) == 1){
      stop("Response variable has only one level, it should have at least two
             levels", call. = FALSE)
    }
  }

  # set number of levels
  x_levels = 0  # numerical variable
  y_levels = 0  # numerical variable
  if (x_type == "categorical") {x_levels = length(levels(x))}
  if (y_type == "categorical") {y_levels = length(levels(y))}

  # error: missing type, method, statistic
  if (length(type) > 1) {
    stop("Missing type: ci (confidence interval) or ht (hypothesis test)", call. = FALSE)
  }
  if (length(method) > 1) {
    stop("Missing method: theoretical or simulation", call. = FALSE)
  }
  if (length(statistic) > 1) {
    stop("Missing statistic: mean, median, or proportion", call. = FALSE)
  }

  # error: method isn't theoretical or simulation
  method_list = c("theoretical", "simulation")
  method = tolower(gsub("\\s","", method))
  which_method = pmatch(method, method_list)
  if(is.na(which_method)){
    stop("Method should be theoretical or simulation", call. = FALSE)
  }
  method = method_list[which_method]

  # error: type isn't ci or ht
  type_list = c("ci", "ht")
  type = tolower(gsub("\\s","", type))
  which_type = pmatch(type, type_list)
  if(is.na(which_type)){stop("Type should be ci or ht", call. = FALSE)}
  type = type_list[which_type]

  # error: missing boot_method
  if (method == "simulation" & type == "ci" & length(boot_method) > 1){
    stop("Missing boot_method: perc (percentile method) or se (standard error method)", call. = FALSE)
  }

  # error: boot_method isn't perc or se
  if (method == "simulation" & type == "ci"){
    boot_method_list = c("perc", "se")
    boot_method = tolower(gsub("\\s","", boot_method))
    which_boot_method = pmatch(boot_method, boot_method_list)
    if(is.na(which_boot_method)){stop("boot_method should be perc or se", call. = FALSE)}
    boot_method = boot_method_list[which_boot_method]
  }

  # error: alternative isn't less, greater, or twosided
  if(type == "ht"){
    alternative_list = c("less", "greater", "twosided")
    alternative = tolower(gsub("\\s","", alternative))
    which_alternative = pmatch(alternative, alternative_list)
    if((length(which_alternative) == 1) & any(is.na(which_alternative))){
      stop("alternative should be less, greater or twosided", call. = FALSE)
    }
    if(any(which_alternative == 4)) {which_alternative = 3}
    alternative = alternative_list[which_alternative]
  }

  # error / warning: issues with null values
  if (!is.null(null) & !is.numeric(null)) {stop("Null value must be numeric", call. = FALSE)}
  if (type == "ht" & x_type == "only1Var" & is.null(null)) {stop("Missing null value", call. = FALSE)}
  if (type == "ht" & (x_levels == 2 & y_levels <= 2) & is.null(null)) {
    null = 0
    warning("Missing null value, set to 0", call. = FALSE)
  }
  if (type == "ht" & (x_levels > 2 | y_levels > 2) & !is.null(null)) {
    if(y_type == "numerical"){
      warning("Ignoring null value since it's undefined for ANOVA", call. = FALSE)
    }
    if(y_type == "categorical"){
      warning("Ignoring null value since it's undefined for chi-square test of independence", call. = FALSE)
    }
  }

  # error / warning: issues with alternative
  if (type == "ht" & length(alternative) > 1) {
    if(x_levels <= 2 & y_levels <= 2) {stop("Missing alternative: less, greater, or twosided", call. = FALSE)}
    if(x_levels > 2 | y_levels > 2) {
      if(y_type == "numerical") {
        alternative = "greater"
        warning('Use alternative = "greater" for ANOVA', call. = FALSE)
      }
      if(y_type == "categorical") {
        alternative = "greater"
        warning('Use alternative = "greater" for chi-square test', call. = FALSE)
      }
    }
  }

  # error: categorical variables have more than two levels, but type is ci
  if ((x_levels > 2 | y_levels > 2) & type == "ci") {
    if(y_type == "numerical"){
      stop("Categorical variable has more than 2 levels, confidence interval is undefined,
             use ANOVA to test for a difference between means", call. = FALSE)
    }
    if(y_type == "categorical"){
      stop("Categorical variable has more than 2 levels, confidence interval is not defined,
             use chi-square test of independence", call. = FALSE)
    }
  }
  if ((x_levels > 2 | y_levels > 2) & statistic == "median") {
    if(y_type == "numerical"){
      stop('This function cannot be used to compare medians across more than 2 groups,
             use statistic = "mean" to compare means across many groups using ANOVA', call. = FALSE)
    }
  }

  # error: statistic isn't mean, median, or proportion
  if (statistic %in% c("mean", "median", "proportion") == FALSE) {
    stop("Statistic should be mean, median, or proportion", call. = FALSE)
  }

  # error: wrong statistic
  if (y_type == "numerical" & statistic == "proportion") {
    stop("Response variable is numerical, sample statistic cannot be a proportion,
           use either mean or median", call. = FALSE)
  }
  if (y_type == "categorical" & (statistic == "mean" | statistic == "median")) {
    stop("Response variable is categorical, sample statistic cannot be a mean or
           a median, use proportion", call. = FALSE)
  }

  # error: x variable has more than two levels, but alternative is not defined properly (chi-square and ANOVA)
  if (x_type == "categorical" & x_levels > 2 & length(alternative) == 1) {
    if(alternative != "greater"){
      stop('Use alternative = "greater" for ANOVA or chi-square test', call. = FALSE)
    }
  }

  # errors about success
  if ((y_type == "categorical" & x_levels == 2 & y_levels == 2) | (y_type == "categorical" & is.null(x))) {
    # error: success not provided for categorical variable for 1 or 2 proportion ci or ht
    if (is.null(success)) {
      y_level_names = levels(y)
      stop(paste("Response variable is categorical, specify which level to call success: ",
                 y_level_names[1], " or ", y_level_names[2]), call. = FALSE)
    }
    # error: success provided is not a level of the categorical variable
    if (success %in% levels(y) == FALSE) {
      stop(paste(success,"is not a level of the success variable"), call. = FALSE)
    }
  }

  # warning: success provided for numerical variable
  if (y_type == "numerical" & !is.null(success)) {
    warning("Ignoring success since y is numerical", call. = FALSE)
  }

  # warning: confidence level greater than 1
  if (conf_level > 1) {
    conf_level = conf_level / 100
    warning(paste("Confidence level converted to ", conf_level, sep = ""), call. = FALSE)
  }

  # warning: significance level greater than 1
  if (sig_level > 1) {
    sig_level = sig_level / 100
    warning(paste("Significance level converted to ", sig_level, sep = ""), call. = FALSE)
  }

  # ci
  if(type == "ci"){

    # only one
    if(is.null(x)){

      # variable assignment
      y <- eval(substitute(y), data, parent.frame())

      # remova NAs
      y <- y[!is.na(y)]

      # one mean
      if(statistic == "mean"){
        if(method == "theoretical"){
          res <- ci_single_mean_theo(y = y, conf_level = conf_level, y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = FALSE,
                                     show_res = show_res)
          return(invisible(list(df = res$df, SE = res$SE, ME = res$ME, CI = res$CI)))
        }
        if(method == "simulation"){
          res <- ci_single_mean_sim(y = y, conf_level = conf_level, y_name = y_name,
                                    boot_method = boot_method, nsim = nsim, seed = seed,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
          if(boot_method == "perc"){
            return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
          } else {
            return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
          }
        }
      }

      # one median
      if(statistic == "median"){
        if(method == "theoretical"){
          stop("Use simulation methods for inference for the median", call. = FALSE)
        }
        if(method == "simulation"){
          res <- ci_single_median_sim(y = y, conf_level = conf_level, y_name = y_name,
                                      boot_method = boot_method, nsim = nsim, seed = seed,
                                      show_var_types = show_var_types,
                                      show_summ_stats = show_summ_stats,
                                      show_eda_plot = show_eda_plot,
                                      show_inf_plot = show_inf_plot,
                                      show_res = show_res)
          if(boot_method == "perc"){
            return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
          } else {
            return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
          }
        }
      }

      # one proportion
      if(statistic == "proportion"){
        if(method == "theoretical"){
          res <- ci_single_prop_theo(y = y, success = success,
                                     conf_level = conf_level, y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = FALSE,
                                     show_res = show_res)
          return(invisible(list(SE = res$SE, ME = res$ME, CI = res$CI)))

        }
        if(method == "simulation"){
          res <- ci_single_prop_sim(y = y, success = success,
                                    conf_level = conf_level, y_name = y_name,
                                    boot_method = boot_method, nsim = nsim, seed = seed,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
          if(boot_method == "perc"){
            return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
          } else {
            return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
          }
        }
      }
    }


    # compare two
    if(!is.null(x)){

      if(length(levels(x)) == 2){

        # remove NAs
        d <- na.omit(data.frame(y = y, x = x))
        x <- d$x
        y <- d$y

        # fix order, if needed
        if(!is.null(order)){
          if(order[1] != levels(x)[1]){
            x <- relevel(x, ref = levels(x)[2])
          }
        }

        # compare two means
        if(statistic == "mean"){

          if(method == "theoretical"){
            res <- ci_two_mean_theo(y = y, x = x, conf_level = conf_level,
                                    y_name = y_name, x_name = x_name,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = FALSE,
                                    show_res = show_res)
            return(invisible(list(df = res$df, SE = res$SE, ME = res$ME, CI = res$CI)))
          }
          if(method == "simulation"){
            res <- ci_two_mean_sim(y = y, x = x, conf_level = conf_level,
                                   y_name = y_name, x_name = x_name,
                                   boot_method = boot_method, nsim = nsim, seed = seed,
                                   show_var_types = show_var_types,
                                   show_summ_stats = show_summ_stats,
                                   show_eda_plot = show_eda_plot,
                                   show_inf_plot = show_inf_plot,
                                   show_res = show_res)
            if(boot_method == "perc"){
              return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
            } else {
              return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
            }
          }
        }

        # compare two medians
        if(statistic == "median"){
          if(method == "theoretical"){
            stop("Use simulation methods for inference for the median", call. = FALSE)
          }
          if(method == "simulation"){
            res <- ci_two_median_sim(y = y, x = x, conf_level = conf_level,
                                     y_name = y_name, x_name = x_name,
                                     boot_method = boot_method, nsim = nsim, seed = seed,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
            if(boot_method == "perc"){
              return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
            } else {
              return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
            }
          }
        }

        # compare two proportions
        if(statistic == "proportion"){
          if(method == "theoretical"){
            res <- ci_two_prop_theo(y = y, x = x, conf_level = conf_level,
                                    success = success,
                                    y_name = y_name, x_name = x_name,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = FALSE,
                                    show_res = show_res)
            return(invisible(list(SE = res$SE, ME = res$ME, CI = res$CI)))
          }
          if(method == "simulation"){
            res <- ci_two_prop_sim(y = y, x = x, conf_level = conf_level,
                                   success = success,
                                   y_name = y_name, x_name = x_name,
                                   boot_method = boot_method, nsim = nsim, seed = seed,
                                   show_var_types = show_var_types,
                                   show_summ_stats = show_summ_stats,
                                   show_eda_plot = show_eda_plot,
                                   show_inf_plot = show_inf_plot,
                                   show_res = show_res)
            if(boot_method == "perc"){
              return(invisible(list(sim_dist = res$sim_dist, CI = res$CI)))
            } else {
              return(invisible(list(sim_dist = res$sim_dist, SE = res$SE, ME = res$ME, CI = res$CI)))
            }
          }
        }

      }

    }

  }


  # ht
  if(type == "ht"){

    # only one
    if(is.null(x)){

      # remove NAs
      y <- y[!is.na(y)]

      # one mean
      if(statistic == "mean"){
        if(method == "theoretical"){
          res <- ht_single_mean_theo(y = y, null = null, alternative = alternative,
                                     y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
          return(invisible(list(SE = res$SE, t = res$t, df = res$df, p_value = res$p_value)))
        }
        if(method == "simulation"){
          res <- ht_single_mean_sim(y = y, null = null, alternative = alternative,
                                    y_name = y_name,
                                    nsim = nsim, seed = seed,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
          return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
        }
      }

      # one median
      if(statistic == "median"){
        if(method == "theoretical"){
          stop("Use simulation methods for inference for the median", call. = FALSE)
        }
        if(method == "simulation"){
          res <- ht_single_median_sim(y = y, null = null, alternative = alternative,
                                      y_name = y_name,
                                      nsim = nsim, seed = seed,
                                      show_var_types = show_var_types,
                                      show_summ_stats = show_summ_stats,
                                      show_eda_plot = show_eda_plot,
                                      show_inf_plot = show_inf_plot,
                                      show_res = show_res)
          return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
        }
      }

      # one proportion
      if(statistic == "proportion"){
        if(method == "theoretical"){
          res <- ht_single_prop_theo(y = y, success = success, null = null,
                                     alternative = alternative,
                                     y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
          return(invisible(list(SE = res$SE, z = res$z, p_value = res$p_value)))
        }
        if(method == "simulation"){
          res <- ht_single_prop_sim(y = y, success = success, null = null,
                                    alternative = alternative,
                                    y_name = y_name,
                                    nsim = nsim, seed = seed,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
          return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
        }
      }
    }


    # compare two
    if(!is.null(x)){

      if(length(levels(x)) == 2){

        # remove NAs
        d <- na.omit(data.frame(y = y, x = x))
        x <- d$x
        y <- d$y

        # fix order, if needed
        if(!is.null(order)){
          if(order[1] != levels(x)[1]){
            x <- relevel(x, ref = levels(x)[2])
          }
        }

        # compare two means
        if(statistic == "mean"){

          if(method == "theoretical"){
            res <- ht_two_mean_theo(y = y, x = x, null = null, alternative = alternative,
                                    y_name = y_name, x_name = x_name,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
            return(invisible(list(SE = res$SE, df = res$df, t = res$t, p_value = res$p_value)))
          }
          if(method == "simulation"){
            res <- ht_two_mean_sim(y = y, x = x, null = null, alternative = alternative,
                                   nsim = nsim, seed = seed,
                                   y_name = y_name, x_name = x_name,
                                   show_var_types = show_var_types,
                                   show_summ_stats = show_summ_stats,
                                   show_eda_plot = show_eda_plot,
                                   show_inf_plot = show_inf_plot,
                                   show_res = show_res)
            return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
          }
        }

        # compare two medians
        if(statistic == "median"){
          if(method == "theoretical"){
            stop("Use simulation methods for inference for the median", call. = FALSE)
          }
          if(method == "simulation"){
            res <- ht_two_median_sim(y = y, x = x, null = null, alternative = alternative,
                                     nsim = nsim, seed = seed,
                                     y_name = y_name, x_name = x_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
            return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
          }
        }

        # compare two proportions
        if(statistic == "proportion"){
          if(method == "theoretical"){
            res <- ht_two_prop_theo(y = y, x = x, success = success,
                                    null = null, alternative = alternative,
                                    x_name = x_name, y_name = y_name,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = show_inf_plot,
                                    show_res = show_res)
            return(invisible(list(SE = res$SE, z = res$z, p_value = res$p_value)))
          }
          if(method == "simulation"){
            res <- ht_two_prop_sim(y = y, x = x, success = success,
                                   nsim = nsim, seed = seed,
                                   null = null, alternative = alternative,
                                   x_name = x_name, y_name = y_name,
                                   show_var_types = show_var_types,
                                   show_summ_stats = show_summ_stats,
                                   show_eda_plot = show_eda_plot,
                                   show_inf_plot = show_inf_plot,
                                   show_res = show_res)
            return(invisible(list(sim_dist = res$sim_dist, p_value = res$p_value)))
          }
        }

      }

      if(length(levels(x)) > 2){

        # compare many means
        if(statistic == "mean"){
          if(method == "theoretical"){
            res <- ht_many_mean_theo(y = y, x = x,
                                     null = null, alternative = alternative,
                                     sig_level = sig_level,
                                     x_name = x_name, y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
            return(invisible(list(F = res$F, df1 = res$df1, df2 = res$df2,
                                  p_value = res$p_value)))
          }
          if(method == "simulation"){
            stop("Simulation based testing for ANOVA is not implemented in this function.", call. = FALSE)
          }
        }

        # compare many proportions
        if(statistic == "proportion"){
          if(method == "theoretical"){
            res <- ht_many_prop_theo(y = y, x = x,
                                     x_name = x_name, y_name = y_name,
                                     show_var_types = show_var_types,
                                     show_summ_stats = show_summ_stats,
                                     show_eda_plot = show_eda_plot,
                                     show_inf_plot = show_inf_plot,
                                     show_res = show_res)
            return(invisible(list(chi_sq = res$chi_sq, df = res$df, p_value = res$p_value)))
          }
          if(method == "simulation"){
            res <- ht_many_prop_sim(y = y, x = x,
                                    nsim = nsim, seed = seed,
                                    x_name = x_name, y_name = y_name,
                                    show_var_types = show_var_types,
                                    show_summ_stats = show_summ_stats,
                                    show_eda_plot = show_eda_plot,
                                    show_inf_plot = FALSE,
                                    show_res = show_res)
            return(invisible(list(chi_sq = res$chi_sq, p_value = res$p_value)))
          }
        }
      }
    }
  }
}
