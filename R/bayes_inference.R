#' Bayesian hypothesis tests and credible intervals
#' @param y Response variable, can be numerical or categorical
#' @param x Explanatory variable, categorical (optional)
#' @param data Name of data frame that y and x are in
#' @param type of inference; "ci" (credible interval) or "ht" (hypothesis test)
#' @param statistic population parameter to estimate: mean or proportion
#' @param success which level of the categorical variable to call "success", i.e. do inference on
#' @param null null value for the hypothesis test
#' @param cred_level confidence level, value between 0 and 1
#' @param alternative direction of the alternative hypothesis; "less","greater", or "twosided"
#' @param verbose whether output should be verbose or not, default is TRUE
#' @param show_summ print summary stats, set to verbose by default
#' @param show_res print results, set to verbose by default
#' @param show_plot print inference plot, set to verbose by default
#' @param hypothesis_prior discrete prior for H1 and H2, default is the uniform prior: c(H1=0.5,H2=0.5)
#' @param n_0 Prior sample size for calculating the Bayes factor of the twosided test of one mean
#' @param beta_prior,beta_prior1,beta_prior2 beta priors for p (or p_1 and p_2) for one or two proportion inference
#' @return Results of inference task performed
#' @export


bayes_inference = function(y, x = NULL, data,
                           type = c("ci", "ht"),
                           statistic = c("mean", "proportion"),
                           success = NULL, 
                           null = NULL, 
                           cred_level = 0.95,
                           alternative = c("twosided","less","greater"),
                           hypothesis_prior = c(H1=0.5,H2=0.5),
                           n_0 = 1,
                           beta_prior  = NULL,
                           beta_prior1 = NULL,
                           beta_prior2 = NULL,
                           verbose = TRUE,
                           show_summ = verbose,
                           show_res = verbose, 
                           show_plot = verbose)
{
  # save axis labels for use later
  y_name = paste(substitute(y))
  x_name = paste(substitute(x))

  # assign x and y
  x = eval(substitute(x), data, parent.frame())
  y = eval(substitute(y), data, parent.frame())

  # error: weird y
  if (length(y) == 1) {stop("Sample size of y is 1", call. = FALSE)}

  # error: y or x is character or logical, make factor
  if (is.character(y) | is.logical(y)) {y = as.factor(y)}
  if (is.character(x) | is.logical(x)) {x = as.factor(x)}

  # set variable type for y: numerical or categorical
  y_type = "categorical"
  if (is.numeric(y)) {y_type = "numerical"}

  # set variable type for x: categorical, numerical (unused), or only1var
  if (!is.null(x)) {
    x_type = "categorical"
    if (is.numeric(x)) {x_type = "numerical"}
  } else {
    x_type = "only1var"
  }

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

  if (length(statistic) > 1) {
    stop("Missing statistic: mean or proportion", call. = FALSE)
  }

  # Check type
  stopifnot(length(type) == 1 & is.character(type))
  if(!type %in% c("ci", "ht"))
    stop(paste0("Invalid type[=",type,"] must be either ci or ht"), call. = FALSE)
  
  # Check statistic
  stopifnot(length(statistic) == 1 & is.character(statistic))
  if(!statistic %in% c("mean", "proportion"))
    stop(paste0("Invalid statistic[=",statistic,"] must be either mean or proportion"), call. = FALSE)
  
  #check alternative
  if(type == "ht")
  {
    stopifnot(length(alternative) == 1 & is.character(alternative))
    if(!alternative %in% c("less", "greater", "twosided"))
      stop(paste0("Invalid alternative[=",alternative,"] must be less, greater, or twosided"), call. = FALSE)

  }

  # error: wrong statistic
  if (y_type == "numerical" & statistic == "proportion") {
    stop("Response variable is numerical, sample statistic cannot be a proportion,
           use either mean or median", call. = FALSE)
  }
  if (y_type == "categorical" & (statistic == "mean")) {
    stop("Response variable is categorical, sample statistic cannot be a mean or
           a median, use proportion", call. = FALSE)
  }

  # errors about success
  if ((y_type == "categorical" & x_levels == 2 & y_levels == 2) | (y_type == "categorical" & is.null(x))) 
  {
    # error: success not provided for categorical variable for 1 or 2 proportion ci or ht
    if (is.null(success))
      stop(paste0("Response variable is categorical, specify which level to call success: ",
                 levels(y)[1], " or ", levels(y)[2]), call. = FALSE)

    # error: success provided is not a level of the categorical variable
    if (!success %in% levels(y)) {
      stop(paste(success,"is not a level of the response variable"), call. = FALSE)
    }
  }

  # warning: success provided for numerical variable
  if (y_type == "numerical" & !is.null(success)) {
    warning("Ignoring success since y is numerical", call. = FALSE)
  }

  # warning: confidence level greater than 1
  if (cred_level > 1 | cred_level < 0) {
    stop("Credible level must be between 0 and 1.")
  }

  # only one variable 
  if(is.null(x))
  {
    y = y[!is.na(y)]

    if(statistic == "mean" & y_type == "numerical")
    {
      if(type == "ci")
        return(invisible(
          bayes_ci_single_mean(y, cred_level, 
                               verbose, show_summ, show_res, show_plot)
        ))
      if(type == "ht")
        return(invisible( 
          bayes_ht_single_mean(y, null, alternative, cred_level, n_0, hypothesis_prior, 
                               verbose, show_summ, show_res, show_plot)
          ))
    }

    if(statistic == "proportion" & y_type == "categorical" & y_levels == 2)
    {


      if(type == "ci")
        return(invisible(
          bayes_ci_single_prop(y, success, cred_level, beta_prior,
                               verbose, show_summ, show_res, show_plot)
        ))
      if(type == "ht")
        return(invisible( 
          bayes_ht_single_prop(y, success, null, alternative, cred_level, 
                               hypothesis_prior, beta_prior, 
                               verbose, show_summ, show_res, show_plot)
        ))
    }
  } 

  if (!is.null(x) & x_type == "categorical" & x_levels == 2)
  {
    d = na.omit(data.frame(y = y, x = x))
    x = d$x
    y = d$y

    if(statistic == "mean" & y_type == "numerical")
    {
      if(type == "ci")
        return(invisible(
          bayes_ci_two_mean(y, x, cred_level,
                            verbose, show_summ, show_res, show_plot)
        ))
      if(type == "ht")
        return(invisible(
          bayes_ht_two_mean(y, x, null, alternative, cred_level, 
                            hypothesis_prior, 
                            verbose, show_summ, show_res, show_plot)
        ))
    }

    if(statistic == "proportion" & y_type == "categorical" & y_levels == 2)
    {
      if(type == "ci")
        return(invisible(
          bayes_ci_two_prop(y, x, success, cred_level,
                            beta_prior1, beta_prior2, 
                            verbose, show_summ, show_res, show_plot)
        ))
      if(type == "ht")
        return(invisible(
          bayes_ht_two_prop(y, x, success, null, cred_level, alternative, 
                            hypothesis_prior, beta_prior1, beta_prior2, 
                            verbose, show_summ, show_res, show_plot) 
        ))   
    }
  }

  y_text = paste0("y (")
  if (y_type == "categorical")
    y_text = paste0(y_text,"Categorical w/ ", y_levels, "levels)")
  else
    y_text = paste0("Numerical)")

  x_text = paste0("y (")
  if (x_type == "categorical")
    x_text = paste0(x_text, "Categorical w/ ", x_levels, "levels)")
  else
    x_text = paste0(x_text, "Numerical)")

  stop(paste0("Inference for ", y_text, " vs. ", x_text, " is not currently supported."), call.=FALSE)
}
