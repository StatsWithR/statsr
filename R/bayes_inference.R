#' Bayesian hypothesis tests and credible intervals
#' @param y Response variable, can be numerical or categorical
#' @param x Explanatory variable, categorical (optional)
#' @param data Name of data frame that y and x are in
#' @param type of inference; "ci" (credible interval) or "ht" (hypothesis test)
#' @param statistic population parameter to estimate: mean or proportion
#' @param method  of inference; "theoretical" (quantile based) or "simulation"
#' @param success which level of the categorical variable to call "success", i.e. do inference on
#' @param null null value for the hypothesis test
#' @param cred_level confidence level, value between 0 and 1
#' @param alternative direction of the alternative hypothesis; "less","greater", or "twosided"
#' @param verbose whether output should be verbose or not, default is TRUE
#' @param show_summ print summary stats, set to verbose by default
#' @param show_res print results, set to verbose by default
#' @param show_plot print inference plot, set to verbose by default
#' @param hypothesis_prior discrete prior for H1 and H2, default is the uniform prior: c(H1=0.5,H2=0.5)
#' @param prior_family character string representing default priors for inference or testing ("JSZ", "JUI","ref"). 
#'        See notes for details.
#' @param n_0,mu_0,s_0,v_0,alpha_0 Prior parameters for the conjugate Normal-Gamma prior
#' or mixtures of NG:
#'   mu_0 is the prior mean (0.0 by default) for one-sample problems;
#'   n_0 is the prior sample size; 
#'   s_0 is the prior standard deviation of the data;
#'   v_0 is the prior degrees of freedom associated with the prior standard deviation, s_0;
#'   alpha_0 is the prior effect size for comparing two normal means (default is 0).
#' The default values correspond to the independent Jeffreys prior for
#' the variance sigma^2  (s_0 = 0, v_0 = -1),  and the unit information prior 
#' mu given sigma^2  (mu_0 = 0, n_0=1).
#' @param  rscale is the scaling parameter in the Cauchy prior:  1/n_0 ~ Gamma(1/2, rscale^2/2)
#'   leads to mu_0 or alpha_0 having a Cauchy(0, rscale*sigma) prior distribution.         
#' @param beta_prior,beta_prior1,beta_prior2 beta priors for p (or p_1 and p_2) for one or two proportion inference
#' @return Results of inference task performed.
#' 
#' 
#' @note For inference and testing for normal means several default options are avialable.
#'  "JZS"  corresponds to using the Jeffreys reference prior on sigma^2, p(sigma^2) = 1/sigma^2, 
#'  and the Zellner-Siow Cauchy prior on the standardized effect size mu/sigma or ( mu_1 - mu_2)/sigma
#'   with a location of mu_0 and scale  sqrt(n_0).  The "JUI" option also uses the
#'   Jeffreys reference prior on sigma^2, but the Unit Information prior on the
#'  standardized effect, N(mu_0, 1).  The option "ref" uses the improper unifrom prior  on 
#'  the standardized effect and the Jeffereys reference prior on  sigma^2.  The latter 
#'  cannot be used for hypothesis testing due to the ill-determination of Bayes
#'  factors.  Finally "NG" corresponds to the conjugate Normal-Gamma prior.
#'  
#'     
#' @examples 
#' 
#' # inference for the mean from a single normal population
#' # Jeffreys Reference prior, p(mu, sigma^2) = 1/sigma^2
#' 
#'
#' data(tapwater)
#' 
#' # Calculate 95% CI using quantiles from Student t derived from NG prior
#' bayes_inference(tthm, data=tapwater,
#'                 statistic="mean", 
#'                 type="ci", prior_family="ref",
#'                 method="theoretical")
#' 
#' # Calculate 95% CI using simulation from Student t using an informative mean and ref
#' # prior for sigma^2
#' 
#' bayes_inference(tthm, data=tapwater,
#'                 statistic="mean", mu_0=9.8,
#'                 type="ci",  prior_family="JUI",
#'                 method="theo")
#' 
#'# Calculate 95% CI using simulation  with the 
#'# Cauchy prior on mu and reference prior on sigma^2 using BayesFactor package
#'
#'
#' statsr:::bayes_ci_single_mean_JZS(tapwater$tthm)
#' 
#' 
#' bayes_inference(tthm, data=tapwater,
#'                 statistic="mean", mu_0 = 9.8, rscale=sqrt(2)/2,
#'                 type="ci", prior_family="JZS",
#'                 method="simulation")
#' 
#' 
#' # Bayesian t-test mu = 0 with JUI prior
#' bayes_inference(tthm, data=tapwater,
#'                 statistic="mean",
#'                 type="ht", alternative="twosided", null=80,
#'                 prior_family="NG", n_0=1, mu_0=80, s_0=0, v_0=-1, 
#'                 method="sim")
#'                 
#'                 
#' # Bayesian t-test mu = 0 with ZJS prior  (using BayesFactor package)
#' bayes_inference(tthm, data=tapwater,
#'                 statistic="mean",
#'                 type="ht", alternative="twosided", null=80,
#'                 prior_family="JZS",
#'                 method="sim")
#'                 
#'                 

#' 
#' @export


bayes_inference = function(y, x = NULL, data,
                           type = c("ci", "ht"),
                           statistic = c("mean", "proportion"),
                           method =  c("theoretical", "simulation"),
                           success = NULL, 
                           null = NULL, 
                           cred_level = 0.95,
                           alternative = c("twosided","less","greater"),
                           hypothesis_prior = c(H1=0.5,H2=0.5),
                           prior_family="JZS",
                           n_0 = 1, mu_0 = null, s_0 = 0, v_0 = -1,
                           alpha_0=0, rscale=sqrt(2)/2,
                           beta_prior  = NULL,
                           beta_prior1 = NULL,
                           beta_prior2 = NULL,
                           verbose = TRUE,
                           show_summ = verbose,
                           show_res = verbose, 
                           show_plot = verbose)
{
    installed_packages <- names(utils::installed.packages()[,"Package"])
    required_packages <- c("ggplot2", "gridExtra", "BayesFactor", "Matrix")
    if(!all(required_packages %in% installed_packages)){
        missing_packages <- required_packages[which(!(required_packages %in% installed_packages))]
        stop(paste("The following required packages are not installed:", missing_packages,
                   "Please install these packages before running the inference function."), call. = FALSE)
    }    
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

  # error: method isn't theoretical or simulation
  method_list = c("theoretical", "simulation")
  method = tolower(gsub("\\s","", method))
  which_method = pmatch(method, method_list)
  if(is.na(which_method)){
      stop("Method should be theoretical or simulation", call. = FALSE)
  }
  method = method_list[which_method]
  
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
  
  # check prior family
  if (y_type == "numerical") {
  # error: method isn't theoretical or simulation
  family_list = c("JZS","JUI", "ref", "NG")
 # prior_family = tolower(gsub("\\s","", prior_family))
  which_prior = pmatch(prior_family, family_list)
  if(is.na(which_prior)){
      stop("Method should be one of JZS, NG, JUI, or ref", call. = FALSE)
  }
  prior_family = family_list[which_prior]
  }

  # only one variable 
  if(is.null(x))
  {
    y = y[!is.na(y)]

    if(statistic == "mean" & y_type == "numerical") { 
      if (prior_family == "ref") {
          n_0 = mu_0 = s_0  =0.0
          v_0 = -1}
      if (prior_family == "JUI") {
          n_0 = 1
          s_0 = 0
          v_0 = -1}
      if (prior_family == "JZS") {
           method="simulation"
      }
            
    # make sure improper prior is set correctly 
     if (prior_family == "NG") {
      if (n_0 == 0)  mu_0 = 0 
      if (v_0 <= 0) s_0 = 0 }
    
     if(is.null(null)) {
        if (is.null(mu_0)) stop("Error: must specify prior mean mu_0\n")
        null = mu_0
        }
     if(is.null(mu_0)) mu_0 = null    
     if(mu_0 != null) stop("Error: null must be the same as mu_0\n")

    # add more checks?
     if(type == "ci")  {
       if (method=="theoretical")   {
          return(invisible(
             bayes_ci_single_mean_theo(y, cred_level,
                                       n_0, mu_0, s_0, v_0, 
                                       verbose, show_summ,
                                       show_res, show_plot)
            )) }            
        if (method=="simulation")   {
            if (prior_family != "JZS") {
              return(invisible(
                  bayes_ci_single_mean_sim(y, cred_level,
                                            n_0, mu_0, s_0, v_0, 
                                            verbose, show_summ, show_res, show_plot)
              ))  }
            else {
              return(invisible(
                  bayes_ci_single_mean_JZS(y,
                                           cred_level,
                                           rscale, mu_0, 
                                           verbose, 
                                           show_summ,
                                           show_res, 
                                           show_plot)
              )) }
          }
     }
     if(type == "ht") {
        if (n_0 == 0) stop("\nImproper priors cannot be used as a prior on mu for hypothesis testing\nPlease use n_0 > 0 or use a different default prior_family, such as JZS.")
        if (s_0 != 0 & v_0 != -1) warning("\nInformative priors on sigma^2 are not available, switching to Jeffreys reference prior for sigma^2")

        if (method=="theoretical") {
           return(invisible( 
            bayes_ht_single_mean_theo(y, null=null,
                                      alternative=alternative,
                                      cred_level=cred_level,
                                      mu_0=mu_0, n_0=n_0,
                                      hypothesis_prior=hypothesis_prior,
                                      verbose=verbose,
                                      show_summ=show_summ, 
                                      show_res=show_res, 
                                      show_plot=show_plot)
           ))}
        if (method=="simulation") {
          if (prior_family != "JZS") {
                 return(invisible(
                     bayes_ht_single_mean_sim(y=y, null=null,
                                              hypothesis_prior=hypothesis_prior,
                                              alternative=alternative,
                                              cred_level=cred_level,
                                              mu_0=mu_0,n_0=n_0,
                                              verbose=verbose,
                                              show_summ=show_summ, 
                                              show_res=show_res, 
                                              show_plot=show_plot)
                 ))  }
          else {
             
             return(invisible( 
                 bayes_ht_single_mean_JZS(y=y, null=null,
                                          hypothesis_prior=hypothesis_prior,
                                          alternative=alternative,
                                          cred_level=cred_level,
                                          mu_0=mu_0, rscale=rscale,
                                          verbose=verbose,
                                          show_summ=show_summ, 
                                          show_res=show_res, 
                                          show_plot=show_plot)
             ))}
         }
     }
    }
    
    if(statistic == "proportion" & y_type == "categorical" & y_levels == 2) {

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
