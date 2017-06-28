bayes_ci_single_mean_sim = function(y, cred_level = 0.95,
                                n_0, mu_0, s_0, v_0, 
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{  
  nsim = 1e6

  n = length(y) 
  y_bar = mean(y)
  
  # update hyperparameters
  n_n = n_0 + n
  post_mean = (n*y_bar + n_0*mu_0)/n_n
  ss = var(y)*(n-1) + s_0^2*v_0 + (n*n_0/n_n)*(y_bar - mu_0)^2
  v_n = v_0 + n
  s = sqrt(ss/v_n)
  post_sd = s/sqrt(n_n)
  
  post = rt(nsim, df=v_n) * s / sqrt(n_n) + post_mean  
  
  ci = quantile(post, probs = c((1-cred_level)/2,1-(1-cred_level)/2))
  
  den = coda_density(post)
  
 
  post_median = median(post)
  post_mode   = den$x[which.max(den$y)]

  if (show_summ)
  {
      cat("Single numerical variable\n")
      cat("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(sd(y), 4), "\n",sep="")
      if (n_0 == 0 )  cat("(Assuming improper prior: P(mu) = 1)\n")
      else  cat("(Assuming proper prior:  mu | sigma^2 ~ N(",
                round(mu_0,4),", ", n_0, "*sigma^2)\n", sep="")
      if (v_0 <= 0)  cat("(Assuming improper prior: P(1/sigma^2) = (sigma^2)^",v_0,"\n",sep="")
      else   cat("(Assuming proper prior: 1/sigma^2 ~ G(",
                 v_0,"/2,", round(s_0^2,4),"*",v_0,"/2)\n", sep="")
      cat("\n")
      cat("Joint Posterior Distribution for mu and 1/sigma^2:\n",
          " N(", round(post_mean, 4), ", sigma^2/", n_n,")",
          " G(", v_n, "/2, ", round(s^2*v_n,4), "/2)\n\n", sep="")
      cat("Marginal Posterior for mu:\n", 
          "Student t with posterior mean = ", 
          round(post_mean, 4), ", posterior scale = ", round(s, 4), 
          " on ", v_n, " df\n",sep="")
     cat("\n")
      
  }

  # print results
  if (show_res)
  {
    cat(paste0(cred_level*100, "% CI: (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))
    
    cat("\n")
    cat("Post. mean   =", round(post_mean,4),   "\n")
    cat("Post. median =", round(post_median,4), "\n")
    cat("Post. mode   =", round(post_mode,4),   "\n")
  }


  if (show_plot)
  {    
    d = data.frame(mu = den$x, dens = den$y)

    d = d[d$dens > 1e-4,]

    li = min(which(d$mu >= ci[1]))  
    ui = max(which(d$mu <  ci[2]))

    ci_poly = data.frame(mu = c(d$mu[c(li,li:ui,ui)]), 
                         dens = c(0, d$dens[li:ui], 0))


    ci_interval = data.frame(mu = ci, dens = 0)
    pos_plot = ggplot(d, aes_string(x="mu", y="dens")) + 
               geom_line() +
               ylab("Density") +  
               xlab("mu") +
               geom_line(data  = ci_interval, size=1.5) +
               geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)


    print(pos_plot)
  }

  # return
  return( invisible(
    list(
      post = post,
      post_den = den,
      cred_level  = cred_level,
      post_mean   = post_mean,
      post_median = post_median,
      post_mode   = post_mode,
      post_sd     = s,
      post_df     = v_n,
      ci          = ci
    )
  ))
}



bayes_ht_single_mean_sim = function(y, null, 
                                alternative = "twosided",
                                cred_level = 0.95,
                                n_0, mu_0=null,
                                hypothesis_prior = NULL,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{
  nsim = 1e6
  s_0=0; v_0 = -1
  
  if (alternative != "twosided")
    stop("One sided hypothesis tests are not currently supported.")

  if (is.null(null))
    stop("Null value for mu in H1 must be specified.")

  hypothesis_prior = check_hypothesis_prior(hypothesis_prior)
  
  n = length(y) 
  y_bar = mean(y)
  s = sd(y)

  t = (y_bar - null)/ (s/sqrt(n))

  #BF12 = sqrt((n+n_0)/n_0) * pow( (t^2 * (n_0 / (n+n_0)) + nu) / (t^2+nu), (nu+1)/2 ) 

  BF12 = exp(0.5*(log(n + n_0) - log(n_0) + (n-1)*(log(t^2*n_0/(n + n_0) + (n - 1)) - log(t^2 + n - 1))))


  prior_H1 = hypothesis_prior[1]
  prior_H2 = hypothesis_prior[2]


  

  if (show_summ)
  {
    cat("Single numerical variable\n")
    cat("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(s, 4), "\n",sep="")
    cat("(Using proper prior: mu | sigma^2 ~ N(", round(mu_0, 4),
        ", ", n_0, "*sigma^2)\n", sep="")
    cat("(Using improper prior: P(sigma^2) = 1/sigma^2)\n")
    cat("\n")
  }
  
  res = list(hypothesis_prior = hypothesis_prior)
  
  if (BF12 >= 1)
  {
      res$order = "H1:H2"
      res$O  = prior_H1 / prior_H2
      res$BF = BF12
      res$PO = res$BF * res$O
      res$post_H1 = res$PO / (res$PO+1)
      res$post_H2 = 1 - res$PO / (res$PO+1) 
      
  } else {
      res$order = "H2:H1"
      res$O  = prior_H2 / prior_H1
      res$BF = 1 / BF12
      res$PO = res$BF * res$O
      res$post_H2 = res$PO / (res$PO+1)
      res$post_H1 = 1 - res$PO / (res$PO+1) 
  }
  
  # print results
  if (show_res)
  {
      alt_sign = switch(alternative,
                        greater = ">",
                        less = "<",
                        twosided = "!=")
      cat("Hypotheses:\n")
      cat("H1: mu = ", null, "\n",sep="")
      cat("H2: mu ", alt_sign, " ", null, "\n",sep="")
      cat("\n")

      cat("Priors:\n")
      cat("P(H1) =",prior_H1,"\n")
      cat("P(H2) =",prior_H2,"\n")
      cat("\n")
      
      cat("Results:\n")
      #cat( "O[",res$order,"] = ", round(res$O , 4), "\n", sep="")
      cat("BF[",res$order,"] = ", round(res$BF, 4), "\n", sep="")
      #cat("PO[",res$order,"] = ", round(res$PO, 4), "\n", sep="")
      
      #cat("\n")
      
      cat("P(H1|data) =", round(res$post_H1,4), "\n")
      cat("P(H2|data) =", round(res$post_H2,4), "\n")
  }
  
  if (show_plot)
  { 
      if (show_res | show_summ) cat("\nPosterior summaries for mu under H2:\n")
      sim =  bayes_ci_single_mean_sim(y, cred_level,
                                n_0, mu_0=null, s_0=0, v_0=-1,
                                verbose    = FALSE,
                                show_summ  = show_summ, 
                                show_res   = show_res,
                                show_plot  = show_plot)
      res$mu = sim$post
  }

  return(invisible(res)) 
}
