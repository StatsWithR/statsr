bayes_ci_single_mean = function(y, cred_level = 0.95,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{  
  nsim = 1e6

  n = length(y) 
  y_bar = mean(y)
  s = sd(y)

  post = rt(nsim, df=n-1) * s / sqrt(n) + y_bar  
  
  ci = quantile(post, probs = c((1-cred_level)/2,1-(1-cred_level)/2))
  
  den = coda_density(post)
  
  post_mean   = mean(post)
  post_median = median(post)
  post_mode   = den$x[which.max(den$y)]

  if (show_summ)
  {
    cat("Single numerical variable\n")
    cat("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(s, 4), "\n",sep="")
    cat("(Assuming improper prior: P(mu, sigma^2) = 1/sigma^2)\n")
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
      ci          = ci
    )
  ))
}



bayes_ht_single_mean = function(y, null = NULL, 
                                alternative = "twosided",
                                cred_level = 0.95,
                                n_0 = 1,
                                hypothesis_prior = NULL,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{
  nsim = 1e6

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
    cat("(Assuming improper prior: P(mu, sigma^2) = 1/sigma^2)\n")
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
    post_mean  = (y_bar*n + n_0*null)/(n + n_0)
    post_scale = sqrt((s^2*(n-1)  + y_bar^2*n*n_0/(n + n_0))/((n-1)*n))

    post_H2 = rt(nsim, df=n-1)*post_scale + post_mean


    ci = quantile(post_H2, probs = c((1-cred_level)/2,1-(1-cred_level)/2))
    den = coda_density(post_H2)

    d_H2 = data.frame(mu = den$x, dens = den$y * res$post_H2 / max(den$y)) 

    li = min(which(d_H2$mu >= ci[1]))  
    ui = max(which(d_H2$mu <  ci[2]))

    ci_poly = data.frame(mu = c(d_H2$mu[c(li,li:ui,ui)]), 
                         dens = c(0, d_H2$dens[li:ui], 0))

    ci_interval = data.frame(mu = ci, dens = c(0,0))

    H1_line = data.frame(mu=c(null,null), dens=c(0,res$post_H1))

    pos_plot = ggplot(d_H2, aes_string(x="mu", y="dens")) + 
               geom_line() +
               ylab("Density") +  
               xlab("mu") +
               #geom_line(data  = ci_interval, size=1.5) +
               geom_line(data  = H1_line, size=1.5, col="blue", alpha=0.5) +
               #geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)

    print(pos_plot)
  }

  return(invisible(res)) 
}
