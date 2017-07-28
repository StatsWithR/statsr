bayes_ci_single_mean_JZS = function(y, cred_level = 0.95,
                                mu_0=0, rscale=1,
                                nsim = 10000,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{  

  v_0 =-1
  n = length(y) 
  y_bar = mean(y)
  s = sd(y)

  # update posterior
  
  JZS.post = BayesFactor::ttestBF(x=y,y=NULL, mu=mu_0, rscale=rscale,
                                  posterior=TRUE, iterations=nsim)
  
  post = JZS.post[,"mu"]
  ci = quantile(post, probs = c((1-cred_level)/2,1-(1-cred_level)/2))
  
  den = coda_density(post)
  
  post_mean = mean(post)
  post_median = median(post)
  post_mode   = den$x[which.max(den$y)]

  if (show_summ)
  {
      stats = summary(JZS.post)$quantiles[-3,]
      rownames(stats) = c("mu", "sigma", "n_0")
      stats["sigma",] = sqrt(stats["sigma",])
      stats["n_0",] = n/stats["n_0",ncol(stats):1]
      cat("Single numerical variable\n")
      cat("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(sd(y), 4), "\n",sep="")

      cat("(Assuming Zellner-Siow Cauchy prior:  mu | sigma^2 ~ C(",
                round(mu_0,4),", ", round(rscale,4), "*sigma)\n", sep="")
      cat("(Assuming improper Jeffreys prior: p(sigma^2) = 1/sigma^2\n")

      cat("\nPosterior Summaries\n")
     
      print(stats)
     cat("\n")
      
  }

  # print results
  if (show_res)
  {
    cat(paste0(cred_level*100, "% CI for mu: (", round(ci[1], 4), ", ", round(ci[2], 4), ")\n"))
    
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

  res = list(mu          = post,
             post_den    = den,
             cred_level  = cred_level,
             post_mean   = post_mean,
             post_sd     = summary(JZS.post)$statistics["mu", "SD"],
             ci          = ci,
             samples     = JZS.post,
             summary     = stats
  )
  if (show_plot) res$plot = pos_plot
  # return
  return( invisible(res ))
}



bayes_ht_single_mean_JZS = function(y, null,  hypothesis_prior,
                                alternative = "twosided",
                                cred_level = 0.95, 
                                mu_0 = null, rscale=1,
                                nsim = 10000,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{
  nsim = 1e6

  if (alternative != "twosided")
    stop("One sided hypothesis tests are not currently supported.")

  if (is.null(null)) {
    null = mu_0
  }

  hypothesis_prior = check_hypothesis_prior(hypothesis_prior)
  
  n = length(y) 
  y_bar = mean(y)
  s = sd(y)

 
  BFout = BayesFactor::ttestBF(x=y, mu=mu_0, rscale=rscale,
                               posterior=FALSE)

  prior_H1 = hypothesis_prior[1]
  prior_H2 = hypothesis_prior[2]


  

  if (show_summ)
  {
    cat("Single numerical variable\n")
    cat("n = ", n, ", y-bar = ", round(y_bar, 4), ", s = ", round(s, 4), "\n",sep="")
    cat("(Using Zellner-Siow Cauchy prior:  mu ~ C(",
        round(mu_0,4),", ", round(rscale,4), "*sigma)\n", sep="")
    cat("(Using Jeffreys prior: p(sigma^2) = 1/sigma^2\n")
    
    cat("\n")
  }
  
 
 
  
  res= list(hypothesis_prior = hypothesis_prior)
  BF12 = exp(-BFout@bayesFactor$bf)

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
      cat("H1: mu =", null, "versus H2: mu", alt_sign, null,
           sep=" ")
      cat("\n")

      cat("Priors:\n")
      cat("P(H1) =",prior_H1, ", P(H2) =" ,prior_H2, sep=" ")
      cat("\n")
      
      cat("Results:\n")
      #cat( "O[",res$order,"] = ", round(res$O , 4), "\n", sep="")
      cat("BF[",res$order,"] = ", round(res$BF, 4), "\n", sep="")
      #cat("PO[",res$order,"] = ", round(res$PO, 4), "\n", sep="")
      
      #cat("\n")
      
      cat("P(H1|data) =", round(res$post_H1,4), " ")
      cat("P(H2|data) =", round(res$post_H2,4), "\n")
  }
  
  if (show_plot)
  { 
      if (show_res | show_summ) cat("\nPosterior summaries for mu under H2:\n")
      samples = bayes_ci_single_mean_JZS(y, cred_level=cred_level,
                                  rscale=rscale, mu_0=mu_0, 
                                  nsim,
                                  verbose    = FALSE,
                                  show_summ  = show_summ, 
                                  show_res   = show_res,
                                  show_plot  = show_plot)
      res = append(res,  samples) 
  }

  return(invisible(res)) 
}
