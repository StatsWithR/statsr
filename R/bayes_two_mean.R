bayes_ci_two_mean = function(y, x, cred_level = 0.95,
                             verbose   = TRUE,
                             show_summ = verbose, 
                             show_res  = verbose,
                             show_plot = verbose)
{
  nsim = 1e6
  
  gr1 = levels(x)[1]
  gr2 = levels(x)[2]

  y1 = y[x == gr1]
  y2 = y[x == gr2]
  
  n1 = length(y1)
  n2 = length(y2)
  
  y_bar1 = mean(y1)
  y_bar2 = mean(y2)

  s1 = sd(y1)
  s2 = sd(y2)
  
  ci_percentiles = c( (1-cred_level)/2,1-(1-cred_level)/2)
  
  mu_post1 = rt(nsim, df=n1-1)*s1/sqrt(n1) + y_bar1
  mu_post2 = rt(nsim, df=n2-1)*s2/sqrt(n2) + y_bar2

  diff_post = mu_post1 - mu_post2

  ci = quantile(diff_post, probs = ci_percentiles)
  
  den = coda_density(diff_post)

  post_mean   = mean(diff_post)
  post_median = median(diff_post)
  post_mode   = den$x[which.max(den$y)]


  # print variable types
  if (show_summ)
  {
    n_levels = length(levels(x))
  
    cat(paste0("Response variable: numerical, Explanatory variable: categorical (", n_levels," levels)\n"))
  
    cat(paste0("n_", gr1, " = ", n1, ", y_bar_", gr1, " = ", round(y_bar1, 4), ", s_", gr1, " = ", round(s1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", y_bar_", gr2, " = ", round(y_bar2, 4), ", s_", gr2, " = ", round(s2, 4), "\n"))
    cat("(Assuming independent Jeffrey's priors for mu and sigma^2)\n")
    cat("\n")
  }



  if(show_res)
  {
    cat(paste0(cred_level*100, "% Cred. Int.: (", round(ci[1], 4), " , ", round(ci[2], 4), ")\n"))

    cat("\n")
    cat("Post. mean   =", round(post_mean,4),   "\n")
    cat("Post. median =", round(post_median,4), "\n")
    cat("Post. mode   =", round(post_mode,4),   "\n")    
  }

  

  if (show_plot)
  {
    d = data.frame(diff = den$x, dens = den$y)


    li = min(which(d$diff >= ci[1]))  
    ui = max(which(d$diff <  ci[2]))

    ci_poly = data.frame(diff = c(d$diff[c(li,li:ui,ui)]), 
                         dens = c(0, d$dens[li:ui], 0))


    ci_interval = data.frame(diff = ci, dens = 0)
    pos_plot = ggplot(d, aes_string(x="diff", y="dens")) + 
               geom_line() +
               ylab("Density") +  
               xlab(paste0("mu_",gr1," - mu_",gr2)) +
               geom_line(data  = ci_interval, size=1.5) +
               geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)


    print(pos_plot)
  }

  # return
  return( invisible(
    list(
      post        = diff_post,
      post_den    = den,
      post_mean   = post_mean,
      post_median = post_median,
      post_mode   = post_mode,
      cred_level = cred_level,
      ci = ci
    )
  ))
}





bayes_ht_two_mean = function(y, x, null = 0, 
                             alternative = "twosided", 
                             cred_level = 0.95,
                             hypothesis_prior = NULL,
                             verbose   = TRUE,
                             show_summ = verbose, 
                             show_res  = verbose,
                             show_plot = verbose)
{
  if (alternative != "twosided")
    stop("One sided hypothesis tests are not currently supported.", call.=FALSE)

  if (null != 0)
    stop(paste0("Currently only H1: mu_", gr1, " - mu_", gr2, " = 0  (null) supported."), call.=FALSE)


  hypothesis_prior = check_hypothesis_prior(hypothesis_prior)
  
  prior_H1 = hypothesis_prior[1]
  prior_H2 = hypothesis_prior[2]

  gr1 = levels(x)[1]
  gr2 = levels(x)[2]

  y1 = y[x == gr1]
  y2 = y[x == gr2]
  
  n1 = length(y1)
  n2 = length(y2)
  
  y_bar1 = mean(y1)
  y_bar2 = mean(y2)

  s1 = sd(y1)
  s2 = sd(y2)

  if (show_summ)
  {
    n_levels = length(levels(x))
  
    cat(paste0("Response variable: numerical, Explanatory variable: categorical (", n_levels," levels)\n"))
  
    cat(paste0("n_", gr1, " = ", n1, ", y_bar_", gr1, " = ", round(y_bar1, 4), ", s_", gr1, " = ", round(s1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", y_bar_", gr2, " = ", round(y_bar2, 4), ", s_", gr2, " = ", round(s2, 4), "\n"))
    cat("(Assuming intrinsic prior on parameters)\n")
  }

  BF = behren_fisher_intrinsic_BF(ybar = c(y_bar1, y_bar2), 
                                  s2 = c(s1, s2)^2, 
                                  n = c(n1, n2),
                                  max_eval=10^6, low_v=0.001, up_v=100, m=4)

  res = list(hypothesis_prior = hypothesis_prior)
  
  if (BF < 1)
  {
      res$order = "H1:H2"
      res$O  = prior_H1 / prior_H2
      res$BF = 1/BF
      res$PO = res$BF * res$O
      res$post_H1 = res$PO / (res$PO+1)
      res$post_H2 = 1 - res$PO / (res$PO+1) 
      
  } else {
      res$order = "H2:H1"
      res$O  = prior_H2 / prior_H1
      res$BF = BF
      res$PO = res$BF * res$O
      res$post_H2 = res$PO / (res$PO+1)
      res$post_H1 = 1 - res$PO / (res$PO+1) 
  }
  
  # print results
  if (show_res)
  {
      alt_sign = switch(alternative,
                        greater = " >",
                        less = " <",
                        twosided = "!=")
      cat("Hypotheses:\n")
      cat("H1: mu_", gr1, "  = mu_", gr2, "\n", sep="")
      cat("H2: mu_", gr1, " ", alt_sign, " mu_", gr2, "\n", sep="")
      cat("\n")

      cat("Priors:\n")
      #cat("P(p_",gr1,") ~ Beta(a=",prior_a1,",b=",prior_b1,")\n",sep="")
      #cat("P(p_",gr2,") ~ Beta(a=",prior_a2,",b=",prior_b2,")\n",sep="")
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
    post = behren_fisher_intrinsic_BF_post_gibbs(ybar = c(y_bar1, y_bar2), 
                                                 s2 = c(s1, s2)^2, 
                                                 n = c(n1, n2), 
                                                 nsim=10000, burnin=1000)

    ci_percentiles = c( (1-cred_level)/2,1-(1-cred_level)/2)
    diff_post = post$mu1 - post$mu2
    ci = quantile(diff_post, probs = ci_percentiles)
    den = coda_density(diff_post)

    res$post     = diff_post
    res$post_den = den


    d_H2 = data.frame(diff = den$x, dens = den$y * res$post_H2 / max(den$y)) 

    li = min(which(d_H2$diff >= ci[1]))  
    ui = max(which(d_H2$diff <  ci[2]))

    ci_poly = data.frame(diff = c(d_H2$diff[c(li,li:ui,ui)]), 
                         dens = c(0, d_H2$dens[li:ui], 0))

    ci_interval = data.frame(diff = ci, dens = c(0,0))

    H1_line = data.frame(diff=c(0,0), dens=c(0,res$post_H1))

    pos_plot = ggplot(d_H2, aes_string(x="diff", y="dens")) + 
               geom_line() +
               ylab("Density") +  
               xlab(paste0("mu_",gr1," - mu_",gr2)) +
               #geom_line(data  = ci_interval, size=1.5) +
               geom_line(data  = H1_line, size=1.5, col="blue", alpha=0.5) +
               #geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)


    print(pos_plot)
  }
  
  # return
  return(invisible(res)) 
}
