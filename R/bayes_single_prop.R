bayes_ci_single_prop = function(y, success, cred_level = 0.95,
                                beta_prior = NULL,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{  
  beta_prior = check_beta_prior(beta_prior)

  n = length(y) 
  k = sum(y == success)
  p_hat = k / n
  
  post_a = beta_prior[1] + k
  post_b = beta_prior[2] + n - k 
  
  percentiles = c( (1-cred_level)/2,1-(1-cred_level)/2)
  
  ci = qbeta(percentiles, post_a, post_b)
  
  post_mean   = post_a / (post_a + post_b)
  post_median = (post_a - 1/3) / (post_a + post_b - 2/3)
  post_mode   = (post_a - 1) / (post_a + post_b - 2)


  # print variable types
  if (show_summ)
  {
    cat(paste0("Single categorical variable, success: ", success,"\n"))
    cat(paste0("n = ", n, ", p-hat = ", round(p_hat, 4), "\n"))
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
    # p = seq(0,1,length.out=1000)
    
    # post = data.frame(type="Posterior", p=p, y=dbeta(p,post_a,post_b))    
    # prior = data.frame(type="Prior", p=p, y=dbeta(p,beta_prior[1],beta_prior[2]))
    # like = data.frame(type="Likelihood", p=p, y=dbinom(k,n,p))
    
  
    # d = rbind(prior,like,post)

    # ci_interval = data.frame(p = ci, y = 0, type="Posterior")

    # plot = ggplot(d, aes(x=p, y=y, col=type)) + 
    #        geom_line() + 
    #        ylab("Density") +
    #        xlab("p") +
    #        facet_wrap( ~ type, scales="free_y") + 
    #        geom_line(data  = ci_interval, size=1.5) +
    #        geom_point(data = ci_interval, size=2)
    
    p = seq(qbeta(0.0001,post_a,post_b),
            qbeta(0.9999,post_a,post_b),
            length.out=1000)

    d = data.frame(p=p, y=dbeta(p,post_a,post_b))    
    
    ci_interval = data.frame(p = ci, y = 0)
    
    li = min(which(d$p >= ci[1]))  
    ui = max(which(d$p <  ci[2]))

    ci_poly = data.frame(p = c(d$p[c(li,li:ui,ui)]), 
                         y = c(0, d$y[li:ui], 0))

    plot = ggplot(d, aes_string(x="p", y="y")) + 
           geom_line() + 
           ylab("Density") +
           xlab("p") +
           geom_line(data  = ci_interval, size=1.5) +
           geom_point(data = ci_interval, size=2) +
           geom_polygon(data = ci_poly, alpha=0.5)
    
    print(plot)
  }

  # return
  return( invisible(
    list(
      beta_prior  = beta_prior,
      cred_level  = cred_level,
      beta_post   = c(post_a, post_b),
      post_mean   = post_mean,
      post_median = post_median,
      post_mode   = post_mode,
      ci          = ci
    )
  ))
}


bayes_ht_single_prop = function(y, success, null = NULL, 
                                alternative = "twosided",
                                cred_level = 0.95,
                                hypothesis_prior = NULL,
                                beta_prior = NULL,
                                verbose    = TRUE,
                                show_summ  = verbose, 
                                show_res   = verbose,
                                show_plot  = verbose)
{
    if (alternative != "twosided")
      stop("One sided hypothesis tests are not currently supported.")
    
    if (is.null(null))
      stop("Null value for p in H1 must be specified.")

    beta_prior = check_beta_prior(beta_prior)
    hypothesis_prior = check_hypothesis_prior(hypothesis_prior)
    
    n = length(y) 
    k = sum(y == success)
    p_hat = k / n
    
    prior_a = beta_prior[1]
    prior_b = beta_prior[2]
    
    post_a = prior_a + k
    post_b = prior_b + n - k 
    
    prior_H1 = hypothesis_prior[1]
    prior_H2 = hypothesis_prior[2]
    
    mlik1 = dbinom(k,n,null)

    # {n \choose k} \frac{\text{B}(a+k,b + n - k)}{\text{B}(a,b)}
    mlik2 = exp( lchoose(n,k) + lbeta(prior_a+k,prior_b+n-k) - lbeta(prior_a,prior_b) )
    

    # print variable types
    if (show_summ){
        cat(paste0("Single categorical variable, success: ", success,"\n"))

        cat(paste0("n = ", n, ", p-hat = ", round(p_hat, 4), "\n"))
        cat("\n")
    }
    
    res = list(beta_prior = beta_prior,
               hypothesis_prior = hypothesis_prior)
    
    if (mlik1 >= mlik2)
    {
        res$order = "H1:H2"
        res$O  = prior_H1 / prior_H2
        res$BF = mlik1 / mlik2
        res$PO = res$BF * res$O
        res$post_H1 = res$PO / (res$PO+1)
        res$post_H2 = 1 - res$PO / (res$PO+1) 
        
    } else {
        res$order = "H2:H1"
        res$O  = prior_H2 / prior_H1
        res$BF = mlik2 / mlik1
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
        cat("H1: p = ", null, "\n",sep="")
        cat("H2: p ", alt_sign, " ", null, "\n",sep="")
        cat("\n")

        cat("Priors:\n")
        cat("P(p) ~ Beta(a=",prior_a,",b=",prior_b,")\n",sep="")
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
      p = seq(qbeta(0.0001,post_a,post_b),
              qbeta(0.9999,post_a,post_b),
              length.out=1000)

      den = dbeta(p,post_a,post_b)
      d = data.frame(p=p, y=den / max(den) * res$post_H2)    
      
      percentiles = c((1-cred_level)/2,1-(1-cred_level)/2)
      ci = qbeta(percentiles, post_a, post_b)
  

      ci_interval = data.frame(p = ci, y = 0)
      
      li = min(which(d$p >= ci[1]))  
      ui = max(which(d$p <  ci[2]))

      ci_poly = data.frame(p = c(d$p[c(li,li:ui,ui)]), 
                           y = c(0, d$y[li:ui], 0))

      H1_line = data.frame(p=c(null,null), y=c(0,res$post_H1))

      plot = ggplot(d, aes_string(x="p", y="y")) + 
             geom_line() + 
             ylab("Density") +
             xlab("p") +
             geom_line(data  = H1_line, size=1.5, col="blue", alpha=0.5) +
             geom_polygon(data = ci_poly, alpha=0.5)
      
      print(plot)
    }

    # return
    return(invisible(res)) 
}
