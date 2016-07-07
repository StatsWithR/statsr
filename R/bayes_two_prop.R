bayes_ci_two_prop = function(y, x, success, cred_level = 0.95,
                             beta_prior1 = NULL,
                             beta_prior2 = NULL,
                             verbose   = TRUE,
                             show_summ = verbose, 
                             show_res  = verbose,
                             show_plot = verbose)
{  
  nsim = 1e6

  beta_prior1 = check_beta_prior(beta_prior1)
  beta_prior2 = check_beta_prior(beta_prior2)
  
  prior_a1 = beta_prior1[1]
  prior_b1 = beta_prior1[2]

  prior_a2 = beta_prior2[1]
  prior_b2 = beta_prior2[2]
  
  ns = by(y, x, length)
  n1 = as.numeric(ns[1])
  n2 = as.numeric(ns[2])
  
  gr1 = levels(x)[1]
  gr2 = levels(x)[2]

  k1 = sum(y[x == gr1] == success)
  k2 = sum(y[x == gr2] == success)

  post_a1 = prior_a1 + k1
  post_b1 = prior_b1 + n1 - k1
  
  post_a2 = prior_a2 + k2
  post_b2 = prior_b2 + n2 - k2
  
  post1 = rbeta(nsim, post_a1, post_b1)
  post2 = rbeta(nsim, post_a2, post_b2)

  post = post1 - post2

  ci = quantile(post, probs = c((1-cred_level)/2,1-(1-cred_level)/2))
  
  den = coda_density(post)
  
  post_mean   = mean(post)
  post_median = median(post)
  post_mode   = den$x[which.max(den$y)]


  # print variable types
  if (show_summ)
  {
    n_x_levels = length(levels(x))
    n_y_levels = length(levels(y))
    cat(paste0("Response variable: categorical (", n_x_levels, " levels, success: ", success, ")\n"))
    cat(paste0("Explanatory variable: categorical (", n_y_levels, " levels) \n"))

    cat(paste0("n_", gr1, " = ", n1, ", p_hat_", gr1, " = ", round(k1/n1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", p_hat_", gr2, " = ", round(k2/n2, 4), "\n"))
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
    # p = seq(0,1,length.out=1000)

    # post1  = data.frame(group=gr1, type="Posterior",  p=p, y=dbeta(p,post_a1,post_b1))    
    # prior1 = data.frame(group=gr1, type="Prior",      p=p, y=dbeta(p,prior_a1,prior_b1))
    # like1  = data.frame(group=gr1, type="Likelihood", p=p, y=dbinom(k1,n1,p))
 
    # post2  = data.frame(group=gr2, type="Posterior",  p=p, y=dbeta(p,post_a2,post_b2))    
    # prior2 = data.frame(group=gr2, type="Prior",      p=p, y=dbeta(p,prior_a2,prior_b2))
    # like2  = data.frame(group=gr2, type="Likelihood", p=p, y=dbinom(k2,n2,p))
 

    # d = rbind(prior1,like1,post1,
    #           prior2,like2,post2)



    # plot = ggplot(d, aes(x=p, y=y, col=type)) + 
    #        geom_line() + 
    #        ylab("Density") + xlab("p") +
    #        facet_wrap(group ~ type, scales="free_y")
    # print(plot)
    

    d_post = data.frame(p = den$x, y = den$y)

    li = min(which(d_post$p >= ci[1]))  
    ui = max(which(d_post$p <  ci[2]))

    ci_poly = data.frame(p = c(d_post$p[c(li,li:ui,ui)]), 
                         y = c(0, d_post$y[li:ui], 0))


    ci_interval = data.frame(p = ci, y = 0)
    pos_plot = ggplot(d_post, aes_string(x="p", y="y")) + 
               geom_line() +
               ylab("Density") +  
               xlab(paste0("p_",gr1," - p_",gr2)) +
               geom_line(data  = ci_interval, size=1.5) +
               geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)


    print(pos_plot)
  }

  # return
  return( invisible(
    list(
      post        = post,
      post_den    = den,
      post_mean   = post_mean,
      post_median = post_median,
      post_mode   = post_mode,
      cred_level = cred_level,
      ci = ci
    )
  ))
}





bayes_ht_two_prop = function(y, x, success, null = 0,
                             cred_level = 0.95,
                             alternative = "twosided", 
                             hypothesis_prior = NULL,
                             beta_prior1 = NULL,
                             beta_prior2 = NULL,
                             verbose   = TRUE,
                             show_summ = verbose, 
                             show_res  = verbose,
                             show_plot = verbose)
{
  if (alternative != "twosided")
    stop("One sided hypothesis tests are not currently supported.", call.=FALSE)

  if (null != 0)
    stop(paste0("Currently only H1: p_", gr1, " - p_", gr2, " = 0  (null) supported."), call.=FALSE)


  beta_prior1 = check_beta_prior(beta_prior1)
  beta_prior2 = check_beta_prior(beta_prior2)
  hypothesis_prior = check_hypothesis_prior(hypothesis_prior)
  
  prior_H1 = hypothesis_prior[1]
  prior_H2 = hypothesis_prior[2]

  prior_a1 = beta_prior1[1]
  prior_b1 = beta_prior1[2]

  prior_a2 = beta_prior2[1]
  prior_b2 = beta_prior2[2]
  
  ns = by(y, x, length)
  n1 = as.numeric(ns[1])
  n2 = as.numeric(ns[2])
  
  gr1 = levels(x)[1]
  gr2 = levels(x)[2]

  k1 = sum(y[x == gr1] == success)
  k2 = sum(y[x == gr2] == success)


  # \binom{n1+n2,k1+k2} B(k1+k2+a1+a2, n1+n2-k1-k2+b1+b2) / B(a1+a2,b1+b2)

  mloglik1 = ( lbeta(k1+k2+prior_a1+prior_a2, n1+n2-k1-k2+prior_b1+prior_b2)
              -lbeta(prior_a1+prior_a2, prior_b1+prior_b2))


  # \binom{n1,k1} B(k1+a1, n1-k1+b1) / B(a1,b1) \times \binom{n2,k2} B(k2+a2, n2-k2+b2) / B(a2,b2)

  mloglik2 = ( lbeta(k1+prior_a1, n1-k1+prior_b1)
              -lbeta(prior_a1, prior_b1)
              +lbeta(k2+prior_a2, n2-k2+prior_b2)
              -lbeta(prior_a2, prior_b2))

  if (show_summ)
  {
    n_x_levels = length(levels(x))
    n_y_levels = length(levels(y))
    cat(paste0("Response variable: categorical (", n_x_levels, " levels, success: ", success, ")\n"))
    cat(paste0("Explanatory variable: categorical (", n_y_levels, " levels) \n"))

    cat(paste0("n_", gr1, " = ", n1, ", p_hat_", gr1, " = ", round(k1/n1, 4), "\n"))
    cat(paste0("n_", gr2, " = ", n2, ", p_hat_", gr2, " = ", round(k2/n2, 4), "\n"))
  }
  
  res = list(beta_prior1 = beta_prior1,
             beta_prior2 = beta_prior2,
             hypothesis_prior = hypothesis_prior)
  
  if (mloglik1 >= mloglik2)
  {
      res$order = "H1:H2"
      res$O  = prior_H1 / prior_H2
      res$BF = exp(mloglik1 - mloglik2)
      res$PO = res$BF * res$O
      res$post_H1 = res$PO / (res$PO+1)
      res$post_H2 = 1 - res$PO / (res$PO+1) 
      
  } else {
      res$order = "H2:H1"
      res$O  = prior_H2 / prior_H1
      res$BF = exp(mloglik2 - mloglik1)
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
      cat("H1: p_", gr1, " =  p_", gr2, "\n",sep="")
      cat("H2: p_", gr1, " ", alt_sign, " p_", gr2, "\n",sep="")
      cat("\n")

      cat("Priors:\n")
      cat("P(p_",gr1,") ~ Beta(a=",prior_a1,",b=",prior_b1,")\n",sep="")
      cat("P(p_",gr2,") ~ Beta(a=",prior_a2,",b=",prior_b2,")\n",sep="")
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
    nsim = 1e6

    post_a1 = prior_a1 + k1
    post_b1 = prior_b1 + n1 - k1
    
    post_a2 = prior_a2 + k2
    post_b2 = prior_b2 + n2 - k2
    
    post1 = rbeta(nsim, post_a1, post_b1)
    post2 = rbeta(nsim, post_a2, post_b2)
  
    post = post1 - post2
  
    ci = quantile(post,probs = c((1-cred_level)/2, 1-(1-cred_level)/2))
    
    den = coda_density(post)
    
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
               xlab(paste0("p_",gr1," - p_",gr2)) +
               #geom_line(data  = ci_interval, size=1.5) +
               geom_line(data  = H1_line, size=1.5, col="blue", alpha=0.5) +
               #geom_point(data = ci_interval, size=2) +
               geom_polygon(data = ci_poly, alpha=0.5)


    print(pos_plot)
  }
  
  # return
  return(invisible(res)) 
}
