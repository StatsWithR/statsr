# Marginal likelihood after integrating out phi1 and phi2 analytically
# This ignores some constants that will be constant across both hypotheses
# such as the gamma term in the sampling model for SS[i]
# ybar[i] ~ N(mu, (phi[i]n[i])^-1)
# ss[i] ~ Gamma((n[i]-1)/2, phi[i]/2
# p(mu, phi[1], phi[2]) proto 1/(phi[1]*phi[2]) 
#  returns the log marginal likelihood of H0

logmarglike_H0 = function(ybar, s2, n, low_mu=NULL, up_mu=NULL, m=4) 
{
  mu_hat = sum(ybar/s2)/sum(1/s2)
  if (is.null(low_mu) | is.null(up_mu))
  {
    offset = max(sqrt(s2))*m
    low_mu = mu_hat - offset
    up_mu  = mu_hat + offset
  }

  ss = s2*(n-1) 
  integrand = function(mu, ybar, ss, n) 
  {
      marg_log_like_H1 = -0.5*(n[1]*log(0.5*(n[1]*((ybar[1] - mu_hat)^2) + ss[1])) + 
                               n[2]*log(0.5*(n[2]*((ybar[2] - mu_hat)^2) + ss[2]))) +
                          0.5*(log(n[1]) + log(n[2])) + lgamma(n[1]/2) + lgamma(n[2]/2)
      exp(-0.5*(n[1]*log(0.5*(n[1]*((ybar[1] - mu)^2) + ss[1])) + 
                n[2]*log(0.5*(n[2]*((ybar[2] - mu)^2) + ss[2]))) +
               0.5*(log(n[1]) + log(n[2])) + lgamma(n[1]/2) + lgamma(n[2]/2) -
           marg_log_like_H1)
  } 
  
  marg_like = integrate(integrand, low_mu, up_mu, ybar, ss, n)

  return( log(marg_like$value) )
}


# Marginal likelihood after integrating out phi1 and phi2 analytically
# This ignores some constants that will be constant across both hypotheses
# such as the gamma term in the sampling model for SS[i]
# ybar[i] ~ N(mu[i], (phi[i]n[i])^-1)
# ss[i] ~ Gamma((n[i]-1)/2, phi[i]/2

# mu[i] ~ N(mu, .5(sigma[i]^2 + tau[i]^2)
# sigma[i] ~ C^+(0, tau[i]
    
# p(mu, tau[1], tau[2]) proto 1/(tau[1]*tau[2]) 
# returns the log marginal likelihood of H0


logmarglike_H1 = function(ybar, s2, n, max_eval=10^6, low_v=.01, up_v=100, m=4) 
{
  mu =(ybar[1]/s2[1] + ybar[2]/s2[2]) / (1/s2[1] + 1/s2[2])
  ss = s2*(n-1)
  
  offset = m*max(sqrt(s2))
  low_mu = mu - offset
  up_mu  = mu + offset
  
  integrand = function(x, ybar, ss, n)
  {
    mu=x[1]
    v1=x[2]
    v2=x[3]
    
    s2 = ss/(n - 1)
    mu_hat =(ybar[1]/s2[1] + ybar[2]/s2[2]) / (1/s2[1] + 1/s2[2])
    
    prec1 = 2*n[1]/(n[1] + 2 + n[1]*v1)
    prec2 = 2*n[2]/(n[2] + 2 + n[2]*v2)
    
    beta1 = 0.5*(prec1*(ybar[1] - mu)^2 + ss[1])
    beta2 = 0.5*(prec2*(ybar[2] - mu)^2 + ss[2])
    
    alpha1 = n[1] 
    alpha2 = n[2] 

    marg_log_like_H1 = -0.5*(n[1]*log(0.5*(n[1]*((ybar[1] - mu_hat)^2) + ss[1])) + 
                             n[2]*log(0.5*(n[2]*((ybar[2] - mu_hat)^2) + ss[2]))) +
                        0.5*(log(n[1]) + log(n[2])) + lgamma(n[1]/2) + lgamma(n[2]/2)
    
    like = exp(0.5*(log(prec1) + log(prec2) -alpha1*log(beta1) -alpha2*log(beta2)) +
              lgamma(alpha1/2) + lgamma(alpha2/2) +
              -0.5*(log(v1) + log(v2)) - log(1 + v1) - log(1 + v2) - 2*log(pi) -
              marg_log_like_H1)
    return(like)
  }

  marg_like = cubature::adaptIntegrate(integrand, 
                                       lowerLimit=c(low_mu, low_v, low_v), 
                                       upperLimit=c( up_mu,  up_v,  up_v),
                                       ybar, ss, n, maxEval=max_eval)
  return( log(marg_like$integral) )
}


behren_fisher_intrinsic_BF = function(ybar, s2, n, max_eval=10^6, low_v=0.001, up_v=100, m=4 )
{
     exp(logmarglike_H1(ybar, s2, n, max_eval, low_v, up_v, m) -  logmarglike_H0(ybar, s2, n, m=m) )
}


# Schwartz approximation to BF
behren_fisher_intrinsic_BF_schwartz = function (ybar, s2, n) 
{
  mu =( ybar[1]/s2[1] + ybar[2]/s2[2])/(1/s2[1] + 1/s2[2])

  return(
    exp(0.5*(n[1]*log(1 + (ybar[1] - mu)^2/s2[1]) +
        n[2]*log(1 + (ybar[2] - mu)^2/s2[2]) -
        log(n[1]) - log(n[2]) + log(n[1] + n[2])))
  )
}




behren_fisher_intrinsic_BF_post = function (ybar, s2, n, nsim=10000) 
{
  v1 = rf(nsim,  2, n[1])
  v2 = rf(nsim,  2, n[2])
  
  prec1 = 2*n[1]/(2 + n[1] + n[1]*v1)
  prec2 = 2*n[2]/(2 + n[2] + n[2]*v2)
  
  mu_hat = (ybar[1]/s2[1] + ybar[2]/s2[2])/(1/s2[1] + 1/s2[2])
  
  sp = sqrt(s2[1]/n[1] + s2[2]/n[2])
  
  mu = rnorm(nsim, mu_hat, sp)
  
  b1 = 0.5*(s2[1]*(n[1] - 1) + (prec1*n[1]*(ybar[1] - mu)^2)/(prec1 + n[1]))
  b2 = 0.5*(s2[2]*(n[2] - 1) + (prec2*n[2]*(ybar[2] - mu)^2)/(prec2 + n[2]))
  
  phi1 = rgamma(nsim, (n[1]-1)/2, b1)
  phi2 = rgamma(nsim, (n[2]-1)/2, b2)
  
  mu1 = rnorm(nsim, (ybar[1]*n[1] + mu*prec1)/(n[1] + prec1), (phi1*prec1)^{-1/2})
  mu2 = rnorm(nsim, (ybar[2]*n[2] + mu*prec2)/(n[2] + prec2), (phi2*prec2)^{-1/2})
  
  logpost = log(dnorm(ybar[1],mu1, 1/sqrt((n[1]*phi1)))) +
            log(dnorm(ybar[2],mu2, 1/sqrt((n[2]*phi2)))) +
            log(dnorm(mu1, mu,  sqrt(.5*(1 + v1)/phi1))) +
            log(dnorm(mu2, mu,  sqrt(.5*(1 + v2)/phi2))) +
            log(dgamma(s2[1]*(n[1]-1), (n[1] - 1)/2, phi1/2)) +
            log(dgamma(s2[2]*(n[2]-1), (n[2] - 1)/2, phi2/2)) +
            log(df(v1, 1/2, 1/2)) + log(df(v2, 1/2, 1/2)) +
            -log(phi1) - log(phi2)
  
  logprop = log(dgamma(phi1, (n[1]-1)/2, b1)) +
            log(dgamma(phi2, (n[2]-1)/2, b2)) +
            log(df(v1, 2, n[1])) +
            log(df(v2, 2, n[2])) +
            log(dnorm(mu, mu_hat, sd= sp))
            log(dnorm(mu1,(ybar[1]*n[1] + mu*prec1)/(n[1] + prec1), 
                  1/sqrt(phi1*(n[1] + prec1))))
            log(dnorm(mu2,(ybar[2]*n[2] + mu*prec2)/(n[2] + prec2),
            1/sqrt(phi1*(n[2] + prec2))))
  
  wt = exp(logpost - logprop)
  wt = wt/sum(wt)
  
  return(list(mu1=mu1, mu2=mu2, wt=wt))  
}




behren_fisher_intrinsic_BF_post_gibbs = function (ybar, s2, n,
                                                  prior_H1 = 0.5, 
                                                  nsim=10000, burnin=1000, thin=5, c=2.38) 
{
  #prior_odds = prior_H1/(1 - prior_H1)
  #post_odds = BF*prior_odds
  #post_H1 = post_odds/(1 + post_odds)
  
  # initialize
  theta_post = as.data.frame(matrix(NA, nrow=nsim, ncol=7))
  names(theta_post) = c("mu1", "mu2", "phi1", "phi2", "mu", "v1", "v2")
  
  theta=NULL
  theta$mu1 = ybar[1]
  theta$mu2 = ybar[2]
  theta$phi1 = 1/s2[1]
  theta$phi2 = 1/s2[2]
  theta$mu = (ybar[1]/s2[1] + ybar[2]/s2[2])/(1/s2[1] + 1/s2[2])
  theta$v1 = 0.01
  theta$v2 = 0.01
  
  for (i in 1:((nsim+burnin)*thin))
  {
    # Gibbs  mu1, mu2, phi1, phi2 | v1, v2, mu  
    theta = update_mu_phi(theta, ybar, s2, n) 
  
    # Gibbs update for mu | mu1, mu2, phi1, phi2, v1, v2  
    theta = update_mu_gibbs(theta, ybar, s2, n) 
    
    # random-walk Metropolis for log(v1)  
    theta = update_v1(theta, ybar, s2, n, c)
  
    # random-walk Metropolis  for log(v2)
    theta = update_v2(theta, ybar, s2, n, c)
    
    if (i > burnin & ((i - burnin) %% thin == 0)) {
       theta_post[as.integer(i/thin) - burnin,] = unlist(theta)
    }
  }

  return(theta_post)  
}


update_mu_phi = function(theta, ybar,s2,n)
{
  # block Gibbs update of mu1, mu2, phi1, phi2 | v1, v2, mu
  # draw phi1 | v1, mu  (marginal gamma)
  # draw phi2 | mid v2, mu  (marginal gamma)  
  # draw mu1 | phi1, v1, mu (conditional normal)
  # draw mu2 | phi2, v2, mu (conditional normal)
  
  v1 = theta$v1
  v2 = theta$v2
  
  mu = theta$mu
  
  prec1 =  2/(1 + v1)  #prior prec multiplier
  prec2 =  2/(1 + v2)
  
  b1 = 0.5*(s2[1]*(n[1] - 1) + (prec1*n[1]*(ybar[1] - mu)^2)/(prec1 + n[1]))
  b2 = 0.5*(s2[2]*(n[2] - 1) + (prec2*n[2]*(ybar[2] - mu)^2)/(prec2 + n[2]))
  
  theta$phi1 = rgamma(1, (n[1]-1)/2, rate=b1)
  theta$phi2 = rgamma(1, (n[2]-1)/2, rate=b2)
  
  theta$mu1 = rnorm(1, (ybar[1]*n[1] + mu*prec1)/(n[1] + prec1), 1/sqrt(theta$phi1*(n[1] + prec1)))
  theta$mu2 = rnorm(1, (ybar[2]*n[2] + mu*prec2)/(n[2] + prec2), 1/sqrt(theta$phi2*(n[2] + prec2)))
  
  return(theta)
}
  
update_mu_gibbs = function(theta, ybar, s2, n) 
{
  prec = 2*(theta$phi1/(1 + theta$v1)  + theta$phi2/(1 + theta$v2))
  mean = 2*(theta$mu1*theta$phi1/(1 + theta$v1) + theta$mu2*theta$phi2/(1 + theta$v2))/prec
  theta$mu = rnorm(1, mean, sd=1/sqrt(prec)) 
  
  return(theta)
}


update_v1 = function(theta, ybar,s2,n, c) 
{
  theta_prop = theta
  theta_prop$v1 = exp(log(theta$v1) + c*rnorm(1))
  
  R = logpost_behren_fisher_intrinsic(theta_prop,ybar,n,s2) -
      logpost_behren_fisher_intrinsic(theta,ybar,n,s2) +
      log(theta_prop$v1) - log(theta$v1)
 
  if (R > log(runif(1))) 
    theta = theta_prop

  return(theta)
}

update_v2 = function(theta, ybar, s2, n, c) 
{
  theta_prop = theta
  theta_prop$v2 = exp( rnorm(1)*c + log(theta$v2))
  
  R = logpost_behren_fisher_intrinsic(theta_prop,ybar,n,s2) -
      logpost_behren_fisher_intrinsic(theta,ybar,n,s2) +
      log(theta_prop$v2) - log(theta$v2)
  
  if (R > log(runif(1)))
    theta = theta_prop

  return(theta)   
}




logpost_behren_fisher_intrinsic = function(theta, ybar, n, s2)
{
  mu1 = theta$mu1
  mu2 = theta$mu2
  mu = theta$mu
  
  phi1= theta$phi1
  phi2 = theta$phi2
  
  v1 = theta$v1
  v2 = theta$v2

  logpost = log(dnorm(ybar[1],mu1, 1/sqrt((n[1]*phi1)))) +
            log(dnorm(ybar[2],mu2, 1/sqrt((n[2]*phi2)))) +
            log(dnorm(mu1, mu,  sqrt(.5*(1 + v1)/phi1))) +
            log(dnorm(mu2, mu,  sqrt(.5*(1 + v2)/phi2))) +
            log(dgamma(s2[1]*(n[1]-1), (n[1] - 1)/2, phi1/2)) +
            log(dgamma(s2[2]*(n[2]-1), (n[2] - 1)/2, phi2/2)) +
            -0.5*log(v1) - log(1 + v1) - 0.5*log(v2) - log(1 + v2) +
            -log(phi1) - log(phi2) 
 
 return(logpost)
}


# Example Data
# ybar = c(52.10, 27.10)
# s2 = c(45.10, 26.40)^2
# n = c(22, 22)
# 
# BF = behren_fisher_intrinsic_BF(ybar, s2, n, max_eval=10^6, low_v=.001, up_v=100, m=4)
# 
# prior_H1 = 0.5
# prior_odds = prior_H1/(1 - prior_H1)
# post_odds = BF*prior_odds
# post_H1 = post_odds/(1 + post_odds)
# 
# 
# post1 = behren_fisher_intrinsic_BF_post_gibbs(ybar, s2, n, nsim=100000, burnin=10000)
# post2 = behren_fisher_intrinsic_BF_post(ybar, s2, n)
