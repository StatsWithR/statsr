coda_density = function(x, from, to)
{
    bwf = 1.06 * min(sd(x), IQR(x)/1.34) * length(x)^-0.2
    
    return(density(x, from=from, to=to, bw=bwf))
}


check_beta_prior = function(beta_prior, group="")
{
    arg_name = paste(substitute(beta_prior))
    if (arg_name == "") arg_name = "beta_prior"

    param = ifelse(group == "", "p", paste0("p_",group))    
    
    if (is.null(beta_prior))
    {
        warning("No beta prior for ",param," was specified, assuming a uniform prior (p ~ Beta(a=1,b=1)).\n",
                "  This beta prior is specified using the argument ",arg_name,"=c(a,b),\n",
                "  where a and b are your desired hyperparameters.")
        beta_prior = c(a=1,b=1)
    }
    
    stopifnot(length(beta_prior) == 2)
    
    if (is.null(names(beta_prior)))
        names(beta_prior) = c("a","b")
    stopifnot(all(sort(names(beta_prior)) == c("a","b")))
    beta_prior = beta_prior[c("a","b")]
    
    return(beta_prior)
}

check_hypothesis_prior = function(prior)
{
    if (is.null(prior))
    {
        warning("No prior set for H1 and H2, assuming a uniform prior of P(H1) = 0.5 and P(H2) = 0.5. The hypothesis prior is assigned using the argument  prior=c(H1=a,H2=b). ")
        prior = c(H1=0.5,H2=0.5)
    }

    if (length(prior) == 1)
    {   
        if (names(prior) %in% c("H1","H2"))
            prior[ setdiff(c("H1","H2"), names(prior)) ] = 1 - prior
    }

    stopifnot(length(prior) == 2)
    stopifnot(all(prior >= 0))
    stopifnot(sum(prior) == 1)

    if (is.null(names(prior)))
        names(prior) = c("H1","H2")

    stopifnot(all(sort(names(prior)) == c("H1","H2")))
    
    return(prior[c("H1","H2")])
}