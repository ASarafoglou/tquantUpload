# load packages
library(pks)
library(R2jags)
library(ggplot2)

# general settings 
#   N.R <- rep(10, 7)
#   names(N.R) <- as.pattern(K)
  
# Frequentist parameter estimation
frequentistParameterEstimation <- function(N.R, estimation.method, beta.parameters = rep(0.1, 4),
                                           eta.parameters  = rep(0.1, 4)){
  # estimates the frequentist model for blim
  
  K           <- as.binmat(c("0000", "0110", "0101", "1110", "1011", "1101", "1111")) 
  rownames(K) <- as.pattern(K)
  names(N.R)  <- as.pattern(K)
  classic.blim <- blim(K, N.R, method = estimation.method, beta = beta.parameters,
                       eta = eta.parameters)
}
# Bayesian parameter estimation
bayesianParameterEstimation <- function(N.R, beta.percentages, eta.percentages, n.iter, n.thin, n.burnin){
  # estimates the bayesian graphical model for blim
  
  # general settings
  K           <- as.binmat(c("0000", "0110", "0101", "1110", "1011", "1101", "1111")) 
  rownames(K) <- as.pattern(K)
  nitems  <- ncol(K)
  nstates <- nrow(K)
  names(N.R) <- as.pattern(K)
  R          <- as.binmat(N.R)
  npatterns <- 7
  n.observ  <- sum(N.R)
  # beta and eta priors
  beta.a <- beta.percentages*10
  beta.b <- 10- beta.a
  eta.a  <- eta.percentages*10
  eta.b  <- 10- eta.a
  # set up Bayesian graphical model
  data    <- list("N.R", "n.observ", "R", "K", "nitems", "nstates", "npatterns", 
                  "beta.a", "beta.b", "eta.a", "eta.b")
  myinits <- list(list(beta = rep(0.1, nitems), eta = rep(0.1, nitems), 
                       P.K = rep(1/nstates, nstates)),
                  list(beta = rep(0.1, nitems), eta = rep(0.1, nitems), 
                       P.K = rep(1/nstates, nstates)))
  parameters <- c("P.K", "eta", "beta")
  
  # all posterior samples
  samples    <- jags(data, inits = myinits, parameters,
                     model.file  = "blim_model.txt", n.chains = 2, n.iter = n.iter,
                     n.thin = n.thin, n.burnin=n.burnin)
  # information about beta parameters
  beta <- list(post.samples  = samples$BUGSoutput$sims.list[["beta"]],
               prior.samples = sapply(1:4, function(i) rbeta(1e5, beta.a[i], 10-beta.a[i])),
               info          = estimateMedian(samples$BUGSoutput$sims.list[["beta"]]))
  # information about eta parameters
  eta <- list(post.samples  = samples$BUGSoutput$sims.list[["eta"]],
              prior.samples = sapply(1:4, function(i) rbeta(1e5, eta.a[i], 10-eta.a[i])),
              info          = estimateMedian(samples$BUGSoutput$sims.list[["eta"]]))
  PK <-  list(post.samples  = samples$BUGSoutput$sims.list[["P.K"]],
              prior.samples = sapply(1:7, function(i) rbeta(1e5, 1, 6)),
              info          = estimateMedian(samples$BUGSoutput$sims.list[["P.K"]]))
  # output
  output <- list(samples = samples,
                 beta = beta,
                 eta = eta,
                 PK = PK)
  return(output)
}
# convergence diagnostics
convergenceDiagnostics <- function(samples, item.number = c(1:4)){
  # calculates for each item Gelmans R for beta and eta parameter
  
  convergenceTable <- data.frame(Item = NA, Scale.reduction.factor = NA, Upper.CI = NA)
  for(i in seq_along(item.number)){
    item.name.beta <- paste0("beta[", i, "]")
    item.name.eta  <- paste0("eta[", i, "]")
    # transform samples into mcmc list
    beta.list <- list(mcmc(samples$BUGSoutput$sims.array[, 1, item.name.beta]), # chain 1
                      mcmc(samples$BUGSoutput$sims.array[, 2, item.name.beta])) # chain 2
    
    eta.list  <- list(mcmc(samples$BUGSoutput$sims.array[, 1, item.name.beta]), # chain 1
                      mcmc(samples$BUGSoutput$sims.array[, 2, item.name.beta])) # chain 2
    # compute gelmans R   
    convergenceTable[i, "Item"] <- i
    convergenceTable[i, "Scale.reduction.factor"] <- round(gelman.diag(beta.list)$psrf[1],5) 
    convergenceTable[i, "Upper.CI"] <- round(gelman.diag(beta.list)$psrf[2],5)
    
  }
  return(convergenceTable)
}
# Calculate median plus HDI
estimateMedian <- function(samples) {
  medianPlusHDI <- data.frame(lower = NA, median = NA, upper = NA)
  for(i in 1:ncol(samples)){
    d <- quantile(samples[,i], c(0.025, 0.5, 0.975))
    medianPlusHDI[i,] <- d
  }
  return(medianPlusHDI)
}


## Customize beta and eta priors

# # adjust priors for beta and eta
# beta.percentages <- c(50, 60, 10, 90)
# eta.percentages  <- c(50, 60, 20, 80)
# # beta and eta priors
# beta.a <- beta.percentages/10
# beta.b <- 10- beta.a
# eta.a  <- eta.percentages/10
# eta.b  <- 10- eta.a
# n.iter <- 120000
# n.thin <- 10
# n.burnin <- 5000








