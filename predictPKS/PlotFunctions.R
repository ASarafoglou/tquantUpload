# frequentist Plot functions
NRplot <- function(N.R){
    # N.R: named vector with N.R values 
    
    sets <- c("0", "a", "b", "c", "d", "ab",  "ac", "ad", "bc",  
              "bd", "cd", "abc", "abd", "acd", "bcd", "abcd")
    N.R  <- N.R[order(match(names(N.R), sets))]                    # match order of sets and P.K
    
    N.R <- as.table(N.R)                          
    barplot(N.R, 
            horiz     = TRUE, 
            names.arg = names(N.R),
            axes      = FALSE,
            cex.names = 1.3,
            las       = 2, 
            col       = "grey36",
            xlim      = c(0, max(N.R) + 5))
    # include axis
    axis(side     = 3, 
         cex.axis = 1.3)
}
betaPlot <- function(beta.vector){
    # beta.vector: vector with beta values (beta.a, beta.b, beta.c, beta.d)
    
    beta <- as.table(rev(beta.vector))                             # Reverse Eta 
    barplot(beta, 
            horiz     = TRUE, 
            names.arg = expression(beta[d], beta[c], beta[b], beta[a]),
            axes      = FALSE,
            cex.names = 2,
            las       = 2, 
            col       = "grey36")
    # include axis
    axis(side     = 3, 
         cex.axis = 1.5, 
         at       = c(0, 0.1, 0.2), 
         labels   = c("0", ".1", ".2"))
}
etaPlot <- function(eta.vector){
    # eta.vector: vector with eta values (eta.a, eta.b, eta.c, eta.d)
    
    eta <- as.table(rev(eta.vector))                               # Reverse Eta 
    barplot(eta, 
            horiz     = TRUE, 
            names.arg = expression(eta[d], eta[c], eta[b], eta[a]),
            axes      = FALSE,
            cex.names = 2,
            las       = 2,
            col = "grey36")
    axis(side = 3, cex.axis = 1.5, 
         at = c(0, 0.1, 0.2), 
         labels = c("0", ".1", ".2"))
}
ksPlot <- function(P.K){
    # P.K: vector with P.K values 
    
    hasNoNames <- is.null(names(P.K))
    if(hasNoNames){
      names(P.K) <- c("0000", "0110", "0101", "1110", "1011", "1101", "1111")
    }
    sets <- c("bc", "abc", "acd", "abcd", "bd", "abd")
    P.K  <- P.K[order(match(names(P.K), sets))]             # match order of sets and P.K
    
    x0 <- c(1, 1, 4, 4, 4, 7, 7)                                   # coordinates
    y0 <- c(4, 7, 1, 7, 10, 4, 7)-0.5
    # Plot Knowledge Structure
    # Empty Plot
    plot(1, type = "n", xlab = "", ylab = "", 
         xlim = c(0, 10), 
         ylim = c(0, 12), 
         axes = FALSE)
    # Add Lines
    polygon(x = c(5, 2, 2, 5, 5, 5, 8, 8, 5),
            y = c(1, 4, 7, 10, 7, 1, 4, 7, 10),
            border = "grey36",
            lwd    = 2) 
    # Add Points
    symbols(x = c(2, 2, 5, 5, 5, 8, 8),
            y = c(4, 7, 1, 7, 10, 4, 7),
            circles = rep(0.6, 7),
            add     = TRUE, 
            inches  = FALSE, 
            bg      = "lightgrey", 
            fg      = "grey36",
            lwd     = 2)
    # Add Knowledge States
    text(x = c(2, 2, 5, 5, 8, 8),
         y = c(4, 7, 7, 10, 4, 7),
         labels = sets,
         col    = "darkred",
         cex    = 1.5)
    text(x = 5, 
         y = 1, 
         labels = expression(symbol("\306")),                      # Empty Set
         col    = "darkred", 
         cex    = 1.5)
    # Barplots for P.K values
    for(i in 1:7){
        rect(x0[i] + 0.1, y0[i], x0[i] + 0.3, y0[i] + (P.K[i] * 4),
             col    = "red3", 
             border = "grey36")
    }
    # Add Axis of Barplots
    segments(x0 = x0, y0 = y0, x1 = x0, y1 = y0 + 0.8)             # Axis 
    segments(x0 = x0, y0 = y0, x1 = x0 - 0.1, y1 = y0)             # Ticks
    segments(x0 = x0, y0 = y0 + 0.8, x1 = x0 - 0.1, y1 = y0 + 0.8)
    segments(x0 = x0, y0 = y0 + 0.4, x1 = x0 - 0.1, y1 = y0 + 0.4)
    text(x0, y0, pos = 2, labels = rep("0", 8))                    # Labels
    text(x0, y0 + 0.8, pos = 2, labels = rep(".2", 8))
}
blimPlot <- function(beta.vector, eta.vector, P.K){
  # define layout
  matrix <- matrix(c(1, 1, 3, 3, 3, 3, 3, 3, 3, 
                     1, 1, 3, 3, 3, 3, 3, 3, 3, 
                     1, 1, 3, 3, 3, 3, 3, 3, 3, 
                     2, 2, 3, 3, 3, 3, 3, 3, 3, 
                     2, 2, 3, 3, 3, 3, 3, 3, 3, 
                     2, 2, 3, 3, 3, 3, 3, 3, 3), 6, 9, byrow = TRUE)
  layout(matrix)
  betaPlot(beta.vector)
  etaPlot(eta.vector)
  ksPlot(P.K)
  
              
}

# bayesian Plot functions
bayesTrace <- function(theta.chain, item.number){
  # plots the traceplots for beta and eta for one item 
  
  item.name.beta <- paste0("beta[", item.number, "]")
  item.name.eta  <- paste0("eta[", item.number, "]")
  # samples for beta and eta parameter
  beta.vector <- samples$BUGSoutput$sims.array[, , item.name.beta]
  eta.vector  <- samples$BUGSoutput$sims.array[, , item.name.eta]

  # plots the two traceplots next to each other
  layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), 2, 6, byrow = TRUE))
  plot(beta.vector[, 1], type = 'l', col = "lightcoral", ylab = item.name.beta, bty ="n")
  lines(beta.vector[, 2], col = "lightblue3")
  plot(beta.vector[, 1], type = 'l', col = "lightcoral", ylab = item.name.eta, bty = "n")
  lines(beta.vector[, 2], col = "lightblue3")
} # traceplot
bayesHist <- function(samples, beta.a = rep(1, 4), eta.a = rep(1, 4),
                      item.number, ci = TRUE){
  # plots the prior and posterior distribution for beta and eta for one item
  # plots two histograms next to each other
  par(cex.lab = 2, cex.axis = 1.5, las = 1, bty="n", mfrow=c(4, 2), mgp = c(2, 0.5, 0))     
  
  
  for(i in item.number){
  item.name.beta <- paste0("beta[", i, "]")
  item.name.eta  <- paste0("eta[", i, "]")
  # samples for beta and eta parameter
  beta.mu    <- mean(samples$BUGSoutput$sims.matrix[, item.name.beta])
  beta.var   <- var(samples$BUGSoutput$sims.matrix[, item.name.beta])
  beta.alpha <- ((1 - beta.mu) / beta.var - (1 / beta.mu)) * beta.mu ^ 2
  beta.beta  <- beta.alpha * (1 / beta.mu - 1)
  eta.mu    <- mean(samples$BUGSoutput$sims.matrix[, item.name.eta])
  eta.var   <- var(samples$BUGSoutput$sims.matrix[, item.name.eta])
  eta.alpha <- ((1 - eta.mu) / eta.var - (1 / eta.mu)) * eta.mu ^ 2
  eta.beta  <- eta.alpha * (1 / eta.mu - 1)
  # plot prior and posterior
  # beta
  curve(dbeta(x, beta.alpha, beta.beta), las = 1, main = " ", ylab = "Density",
        xlab = paste("beta for Item", letters[i], sep=" "), xlim = c(0, 1))
  curve(dbeta(x, beta.a[i], 10-beta.a[i]), add = TRUE, lty = 3)
  # eta
  curve(dbeta(x, eta.alpha, eta.beta), las = 1, main = " ", ylab = "Density",
        xlab = paste("eta for Item", letters[i], sep=" "), xlim = c(0, 1))
  curve(dbeta(x, eta.a[i], 10-eta.a[i]), add = TRUE, lty = 3)
  }
} # plot posterior





