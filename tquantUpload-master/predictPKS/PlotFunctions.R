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
    
    x0 <- c(4, 1, 7, 1, 4, 7, 4)                             # coordinates
    y0 <- c(1, 4, 4, 7, 7, 7, 10)-0.5
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

# Bayesian Plot functions
bayesTrace <- function(samples){
  
  # plot the traceplots for beta and eta for all items
  par(mfrow=c(6, 2),
      cex.axis = 1.3, las = 1, bty="n", mgp = c(3.5, 1, 0),     
      cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
      font.lab = 2, cex.axis = 1.3, bty = "n")

  for(i in 1:4){
    item.name.beta <- paste0("beta[", i, "]")
    item.name.eta  <- paste0("eta[", i, "]")
    # samples for beta and eta parameter
    beta.vector <- samples$BUGSoutput$sims.array[, , item.name.beta]
    eta.vector  <- samples$BUGSoutput$sims.array[, , item.name.eta]
    
    plot(beta.vector[, 1], type = 'l', col = "lightcoral", bty ="n", ylim = c(0,1),
         ylab = eval(bquote(expression(beta[.(letters[i])]))))
    lines(beta.vector[, 2], col = "lightblue3")
    plot(eta.vector[, 1], type = 'l', col = "lightcoral", bty = "n", ylim = c(0,1),
         ylab = eval(bquote(expression(eta[.(letters[i])]))))
    lines(eta.vector[, 2], col = "lightblue3")
  }
} 
bayesHist <- function(samples, item.number){
  par(mfrow = c(4, 2))
  for(i in item.number){
    plotPriorPosterior(samples$beta$prior.samples, samples$beta$post.samples,
                       info = samples$beta$info, param = "beta", item.number = i)
    plotPriorPosterior(samples$eta$prior.samples, samples$eta$post.samples,
                       info = samples$eta$info, param = "eta", item.number = i)
  }
} 
plotPriorPosterior <- function(prior.samples, post.samples, info, param = c("beta", "eta"), item.number = 1:4){
  
  par(cex.axis = 1.3, las = 1, bty="n", mgp = c(3.5, 1, 0),     
      cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
      font.lab = 2, cex.axis = 1.3, bty = "n")
  
  for(i in item.number){
    
    # parameter
    y.high <- round(max(max(density(prior.samples[,i])$y), max(density(post.samples[,i])$y))) 
    y.high.10 <- y.high + ceiling(y.high/100*10) # plus 10%
    y.high.20 <- y.high + ceiling(y.high/100*20) # plus 20%
    x.legend.pos <- ifelse(median(density(post.samples[,i])$x) > 0.5, 0, 1)
    x.lab <- ifelse(param == "beta", eval(bquote(expression(beta[.(letters[i])]))), eval(bquote(expression(eta[.(letters[i])]))))
    
    # plot
    plot(density(post.samples[,i], adjust = 2), 
         xlim = c(0, 1), ylim = c(0, y.high.20),
         xlab = x.lab, main = "", lwd = 2)
    par(new = TRUE)
    plot(density(prior.samples[,i], adjust = 2), 
         axes = FALSE, xlim = c(0, 1),
         xlab = "", ylab = "", main = "",
         ylim = c(0, y.high.20), lwd = 2, lty = 3)
    # add legend
    legend(x.legend.pos, y.high.20, legend = c("Posterior", "Prior"), 
           lty = c(1, 3), bty = "n", lwd = c(2, 2), cex = 1.2, 
           xjust = x.legend.pos, yjust = 1, x.intersp = 0.6, seg.len = 1.2)
    # add parameter information
    mtext(paste("median =", round(info$median,2)[i], sep = " "), side = 3, line = 2.5, adj = 1)
    mtext(paste("95% CI: [", round(info$lower,2)[i], ", ", round(info$upper,2)[i], "]", sep = ""), side = 3, line = 1, adj = 1)
    arrows(info$lower[i], y.high.10, info$upper[i], y.high.10, code = 3, length = 0.05, angle = 90)
  }
}
plotBayesSummary <- function(info, param = c("beta", "eta", "PK")){
  
  # general settings
  K           <- as.binmat(c("0000", "0110", "0101", "1110", "1011", "1101", "1111")) 
  # create axis labels
  x <- NULL
  if(param == "PK"){
    x <- as.pattern(K, as.letters=TRUE)
  } else {
    for(i in 1:4){
      x[i] <- ifelse(param == "beta", eval(bquote(expression(beta[.(letters[i])]))), eval(bquote(expression(eta[.(letters[i])]))))
    }
    x <- unlist(x)
  }
  
  # add label and text 
  info$label <- 1:nrow(info)
  info$text <- paste(sprintf("%.2f", round(info$median, 2)),  "  [", 
                     sprintf("%.2f", round(info$lower, 2)), ", ", 
                     sprintf("%.2f", round(info$upper, 2)), "]", sep = "")
  
  # Create summary plot
  p <- ggplot(info, aes(x = info$label, y = median, ymin = lower, ymax = upper)) + 
    scale_y_continuous("Posterior Parameter Estimates", breaks = seq(0, 1, by = 0.2), limits=c(0,1)) +
    coord_flip() + 
    scale_x_reverse(name = "", breaks = 1:nrow(info), labels = x, minor_breaks = waiver()) +
    geom_pointrange() + 
    geom_point(shape = 21, color = "black", fill = "grey", size = 4.5) +
    theme_classic(base_size = 20) + 
    # extend grid
    theme(plot.margin = grid::unit(c(2, 10.5, 1, 1), "lines")) +
    # annotations on right side
    geom_text(aes(y = Inf, x = info$label, label = info$text), size = 5, hjust = -0.1) +
    annotate("text", x = -Inf, y=Inf, label = "median   95% CI", hjust = -0.1, vjust = -1, size = 5)
  
  # Override clipping
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)
}





