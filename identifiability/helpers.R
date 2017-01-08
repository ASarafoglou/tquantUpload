# calculate beta_c from pi_ab
calc_beta_c <- function(pi.ab, pi.ab.0, beta.c.0, pi.empty) {
    (-pi.ab + pi.ab.0 + beta.c.0 * (1 - pi.empty - pi.ab.0)) /
    (-pi.ab - pi.empty + 1)
}

# calculate pi_ab from beta_c
calc_pi_ab <- function(beta.c, pi.ab.0, beta.c.0, pi.empty) {
    ((beta.c.0 - beta.c) * (1 - pi.empty) + (1 - beta.c.0) *
    pi.ab.0) / (1 - beta.c)
}

# Outer product with vector function
# expand.outer joins functionality of "expand.grid" and "outer", so that
# a vector function can be applied, values of which are then stored in columns.
# Combinations of "x" and "y" are expanded into vectors of length n*m, where
# n = length(x), m = length(y) and this ordering is preserved also in 'values'.
# Matrix of values (list item 'values') and expanded variables (list items 'x','y') are returned.
expand.outer <- function(x, y, vecfun) {
    xy.pairs <- expand.grid(x=x, y=y, KEEP.OUT.ATTRS = FALSE)
    x.exp    <- xy.pairs$x
    y.exp    <- xy.pairs$y
    list(values=matrix(vecfun(x.exp,y.exp), nrow=2, byrow=TRUE),
	 x=x.exp, y=y.exp)
}

# vector field plot function
# grid.points can be defined for both axes at once or separately
plotVectorField <- function(vecfun, xlim, ylim, grid.points) {
    gp <- if (length(grid.points) > 1) grid.points else rep(grid.points, 2)
    maxlength <- c(diff(xlim), diff(ylim)) / (gp - 1) * 0.9

    # prepare data
    x0 <- seq(xlim[1], xlim[2], length=gp[1])
    y0 <- seq(ylim[1], ylim[2], length=gp[2])
    xy.data <- expand.outer(x0, y0, vecfun)
    x0 <- xy.data$x
    y0 <- xy.data$y
    dx <- xy.data$values[1,]
    dy <- xy.data$values[2,]

    # scale
    k <- min(maxlength / c(max(abs(dx)), max(abs(dy))))
    x1 <- x0 + k * dx
    y1 <- y0 + k * dy

    # plot
    plot.default(extendrange(r=range(x0, x1), f=0.067),
		 extendrange(r=range(y0, y1), f=0.12),
                 xlab=expression(beta[c]), ylab=expression(pi[ab]),
		 type="n", frame.plot=FALSE)
    arrows(x0,y0,x1,y1,length = 0.05, angle = 20, code = 2)
}

# prediction function of example K^02
# calculate predictions for response pattern frequencies from BLIM parameters
# see Heller, J. (2016) Identifiability in probabilistic knowledge structures,
# Journal of Mathematical Psychology,
# http://dx.doi.org/10.1016/j.jmp.2016.07.008, p. 5
predictionFunction <- function(beta.a, beta.b, beta.c, pi.empty, pi.ab) {
    pi.abc <- 1 - pi.ab - pi.empty
    c(phi.empty = pi.empty + beta.a*beta.b*beta.c*pi.abc + beta.a*beta.b*pi.ab,
      phi.a     = (1 - beta.a)*beta.b*beta.c*pi.abc +
                  (1 - beta.a)*beta.b*pi.ab,
      phi.b     = beta.a*(1 - beta.b)*beta.c*pi.abc +
                  beta.a*(1 - beta.b)*pi.ab,
      phi.c     = beta.a*beta.b*(1 - beta.c)*pi.abc,
      phi.ab    = (1 - beta.a)*(1 - beta.b)*beta.c*pi.abc +
                  (1 - beta.a)*(1 - beta.b)*pi.ab,
      phi.ac    = (1 - beta.a)*beta.b*(1 - beta.c)*pi.abc,
      phi.bc    = beta.a*(1 - beta.b)*(1 - beta.c)*pi.abc
     )
}
