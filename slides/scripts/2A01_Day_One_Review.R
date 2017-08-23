## ------------------------------------------------------------------------
1:5
seq(1, 5)
seq(from = 1, to = 5)
seq(to = 5, from = 1)
seq(1, 5, by = 2)

## ------------------------------------------------------------------------
severity <- 10000
CV <- .3
sigma <- sqrt(log(1 + CV^2))
mu <- log(severity) - sigma^2/2
plot(function(x) dlnorm(x), mu, sigma, ylab="LN f(x)")

