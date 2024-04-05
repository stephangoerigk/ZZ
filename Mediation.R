library(lavaan)

set.seed(1234)

X = rnorm(100)

M = 0.5 * X + rnorm(100)
Y = 0.5 * M + rnorm(100)

data = data.frame(X = X, Y = Y, M = M)

summary(lm(Y ~ X, data = data))

mod = "

Y ~ c*X
M ~ a*X
Y ~ b*M

ab := a*b
total := c + a*b
"

fit = lavaan::sem(mod, data = data, se  = "bootstrap", bootstrap = 1001)
summary(fit)

library(semPaths)

semPaths(fit, whatLabels = "est")
