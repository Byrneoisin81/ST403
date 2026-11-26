#ex3.1
df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))

#ex3.2
nll_lm <- function(data, par) {
  X <- cbind(1, as.matrix(data[, -1]))
  eps <- data$y - X %*% par[-length(par)]
  -sum(dnorm(eps, mean = 0, sd = par[length(par)], log = TRUE))
}

#ex3.3
fit <- optim(par = c(mean(df$y), 0, 0, 0, 1), fn = nll_lm, data = df,
             method = "L-BFGS-B",
             lower = c(-Inf, -Inf, -Inf, -Inf, 1e-08),
             upper = c(Inf, Inf, Inf, Inf, Inf),
             control = list(maxit = 1000))
fit$convergence
fit$par

#ex3.5
X <- cbind(1, as.matrix(df[, -1]))
y <- df$y
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
cbind(optim = fit$par[1:4], matrix = drop(beta_hat))

#ex3.6
res <- y - X %*% beta_hat
c(optim = fit$par[5], matrix = sqrt(sum(res^2) / (nrow(X) - ncol(X))))

#ex3.8
fit_h <- optim(par = c(mean(df$y), 0, 0, 0, 1), fn = nll_lm, data = df,
               method = "L-BFGS-B",
               lower = c(-Inf, -Inf, -Inf, -Inf, 1e-08),
               upper = c(Inf, Inf, Inf, Inf, Inf),
               control = list(maxit = 1000), hessian = TRUE)
sqrt(diag(solve(fit_h$hessian)))[1:4]