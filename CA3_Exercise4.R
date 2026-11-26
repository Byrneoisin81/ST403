lmfit <- lm(y ~ x1 + x2 + x3, data = df)
coef(lmfit)    
sigma(lmfit)