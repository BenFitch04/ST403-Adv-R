#1.
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
head(df)

#2.
nll_lm <- function(data, par){
  
  B <- par[1:4] #B0, B1, B2, B3
  sigma <- par[5]
  fitted <- B[1] + B[2]*data$x1 + B[3]*data$x2 + B[4]*data$x3
  
  resid <- data$y - fitted
  
  -sum(dnorm(resid, mean = 0, sd = sigma, log = TRUE))}

#3.
start <- c(mean(df$y), 0, 0, 0, sd(df$y))

low <- c(-Inf, -Inf, -Inf, -Inf, 1e-8) #sd cant be <= 0

up <- c(Inf, Inf, Inf, Inf, Inf)

fit <- optim(par = start, fn = nll_lm, data = df,
             method = "L-BFGS-B", lower = low, upper = up)

fit$par #B0, B1, B2, B3, sd

#4.
#We want to find the parameter values that 
#maximise the likelihood.
#optim() minimises a function.
#Minimising negative log-likelihood gets same 
#result as maximising the log-likelihood.

#5.
X <- cbind(1, df$x1, df$x2, df$x3)
y <- df$y

beta_m <- solve(t(X) %*% X) %*% t(X) %*% y
beta_m

#6.
fitted_m <- X %*% beta_m #predicted y for each row
resid_m <- y - fitted_m # e = y - XB

RSS <- sum(resid_m^2)
sqrt(RSS / nrow(df))

#7.
#The estimates for the coefficients are 
#close because both methods use the same 
#least squares matrix equation
#They differ when estimating s.d because 
#max likelihood divides RSS by total observations 
#while the other divides by residual 
#degrees of freedom 

#8.
fit <- optim(par = start, fn = nll_lm, data = df,
             method = "L-BFGS-B", lower = low, upper = up, hessian = TRUE)

H <- fit$hessian
cov_m <- solve(H)
sqrt(diag(cov_m)[1:4])
