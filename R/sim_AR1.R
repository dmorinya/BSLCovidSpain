library(WriteXLS)
library(BSL)
library(ggplot2)
library(doParallel)
library(gdata)
library(ggpubr)
source("Estep.R")
source("Mstep.R")
source("EM.R")

ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

logPrior <- function(theta)
{
  log(theta[2] > 0 & theta[2] < 1 & theta[3] > 0 & 
      theta[3] < 1 & theta[4] > 0 & theta[4] < 1 &
      theta[5] > 0)
}

sim <- function(theta, T)
{
  mu <- (exp(log(exp(theta[6])*exp(theta[7]))-log(exp(theta[6])+(exp(theta[7])-1)))-1)
  x  <- theta[1]+arima.sim(model=list(order=c(1,0,0), ar=theta[2]), rand.gen=function(n, ...) rnorm(n, mean=mu, sd=theta[5]), n=T)
  z  <- rbinom(T, 1, theta[3])
  y  <- x[1:T]*(1-z[1:T])+theta[4]*z[1:T]*x[1:T]
  return(y)
}

st <- function(z){ 
  s1=mean(z); s2=sd(z); s3=acf(z,plot=F)$acf[2] 
  s4=acf(z,plot=F)$acf[3]; s5=acf(z,plot=F)$acf[4]
  c(s1,s2,s3,s4,s5)}

ar1 <- round(seq(0.1, 0.9, 0.1), 1); q <- round(seq(0.1, 0.9, 0.1), 1); 
w <- round(seq(0.1, 0.9, 0.1), 1); phi0 <- 500; se <- 1;
m <- 5; beta <- 0.4
res <- data.frame(expand.grid(phi0=phi0, ar1=ar1, w=w, q=q, se=se))

genEsts <- function(i)
{
  library(mixtools)
  library(BSL)
  print(paste0("Simulation step ", i, " out of ", dim(res)[1]))
  theta=c(res[i, 1], res[i, 2], res[i, 3], res[i, 4], res[i, 5])
  yy=sim(c(theta[1], theta[2], theta[3], theta[4], theta[5], m, beta), 1000)
  ### Initial values
  init <- normalmixEM(yy, maxit=10000)
  q_init <- init$mu[init$mu==min(init$mu)]/init$mu[init$mu==max(init$mu)]
  init_sigma <- init$sigma[init$sigma==max(init$sigma)]
  ind <- ifelse(init$posterior[, init$mu==min(init$mu)]<0.5, 0, 1)
  x <- ifelse(ind==1, yy/q_init, yy)
  init_ar1 <- arima(x, order=c(1, 0, 0))$coef[1]
  if (init_ar1<0) init_ar1 <- 0.1
  init_vals <- c(mean(x), init_ar1, init$lambda[init$mu==min(init$mu)],
                 q_init, sd(x), m, beta)
  model <- BSLModel(fnSim = sim, fnSum = st,
                    simArgs = list(T = 100), 
                    theta0 = init_vals,
                    fnLogPrior = logPrior, 
                    thetaNames=c(expression(phi[0]), expression(alpha[1]), expression(omega), "q", expression(sigma), "m", expression(beta)))

  resultAr1 <- bsl(y = yy, n = 500, M = 300, model = model,
                   covRandWalk = diag(c(.3^2,.1^2,.01^2,.001^2,.3^2,.01^2,.01^2)),
                   method = 'BSL', parallel=TRUE, verbose = FALSE)
  # Keep the results
  phi0_p50 <- median(resultAr1@theta[, 1])
  phi0_p2.5 <- quantile(resultAr1@theta[, 1], 0.025)
  phi0_p97.5 <- quantile(resultAr1@theta[, 1], 0.975)
  phi0_mean <- mean(resultAr1@theta[, 1])
  phi0_sd   <- sd(resultAr1@theta[, 1])
  ar1_p50  <- median(resultAr1@theta[, 2])
  ar1_p2.5 <- quantile(resultAr1@theta[, 2], 0.025)
  ar1_p97.5 <- quantile(resultAr1@theta[, 2], 0.975)
  ar1_mean <- mean(resultAr1@theta[, 2])
  ar1_sd   <- sd(resultAr1@theta[, 2])
  w_p50    <- median(resultAr1@theta[, 3])
  w_p2.5 <- quantile(resultAr1@theta[, 3], 0.025)
  w_p97.5 <- quantile(resultAr1@theta[, 3], 0.975)
  w_mean <- mean(resultAr1@theta[, 3])
  w_sd   <- sd(resultAr1@theta[, 3])
  q_p50    <- median(resultAr1@theta[, 4])
  q_p2.5 <- quantile(resultAr1@theta[, 4], 0.025)
  q_p97.5 <- quantile(resultAr1@theta[, 4], 0.975)
  q_mean <- mean(resultAr1@theta[, 4])
  q_sd   <- sd(resultAr1@theta[, 4])
  se_p50   <- median(resultAr1@theta[, 5])
  se_p2.5 <- quantile(resultAr1@theta[, 5], 0.025)
  se_p97.5 <- quantile(resultAr1@theta[, 5], 0.975)
  se_mean <- mean(resultAr1@theta[, 5])
  se_sd   <- sd(resultAr1@theta[, 5])
  m_p50   <- median(resultAr1@theta[, 6])
  m_p2.5 <- quantile(resultAr1@theta[, 6], 0.025)
  m_p97.5 <- quantile(resultAr1@theta[, 6], 0.975)
  m_mean <- mean(resultAr1@theta[, 6])
  m_sd   <- sd(resultAr1@theta[, 6])
  beta_p50   <- median(resultAr1@theta[, 7])
  beta_p2.5 <- quantile(resultAr1@theta[, 7], 0.025)
  beta_p97.5 <- quantile(resultAr1@theta[, 7], 0.975)
  beta_mean <- mean(resultAr1@theta[, 7])
  beta_sd   <- sd(resultAr1@theta[, 7])
  return(c(res$phi0[i], res$ar1[i], res$w[i], res$q[i], res$se[i], 
           phi0_p50, phi0_p2.5, phi0_p97.5, phi0_mean, phi0_sd, 
           ar1_p50, ar1_p2.5, ar1_p97.5, ar1_mean, ar1_sd,
           w_p50, w_p2.5, w_p97.5, w_mean, w_sd, 
           q_p50, q_p2.5, q_p97.5, q_mean, q_sd, 
           se_p50, se_p2.5, se_p97.5, se_mean, se_sd,
           m_p50, m_p2.5, m_p97.5, m_mean, m_sd,
           beta_p50, beta_p2.5, beta_p97.5, beta_mean, beta_sd))
}

dat.fin <- foreach(k=1:dim(res)[1], .combine=rbind) %do% genEsts(k)
colnames(dat.fin) <- c("phi0", "alpha", "w", "q", "se", 
                       "phi0_p50", "phi0_p2.5", "phi0_p97.5", "phi0_mean", "phi0_sd", 
                       "ar1_p50", "ar1_p2.5", "ar1_p97.5", "ar1_mean", "ar1_sd",
                       "w_p50", "w_p2.5", "w_p97.5", "w_mean", "w_sd", 
                       "q_p50", "q_p2.5", "q_p97.5", "q_mean", "q_sd", 
                       "se_p50", "se_p2.5", "se_p97.5", "se_mean", "se_sd",
                       "m_p50", "m_p2.5", "m_p97.5", "m_mean", "m_sd",
                       "beta_p50", "beta_p2.5", "beta_p97.5", "beta_mean", "beta_sd")

### Excel exportation
WriteXLS(as.data.frame(dat.fin), "../Results/sim_AR1.xls")
