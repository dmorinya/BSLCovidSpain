### Summarise simulation study results
library(gdata)
ar1    <- read.xls("Results/sim_AR1.xls")
ma1    <- read.xls("Results/sim_MA1.xls")
arma11 <- read.xls("Results/sim_ARMA11.xls")
ar1$m <- 5
ar1$beta <- 0.4
ma1$m <- 5
ma1$beta <- 0.4
arma11$m <- 5
arma11$beta <- 0.4

### AR(1)
### Bias
ar1$phi0_bias  <- ar1$phi0-ar1$phi0_mean
ar1$alpha_bias <- ar1$alpha-ar1$ar1_mean
ar1$q_bias     <- ar1$q-ar1$q_mean
ar1$w_bias     <- ar1$w-ar1$w_mean
ar1$m_bias     <- ar1$m-ar1$m_mean
ar1$beta_bias  <- ar1$beta-ar1$beta_mean
ar1$se_bias    <- ar1$se-ar1$se_mean

round(mean(ar1$phi0_bias), 3)
round(mean(ar1$alpha_bias), 3)
round(mean(ar1$q_bias), 3)
round(mean(ar1$w_bias), 3)
round(mean(ar1$m_bias), 3)
round(mean(ar1$beta_bias), 3)
round(mean(ar1$se_bias), 3)

### AIL
ar1$phi0_AIL  <- ar1$phi0_p97.5-ar1$phi0_p2.5
ar1$alpha_AIL <- ar1$ar1_p97.5-ar1$ar1_p2.5
ar1$q_AIL     <- ar1$q_p97.5-ar1$q_p2.5
ar1$w_AIL     <- ar1$w_p97.5-ar1$w_p2.5
ar1$m_AIL     <- ar1$m_p97.5-ar1$m_p2.5
ar1$beta_AIL  <- ar1$beta_p97.5-ar1$beta_p2.5
ar1$se_AIL    <- ar1$se_p97.5-ar1$se_p2.5

round(mean(ar1$phi0_AIL, na.rm=T), 3)
round(mean(ar1$alpha_AIL, na.rm=T), 3)
round(mean(ar1$q_AIL, na.rm=T), 3)
round(mean(ar1$w_AIL, na.rm=T), 3)
round(mean(ar1$m_AIL, na.rm=T), 3)
round(mean(ar1$beta_AIL, na.rm=T), 3)
round(mean(ar1$se_AIL, na.rm=T), 3)

### Coverage
ar1$phi0_coverage  <- ifelse(ar1$phi0_p2.5<=ar1$phi0 & ar1$phi0_p97.5>=ar1$phi0, 1, 0)
ar1$alpha_coverage <- ifelse(ar1$ar1_p2.5<=ar1$alpha & ar1$ar1_p97.5>=ar1$alpha, 1, 0)
ar1$q_coverage     <- ifelse(ar1$q_p2.5<=ar1$q & ar1$q_p97.5>=ar1$q, 1, 0)
ar1$w_coverage     <- ifelse(ar1$w_p2.5<=ar1$w & ar1$w_p97.5>=ar1$w, 1, 0)
ar1$m_coverage     <- ifelse(ar1$m_p2.5<=ar1$m & ar1$m_p97.5>=ar1$m, 1, 0)
ar1$beta_coverage  <- ifelse(ar1$beta_p2.5<=ar1$beta & ar1$beta_p97.5>=ar1$beta, 1, 0)
ar1$sigma_coverage <- ifelse(ar1$se_p2.5<=ar1$se & ar1$se_p97.5>=ar1$se, 1, 0)
round(table(ar1$phi0_coverage)[2]/length(ar1$phi0_coverage)*100, 2)
round(table(ar1$alpha_coverage)[2]/length(ar1$alpha_coverage)*100, 2)
round(table(ar1$q_coverage)[2]/length(ar1$q_coverage)*100, 2)
round(table(ar1$w_coverage)[2]/length(ar1$w_coverage)*100, 2)
round(table(ar1$m_coverage)[2]/length(ar1$m_coverage)*100, 2)
round(table(ar1$beta_coverage)[2]/length(ar1$beta_coverage)*100, 2)
round(table(ar1$sigma_coverage)[2]/length(ar1$sigma_coverage)*100, 2)

### MA(1)
### Bias
ma1$phi0_bias  <- ma1$phi0-ma1$phi0_mean
ma1$theta_bias <- ma1$theta-ma1$ma1_mean
ma1$q_bias     <- ma1$q-ma1$q_mean
ma1$w_bias     <- ma1$w-ma1$w_mean
ma1$m_bias     <- ma1$m-ma1$m_mean
ma1$beta_bias  <- ma1$beta-ma1$beta_mean
ma1$se_bias    <- ma1$se-ma1$se_mean

round(mean(ma1$phi0_bias), 3)
round(mean(ma1$theta_bias), 3)
round(mean(ma1$q_bias), 3)
round(mean(ma1$w_bias), 3)
round(mean(ma1$m_bias), 3)
round(mean(ma1$beta_bias), 3)
round(mean(ma1$se_bias), 3)

### AIL
ma1$phi0_AIL  <- ma1$phi0_p97.5-ma1$phi0_p2.5
ma1$theta_AIL <- ma1$ma1_p97.5-ma1$ma1_p2.5
ma1$q_AIL     <- ma1$q_p97.5-ma1$q_p2.5
ma1$w_AIL     <- ma1$w_p97.5-ma1$w_p2.5
ma1$m_AIL     <- ma1$m_p97.5-ma1$m_p2.5
ma1$beta_AIL  <- ma1$beta_p97.5-ma1$beta_p2.5
ma1$se_AIL    <- ma1$se_p97.5-ma1$se_p2.5

round(mean(ma1$phi0_AIL, na.rm=T), 3)
round(mean(ma1$theta_AIL, na.rm=T), 3)
round(mean(ma1$q_AIL, na.rm=T), 3)
round(mean(ma1$w_AIL, na.rm=T), 3)
round(mean(ma1$m_AIL, na.rm=T), 3)
round(mean(ma1$beta_AIL, na.rm=T), 3)
round(mean(ma1$se_AIL, na.rm=T), 3)

### Coverage
ma1$phi0_coverage  <- ifelse(ma1$phi0_p2.5<=ma1$phi0 & ma1$phi0_p97.5>=ma1$phi0, 1, 0)
ma1$theta_coverage <- ifelse(ma1$ma1_p2.5<=ma1$theta & ma1$ma1_p97.5>=ma1$theta, 1, 0)
ma1$q_coverage     <- ifelse(ma1$q_p2.5<=ma1$q & ma1$q_p97.5>=ma1$q, 1, 0)
ma1$w_coverage     <- ifelse(ma1$w_p2.5<=ma1$w & ma1$w_p97.5>=ma1$w, 1, 0)
ma1$m_coverage     <- ifelse(ma1$m_p2.5<=ma1$m & ma1$m_p97.5>=ma1$m, 1, 0)
ma1$beta_coverage  <- ifelse(ma1$beta_p2.5<=ma1$beta & ma1$beta_p97.5>=ma1$beta, 1, 0)
ma1$sigma_coverage <- ifelse(ma1$se_p2.5<=ma1$se & ma1$se_p97.5>=ma1$se, 1, 0)
round(table(ma1$phi0_coverage)[2]/length(ma1$phi0_coverage)*100, 2)
round(table(ma1$theta_coverage)[2]/length(ma1$theta_coverage)*100, 2)
round(table(ma1$q_coverage)[2]/length(ma1$q_coverage)*100, 2)
round(table(ma1$w_coverage)[2]/length(ma1$w_coverage)*100, 2)
round(table(ma1$m_coverage)[2]/length(ma1$m_coverage)*100, 2)
round(table(ma1$beta_coverage)[2]/length(ma1$beta_coverage)*100, 2)
round(table(ma1$sigma_coverage)[2]/length(ma1$sigma_coverage)*100, 2)

### ARMA(1, 1)
### Bias
arma11$phi0_bias  <- arma11$phi0-arma11$phi0_mean
arma11$alpha_bias <- arma11$alpha-arma11$ar1_mean
arma11$theta_bias <- arma11$theta-arma11$ma1_mean
arma11$q_bias     <- arma11$q-arma11$q_mean
arma11$w_bias     <- arma11$w-arma11$w_mean
arma11$m_bias     <- arma11$m-arma11$m_mean
arma11$beta_bias  <- arma11$beta-arma11$beta_mean
arma11$se_bias    <- arma11$se-arma11$se_mean

round(mean(arma11$phi0_bias), 3)
round(mean(arma11$alpha_bias), 3)
round(mean(arma11$theta_bias), 3)
round(mean(arma11$q_bias), 3)
round(mean(arma11$w_bias), 3)
round(mean(arma11$m_bias), 3)
round(mean(arma11$beta_bias), 3)
round(mean(arma11$se_bias), 3)

### AIL
arma11$phi0_AIL  <- arma11$phi0_p97.5-arma11$phi0_p2.5
arma11$alpha_AIL <- arma11$ar1_p97.5-arma11$ar1_p2.5
arma11$theta_AIL <- arma11$ma1_p97.5-arma11$ma1_p2.5
arma11$q_AIL     <- arma11$q_p97.5-arma11$q_p2.5
arma11$w_AIL     <- arma11$w_p97.5-arma11$w_p2.5
arma11$m_AIL     <- arma11$m_p97.5-arma11$m_p2.5
arma11$beta_AIL  <- arma11$beta_p97.5-arma11$beta_p2.5
arma11$se_AIL    <- arma11$se_p97.5-arma11$se_p2.5

round(mean(arma11$phi0_AIL, na.rm=T), 3)
round(mean(arma11$alpha_AIL, na.rm=T), 3)
round(mean(arma11$theta_AIL, na.rm=T), 3)
round(mean(arma11$q_AIL, na.rm=T), 3)
round(mean(arma11$w_AIL, na.rm=T), 3)
round(mean(arma11$m_AIL, na.rm=T), 3)
round(mean(arma11$beta_AIL, na.rm=T), 3)
round(mean(arma11$se_AIL, na.rm=T), 3)

### Coverage
arma11$phi0_coverage  <- ifelse(arma11$phi0_p2.5<=arma11$phi0 & arma11$phi0_p97.5>=arma11$phi0, 1, 0)
arma11$alpha_coverage <- ifelse(arma11$ar1_p2.5<=arma11$alpha & arma11$ar1_p97.5>=arma11$alpha, 1, 0)
arma11$theta_coverage <- ifelse(arma11$ma1_p2.5<=arma11$theta & arma11$ma1_p97.5>=arma11$theta, 1, 0)
arma11$q_coverage     <- ifelse(arma11$q_p2.5<=arma11$q & arma11$q_p97.5>=arma11$q, 1, 0)
arma11$w_coverage     <- ifelse(arma11$w_p2.5<=arma11$w & arma11$w_p97.5>=arma11$w, 1, 0)
arma11$m_coverage     <- ifelse(arma11$m_p2.5<=arma11$m & arma11$m_p97.5>=arma11$m, 1, 0)
arma11$beta_coverage  <- ifelse(arma11$beta_p2.5<=arma11$beta & arma11$beta_p97.5>=arma11$beta, 1, 0)
arma11$sigma_coverage <- ifelse(arma11$se_p2.5<=arma11$se & arma11$se_p97.5>=arma11$se, 1, 0)
round(table(arma11$phi0_coverage)[2]/length(arma11$phi0_coverage)*100, 2)
round(table(arma11$alpha_coverage)[2]/length(arma11$alpha_coverage)*100, 2)
round(table(arma11$theta_coverage)[2]/length(arma11$theta_coverage)*100, 2)
round(table(arma11$q_coverage)[2]/length(arma11$q_coverage)*100, 2)
round(table(arma11$w_coverage)[2]/length(arma11$w_coverage)*100, 2)
round(table(arma11$m_coverage)[2]/length(arma11$m_coverage)*100, 2)
round(table(arma11$beta_coverage)[2]/length(arma11$beta_coverage)*100, 2)
round(table(arma11$sigma_coverage)[2]/length(arma11$sigma_coverage)*100, 2)
