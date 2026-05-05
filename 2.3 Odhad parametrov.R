#nacitanie potrebnych kniznic
library(tscount)
library(spINAR)

#######################
#ACP(1,1) -> lambda_t = omega + alpha*X_{t-1}+beta*lambda_{t-1}

#inicializacia
set.seed(123)
n_sim <- 2000 
n <- 200
omega_acp <- 1
alpha_acp <- 0.4
beta_acp <- 0.5

odhady_acp <- matrix(NA, nrow = n_sim, ncol = 3)
lambda_acp <- numeric(n)
X_acp <- numeric(n)


for (i in 1:n_sim) {
  #generovanie ACP modelu
  lambda_acp[1] <- omega_acp/(1 - alpha_acp - beta_acp) # vzorec pre strednu hodnotu
  X_acp[1] <- rpois(1, lambda_acp[1])
  
  for (t in 2:n) {
    lambda_acp[t] <- omega_acp + alpha_acp*X_acp[t-1] + beta_acp*lambda_acp[t-1]
    X_acp[t] <- rpois(1, lambda_acp[t])
  }
  
  #odhad parametrov
  odhady_acp[i, ] <- tsglm(X_acp, model = list(past_obs = 1, past_mean = 1),
        link = "identity", distr = "poisson")$coefficients
}

#ulozenie strednej hodnoty a variancie jednotlivych odhadnutych parametrov
#omega
mean_omega <- mean(odhady_acp[ ,1])
var_omega <- var(odhady_acp[, 1])
#alpha
mean_alpha <- mean(odhady_acp[ ,2])
var_alpha <- var(odhady_acp[, 2])
#beta
mean_beta <- mean(odhady_acp[ ,3])
var_beta <- var(odhady_acp[, 3])

#_________________________________
#omega
# interval spolahlivosti
sd_omega <- sd(odhady_acp[,1])
IS_omega <- c(mean_omega - 1.96*sd_omega, mean_omega + 1.96*sd_omega)

plot(density(odhady_acp[ ,1]), xlab = "", ylab = "", 
     main=expression("Distribúcia odhadnutého parametra " * hat(omega) * " pre model ACP(1,1)"))
abline(v=omega_acp, col="red", lwd=2)
abline(v=IS_omega, col="green", lwd=2, lty=5)
x <- seq(min(odhady_acp[,1]), max(odhady_acp[,1]), length=1000)
lines(x, dnorm(x, mean(odhady_acp[,1]), sd(odhady_acp[,1])), col="blue", lwd=2)
legend("topright", legend = c( "Jadrový odhad hustoty",
                               "Skutočná hodnota parametra",
                               "95% interval spoľahlivosti",
                               "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#alpha
#interval spolahlivosti
sd_alpha <- sd(odhady_acp[,2])
IS_alpha <- c(mean_alpha - 1.96*sd_alpha, mean_alpha + 1.96*sd_alpha)

plot(density(odhady_acp[ ,2]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(alpha) * " pre model ACP(1,1)"))
abline(v=alpha_acp, col="red", lwd=2)
abline(v=IS_alpha, col="green", lwd=2, lty=5)
x <- seq(min(odhady_acp[,2]), max(odhady_acp[,2]), length=1000)
lines(x, dnorm(x, mean(odhady_acp[,2]), sd(odhady_acp[,2])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#beta
#interval spolahlivosti
sd_beta <- sd(odhady_acp[,3])
IS_beta <- c(mean_beta - 1.96*sd_beta, mean_beta + 1.96*sd_beta)

plot(density(odhady_acp[ ,3]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(beta) * " pre model ACP(1,1)"))
abline(v=beta_acp, col="red", lwd=2)
abline(v=IS_beta, col="green", lwd=2, lty=5)
x <- seq(min(odhady_acp[,3]), max(odhady_acp[,3]), length=1000)
lines(x, dnorm(x, mean(odhady_acp[,3]), sd(odhady_acp[,3])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

################################################################################
################################################################################
#PAR(1) -> lambda_t = omega + alpha*X_{t-1}

#inicializacia
set.seed(123)
n_sim <- 2000
n <- 200
alpha_par <- 0.6
omega_par <- 1
odhady_par <- matrix(NA, nrow = n_sim, ncol = 2)
lambda_par <- numeric(n)
X_par <- numeric(n)

for (i in 1:n_sim) {
  #generovanie PAR modelu
  lambda_par[1] <- omega_par/(1 - alpha_par)
  X_par[1] <- rpois(1, lambda_par[1])
  
  for (t in 2:n) {
    lambda_par[t] <- omega_par + alpha_par*X_par[t-1]
    X_par[t] <- rpois(1, lambda_par[t])
  }
  
  #odhad parametrov
  odhady_par[i, ] <- tsglm(X_par, model = list(past_obs = 1),
                          link = "identity", distr = "poisson")$coefficients
}

#ulozenie strednej hodnoty a variancie jednotlivych odhadnutych parametrov
#omega
mean_omega <- mean(odhady_par[ ,1])
var_omega <- var(odhady_par[, 1])
#alpha
mean_alpha <- mean(odhady_par[ ,2])
var_alpha <- var(odhady_par[, 2])

#_________________________________
#omega
#interval spolahlivosti
sd_omega <- sd(odhady_par[,1])
IS_omega <- c(mean_omega - 1.96*sd_omega, mean_omega + 1.96*sd_omega)

plot(density(odhady_par[ ,1]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(omega) * " pre model PAR(1)"))
abline(v=omega_par, col="red", lwd=2)
abline(v=IS_omega, col="green", lwd=2, lty=5)
x <- seq(min(odhady_par[,1]), max(odhady_par[,1]), length=1000)
lines(x, dnorm(x, mean(odhady_par[,1]), sd(odhady_par[,1])), col="blue", lwd=2)
legend("topright", legend = c( "Jadrový odhad hustoty",
                               "Skutočná hodnota parametra",
                               "95% interval spoľahlivosti",
                               "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n", cex = 0.9)

#_________________________________
#alpha
#interval spolahlivosti
sd_alpha <- sd(odhady_par[,2])
IS_alpha <- c(mean_alpha - 1.96*sd_alpha, mean_alpha + 1.96*sd_alpha)

plot(density(odhady_par[ ,2]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(alpha) * " pre model PAR(1)"))
abline(v=alpha_par, col="red", lwd=2)
abline(v=IS_alpha, col="green", lwd=2, lty=5)
x <- seq(min(odhady_par[,2]), max(odhady_par[,2]), length=1000)
lines(x, dnorm(x, mean(odhady_par[,2]), sd(odhady_par[,2])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

################################################################################
# INAR(1): X_t = alpha o X_{t-1} + eps_t

#inicializacia
set.seed(123)
n_sim <- 2000
n <- 200
alpha_inar <- 0.6
lambda_inar <- 5

odhady_inar <- matrix(NA, nrow = n_sim, ncol = 2)
X_inar <- numeric(n)

for (i in 1:n_sim) {
  
  #generovanie modelu
  X_inar[1] <- rpois(1, lambda_inar)  #pociatocna hodnota
  
  for (t in 2:n) {
    thinning <- rbinom(1, X_inar[t-1], alpha_inar) 
    X_inar[t] <- thinning + rpois(1, lambda_inar)
  }
  
  #odhad parametrov
  odhady_inar[i, ] <- spinar_est_param(X_inar, p = 1, type = "ml", distr = "poi")
}

#ulozenie strednej hodnoty a variancie jednotlivych odhadnutych parametrov
#alpha
mean_alpha <- mean(odhady_inar[ ,1])
var_alpha <- var(odhady_inar[, 1])
#lambda
mean_lambda <- mean(odhady_inar[ ,2])
var_lambda <- var(odhady_inar[, 2])

#_________________________________
#alpha
#interval spolahlivosti
sd_alpha <- sd(odhady_inar[,1])
IS_alpha <- c(mean_alpha - 1.96*sd_alpha, mean_alpha + 1.96*sd_alpha)

plot(density(odhady_inar[ ,1]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(alpha) * " pre model INAR(1)"))
abline(v=alpha_inar, col="red", lwd=2)
abline(v=IS_alpha, col="green", lwd=2, lty=5)
x <- seq(min(odhady_inar[,1]), max(odhady_inar[,1]), length=1000)
lines(x, dnorm(x, mean(odhady_inar[,1]), sd(odhady_inar[,1])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#lambda
#predikcny interval
sd_lambda <- sd(odhady_inar[,2])
IS_lambda <- c(mean_lambda - 1.96*sd_lambda, mean_lambda + 1.96*sd_lambda)

plot(density(odhady_inar[ ,2]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(lambda) * " pre model INAR(1)"))
abline(v=lambda_inar, col="red", lwd=2)
abline(v=IS_lambda, col="green", lwd=2, lty=5)
x <- seq(min(odhady_inar[,2]), max(odhady_inar[,2]), length=1000)
lines(x, dnorm(x, mean(odhady_inar[,2]), sd(odhady_inar[,2])), col="blue", lwd=2)
legend("topright",legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

