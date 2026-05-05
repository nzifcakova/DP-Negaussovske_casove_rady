# nacitanie potrebnych kniznic
library(dplyr)
library(gets)

#inicializacia
set.seed(123)
n_sim <- 10000
n <- 202
X <- numeric(n)
# inicializacia prvych 2 hodnot - nami dane
X[1:2] <- c(1,0)
beta0 <- 0
beta1 <- 0.6
beta2 <- 0.3
p <- numeric(n)
p_logit <- numeric(n)
odhady <- matrix(NA, nrow = n_sim, ncol = 2) 
X_mat <- matrix(NA, nrow = n_sim, ncol = n)

#_________________________________

for (i in 1:n_sim ) {
  
  # generovanie procesu
  for (t in 3:n) {
    p_logit[t-2] <- beta0 + beta1*X[t-1] + beta2*X[t-2] 
    p[t-2] <- 1/(1+exp(-p_logit[t-2]))
    # generovanie y
    X[t] <- rbinom(1,1,p[t-2])
  }
  
  lag1 <- lag(X, n=1)
  lag2 <- lag(X, n=2)
  data <- data.frame(X,lag1,lag2)
  data <- na.omit(data) # odstranenie X_1 a X_2 kvoli chybajucim lagom
  
  X_mat[i,] <- X 
  
  # odhad parametrov
  model <- glm(X ~ 0 + lag1 + lag2, family = binomial, data = data)
  est <- model$coeff
  odhady[i,] <- est
}

#ulozenie strednej hodnoty a variancie jednotlivych odhadnutych parametrov
#__________________________________
#beta1
mean_beta1 <- mean(odhady[ ,1])
var_beta1 <- var(odhady[,1])
#beta2
mean_beta2 <- mean(odhady[ ,2])
var_beta2 <- var(odhady[,2])

#_________________________________
# beta1
# interval spolahlivosti
sd_beta1 <- sd(odhady[,1])
IS_beta1 <- c(mean_beta1 - 1.96*sd_beta1, mean_beta1 + 1.96*sd_beta1)

plot(density(odhady[ ,1]), xlab = "", ylab = "",
     main=expression(paste("Distribúcia odhadnutého parametra ", hat(beta)[1], " pre model BAR(2)")))
abline(v=beta1, col="red", lwd=2)
abline(v=IS_beta1, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,1]), max(odhady[,1]), length=1000)
lines(x, dnorm(x, mean(odhady[,1]), sd(odhady[,1])), col="blue", lwd=2)
legend("topright", legend = c( "Jadrový odhad hustoty",
                               "Skutočná hodnota parametra",
                               "95% interval spoľahlivosti",
                               "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
# beta2
# interval spolahlivosti
sd_beta2 <- sd(odhady[,2])
IS_beta2 <- c(mean_beta2 - 1.96*sd_beta2, mean_beta2 + 1.96*sd_beta2)

plot(density(odhady[ ,2]), xlab = "", ylab = "",
     main=expression(paste("Distribúcia odhadnutého parametra ", hat(beta)[2], " pre model BAR(2)")))
abline(v=beta2, col="red", lwd=2)
abline(v=IS_beta2, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,2]), max(odhady[,2]), length=1000)
lines(x, dnorm(x, mean(odhady[,2]), sd(odhady[,2])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

