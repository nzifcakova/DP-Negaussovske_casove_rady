#nacitanie potrebnych kniznic
library(BTSR)
library(car)

#inicializacia
set.seed(1) 
n_sim <- 10000
n <- 100
eps <- 1e-6 
p <- 1
q <- 1
alpha <- 0
precision <- 20 
X <- numeric(n)
mu <- numeric(n)
mu[1] <- 0.5 # hodnota na nastartovanie procesu
X[1] <- rbeta(1,mu[1]*precision, (1-mu[1])*precision)
phi <- c(0.3)
theta <- c(0.4)

odhady <- matrix(NA, nrow = n_sim, ncol = 4) #4 odhadnute parametre: alpha,phi,theta,nu(precision)
X_matrix <- matrix(NA, nrow = n_sim, ncol = n)

##################################################################

for(s in 1:n_sim){
  
  for(t in 2:n){
    mu_logit <- alpha + phi*logit(X[t-1]) + theta*(logit(X[t-1]) - logit(mu[t-1]))
    mu[t] <- 1/(1+exp(-mu_logit)) 
    X[t] <- rbeta(1,mu[t]*precision, (1-mu[t])*precision) # vygenerovanie nahod. realizacie y_t
    X[t] <- min(max(X[t], eps), 1 - eps)
  } 
  
  X_matrix[s,] <- X
  # odhad parametrov
  model <- btsr.fit(yt=X, p=1, q=1, link="logit", model = "BARMA")
  est <- model$coefficients
  odhady[s, ] <- est
}

#ulozenie strednej hodnoty a variancie jednotlivych odhadnutych parametrov
#alpha
mean_alpha <- mean(odhady[ ,1])
var_alpha <- var(odhady[, 1])
#phi
mean_phi <- mean(odhady[ ,2])
var_phi <- var(odhady[, 2])
#theta
mean_theta <- mean(odhady[ ,3])
var_theta <- var(odhady[, 3])
#nu
mean_nu <- mean(odhady[ ,4])
var_nu <- var(odhady[, 4])

#_________________________________
#alpha
#interval spolahlivosti
sd_alpha <- sd(odhady[,1])
IS_alpha <- c(mean_alpha - 1.96*sd_alpha, mean_alpha + 1.96*sd_alpha)

plot(density(odhady[ ,1]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(alpha) * " pre model BetaARMA(1,1)"))
abline(v=alpha, col="red", lwd=2)
abline(v=IS_alpha, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,1]), max(odhady[,1]), length=1000)
lines(x, dnorm(x, mean(odhady[,1]), sd(odhady[,1])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#phi
#interval spolahlivosti
sd_phi <- sd(odhady[,2])
IS_phi <- c(mean_phi - 1.96*sd_phi, mean_phi + 1.96*sd_phi)

plot(density(odhady[ ,2]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(phi) * " pre model BetaARMA(1,1)"))
abline(v=phi, col="red", lwd=2)
abline(v=IS_phi, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,2]), max(odhady[,2]), length=1000)
lines(x, dnorm(x, mean(odhady[,2]), sd(odhady[,2])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#theta
#interval spolahlivosti
sd_theta <- sd(odhady[,3])
IS_theta <- c(mean_theta - 1.96*sd_theta, mean_theta + 1.96*sd_theta)

plot(density(odhady[ ,3]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(theta) * " pre model BetaARMA(1,1)"))
abline(v=theta, col="red", lwd=2)
abline(v=IS_theta, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,3]), max(odhady[,3]), length=1000)
lines(x, dnorm(x, mean(odhady[,3]), sd(odhady[,3])), col="blue", lwd=2)
legend("topleft", legend = c( "Jadrový odhad hustoty",
                              "Skutočná hodnota parametra",
                              "95% interval spoľahlivosti",
                              "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________________________
#nu
#interval spolahlivosti
sd_nu <- sd(odhady[,4])
IS_nu <- c(mean_nu - 1.96*sd_nu, mean_nu + 1.96*sd_nu)

plot(density(odhady[ ,4]), xlab = "", ylab = "",
     main=expression("Distribúcia odhadnutého parametra " * hat(nu) * " v Beta rozdelení pre model BetaARMA(1,1)"))
abline(v=precision, col="red", lwd=2)
abline(v=IS_nu, col="green", lwd=2, lty=5)
x <- seq(min(odhady[,4]), max(odhady[,4]), length=1000)
lines(x, dnorm(x, mean(odhady[,4]), sd(odhady[,4])), col="blue", lwd=2)
legend("topright", legend = c( "Jadrový odhad hustoty",
                               "Skutočná hodnota parametra",
                               "95% interval spoľahlivosti",
                               "Normálna hustota"),
       col = c("black","red","green","blue"), lty = c(1, 1, 1), bty = "n")
