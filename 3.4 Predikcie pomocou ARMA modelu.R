#nacitanie potrebnych kniznic
library(BTSR)
library(astsa)
library(forecast)
library(urca)
library(car)

#inicializacia
set.seed(12) 
n.sim <- 1000
n.ahead <- 10
n <- 110 
eps <- 1e-6 
p <- 1
q <- 1
alpha <- 0
precision <- 20 
X <- numeric(n)
mu <- numeric(n)
mu[1] <- 0.5 # hodnota na naštartovanie
X[1] <- rbeta(1,mu[1]*precision, (1-mu[1])*precision)
phi <- c(0.3)
theta <- c(0.4)

pred_beta_arma <- matrix(NA, nrow = n.sim, ncol = n.ahead)   
x_skutocne <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_pred <- matrix(NA, nrow = n.sim, ncol = n.ahead)  
X_pred_arma <- matrix(NA, nrow = n.sim, ncol = n.ahead)
BetaARMA_lower <- matrix(NA, nrow = n.sim, ncol = n.ahead)
BetaARMA_upper <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_lower <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_upper <- matrix(NA, nrow = n.sim, ncol = n.ahead)

##################################################################

for(s in 1:n.sim){
  
  for(t in 2:n){
    
    mu_logit <- alpha + phi*logit(X[t-1]) + theta*(logit(X[t-1]) - logit(mu[t-1]))
    mu[t] <- 1/(1+exp(-mu_logit)) 
    X[t] <- rbeta(1,mu[t]*precision, (1-mu[t])*precision) # vygenerovanie nahod. realizacie y_t
    X[t] <- min(max(X[t], eps), 1 - eps) # odstranenie cistych jednotiek a nul
  } 
  
  #rozdelenie dat
  x_train <- X[1:100]
  x_test <- X[101:110]
  x_skutocne[s, ] <- x_test
  
  #odhad parametrov BetaARMA modelu
  BetaARMA_model <- btsr.fit(yt=x_train, p=1, q=1, link="logit", model = "BARMA")
  
  # predikcie BetaARMA modelu
  pred <- predict(BetaARMA_model, nnew = n.ahead)
  pred_beta_arma[s, ] <- pred$forecast[,1] #predikcie betaarma
  
  # vypocet 95% PI
  for(j in 1:n.ahead){
    
    BetaARMA_lower[s, j] <- qbeta(0.025, shape1 = pred$forecast[j,1]*BetaARMA_model$coefficients[4],
                                  shape2 = (1-pred$forecast[j,1])*BetaARMA_model$coefficients[4])
    BetaARMA_upper[s, j] <- qbeta(0.975, shape1 = pred$forecast[j,1]*BetaARMA_model$coefficients[4],
                                  shape2 = (1-pred$forecast[j,1])*BetaARMA_model$coefficients[4])
  }
  
  #_____________________________________
  # ARMA
  #zlogaritmovanie dat
  x_odhad_log <- log(x_train/(1-x_train))
  
  # #rozdelenie dat
  # arma_train <- y_odhad_log[1:100]
  # arma_test <- y_odhad_log[101:110]
  
  #odhad arma modelu-bez diferencii
  arma_model <- auto.arima(x_odhad_log, d=0, D=0)
  
  #predikcie sarima.for
  predikcie <- sarima.for(ts(x_odhad_log), n.ahead= n.ahead, p=arma_model$arma[1], d=arma_model$arma[6], q=arma_model$arma[2])
  arma_pred[s, ] <- predikcie$pred
  
  # 95% PI
  arma_lower[s, ] <- predikcie$pred - 1.96*predikcie$se
  arma_upper[s, ] <- predikcie$pred + 1.96*predikcie$se
  
  #odlogaritmovanie 
  X_pred_arma[s, ] <- exp(arma_pred[s, ])/(1+exp(arma_pred[s, ]))
  arma_lower[s, ] <- exp(arma_lower[s, ])/(1+exp(arma_lower[s, ]))  
  arma_upper[s, ] <- exp(arma_upper[s, ])/(1+exp(arma_upper[s, ]))
}

##################
# sirka intervalov
sirka_BetaARMA <- BetaARMA_upper - BetaARMA_lower
sirka_arma <- arma_upper - arma_lower

#priemerna sirka
mean_BetaARMA_sirka <- mean(sirka_BetaARMA)
mean_arma_sirka <- mean(sirka_arma) 

#presnost intervalov
# logicka matica: TRUE ak skutocna hodnota spadla do intervalu
pokrytie_BetaARMA <- (x_skutocne >= BetaARMA_lower) & (x_skutocne <= BetaARMA_upper)
pokrytie_arma <- (x_skutocne >= arma_lower) & (x_skutocne <= arma_upper)

# celkova presnost
celkova_presnost_BetaARMA <- mean(pokrytie_BetaARMA)
celkova_presnost_arma <- mean(pokrytie_arma)

#boxplot sirok intervalov
boxplot(list(BetaARMA = sirka_BetaARMA, ARMA = sirka_arma),
        col = c("red", "blue"),
        main = "Porovnanie šírok 95%-ných predikčných intervalov",
        ylab = "Šírka intervalu")
