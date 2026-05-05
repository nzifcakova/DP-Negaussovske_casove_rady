#nacitanie potrebnych kniznic
library(astsa)
library(forecast)
library(urca)
library(tscount)

#inicializacia
set.seed(1)
n.sim <- 1000
n.ahead <- 10
n <- 110
omega <- 1.5
alpha <- 0.5
beta <- 0.3

x_skutocne <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_pred <- matrix(NA, nrow = n.sim, ncol = n.ahead)  
x_pred_arma <- matrix(NA, nrow = n.sim, ncol = n.ahead)
tsglm_pred <- matrix(NA, nrow = n.sim, ncol = n.ahead)
lambda <- numeric(n)
X_acp <- numeric(n)

tsglm_lower <- matrix(NA, nrow = n.sim, ncol = n.ahead)
tsglm_upper <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_lower <- matrix(NA, nrow = n.sim, ncol = n.ahead)
arma_upper <- matrix(NA, nrow = n.sim, ncol = n.ahead)


for(i in 1:n.sim){
  
  #generovanie acp(1,1) modelu
  lambda[1] <- omega/(1-alpha-beta) 
  X_acp[1] <- rpois(1,lambda[1]) 
  
  for (t in 2:n) {
    lambda[t] <- omega + alpha*X_acp[t-1] + beta*lambda[t-1]
    X_acp[t] <- rpois(1,lambda[t])
  }
  
  #rozdelenie dat
  x_train <- X_acp[1:100] 
  x_test <- X_acp[101:110]
  x_skutocne[i, ] <- x_test
  
  #odhad parametrov
  acp_model <- tsglm(x_train, model = list(past_obs = 1, past_mean = 1), 
                     link = "identity", distr = "poisson")
  
  # vypocet predikcii
  pred_acp <- predict(acp_model, n.ahead = n.ahead, level = 0.95, type = "quantiles")
  tsglm_pred[i, ] <- pred_acp$pred
  
  # 95% PI
  tsglm_lower[i, ] <- pred_acp$interval[,1]
  tsglm_upper[i, ] <- pred_acp$interval[,2]
  
  #_____________________________________
  #zlogaritmovanie dat
  x_train_log <- ts(log(x_train + 1))
  
  #odhad arma modelu-bez diferencii
  arma_model <- auto.arima(x_train_log, d=0, D=0)
  
  #predikcie sarima.for
  predikcie <- sarima.for(x_train_log, n.ahead= 10, p=arma_model$arma[1], 
                          d=arma_model$arma[6], q=arma_model$arma[2])
  arma_pred[i, ] <- predikcie$pred
  
  # 95% PI
  arma_lower[i, ] <- predikcie$pred - 1.96*predikcie$se
  arma_upper[i, ] <- predikcie$pred + 1.96*predikcie$se
  
  #odlogaritmovanie 
  x_pred_arma[i, ] <- exp(arma_pred[i, ]) - 1
  arma_lower[i, ] <- exp(arma_lower[i, ]) - 1
  arma_upper[i, ] <- exp(arma_upper[i, ]) - 1
}

##################
# sirka intervalov
sirka_tsglm <- tsglm_upper - tsglm_lower
sirka_arma <- arma_upper - arma_lower

#priemerna sirka
mean_tsglm_sirka <- mean(sirka_tsglm)
mean_arma_sirka <- mean(sirka_arma)  

#presnost intervalov
# logicka matica: TRUE ak skutocna hodnota spadla do intervalu
pokrytie_tsglm <- (x_skutocne >= tsglm_lower) & (x_skutocne <= tsglm_upper)
pokrytie_arma <- (x_skutocne >= arma_lower) & (x_skutocne <= arma_upper)

# celkova presnost
celkova_presnost_tsglm <- mean(pokrytie_tsglm)
celkova_presnost_arma <- mean(pokrytie_arma)

#boxplot sirok intervalov
boxplot(list(TSGLM = sirka_tsglm, ARMA = sirka_arma),
        col = c("red", "blue"),
        main = "Porovnanie šírok 95%-ných predikčných intervalov",
        ylab = "Šírka intervalu")
