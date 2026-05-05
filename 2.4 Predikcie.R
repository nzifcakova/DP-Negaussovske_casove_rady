#nacitanie potrebnych kniznic
library(datasets)
library(tscount)
library(spINAR)

#nacitanie a vykreslenie dat
data <- discoveries
#data vyjadruju pocet objavov za jednotlive roky pocas 100 rokov
plot(data, main = "Počet vynálezov a vedeckých objavov v jednotlivých rokoch", 
     xlab = "Čas v rokoch", ylab = "Počet")

mean(data) 

#rozdelenie dat na train a test
data_train <- window(data,start=1860,end=1949,frequency=1) # 90 dat
data_test <- window(data,start=1950,end=1959,frequency=1) # 10 dat


#odhad modelu
#ACP(1,1)
acp_model <- tsglm(data_train, model = list(past_obs = 1, past_mean = 1), 
                   link = "identity", distr = "poisson") 
####################################################################
#quantiles  
set.seed(123)
pred_quant_acp <- predict(acp_model, n.ahead = 10, level = 0.95, type = "quantiles")

plot(data, main = "Skutočné vs predikované hodnoty modelu ACP(1,1) - quantiles", 
     type='l', xlim = c(1860, 1960), xlab = "Čas v rokoch", ylab = "")
lines(1950:1959, pred_quant_acp$interval[,1] , col = "blue", type = "l")
lines(1950:1959, pred_quant_acp$interval[,2] , col = "blue", type = "l")
lines(1950:1959, pred_quant_acp$pred , col = "green", type = "l")
polygon(c(1950:1959, rev(1950:1959)), c(pred_quant_acp$interval[,1], rev(pred_quant_acp$interval[,2])),
        col = rgb(0,0,1,0.1), border = NA)
lines(1950:1959, data_test , col = "red", type = "l")
legend("topright", legend = c("Dáta 1860–1949", 
                              "Dáta 1950–1959",
                              "Predikovaná lambda",
                              "95% predikčný interval"),
       col = c("black", "red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________
#simulacia 1000 IS
set.seed(123)
N.sim <- 1000
n.ahead <- 10
diff_quant_acp <- matrix(NA, nrow = N.sim, ncol = n.ahead)

for (i in 1:N.sim){
  pred_quant_acp <- predict(acp_model, n.ahead = 10, level = 0.95, type = "quantiles")
  diff_quant_acp[i, ] <- pred_quant_acp$interval[,2] - pred_quant_acp$interval[,1]
}

# celkova priemerna sirka 
mean(diff_quant_acp)

###############
# #shortest
set.seed(123)
pred_shortest_acp <- predict(acp_model, n.ahead = 10, level = 0.95, type = "shortest")

plot(data, main = "Skutočné vs predikované hodnoty modelu ACP(1,1) - shortest", 
     type='l', xlim = c(1860, 1960), xlab = "Čas v rokoch", ylab = "")
lines(1950:1959, pred_shortest_acp$interval[,1] , col = "blue", type = "l")
lines(1950:1959, pred_shortest_acp$interval[,2] , col = "blue", type = "l")
lines(1950:1959, pred_shortest_acp$pred , col = "green", type = "l")
polygon(c(1950:1959, rev(1950:1959)), c(pred_shortest_acp$interval[,1], rev(pred_shortest_acp$interval[,2])),
        col = rgb(0,0,1,0.1), border = NA)
lines(1950:1959, data_test , col = "red", type = "l")
legend("topright", legend = c("Dáta 1860–1949", 
                              "Dáta 1950–1959",
                              "Predikovaná lambda",
                              "95% predikčný interval"),
       col = c("black", "red","green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________
#simulacia 1000 IS
set.seed(123)
N.sim <- 1000
n.ahead <- 10
diff_shortest_acp <- matrix(NA, nrow = N.sim, ncol = n.ahead)

for (i in 1:N.sim){
  pred_shortest_acp <- predict(acp_model, n.ahead = 10, level = 0.95, type = "shortest")
  diff_shortest_acp[i, ] <- pred_shortest_acp$interval[,2] - pred_shortest_acp$interval[,1]
}

# celkova priemerna sirka
mean(diff_shortest_acp) 

################################################################################
# PAR(1)

#odhad modelu
par_model <- tsglm(data_train, model = list(past_obs = 1), distr = "poisson", link = "identity") 

#quantiles
set.seed(12345)
pred_quant_par <- predict(par_model, n.ahead = 10, level = 0.95, type = "quantiles")

plot(data, main = "Skutočné vs predikované hodnoty modelu PAR(1) - quantiles", 
     type='l', xlim = c(1860, 1960), xlab = "Čas v rokoch", ylab = "")
lines(1950:1959, pred_quant_par$interval[,1] , col = "blue", type = "l")
lines(1950:1959, pred_quant_par$interval[,2] , col = "blue", type = "l")
lines(1950:1959, pred_quant_par$pred , col = "green", type = "l")
polygon(c(1950:1959, rev(1950:1959)), c(pred_quant_par$interval[,1], rev(pred_quant_par$interval[,2])),
        col = rgb(0,0,1,0.1), border = NA)
lines(1950:1959, data_test , col = "red", type = "l")
legend("topright", legend = c("Dáta 1860–1949", 
                              "Dáta 1950–1959", 
                              "Predikovaná lambda",
                              "95% predikčný interval"),
       col = c("black", "red", "green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________
#simulacia 1000 IS
set.seed(123)
N.sim <- 1000
n.ahead <- 10
diff_quant_par <- matrix(NA, nrow = N.sim, ncol = n.ahead)

for (i in 1:N.sim){
  pred_quant_par <- predict(par_model, n.ahead = 10, level = 0.95, type = "quantiles")
  diff_quant_par[i, ] <- pred_quant_par$interval[,2] - pred_quant_par$interval[,1]
}

# celkova priemerna sirka
mean(diff_quant_par) 

#################
#shortest
set.seed(12345)
pred_shortest_par <- predict(par_model, n.ahead = 10, level = 0.95, type = "shortest")

plot(data, main = "Skutočné vs predikované hodnoty modelu PAR(1) - shortest", 
     type='l', xlim = c(1860, 1960), xlab = "Čas v rokoch", ylab = "")
lines(1950:1959, pred_shortest_par$interval[,1] , col = "blue", type = "l")
lines(1950:1959, pred_shortest_par$interval[,2] , col = "blue", type = "l")
lines(1950:1959, pred_shortest_par$pred , col = "green", type = "l")
polygon(c(1950:1959, rev(1950:1959)), c(pred_shortest_par$interval[,1], rev(pred_shortest_par$interval[,2])),
        col = rgb(0,0,1,0.1), border = NA)
lines(1950:1959, data_test , col = "red", type = "l")

legend("topright", legend = c("Dáta 1860–1949", 
                              "Dáta 1950–1959", 
                              "Predikovaná lambda",
                              "95% predikčný interval"),
       col = c("black", "red", "green","blue"), lty = c(1, 1, 1), bty = "n")

#_________________
#simulacia 1000 IS
set.seed(123)
N.sim <- 1000
n.ahead <- 10
diff_shortest_par <- matrix(NA, nrow = N.sim, ncol = n.ahead)

for (i in 1:N.sim){
  pred_shortest_par <- predict(par_model, n.ahead = 10, level = 0.95, type = "shortest")
  diff_shortest_par[i, ] <- pred_shortest_par$interval[,2] - pred_shortest_par$interval[,1]
}

# celkova priemerna sirka
mean(diff_shortest_par) 

################################################################################
#INAR

#odhad modelu
model_inar <- spinar_est_param(data_train, p = 1, type = "ml", distr = "poi")

# ulozenie odhadnutych parametrov
alpha_hat <- model_inar["alpha1"]
lambda_hat <- model_inar["lambda"]

#simulacia realizacii predikcii
set.seed(123)
y_last <- data_train[length(data_train)]  # posledná pozorovaná hodnota bez 10 posledných
n.sim <- 1000
n.ahead <- 10
sim_mat_inar <- matrix(NA, nrow = n.sim, ncol = n.ahead)

for(i in 1:n.sim){
  X_predch <- y_last
  for(t in 1:n.ahead){
    thinning <- rbinom(1, X_predch, alpha_hat) #X_predch jednotiek môže prežiť
    eps <- rpois(1, lambda_hat)
    X_next <- thinning + eps
    sim_mat_inar[i, t] <- X_next
    X_prev <- X_next
  }
}

# Vypocet 95% predikcneho intervalu
lower_inar <- apply(sim_mat_inar, 2, quantile, probs = 0.025)
upper_inar <- apply(sim_mat_inar, 2, quantile, probs = 0.975)

#vykreslenie
plot(data, main = "Skutočné vs predikované hodnoty modelu INAR(1)", 
     type='l', xlim = c(1860, 1960), xlab = "Čas v rokoch", ylab = "")  
polygon(c(1950:1959, rev(1950:1959)), c(lower_inar, rev(upper_inar)),
        col = rgb(0,0,1,0.1), border = NA)
lines(1950:1959, lower_inar , col = "blue", type = "l")
lines(1950:1959, upper_inar , col = "blue", type = "l")
lines(1950:1959, ts(window(data, start = 1950, end = 1959)), col = "red", lty = 1)
legend("topright", legend = c("Dáta 1860–1949", 
                              "Dáta 1950–1959", 
                              "95% predikčný interval"),
       col = c("black", "red", "blue"), lty = c(1, 1, 1), bty = "n")


#_____________________________________________________________
# podmienena stredna hodnota: E[X_t | X_{t-1}] = alpha*X_{t-1} + lambda
# vypocet predikovanej strednej hodnoty 
n.ahead <- 10
X_hat <- numeric()

for(i in 1:n.ahead){
  if(i==1){
    X_hat[i] <- alpha_hat*data_train[90] + lambda_hat
  } 
  else {
    X_hat[i] <- alpha_hat*X_hat[i-1] + lambda_hat
  }
}
################################################################################
################################################################################

# POROVNANIE MODELOV na zaklade MAE - mean absolute error
ACP_MAE <- 1/length(data_test)*sum(abs(pred_quant_acp$pred - data_test))
PAR_MAE <- 1/length(data_test)*sum(abs(pred_quant_par$pred - data_test))
INAR_MAE <- 1/length(data_test)*sum(abs(X_hat - data_test))
