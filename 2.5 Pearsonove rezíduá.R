#nacitanie potrebynch kniznic
library(datasets)
library(tscount)
library(spINAR)
library(astsa)

#nacitanie dat
data <- discoveries

################################################################################
#ACP(1,1)
acp_model <- tsglm(data, model = list(past_obs = 1, past_mean = 1), 
                   link = "identity", distr = "poiss")  

#pearsonove rezidua
pearson_rez_acp <- residuals(acp_model, type="pearson")
# vypocet priemeru a rozptylu
mean(pearson_rez_acp) 
var(pearson_rez_acp) 

acf2(pearson_rez_acp, main = "ACF a PACF pre Pearsonove rezíduá v modeli ACP(1,1)")

################################################################################
#PAR
par_model <- tsglm(data, model = list(past_obs = 1), distr = "poisson", 
                   link = "identity") 

#pearsonove rezidua
pearson_rez_par <- residuals(par_model, type="pearson")

mean(pearson_rez_par)
var(pearson_rez_par)

acf2(pearson_rez_par, main = "ACF a PACF pre Pearsonove rezíduá v modeli PAR(1)")

################################################################################
#INAR
model_inar <- spinar_est_param(data, p = 1, type = "ml", distr = "poi")

# ulozenie odhadov
alpha_hat <- model_inar[1]
lambda_hat <- model_inar[2] # poisson e_t -> E[e_t]=Var[e_t]=lambda

#pearsonove rezidua - vypocet rucne
n <- length(data)
pearson_rez_inar <- numeric(n)

for (t in 2:n) {
  mu_t <- alpha_hat*data[t-1] + lambda_hat #podmienena stredna hodnota
  var_t <- alpha_hat*(1 - alpha_hat)*data[t-1] + lambda_hat #podmieneny rozptyl
  pearson_rez_inar[t] <- (data[t] - mu_t)/sqrt(var_t) 
}

# bez prvej hodnoty pearson_rez_inar, ktora je 0, lebo k nej nemame 
# predchadzajucu hodnotu, kedze ideme od t=2, preto ju z vypoctu mean a var vynechame
mean(pearson_rez_inar[2:length(pearson_rez_inar)])
var(pearson_rez_inar[2:length(pearson_rez_inar)])

acf2(pearson_rez_inar[2:length(pearson_rez_inar)], main = "ACF a PACF pre Pearsonove rezíduá v modeli INAR(1)")
