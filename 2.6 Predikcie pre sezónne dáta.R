#nacitanie potrebynch kniznic
library(tscount)
library(astsa)
library(tsbox) # prevod z ts na df 

#nacitanie dat
data <- ts_df(ldeaths)

# vykreslenie
plot(data ,type="l",main = "Mesačné počty úmrtí na bronchitídu, emfyzém a astmu\nv Spojenom kráľovstve pre obe pohlavia, 1974 – 1979", xlab = "rok", ylab="počet")

#vytiahnutie mesiaca z datumu a prevod na cislo
data$month <- as.numeric(format(data$time, "%m")) 
# vypocet kovariatov
data$cos <- cos(2*pi*data$month/12)
data$sin <- sin(2*pi*data$month/12)
#ulozenie unikatnych rokov
year <- as.numeric(unique(format(data$time, "%Y")))

# 6 predikcii 
n.ahead <- 6
#skratene data - bez posledných 6 pozorovaní
data_train_all <- data[1:(nrow(data)-n.ahead),]
data_train <- data_train_all[,2]
# kovariaty 
covariates_train <- data_train_all[,4:5]

# predikcie na 6 pozorovani dopredu
data_test_all <- data[(nrow(data)-n.ahead+1):nrow(data),]
data_test <- data_test_all[,2]
# kovariaty 
covariates_test <- data_test_all[,4:5]

#odhady modelov
#_______________________________________________________________________________
model1 <- tsglm(data_train, model = list(past_obs = 1),  
                link = "log", distr = "poisson", xreg = covariates_train)
summary(model1)
acf2(residuals(model1)) 

#_____________________________
model2 <- tsglm(data_train, model = list(past_obs = 1, past_mean = 1), 
                link = "log", distr = "poisson", xreg = covariates_train)
summary(model2)
acf2(residuals(model2))

#_____________________________
model3 <- tsglm(data_train, model = list(past_obs = c(1,12) , past_mean = c(1,12)),
                link = "log", distr = "poisson", xreg = covariates_train)
summary(model3) 
acf2(residuals(model3)) 

#_____________________________
model4 <- tsglm(data_train, model = list(past_obs = c(6,12), past_mean = c(6,12)), 
                link = "log", distr = "poisson", xreg = covariates_train)
summary(model4)  
acf2(residuals(model4))

#________________________
#porovnanie modelov 
summary(model1)$BIC
summary(model2)$BIC
summary(model3)$BIC
summary(model4)$BIC

# vypocet predikcii
predikcie <- predict(model4, n.ahead = n.ahead, newxreg = covariates_test)

#vykreslenie
plot(data[,2], main = "Skutočné vs predikované hodnoty modelu ACP(1,1)", 
     type='l', xlab = "Čas v rokoch", ylab = "", xaxt="n")
lines((nrow(data)-n.ahead+1):nrow(data), predikcie$pred, col = "red", type = "l")
axis(1, at=seq(1, nrow(data), by=12), labels=year)
legend("topright", legend = c("Skutočné dáta", 
                              "Predikovaná lambda"),
       col = c("black","red"), lty = c(1, 1, 1), bty = "n")
