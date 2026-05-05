#nacitanie potrebynch kniznic
library(BTSR)
library(astsa)

#nacitanie dat - treba cestu, kde je subor ulozeny na nacitanie .csv suboru
dataset <- read.csv("electricity_production.csv", sep = ";", header = TRUE)

# prevod na [0,1]
data <- dataset$value/100 
year <- dataset$year

# vykreslenie
plot(year, data, type = "l", main = "Slovensko - výroba elektriny z ropy, plynu a uhlia", ylab = "Hodnoty vyjadrené %", xlab = "Rok", xaxt = "n")
axis(1, at = year, labels = year, xlab = "rok")

# rozdelenie na train a test
new <- 3 # pocet predikcii
train <- data[1:(length(data)-new)]
test <- data[(length(data)-new+1):length(data)]

# pridanie casu kvoli trendu
t <- (1:length(data))/length(data)

# beta <-> xreg
# alpha <-> alpha
# p <-> phi
# q <-> theta

#odhady modelov
model1 <- btsr.fit(yt=train, p=1, q=0, link="logit", model = "BARMA", xreg = t[1:length(train)])

model2 <- btsr.fit(yt=train, p=2, q=0, link="logit", model = "BARMA", xreg = t[1:length(train)])

model3 <- btsr.fit(yt=train, p=0, q=1, link="logit", model = "BARMA", xreg = t[1:length(train)])

model4 <- btsr.fit(yt=train, p=0, q=2, link="logit", model = "BARMA", xreg = t[1:length(train)])

model5 <- btsr.fit(yt=train, p=1, q=1, link="logit", model = "BARMA", xreg = t[1:length(train)])

model6 <- btsr.fit(yt=train, p=2, q=1, link="logit", model = "BARMA", xreg = t[1:length(train)])

model7 <- btsr.fit(yt=train, p=1, q=2, link="logit", model = "BARMA", xreg = t[1:length(train)])

model8 <- btsr.fit(yt=train, p=2, q=2, link="logit", model = "BARMA", xreg = t[1:length(train)])

#rezidua
acf2(model1$residual) 
Box.test(model1$residual, lag = 8, type = "Ljung-Box", fitdf= 1)
acf2(model2$residual)
acf2(model3$residual) 
Box.test(model3$residual, lag = 8, type = "Ljung-Box", fitdf= 1)
acf2(model4$residual) 
Box.test(model4$residual, lag = 10, type = "Ljung-Box", fitdf= 2)
acf2(model5$residual)
acf2(model6$residual) 
acf2(model7$residual) 
Box.test(model4$residual, lag = 10, type = "Ljung-Box", fitdf= 3)
acf2(model8$residual) 

# BIC - okrem model3
summary(model1)$bic 
summary(model2)$bic 
summary(model4)$bic 
summary(model5)$bic
summary(model6)$bic 
summary(model7)$bic
summary(model8)$bic

#########################################
# nove hodnoty casu pre predikcie
xreg_new <- t[(length(train)+1):length(data)]
# vypocet predickii
pred <- predict(model4, nnew=new, newdata = xreg_new)

# vykreslenie
plot(data, type = "l", ylim = c(0.1,0.6), main = "Slovensko - výroba elektriny z ropy, plynu a uhlia", 
     xaxt = "n", ylab = "Hodnoty vyjadrené v %", xlab = "Rok", lwd = 1)
axis(1, at = 1:length(year), labels = year)
lines((length(data)-new+1):length(data), pred$forecast[,1], col = "red", lwd = 2)
#lines(1:31,pred$fitted.values, col="blue")
legend("topright", legend = c( "Skutočné dáta od 1990 po 2023",
                               "Odhadnutá stredná hodnota pre roky 2021-2023"),
       #"Modelom odhadnuté hodnoty pre roky 1990-2019"),
       col = c("black","red","blue"), lty = c(1, 1, 1), bty = "n")
