#nacitanie potrebnych kniznic
library(stats)
library(dplyr)

#nacitanie dat - treba cestu, kde je subor ulozeny na nacitanie .csv suboru
dataset <- read.csv("BA_jan_2026.csv", sep = ";", header = TRUE)

#ulozenie datumov
datum <- substr(dataset[, 1],1,10)
# pridanie 1,ak prsalo, inak 0
dataset$precipiation_binary <- as.numeric(dataset$precipitation > 0) 

#vykreslenie čr
plot(dataset[, 6], type = "l", main = "Graf výskytu zrážok v oblasti Bratislava od 1.1.- 31.1.2026",
     xlab = "", ylab = "", xaxt="n", yaxt="n")
axis(1, at=1, labels=datum[1])
axis(1, at=149, labels=datum[149])
axis(1, at=297, labels=datum[297])
axis(1, at=445, labels=datum[445])
axis(1, at=593, labels=datum[593])
axis(1, at=length(datum), labels=datum[length(datum)])
axis(2, at = c(0, 1))

# pridanie oneskoreni/lagov
data <- dataset[,c(2,4,5,6)]
precipiation_binary_lag <- lag(data$precipiation_binary, n=1)
relative_humidity_lag <- lag(data$relative_humidity, n=1)
cloud_cover_lag <- lag(data$cloud_cover, n=1)
surface_pressure_lag <- lag(data$surface_pressure, n=1)
data <- data.frame(data, precipiation_binary_lag, relative_humidity_lag, 
                   cloud_cover_lag, surface_pressure_lag)
data <- na.omit(data)

# rozdelenie dat na train a test
n.ahead <- 72 # 3 dni
train <- data[1:(nrow(data)-n.ahead), ]
test <- data[(nrow(data)-n.ahead+1): nrow(data), ]

# odhady modelov
model <- glm(precipiation_binary ~ 0 + precipiation_binary_lag + relative_humidity_lag +
               cloud_cover_lag + surface_pressure_lag, family = binomial, data = train)
summary(model)

#____________
model2 <- glm(precipiation_binary ~ 0 + precipiation_binary_lag +
                cloud_cover_lag + surface_pressure_lag, family = binomial, data = train)
summary(model2)

# vypocet predikcii
pred <- predict(model2, newdata = test[c(5,7,8)], type = "response") 
predikcie <- ifelse(pred > 0.5, 1, 0)

###############################################################################
# vykreslenie s priblizenim
index2 <- (nrow(data)-n.ahead-12):nrow(data)

plot(data[index2, 4], type="l",main="Skutočné vs predikované hodnoty modelu BAR(1) s kovariátmi",
     xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(0,1.1), xlim=c(0,89))
axis(1, at= (length(index2)-n.ahead+1), labels = dataset[length(datum)-n.ahead+1,1])
axis(1, at= 37, labels = dataset[length(datum)-length(index2)+1+37,1])
axis(1, at= 61, labels = dataset[length(datum)-length(index2)+1+61,1])
axis(1, at = length(index2), labels = dataset[length(datum),1])
axis(2, at = c(0, 0.5, 0.7, 1),las = 1, cex.axis = 1)
lines((length(index2)-n.ahead+1):length(index2), pred, col="green",lwd=1.5)
lines((length(index2)-n.ahead+1):length(index2), predikcie, col="red", lty = 2, lwd = 2)
legend("topright", legend = c("Skutočné dáta", 
                         "Predikovaná pravdepodobnosť",
                         "Binárna predikcia"),
       col = c("black","green","red"), lty = c(1, 1, 1), bty = "n")

################################################################################
# presnost uspesnych predikcii
realita <- data[(nrow(data)-n.ahead+1): nrow(data),4]
uspesnost <- (realita == predikcie)

summary(uspesnost)
mean(uspesnost) 
mean(1-uspesnost)
