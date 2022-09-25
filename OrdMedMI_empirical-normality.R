####2022/08/15
### OrdMedMI: Checking empirical normality of [.] in Equations 9 & 10 
# install.packages("moments")
library(moments)
n <- 10000

b10 <- rep(NA, n)
b11 <- rep(NA, n)
for (i in 1:n) {
  gama0 <- rnorm(1,0,1)
  gama1 <- rnorm(1,0,1)
  b10[i] <- exp(gama0 + gama1)/(1+exp(gama0 + gama1)) - exp(gama0)/(1+exp(gama0))
  b11[i] <- pnorm(gama0 + gama1) - pnorm(gama0)
}

par(mfrow = c(1,2))
hist(b10)
hist(b11)

skewness(b10)
kurtosis(b10)

skewness(b11)
kurtosis(b11)

qqnorm(b10, pch = 1, frame = FALSE)
qqline(b10, col = "steelblue", lwd = 2)

qqnorm(b11, pch = 1, frame = FALSE)
qqline(b11, col = "steelblue", lwd = 2)

