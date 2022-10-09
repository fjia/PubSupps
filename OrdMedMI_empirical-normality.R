####2022/08/15
### OrdMedMI: Checking empirical normality of [.] in Equations 7
# install.packages("moments")
library(moments)
n <- 10000

b7 <- rep(NA, n)
for (i in 1:n) {
  gama0 <- rnorm(1,0,1)
  gama1 <- rnorm(1,0,1)
  b7[i] <- exp(gama0 + gama1)/(1+exp(gama0 + gama1)) - exp(gama0)/(1+exp(gama0))
}

par(mfrow = c(1,2))
hist(b7)

skewness(b7)
kurtosis(b7)

qqnorm(b7, pch = 1, frame = FALSE)
qqline(b7, col = "steelblue", lwd = 2)
