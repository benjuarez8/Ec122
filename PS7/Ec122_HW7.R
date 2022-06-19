## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
homes = read.table("dat/homes.dat")
colnames(homes) = c("homes", "irate")


## ----------------------------------------------------------------------------------
HOMES = homes$homes
IRATE = homes$irate
DHOMES = data.frame(matrix(ncol = 1, nrow = 219))
DHOMES[1,] = 0
DIRATE = data.frame(matrix(ncol = 1, nrow = 219))
DIRATE[1,] = 0

for (i in 3:220) {
    DHOMES[i-1,] = homes$homes[i-1] - homes$homes[i-2]
    DIRATE[i-1,] = homes$irate[i-1] - homes$irate[i-2]
}

plot(ts(HOMES, start(1992,1), frequency = 12), ylab="",xlab="Years after Jan. 1992",main="HOMES")
plot(ts(IRATE, start(1992,1), frequency = 12), ylab="",xlab="Years after Jan. 1992",main="IRATE")
plot(ts(DHOMES, start(1992,1), frequency = 12), ylab="",xlab="Years after Jan. 1992",main="DHOMES")
plot(ts(DIRATE, start(1992,1), frequency = 12), ylab="",xlab="Years after Jan. 1992",main="DIRATE")



## ----------------------------------------------------------------------------------
library("dynlm")
DHOMES = ts(diff(HOMES))
DIRATE = ts(diff(IRATE))

homes_model = dynlm(DHOMES ~ L(DHOMES) + L(DIRATE) + L(DIRATE,2))
summary(homes_model)


## ----------------------------------------------------------------------------------
sqrt(c(coef(homes_model)[3],coef(homes_model)[2],1) %*% vcov(homes_model)[2:4,2:4] %*% c(coef(homes_model)[3],coef(homes_model)[2],1))


## ----------------------------------------------------------------------------------
acf(ts(homes_model$residuals), main="")


## ----------------------------------------------------------------------------------
library("lmtest")
bgtest(homes_model, order=2, type="Chisq", fill=NA)


## ----------------------------------------------------------------------------------
f_model = dynlm(DHOMES ~ L(DHOMES) + L(DHOMES,5) + L(DIRATE) + L(DIRATE,3))
summary(f_model)
acf(ts(f_model$residuals), main="")

