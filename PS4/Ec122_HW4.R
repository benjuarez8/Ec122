## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
data1 = read.table("dat/cps4_small.dat")
colnames(data1) = c("wage", "educ", "exper", "hrswk", "married", "female", "metro", 
                    "midwest", "south", "west", "black", "asian")


## ----------------------------------------------------------------------------------
hist1 = hist(data1$wage, breaks = 30, xlab = "WAGE", main = "WAGE Histogram")
hist2 = hist(log(data1$wage), breaks = 30, xlab = "ln(WAGE)", main = "ln(WAGE) Histogram")



## ----------------------------------------------------------------------------------
educ = data1$educ
wage = data1$wage

wage_lm = lm(wage ~ educ)
wage_llr = lm(log(wage) ~ educ)
summary(wage_lm)
summary(wage_llr)

mean(wage)
unname(coef(wage_lm)[2]/mean(wage) * 100)
unname(coef(wage_llr)[2] * 100)


## ----------------------------------------------------------------------------------
hist3 = hist(resid(wage_lm), breaks=30, xlab = "residuals (wage_lm)", main = 
               "Linear Regression Residuals Histogram")
hist3 = hist(resid(wage_llr), breaks=30, xlab = "residuals (wage_llr)", main = 
               "Log-Linear Regression Residuals Histogram")

library("tseries")
jarque.bera.test(resid(wage_lm))
jarque.bera.test(resid(wage_llr))


## ----------------------------------------------------------------------------------
plot(educ, resid(wage_lm), xlab="EDUC", ylab="residual")
plot(educ, resid(wage_llr), xlab="EDUC", ylab="residual")


## ----------------------------------------------------------------------------------
pred_lm = coef(wage_lm)[1] + coef(wage_lm)[2] * 16
pred_llr = exp(coef(wage_llr)[1] + coef(wage_llr)[2] * 16)
unname(pred_lm)
unname(pred_llr)
data_16 = as.data.frame(filter(data1, educ == 16))
mean(data_16$wage)


## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
data2 = read.table("dat/cocaine.dat")
colnames(data2) = c("price", "quant", "qual", "trend")


## ----------------------------------------------------------------------------------
PRICE = data2$price
QUANT = data2$quant
QUAL = data2$qual
TREND = data2$trend
coke_lm = lm(PRICE ~ QUANT + QUAL + TREND)
summary(coke_lm)

