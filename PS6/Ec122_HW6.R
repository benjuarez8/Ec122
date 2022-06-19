## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
metrics = read.table("dat/metrics.dat")
colnames(metrics) = c("salary", "gpa", "metrics", "female")


## ----------------------------------------------------------------------------------
SAL = metrics$salary
GPA = metrics$gpa
METRICS = metrics$metrics

metrics_lm = lm(SAL ~ GPA + METRICS)
summary(metrics_lm)


## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
utown = read.table("dat/utown.dat")
colnames(utown) = c("price", "sqft", "age", "utown", "pool", "fplace")


## ----------------------------------------------------------------------------------
PRICE = utown$price
UTOWN = utown$utown
SQFT = utown$sqft
AGE = utown$age
POOL = utown$pool
FPLACE = utown$fplace

price_llr = lm(log(PRICE) ~ UTOWN + SQFT + (SQFT * UTOWN) + AGE + POOL + FPLACE)
summary(price_llr)


## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
star = read.table("dat/star.dat")
colnames(star) = c("id", "schid", "tchid", "tchexper", "absent", "readscore", "mathscore", "totalscore", "boy", "white_asian", "black", "tchwhite", "tchmasters", "freelunch", "schurban", "schrural", "small", "regular", "aide")


## ----------------------------------------------------------------------------------
mean(subset(star, regular==1)$totalscore)
mean(subset(star, aide==1)$totalscore)
mean(subset(star, small==1)$totalscore)


## ----------------------------------------------------------------------------------
TOTALSCORE = star$totalscore
SMALL = star$small
AIDE = star$aide

b_lm = lm(TOTALSCORE ~ SMALL + AIDE)
summary(b_lm)


## ----------------------------------------------------------------------------------
TOTALSCORE = star$totalscore
SMALL = star$small
AIDE = star$aide
TCHEXPER = as.integer(star$tchexper)

c_lm = lm(TOTALSCORE ~ SMALL + AIDE + TCHEXPER)
summary(c_lm)


## ----------------------------------------------------------------------------------
TOTALSCORE = star$totalscore
SMALL = star$small
AIDE = star$aide
TCHEXPER = as.integer(star$tchexper)
BOY = star$boy
FREELUNCH = star$freelunch
WHITE_ASIAN = star$white_asian

d_lm = lm(TOTALSCORE ~ SMALL + AIDE + TCHEXPER + BOY + FREELUNCH + WHITE_ASIAN)
summary(d_lm)


## ----------------------------------------------------------------------------------
TOTALSCORE = star$totalscore
SMALL = star$small
AIDE = star$aide
TCHEXPER = as.integer(star$tchexper)
BOY = star$boy
FREELUNCH = star$freelunch
WHITE_ASIAN = star$white_asian
TCHWHITE = star$tchwhite
TCHMASTERS = star$tchmasters
SCHURBAN = star$schurban
SCHRURAL = star$schrural

e_lm = lm(TOTALSCORE ~ SMALL + AIDE + TCHEXPER + BOY + FREELUNCH + WHITE_ASIAN + TCHWHITE + TCHMASTERS + SCHURBAN + SCHRURAL)
summary(e_lm)


## ----------------------------------------------------------------------------------
library("car")
SCHID = star$schid
g_lm = lm(TOTALSCORE ~ SMALL + AIDE + TCHEXPER + BOY + FREELUNCH + WHITE_ASIAN + TCHWHITE + TCHMASTERS + as.factor(SCHID))
linearHypothesis(g_lm, matchCoefs(g_lm,"SCHID"))


## ----------------------------------------------------------------------------------
setwd("~/Desktop/R")
data = read.table("dat/cps4_small.dat")
colnames(data) = c("wage", "educ", "exper", "hrswk", "married", "female", "metro", "midwest", "south", "west", "black", "asian")

WAGE = data$wage
EDUC = data$educ
EXPER = data$exper
EXPER_2 = EXPER^2
library("lmtest")
wage_llr = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EXPER*EDUC))
summary(wage_llr)
coeftest(wage_llr, vcov.=hccm(wage_llr,type="hc1"))


## ----------------------------------------------------------------------------------
WAGE = data$wage
EDUC = data$educ
EXPER = data$exper
EXPER_2 = EXPER^2
MARRIED = data$married

wage_llr_b = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EXPER*EDUC) + MARRIED)
summary(wage_llr_b)


## ----------------------------------------------------------------------------------
plot(MARRIED, resid(wage_llr))


## ----------------------------------------------------------------------------------
WAGE = subset(data, married==1)$wage
EDUC = subset(data, married==1)$educ
EXPER = subset(data, married==1)$exper
EXPER_2 = EXPER^2

wage_llr_d1 = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EXPER*EDUC))
summary(wage_llr_d1)


## ----------------------------------------------------------------------------------
WAGE = subset(data, married==0)$wage
EDUC = subset(data, married==0)$educ
EXPER = subset(data, married==0)$exper
EXPER_2 = EXPER^2

wage_llr_d2 = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EXPER*EDUC))
summary(wage_llr_d2)


## ----------------------------------------------------------------------------------
sigma(wage_llr_d1)^2
sigma(wage_llr_d2)^2


## ----------------------------------------------------------------------------------
library("nlme")
WAGE = data$wage
EDUC = data$educ
EXPER = data$exper
EXPER_2 = EXPER^2

w = rep(1:1000)
for (i in 1:1000) {
  if (data$married[i] == 1) {
    w[i] = 1/(sigma(wage_llr_d1)^2)
  }
  else {
    w[i] = 1/(sigma(wage_llr_d2)^2)
  }
}

glsm = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EXPER*EDUC), weights = w)
summary(glsm)


## ----------------------------------------------------------------------------------
library("sandwich")
a = lm(log(WAGE) ~ EDUC + EXPER + EXPER_2 + (EDUC*EXPER))
var_b2 = vcov(a)[2,2]
var_b5 = vcov(a)[5,5]
cov_b2_b5 = vcov(a)[2,5]
sqrt(var_b2 + 10^2*var_b5 + 2*10*cov_b2_b5)


## ----------------------------------------------------------------------------------
library("sandwich")
var_b2 = vcov(glsm)[2,2]
var_b5 = vcov(glsm)[5,5]
cov_b2_b5 = vcov(glsm)[2,5]
sqrt(var_b2 + 10^2*var_b5 + 2*10*cov_b2_b5)

