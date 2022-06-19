## ----------------------------------------------------------------------------------
setwd("~/Desktop/R/")
capm4 = read.table("dat/capm4.dat")
colnames(capm4) = c("date", "dis", "ge", "gm", "ibm", "msft", "xom", "mkt", 
                    "riskfree")


## ----------------------------------------------------------------------------------
library("plotrix")
library("knitr")
companies = c("Disney", "GE", "GM", "IBM", "Microsoft", "Exxon")
c = capm4$mkt - capm4$riskfree

dis = capm4$dis - capm4$riskfree
ge = capm4$ge - capm4$riskfree
gm = capm4$gm - capm4$riskfree
ibm = capm4$ibm - capm4$riskfree
msft = capm4$msft - capm4$riskfree
xom = capm4$xom - capm4$riskfree
parta = data.frame(matrix(nrow = 2, ncol = 6, dimnames = list(c("beta","t"), companies)))

dis_lm = lm(dis ~ c)
ge_lm = lm(ge ~ c)
gm_lm = lm(gm ~ c)
ibm_lm = lm(ibm ~ c)
msft_lm = lm(msft ~ c)
xom_lm = lm(xom ~ c)

parta[1,1] = coef(dis_lm)[2]
parta[1,2] = coef(ge_lm)[2]
parta[1,3] = coef(gm_lm)[2]
parta[1,4] = coef(ibm_lm)[2]
parta[1,5] = coef(msft_lm)[2]
parta[1,6] = coef(xom_lm)[2]

se_dis = summary(dis_lm)$coefficients[2,2]
se_ge = summary(ge_lm)$coefficients[2,2]
se_gm = summary(gm_lm)$coefficients[2,2]
se_ibm = summary(ibm_lm)$coefficients[2,2]
se_msft = summary(msft_lm)$coefficients[2,2]
se_xom = summary(xom_lm)$coefficients[2,2]

parta[2, 1] = (parta[1, 1] - 1) / se_dis
parta[2, 2] = (parta[1, 2] - 1) / se_ge
parta[2, 3] = (parta[1, 3] - 1) / se_gm
parta[2, 4] = (parta[1, 4] - 1) / se_ibm
parta[2, 5] = (parta[1, 5] - 1) / se_msft
parta[2, 6] = (parta[1, 6] - 1) / se_xom

kable(parta)


## ----------------------------------------------------------------------------------
(parta[1, 6] - 1) / se_xom


## ----------------------------------------------------------------------------------
(parta[1, 5] - 1) / se_msft


## ----------------------------------------------------------------------------------
parta[1, 5] + 1.9784 * se_msft
parta[1, 5] - 1.9784 * se_msft


## ----------------------------------------------------------------------------------
parte = data.frame(matrix(nrow = 2, ncol = 6, dimnames = list(c("alpha","t"), companies)))

parte[1,1] = coef(dis_lm)[1]
parte[1,2] = coef(ge_lm)[1]
parte[1,3] = coef(gm_lm)[1]
parte[1,4] = coef(ibm_lm)[1]
parte[1,5] = coef(msft_lm)[1]
parte[1,6] = coef(xom_lm)[1]

se_dis_a = summary(dis_lm)$coefficients[1,2]
se_ge_a = summary(ge_lm)$coefficients[1,2]
se_gm_a = summary(gm_lm)$coefficients[1,2]
se_ibm_a = summary(ibm_lm)$coefficients[1,2]
se_msft_a = summary(msft_lm)$coefficients[1,2]
se_xom_a = summary(xom_lm)$coefficients[1,2]

parte[2, 1] = (parte[1, 1]) / se_dis_a
parte[2, 2] = (parte[1, 2]) / se_ge_a
parte[2, 3] = (parte[1, 3]) / se_gm_a
parte[2, 4] = (parte[1, 4]) / se_ibm_a
parte[2, 5] = (parte[1, 5]) / se_msft_a
parte[2, 6] = (parte[1, 6]) / se_xom_a


for (i in 1:6) {
  for (j in 1:2) {
    parte[j,i] = signif(parte[j,i],4)
  }
}

kable(parte)


## ----------------------------------------------------------------------------------
setwd("~/Desktop/R/")
fair4 = read.table("dat/fair4.dat")
colnames(fair4) = c("year", "vote", "party", "person", "duration", "war", "growth",
                    "inflation", "goodnews")


## ----------------------------------------------------------------------------------
growth = subset(fair4, year > 1915)$growth
vote = subset(fair4, year > 1915)$vote
growth_lm = lm(vote ~ growth)
b2 = coef(growth_lm)[2]
se_b2 = summary(growth_lm)$coefficients[2,2]
t = b2/se_b2
print(t)


## ----------------------------------------------------------------------------------
i1 = b2 + 2.0739 * se_b2
i2 = b2 - 2.0739 * se_b2
unname(i1)
unname(i2)


## ----------------------------------------------------------------------------------
inflation = subset(fair4, year > 1915)$inflation
inflation_lm = lm(vote ~ inflation)
b2_i = coef(inflation_lm)[2]
se_b2_i = summary(inflation_lm)$coefficients[2,2]
t = b2_i/se_b2_i
print(t)


## ----------------------------------------------------------------------------------
i11 = b2_i + 2.0739 * se_b2_i
i22 = b2_i - 2.0739 * se_b2_i
unname(i11)
unname(i22)


## ----------------------------------------------------------------------------------
b1 = coef(inflation_lm)[1]
se_b1 = summary(inflation_lm)$coefficients[1,2]
t = (b1 - 50) / se_b1
print(unname(t))


## ----------------------------------------------------------------------------------
cov_matrix = vcov(inflation_lm)
c_2 = 0.02
low = (b1 + b2 * c_2) - 2.0739 * sqrt(cov_matrix[1,1] + c_2^2 * cov_matrix[2,2] + 2 * 
                                      c_2 * cov_matrix[1,2])
high = (b1 + b2 * c_2) + 2.0739 * sqrt(cov_matrix[1,1] + c_2^2 * cov_matrix[2,2] + 2 * c_2 
                                     * cov_matrix[1,2])
print(unname(low))
print(unname(high))

