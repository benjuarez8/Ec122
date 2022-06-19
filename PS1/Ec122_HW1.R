## ----------------------------------------------------------------------------------
capm4 = read.table("dat/capm4.dat")
colnames(capm4) = c("date", "dis", "ge", "gm", "ibm", "msft", "xom", "mkt", 
                    "riskfree")


## ----------------------------------------------------------------------------------
companies = c("Disney", "GE", "GM", "IBM", "Microsoft", "Exxon")

disney = lm((capm4$dis - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
disney_est = coef(disney)[2]

general_electric = lm((capm4$ge - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
general_electric_est = coef(general_electric)[2]

general_motors = lm((capm4$gm - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
general_motors_est = coef(general_motors)[2]

ibm = lm((capm4$ibm - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
ibm_est = coef(ibm)[2]

microsoft = lm((capm4$msft - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
microsoft_est = coef(microsoft)[2]

exxon = lm((capm4$xom - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree))
exxon_est = coef(exxon)[2]

estimates = matrix(c(disney_est, general_electric_est, general_motors_est, ibm_est, 
      microsoft_est, exxon_est), dimnames = list(companies, "beta estimate"))

as.table(estimates)


## ----------------------------------------------------------------------------------
intercepts = matrix(c(coef(disney)[1], coef(general_electric)[1], coef(general_motors)[1],
                    coef(ibm)[1], coef(microsoft)[1], coef(exxon)[1]), dimnames = 
                      list(companies, "alpha estimate"))
as.table(intercepts)

plot((capm4$mkt - capm4$riskfree), (capm4$msft - capm4$riskfree), xlab = "mkt - riskfree",
     ylab = "msft - riskfree")
abline(lm((capm4$msft - capm4$riskfree) ~ (capm4$mkt - capm4$riskfree)), col = "blue")



## ----------------------------------------------------------------------------------
new_estimates = matrix(c(
  coef(lm((capm4$dis - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree))),
  coef(lm((capm4$ge - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree))),
  coef(lm((capm4$gm - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree))),
  coef(lm((capm4$ibm - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree))),
  coef(lm((capm4$msft - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree))),
  coef(lm((capm4$xom - capm4$riskfree) ~ 0 + (capm4$mkt - capm4$riskfree)))),
  dimnames = list(companies, "beta estimate with alpha = 0"))
as.table(new_estimates)


## ----------------------------------------------------------------------------------
cps4_s = read.table("dat/cps4_small.dat")
colnames(cps4_s) = c("wage", "educ", "exper", "hrswk", "married", "female", "metro", 
                     "midwest", "south", "west", "black", "asian")


## ----------------------------------------------------------------------------------
labels = c("mean", "median", "max", "min", "sd")
educ_hist = hist(cps4_s$educ, breaks = 25, xlab = "years of education", main = "")
educ_stats = matrix(c(mean(cps4_s$educ), median(cps4_s$educ), max(cps4_s$educ),       min(cps4_s$educ), sd(cps4_s$educ)), dimnames = list(labels, ""))
educ_stats


## ----------------------------------------------------------------------------------
wage_hist = hist(cps4_s$wage, breaks = 100, xlab = "hourly wages", main = "")
wage_stats = matrix(c(mean(cps4_s$wage), median(cps4_s$wage), max(cps4_s$wage),       min(cps4_s$wage), sd(cps4_s$wage)), dimnames = list(labels, ""))
as.table(wage_stats)


## ----------------------------------------------------------------------------------
wage_lm = lm((B_1 = cps4_s$wage) ~ (B_2 = cps4_s$educ))
coef(wage_lm)


## ----------------------------------------------------------------------------------
resid_plot = plot(cps4_s$educ, resid(wage_lm), xlab = "educ", 
                  ylab = "least squares residuals")


## ----------------------------------------------------------------------------------
female_data = subset(cps4_s, cps4_s$female > 0)
male_data = subset(cps4_s, cps4_s$female == 0)
black_data = subset(cps4_s, cps4_s$black > 0)
white_data = subset(cps4_s, cps4_s$black == 0 & cps4_s$asian == 0)

female_lm = lm(female_data$wage ~ (B_2 = female_data$educ))
male_lm = lm(male_data$wage ~ (B_2 = male_data$educ))
black_lm = lm(black_data$wage ~ (B_2 = black_data$educ))
white_lm = lm(white_data$wage ~ (B_2 = white_data$educ))

regressions = cbind(
  c(coef(female_lm)[1], coef(male_lm)[1], coef(black_lm)[1], coef(white_lm)[1]),
  c(coef(female_lm)[2], coef(male_lm)[2], coef(black_lm)[2], coef(white_lm)[2]))
colnames(regressions) = c("B_1", "B_2")
rownames(regressions) = c("female", "male", "black", "white")

as.table(regressions)


## ----------------------------------------------------------------------------------
educ2 = cps4_s$educ^2
quad = lm(cps4_s$wage ~ educ2)
quad_coeff = as.table(matrix(c(coef(quad)[1], coef(quad)[2])))
colnames(quad_coeff) = c("")
rownames(quad_coeff) = c("a_1", "a_2")
as.table(quad_coeff)


## ----------------------------------------------------------------------------------
quad_function = coef(quad)[1] + coef(quad)[2] * cps4_s$educ^2
educ_v_wage = plot(cps4_s$educ, cps4_s$wage, xlab="educ", ylab="wage")
lines(sort(cps4_s$educ), quad_function[order(cps4_s$educ)], col = "red")
abline(wage_lm, col = "blue")



## ----------------------------------------------------------------------------------
hist(log(cps4_s$wage), breaks = 50, main="", xlab="hourly wages")


## ----------------------------------------------------------------------------------
llr = lm(log(cps4_s$wage) ~ (y_2 = cps4_s$educ))
llr_coeff = as.table(matrix(c(coef(llr)[1], coef(llr)[2])))
colnames(llr_coeff) = c("")
rownames(llr_coeff) = c("y_1", "y_2")
as.table(llr_coeff)


## ----------------------------------------------------------------------------------
wage1 = exp(1.6094 + (0.0904 * 12)) # wage @ 12 years of education
wage2 = exp(1.6094 + (0.0904 * 14)) # wage @ 14 years of education

me1 = wage1 * coef(llr)[2] # marginal effect for 12 years of previous educ.
me2 = wage2 * coef(llr)[2] # marginal effect for 14 years of previous educ.

result = as.table(matrix(c(me1, me2)))
colnames(result) = c("Marginal Effect") 
rownames(result) = c("12 yrs. prev. educ.", "14 yrs. prev. educ.")
result

