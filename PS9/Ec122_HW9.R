## ----------------------------------------------------------------------------------
library("MASS")
mu = c(0,0)
Sigma_e = matrix(c(1,-.9,-.9,1),2,2)
Sigma_x = matrix(c(1,.9,.9,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)


## ----------------------------------------------------------------------------------
y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out2 = lm(y1z ~ x1z)
summary(out2)


## ----------------------------------------------------------------------------------
library("stats")
out3 = glm(z ~ x2, family = binomial(link = probit))
summary(out3)
B2 = out3$coefficients[2]
B2


## ----------------------------------------------------------------------------------
x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out4 = lm(y1z ~ x1z + lambda)
summary(out4)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,-.5,-.5,1),2,2)
Sigma_x = matrix(c(1,.9,.9,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out5ia = lm(y1z ~ x1z)
summary(out5ia)

out5ib = glm(z ~ x2, family = binomial(link = probit))
summary(out5ib)
B2 = out5ib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out5ic = lm(y1z ~ x1z + lambda)
summary(out5ic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,0,0,1),2,2)
Sigma_x = matrix(c(1,.9,.9,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out5iia = lm(y1z ~ x1z)
summary(out5iia)

out5iib = glm(z ~ x2, family = binomial(link = probit))
summary(out5iib)
B2 = out5iib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out5iic = lm(y1z ~ x1z + lambda)
summary(out5iic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,.5,.5,1),2,2)
Sigma_x = matrix(c(1,.9,.9,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out5iiia = lm(y1z ~ x1z)
summary(out5iiia)

out5iiib = glm(z ~ x2, family = binomial(link = probit))
summary(out5iiib)
B2 = out5iiib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out5iiic = lm(y1z ~ x1z + lambda)
summary(out5iiic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,.9,.9,1),2,2)
Sigma_x = matrix(c(1,.9,.9,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out5iva = lm(y1z ~ x1z)
summary(out5iva)

out5ivb = glm(z ~ x2, family = binomial(link = probit))
summary(out5ivb)
B2 = out5ivb$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out5ivc = lm(y1z ~ x1z + lambda)
summary(out5ivc)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,-.9,-.9,1),2,2)
Sigma_x = matrix(c(1,0,0,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out6ia = lm(y1z ~ x1z)
summary(out6ia)

out6ib = glm(z ~ x2, family = binomial(link = probit))
summary(out6ib)
B2 = out6ib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out6ic = lm(y1z ~ x1z + lambda)
summary(out6ic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,-.5,-.5,1),2,2)
Sigma_x = matrix(c(1,0,0,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out6iia = lm(y1z ~ x1z)
summary(out6iia)

out6iib = glm(z ~ x2, family = binomial(link = probit))
summary(out6iib)
B2 = out6iib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out6iic = lm(y1z ~ x1z + lambda)
summary(out6iic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,0,0,1),2,2)
Sigma_x = matrix(c(1,0,0,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out6iiia = lm(y1z ~ x1z)
summary(out6iiia)

out6iiib = glm(z ~ x2, family = binomial(link = probit))
summary(out6iiib)
B2 = out6iiib$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out6iiic = lm(y1z ~ x1z + lambda)
summary(out6iiic)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,.5,.5,1),2,2)
Sigma_x = matrix(c(1,0,0,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out6iva = lm(y1z ~ x1z)
summary(out6iva)

out6ivb = glm(z ~ x2, family = binomial(link = probit))
summary(out6ivb)
B2 = out6ivb$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out6ivc = lm(y1z ~ x1z + lambda)
summary(out6ivc)


## ----------------------------------------------------------------------------------
mu = c(0,0)
Sigma_e = matrix(c(1,.9,.9,1),2,2)
Sigma_x = matrix(c(1,0,0,1),2,2)
epsilon = mvrnorm(4000,mu,Sigma_e)
x = mvrnorm(4000,mu,Sigma_x)

x1 = x[,1]
x2 = x[,2]
e1 = epsilon[,1]
e2 = epsilon[,2]

y1 = 2 + 2 * x1 + e1
y2 = 4 * x2 + e2
z = ifelse(y2>0,1,0)

y1z = y1[z > 0]
x1z = x1[z > 0]
length(y1z)
out6va = lm(y1z ~ x1z)
summary(out6va)

out6vb = glm(z ~ x2, family = binomial(link = probit))
summary(out6vb)
B2 = out6vb$coefficients[2]
B2

x2z = x2[z>0]
lambda = dnorm(-B2*x2z) / (1-pnorm(-B2*x2z))
out6vc = lm(y1z ~ x1z + lambda)
summary(out6vc)

