# model and variable explanation prediction vs explanation

## explanation
data("airquality")

## directed acyclic graph
library(dagitty)
library(ggdag)

ozone_graph <- dagify(
  ozone ~ temp + wind + solar.r,
  wind ~ temp,
  temp ~ solar.r,
  exposure = "temp",
  outcome = "ozone"
) |> tidy_dagitty()

ggdag(ozone_graph)
ggdag_adjustment_set(ozone_graph, effect = "total")

## lm model total effect of temp
m_total <- lm(Ozone ~ Temp + Solar.R, data = airquality)
summary(m_total)

ggdag_adjustment_set(ozone_graph, effect = "direct")

## lm model direct effect of temp
m_direct <- lm(Ozone ~ Temp + Solar.R + Wind, data = airquality)
summary(m_direct)

# prediction
## simulated dataset
## biomass ~ X1 + ... + X10
X <- matrix(runif(11 * 1000), ncol = 11)
X[,1] <- 1
betas <- rnorm(11)
mu <- X %*% betas
biomass <- rnorm(n = 1000, mean = mu, sd = 1)

dat <- as.data.frame(X[,-1])
dat$biomass <- biomass
head(dat)

## AIC model selection
m_full <- lm(biomass ~ ., data = dat)
summary(m_full)

library(MASS)
m_opt <- stepAIC(m_full)
coef(m_opt)

## LASSO  shrinkage estimation
library(glmnet)
m_lasso <- glmnet(X, biomass, alpha = 1) # alpha=1, LASSO, alpha=0, ridge
plot(m_lasso, xvar = "lambda", label = TRUE)

## cv for checking penalty strength
m_cv <- cv.glmnet(X, biomass, alpha = 1)
plot(m_cv)
m_cv$lambda.1se
m_cv$lambda.min

m_lasso <- glmnet(X, biomass, alpha = 1, lambda = m_cv$lambda.1se)
selVars <- rownames(coef(m_lasso))[which(coef(m_lasso) != 0)]
print(selVars)

predict(m_lasso, newx = X)

