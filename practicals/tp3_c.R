# tp3
library(tidyverse)
## read-in the data
dat_tp3 <- read.csv("data/dat_tp3.csv")

# scale continuous variables
dat_tp3 |>
  dplyr::select(-biomass, -idp) |>
  mutate(across(where(is.numeric), ~ scale(.x)[,1])) |>
  bind_cols(dplyr::select(dat_tp3, idp, biomass)) -> dat

head(dat)
summary(dat)

## fit full model
dat_noidp <- dplyr::select(dat, -idp)
dat_noidp <- dat[,-43]
m_full <- lm(biomass ~ ., data = dat_noidp)
summary(m_full)

## AIC model selection
library(MASS)
?stepAIC
m_opt <- stepAIC(m_full)
coef(m_opt)
pred <- predict(m_opt)
plot(dat_noidp$biomass, pred)
attr(attr(m_opt$model, "terms"), "predvars")
dat_sel <- dplyr::select(dat_noidp, biomass, mqd, reserutile, troph_r, species, ownership, tmoy_spr_min_anomaly, 
                         tmoy_sum_min_anomaly, spei3.13_mean, spei5.13_mean, spei6.13_mean, 
                         spei7.13_mean, spei9.13_mean, spei10.13_mean, spei11.13_mean, 
                         spei14.13_mean, spei20.13_mean, spei21.13_mean, spei23.13_mean)
dat_nona <- na.omit(dat_sel)
m_nona <- lm(biomass ~ ., dat_nona)
pred <- predict(m_nona)
plot(dat_nona$biomass, pred)
abline(0, 1)

# LASSO
library(glmnet)
dat_noidp <- na.omit(dat_noidp) # NA drops
X <- model.matrix(biomass ~ . -1, dat_noidp) # create X matrix
# fit lasso
m_lasso <- glmnet(X, dat_noidp$biomass, alpha = 1)
plot(m_lasso, xvar = "lambda", label = TRUE, sign.lambda = 1)

# cv to find lambda
m_cv <- cv.glmnet(X, dat_noidp$biomass, alpha = 1)
plot(m_cv)
m_cv$lambda.1se # optimized lambda
m_opt <- glmnet(X, dat_noidp$biomass, alpha = 1, lambda = m_cv$lambda.1se)
pred_lasso <- predict(m_opt, newx = X)
plot(dat_noidp$biomass, pred_lasso)
abline(0, 1)
