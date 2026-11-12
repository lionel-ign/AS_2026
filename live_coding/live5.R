# Gam in R

library(mgcv)

## get some data
data("airquality")
dat_nona <- na.omit(airquality)

## fit our first GAM
m1 <- gam(Ozone ~ s(Temp), data = dat_nona)
summary(m1)
coef(m1)
plot(m1)

## different smooth
?smooth.terms

## get the basis functions
newX <- data.frame(Temp = seq(min(airquality$Temp),
                              max(airquality$Temp),
                              length = 50))
bf <- predict(m1, newdata = newX, type = "lpmatrix")
head(bf)
plot(newX$Temp, bf[,5])

## control on the smooth
m2 <- gam(Ozone ~ s(Temp, bs = "cr", k = 20), data = dat_nona,
          family = gaussian(link = "log"))
coef(m2)
plot(m2)
library(ggeffects)
pp <- predict_response(m2)
plot(pp)

## adding other covariates
m3 <- gam(Ozone ~ s(Temp, bs = "cr", k = 25) + s(Wind, k = 15) + s(Solar.R, bs = "tp"),
          data = dat_nona)

## model checks
library(DHARMa)
ss <- simulateResiduals(m3)
plot(ss)

concurvity(m3)
k.check(m3)

## tensors
m4 <- gam(Ozone ~ te(Temp, Wind, bs = c("cr", "tp"), k = c(5, 25)) + s() + Solar.R,
          data = dat_nona)

## derive prediction by hand
newX <- expand.grid(Temp = seq(min(dat_nona$Temp),
                               max(dat_nona$Temp),
                               length.out = 50),
                    Wind = seq(min(dat_nona$Wind),
                               max(dat_nona$Wind),
                               length.out = 50))
newX$pred <- predict(m4, newdata = newX)

library(ggplot2)
ggplot(newX, aes(x = Temp, y = Wind, fill = pred)) +
  geom_raster() +
  scale_fill_viridis_c()
