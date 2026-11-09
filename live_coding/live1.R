# live coding 

## univariate linear models

data("airquality")
head(airquality)

## fitting a Ozone ~ Temp model
m <- lm(Ozone~Temp, data = airquality)

summary(m)
plot(m)
fitted(m) # fitted values
residuals(m)

airquality$Ozone[na.omit(airquality$Ozone)] - fitted(m) # not working

predict(m)

## check model assumptions
library(DHARMa)
ss <- simulateResiduals(m)
plot(ss)

## Model interpretation
summary(m)
library(ggeffects)
pp <- predict_response(m)
plot(pp, show_data = TRUE)

