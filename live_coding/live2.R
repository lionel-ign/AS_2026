# linear extension

## data airquality
data("airquality")
airquality

## clean up data
airquality <- na.omit(airquality)
summary(airquality)

## multiple variables
m <- lm(Ozone ~ Temp + Solar.R + Wind, data = airquality)
summary(m)

## scale variables
airquality$sTemp <- scale(airquality$Temp)[,1]
airquality$sWind <- scale(airquality$Wind)[,1]
airquality$sSolar <- scale(airquality$Solar.R)[,1]

m <- lm(Ozone ~ sTemp + sSolar + sWind, data = airquality)
summary(m)

## interaction
m <- lm(Ozone ~ sTemp * sWind + sSolar, data = airquality)
summary(m)

library(ggeffects)
pp <- predict_response(m, terms = c("sTemp", "sWind"))
plot(pp)

## transformation, polynomes
m <- lm(log(Ozone) ~ Temp, airquality)
plot(predict_response(m), show_data = TRUE)

m <- lm(Ozone ~ poly(Temp, 2), airquality)
plot(predict_response(m), show_data = TRUE)
