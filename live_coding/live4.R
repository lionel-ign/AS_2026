# live coding glm

## oiseaux data
oiseaux <- read.csv("data/oiseaux.csv")
summary(oiseaux)
## fit first glm
m <- glm(count ~ habitat * temp_C, data = oiseaux,
         family = "poisson")
summary(m)

## check model assumptions
library(DHARMa)
ss <- simulateResiduals(m)
plot(ss)
testDispersion(ss)

## negative binomial
m <- glm.nb(count ~ habitat * temp_C, data = oiseaux)
summary(m)

ss <- simulateResiduals(m)
plot(ss)
testDispersion(ss)

## plot regression line
library(ggeffects)
pp <- predict_response(m, terms = c("temp_C", "habitat"))
plot(pp)
