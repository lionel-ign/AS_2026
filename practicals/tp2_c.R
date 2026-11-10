# TP2

## read-in data
setwd("~/LIF/Enseignement/APT_2026/")
dat <- read.csv("data/dat_tp2.csv")
str(dat)
summary(dat)
dat_nona <- na.omit(dat)

## scaling variables
dat_nona$stroph <- scale(dat_nona$troph_r)[,1]
dat_nona$szp <- scale(dat_nona$zp)[,1]
dat_nona$sreser <- scale(dat_nona$reserutile)[,1]

## first model
m1 <- lm(biomass ~ sreser + szp + stroph, data = dat_nona)
summary(m1)

## check model assumptions
library(DHARMa)
ss <- simulateResiduals(m1)
plot(ss)

## plot model predictions
library(ggeffects)
pp <- predict_response(m1, terms = c("stroph", "szp", "sreser"))
plot(pp)

## second model
dat_nona$fowner <- factor(dat_nona$ownership, levels = c("Forêts domaniales RF",
                                                         "Autres forêts publiques RF",
                                                         "Forêts privées et autres - En PSG",
                                                         "Forêts privées et autres - Hors PSG"))

m2 <- lm(biomass ~ fowner + stroph, data = dat_nona)
summary(m2)

pp <- predict_response(m2, terms = "fowner")
plot(pp)

## third model
m3 <- lm(biomass ~ stroph + fowner + stroph:fowner, data = dat_nona)
m3 <- lm(biomass ~ stroph * fowner, data = dat_nona)
summary(m3)

pp <- predict_response(m3, terms = c("stroph", "fowner"))
plot(pp)

## fourth - fifth
m4 <- lm(biomass ~ troph_r * zp, data = dat_nona)
m5 <- lm(biomass ~ stroph * szp, data = dat_nona)

## plotting
library(tidyverse)
ggplot(dat, aes(x=zp, y=biomass)) +
  stat_smooth() +
  labs(x = "Elevation (m)",
       y = "Biomass (t/ha)")

## six - log transformation
m6 <- lm(biomass ~ log(zp), data = dat_nona)

pp <- predict_response(m6)
plot(pp)

## seven - polynomes
m7 <- lm(biomass ~ mqd + I(mqd^2), dat = dat_nona) # correlated - interprete regression slope
m8 <- lm(biomass ~ poly(mqd, 2), dat = dat_nona) # uncorrelated polynomes - interpretation not possible

pp7 <- predict_response(m7)
pp8 <- predict_response(m8)

plot(pp7)
plot(pp8)

