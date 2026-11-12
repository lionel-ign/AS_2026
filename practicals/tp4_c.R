# tree cuts TP4

## load the data
dat <- read.csv("~/LIF/Enseignement/APT_2026/data/dat_tp4.csv")
summary(dat)
table(dat$pro_psg)
table(dat$pnr)
table(dat$expl)

## fit our binomial GLM
dat$p_cut <- with(dat, nb_cut / nb_tree)
m1 <- glm(cbind(nb_cut, nb_tree-nb_cut) ~ pro_psg + pnr + expl, data = dat,
         family = "binomial")
m2 <- glm(p_cut ~ pro_psg + pnr + expl, data = dat,
         family = "binomial", weights = dat$nb_tree)
summary(m1)
summary(m2)

## beta-binomial distribution
library(glmmTMB)
m2 <- glmmTMB(p_cut ~ pro_psg + pnr + expl, data = dat,
          family = "betabinomial", weights = dat$nb_tree)
m1 <- glmmTMB(cbind(nb_cut, nb_tree - nb_cut) ~ pro_psg + pnr + expl, data = dat,
              family = "betabinomial")

## check model assumption
library(DHARMa)
ss <- simulateResiduals(m2)
plot(ss)

## interpretation of the effect
library(ggeffects)
pp <- predict_response(m1)
plot(pp)
