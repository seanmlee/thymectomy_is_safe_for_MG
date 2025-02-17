

# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggsignif)


# source propensity weighting script -------------------------------------------
source("scripts/02b_logistic_psm.R")


# fit narrow model -------------------------------------------------------------
mod_narrow <- glm(
  
  narrow ~
    mg,
  family = "binomial",
  weights = w.out$weights,
  data = nsqip_logistic
  
)

summary(mod_narrow)


# fit broad model --------------------------------------------------------------
mod_broad <- glm(
  
  broad ~
    mg,
  family = "binomial",
  weights = w.out$weights,
  data = nsqip_logistic
  
)

summary(mod_broad)
