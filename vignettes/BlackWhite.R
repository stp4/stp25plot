## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)




require(tidyverse)
require(stp25tools)
require(stp25plot)

require(emmeans) # for data pigs
require(effects)
require(cowplot)
#require(gridExtra)




## -----------------------------------------------------------------------------
pigs.lm1 <- lm(conc ~ source * percent, data = pigs)

