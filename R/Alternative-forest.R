# # require(magrittr)
# # require(tidyverse)
#   require(stp25plot)
# # #devtools::install_github("NightingaleHealth/ggforestplot")
# # require(stp25tools)
# require(ggplot2)
# 
# #library("extrafont")
# require(survival)
# #data(package = "survival", colon)
# colon<- stp25tools::Label(colon, sex="Geschlecht")
# #colon$sex <- factor(as.numeric(colon$sex), 0:1, c("male", "female"))
# 
# fit1 <- lm(status ~ sex + rx + adhere, data = colon)
# fit2 <- lm(status ~ sex + rx* adhere, data = colon)
# 
# 
# A <- prepare_forest("Base Model" = fit1,
#                     fit2,
#                     include.referenze=FALES
# )
# A
# 
# A <- prepare_forest(  fit1 )
# library(broom.helpers)
# library(gtsummary)
# library(ggplot2)
# library(dplyr)
# model_logit <- glm(response ~ trt + grade, trial, family = binomial)
# broom::tidy(model_logit)
# 
# 
# 
# 
# 
# 
#  model_get_n(fit1)
# 
#  
#  fit1 <- lm(status ~ sex + rx + adhere -1, data = colon)
#  
#  tidy_forest <-
#   fit1 |>
#   # perform initial tidying of the model
#   tidy_and_attach(exponentiate = F, conf.int = TRUE) |>
#   # adding in the reference row for categorical variables
#   tidy_add_reference_rows() |>
#   # adding a reference value to appear in plot
#   tidy_add_estimate_to_reference_rows() |>
#   # adding the variable labels
#   tidy_add_term_labels() |>
#   # removing intercept estimate from model
#  # tidy_remove_intercept() |>
#   tidy_add_n()
#  
#  tidy_forest[c("reference_row","label", "n_obs",
#                "estimate",  #,"std.error","statistic",,"conf.low","conf.high"
#                "p.value")]
#  
#  stp25stat2::Tbll_reg_long(fit1)
#  