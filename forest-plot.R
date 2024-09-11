### Forest

require(magrittr)
require(tidyverse)
require(stp25plot)
#devtools::install_github("NightingaleHealth/ggforestplot")
require(stp25tools)
require(stp25plot)

require(ggplot2)

#'
#' forest_plot() Tabelle und Vertikaler-Plot gestohlen von survminer::ggforest()
#' 
#' ggplot_forest() Vertikaler-Plot ohne Tabelle aber dafuer sind Gruppen moeglich -
#' stolen from https://github.com/NightingaleHealth/ggforestplot
#' 

model1 <- lm(mpg ~ wt, data = mtcars)
model2 <- lm(mpg ~ wt + cyl, data = mtcars)
model3 <- lm(mpg ~ wt * cyl, data = mtcars)
prepare_forest(model1, model2, model3)


#try(data(package = "survival"), silent = TRUE)
require(survival)
#data(package = "survival", colon)
colon<- Label(colon, sex="Geschlecht")

#+ forest-2, fig.height=4, fig.width=7
fit1 <- lm(status ~ sex + rx + adhere,data = colon)
forest_plot(fit1)

#+ forest-2, fig.height=4, fig.width=7

fit2 <- glm(status ~ sex + rx + adhere,
            data = colon, family = binomial())

forest_plot(fit2)

#+ forest-3, fig.height=4, fig.width=7

fit3 <- coxph(Surv(time, status) ~ sex + rx + adhere,
              data = colon)

forest_plot(fit3, colon)


#+  include=FALSE}
set.seed(1)
n <- 10 * 2 * 3 *100
dat <- data.frame(
  y = rnorm(n),
  sex = gl(2, n / 2, labels = c("male", "female")) ,
  rx = gl(3, n / 3, labels = c("Obs",  "Tev", "Tev+5FU"))[sample.int(n)],
  age = 1:n,
  bmi = rnorm(n )
)
dat <- transform(dat,
                 y = y +
                   as.numeric(sex) / 2 +
                   as.numeric(rx)
)






fit1 <- lm(y ~ sex + rx + age + bmi,  dat)
tab<-forest_plot(fit1, plot=FALSE)

tab





ggplot_forest(tab)





ggplot_table(
  data.frame(
    var = c("Intercept", "Sex", "Sex", "Alter"),
    level = c(NA, "male", "female", NA),
    N = c(NA, 25, 47, 25+47),
    estimate = c(-.77, NA, .51 , .4),
    conf.low = c(-1.53, NA, -.17, .2),
    conf.high = c(-0.1, NA, 1.2, .6),
    p.value = c(0.046, NA, 0.1407, 0.0021)
  )
)




### Balken mít Errorbars



mycol <- c("#0433FF",
           "#00F801",
           "#FF2600",
           "#918E00",
           "#FE9300")

data <- data.frame(
  name = c("0h", "1h", "24h"),
  value = c(1.4,	2.6,	2) / 100,
  sd1 =   c(1.2,	2.8,	1.9) / 100,
  sd2 =   c(2,  	0.75,	2.4) / 100
)

# Most basic error bar
ggplot(data) +
  geom_bar(
    aes(x = name, y = value),
    stat = "identity",
    fill = "#64B2FC",
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(x = name,
        ymin = sd1,
        ymax = sd2),
    width = 0.2,
    colour = "gray40",
    alpha = 0.9,
    size = 1
  ) + scale_y_continuous(labels = scales::percent) +
  labs(title = "Non-viable cells",
       # subtitle = "(1973-74)",
       # caption = "Data from the 1974 Motor Trend US magazine.",
       #   tag = "B",
       x = "Time [h]",
       y = "",) + 
  theme_classic()


