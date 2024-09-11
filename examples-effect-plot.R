
# setup -------------------------------------------------------------------

require(tidyverse)

require(latticeExtra)
require(stp25stat2)
require(cowplot)
require(lattice)
require(effects)
require(stp25tools)

set.seed(1)

# data --------------------------------------------------------------------


#' Set coefficients
alpha = 10
beta1 = .3
beta2 = -.5
beta3 = -1.1
beta4 =  0.01
n <- 200
# Generate 200 trials
A = c(rep(c(0), 110), rep(c(1), 90)) # '0' 100 times, '1' 100 times
B = rep(c(rep(c(0), 42), rep(c(1), 58)), 2) # '0'x50, '1'x50, '0'x50, '1'x50
C = round(runif(n, 80, 120))
er = rnorm(n, 0, sd = 1) # Random noise, with standard deviation of 1

# Generate your data using the regression equation
y = alpha + beta1 * A + beta2 * B + beta3 * A * B +  beta4 * C + er

# Join the variables in a data frame
DF = data.frame(
  medien = factor(A, 0:1, c("<2h", ">2h")),
  mtvt = factor(B, 0:1, c("hoch", "gering")),
  iq = C,
  y = y,
  y0 = C#+er*2
)
scale2 <- function(x)
  as.vector((x - min(x)) / (max(x) - min(x))) + 1

DF <-
  Label(
    DF,
    y = "Note",
    medien = "Medienkonsum",
    mtvt = "Motivation",
    iq = "IQ"
  )
DF <- mutate(
  DF,
  
  y1 =  scale2(y0) ,
  y1.t = scale2(y0) + .01,
  y1.h = y0 * 4,
  
  y2 =  exp(y1 * 2),
  y3 = log(scale2(y0) + .01),
  y4 = -1 / (1 + exp(y0 * 4)),
  # y5 = scale2(y0 / sqrt(1 + y0 ^ 2)),
  # y6 = scale2(y0 / (1 + abs(y0 * 5))),
  #
  #   y1.t = auto_trans(y1),
  #   y2.t = auto_trans(y2),
  #   y3.t = auto_trans(y3),
  #   y4.t = auto_trans(y4),
  
  
  y1.h =  y1,
  y2.h = log(y2),
  y3.h = exp(y3),
  y4.h =  as.vector(log(1 - (1 / y4)))
  
)

#attr(DF$y2.t, "inverse")


1 / DF$y4

#DF<- DF[sample.int(200,25),]
head(DF)

# dat <-
#   Long(y1 + y2 + y3 + y4 + y5 + y6 ~ iq, DF, key = "m", value = "y")
# xyplot(y ~ iq | m, dat, scales = list(relation = "free"))
#

dat <-
  Long(y1 +  y1.h +
         y2 + y2.h +
         y3 + y3.h +
         y4 +  y4.h ~ iq,
       DF,
       key = "m",
       value = "y")


# xyplot ------------------------------------------------------------------

#+ # Streudiagramm
#+ 
xyplot(y ~ iq | m,
       dat,
       scales = list(relation = "free"),
       layout = c(2, 4))

# lm fit ------------------------------------------------------------------

fit1 <- lm(y2 ~ iq, DF)
fit2 <- lm(y2.h ~ iq, DF)

# partial.residuals -------------------------------------------------------

p1 <-
  plot(effect("iq", fit1 , partial.residuals = TRUE), main = "linear")
p2 <-
  plot(effect("iq", fit2, partial.residuals = TRUE), main = "trans")
p3 <- plot(effect(
  "iq",
  fit2,
  partial.residuals = TRUE,
  transformation =
    list(link =  log,
         inverse = exp)
),
main = "trans+")
#+ # partial.residuals
#+ 
plot_grid(p1, p2, p3, ncol = 3)




DF$y2.t <-  auto_trans(DF$y2)
attr(DF$y2.t , "name")

fit1 <- lm(y1 ~ iq, DF)
fit2 <- lm(y2 ~ iq, DF)
fit2t <- lm(y2.t ~ iq, DF)
#plot(allEffects(fit2, partial.residuals=TRUE))



plot_grid(plot(effect("iq", fit1, partial.residuals = TRUE)),
          plot(effect("iq", fit2, partial.residuals = TRUE)),
          plot(
            effect("iq", fit2t, partial.residuals = TRUE)
            ,
            transformation =
              list(
                link = attr(DF$y2.t, "link"),
                inverse = attr(DF$y2.t, "inverse")
              )
          ))




# interaction -------------------------------------------------------------



DF %>% Tbll(y, medien, mtvt, iq)

# Fit an ANOVA
mod <- aov(y ~ medien * mtvt + iq, DF)
summary(mod)

mod <- lm(y ~ medien * mtvt + iq, DF)
summary(mod)

p1 <- xyplot(y1 ~ iq, DF, xlab = "", type = c("p", "r"))
p2 <- xyplot(y2 ~ iq, DF, xlab = "", type = c("p", "r"))
p3 <- xyplot(y3 ~ iq, DF, xlab = "", type = c("p", "r"))
plot_grid(
  p1,
  p2,
  p3,
  rel_widths = c(1, 1, 1.5),
  ncol = 3,
  labels = c('1', '2', '3')
)


p1 <- bwplot(y ~ medien, DF)
p2 <- bwplot(y ~ mtvt, DF)
p3 <- xyplot(y ~ iq, DF, xlab = "", type = c("p", "r"))
plot_grid(
  p1,
  p2,
  p3,
  rel_widths = c(1, 1, 1.5),
  ncol = 3,
  labels = c('Medienkonsum', 'Motivation', 'IQ')
)


p4 <- bwplot(y ~ medien | mtvt, DF)
p5 <- bwplot(y ~ mtvt | medien, DF)
plot_grid(
  p4,
  p5,
  p3,
  rel_widths = c(1, 1, 1),
  ncol = 3,
  labels = c('Motivation', 'Medienkonsum', 'IQ')
)



plot(allEffects(mod), main = "")



# predictorEffect ---------------------------------------------------------


plot(predictorEffect("medien", mod), main = "Medienkonsum")
plot(predictorEffect("mtvt", mod), main = "Motivation")

plot(predictorEffects(mod, ~ medien + mtvt), main = "")



plot(
  predictorEffects(mod, ~ medien + iq),
  factor.names = FALSE,
  axes = list(x = list(
    medien = list(lab = "Medienkonsum"),
    iq = list(lab = "IQ")
  )),
  main = "",
  rug = FALSE
)




plot_grid(plot(effect("medien", mod)),
          plot(effect("mtvt", mod)))


n<- 100

DF <- data.frame(y = rnorm(n),
                 a = rnorm(n),
                 b = rnorm(n),
                 c = rnorm(n),
                 d = rnorm(n),
                 e = rnorm(n),
                 f = rnorm(n),
                 g = gl(2, n/2, labels = c("Control", "Treat"))) |> 
  transform(y = y+a+b+d+as.numeric(g))


fit<- lm(y~ a+b+c+d+e+f+g, DF)
fit2 <- step(fit)

broom.helpers::tidy_all_effects( fit )
effects::allEffects(fit)

