require(ggplot2)
require(stp25tools)
require(stp25stat2)
require(stp25plot)


mtcars2 <- mtcars |> dplyr::mutate(
  vs   = factor(vs, labels = c("V-shaped", "straight")),
  am   = factor(am, labels = c("automatic", "manual")),
  cyl  = ordered(cyl),
  gear = ordered(gear),
  carb = ordered(carb)
)  |> Label(
  mpg	 = "Miles/(US) gallon",
  cyl	 = "Number of cylinders",
  disp = "Displacement (cu.in.)",
  hp	 = "Gross horsepower",
  drat = "Rear axle ratio",
  wt   = "Weight (1000 lbs)",
  qsec = "1/4 mile time",
  vs   = "Engine",
  am   = "Transmission",
  gear = "Number of forward gears",
  carb = "Number of carburetors"
)

fit <- lm(mpg ~ hp * wt + vs +  am * cyl  , data = mtcars2)

# lattice::trellis.par.set(effectsTheme())
lattice::trellis.par.set(bw_theme(farbe(n=5), lwd =1,  cex.xlab = 2))
plot_allEffects(
  fit,
  colors=c(3,4),
  xlevels = list(wt = c(1.5, 3.5, 5.5)),
  labels = get_label(mtcars2),
  main = letters[1:3],  #as.roman(1:3),
 # cex= 1, 
 layout = list(am= c(1,3)),
  space = "right",
  columns = 1

 
  #rel_widths = c(3, 4),
#   rel_heights = c(1,1,2),
# ncol =1
 
)


forest_plot(fit)


#require(effects)
#plot(allEffects(fit), multiline=TRUE)

if(0){
ef1 <-  as.data.frame(effects::effect(term = "hp", fit))
p1 <- ggplot(ef1, aes(hp, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey70",
              alpha = 0.3) +
  labs(y = 'Miles/(US) gallon',
       x = 'Gross horsepower',
       title = 'Main-Effect-Plot') +
  theme_classic()



ef2 <- as.data.frame(effects::effect("hp:wt", fit))
ef2$Weight <- factor(ef2$wt)

p2 <- ggplot(ef2, aes(hp, fit, col = Weight)) +
  geom_line() +
  labs(y = 'Miles/(US) gallon',
       x = 'Gross horsepower',
       # fill = "Weight (1000 lbs)",
       title = 'Interaction-Plot') +
  theme_classic()



ef3 <-  as.data.frame(effects::effect("cyl", fit))
p3 <- ggplot(ef3, aes(cyl, fit, group = 1)) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(x = cyl,
        ymin = lower,
        ymax = upper),
    width = 0.2,
    colour = "gray40",
    alpha = 0.9,
    linewidth = .75
  ) +
  labs(y = 'Miles/(US) gallon',
       x = 'Number of cylinders',
       title = 'Cylinder Ordinal (nicht linear')  +
  theme_classic()

ef4 <-  as.data.frame(effects::effect("am", fit))

p4 <- ggplot(ef4, aes(am, fit, group = 1)) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(x = am,
        ymin = lower,
        ymax = upper),
    width = 0.2,
    colour = "gray40",
    alpha = 0.9,
    linewidth = .75
  ) +
  labs(y = 'Miles/(US) gallon',
       x = 'Number of cylinders ',
       title = 'Transmission')  +
  ylim(c(8, 23)) +
  # xlim(c(4, 8))+
  annotate("text",
           x = 1,
           y = 12,
           label = stp25stat2::APA(fit)) +
  theme_classic()


library(patchwork)
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2)
}