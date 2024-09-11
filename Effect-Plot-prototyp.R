
# setup -------------------------------------------------------------------


require(tidyverse)
require(stp25tools)
require(stp25plot)
require(lattice)
require(emmeans) # for data pigs
require(effects)
require(cowplot)







axis.padding<- lattice.getOption("axis.padding")

layout.heights <- lattice.getOption("layout.heights")
layout.widths <- lattice.getOption("layout.widths")

#lattice::trellis.par.set(bw_theme( ))
lattice.options(
  # layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=
    list(
      left.padding=list(x=.4), 
      right.padding=list(x=1)
    ),
  axis.padding =list(numeric=0.2)
)



 
pigs.lm1 <- lm(conc ~ source * percent, data = pigs)
 
plot(
  emmeans(pigs.lm1, ~ percent | source, 
          at=list(percent = c(11,14,17))
  ),
  
  CIs=TRUE, # confidence intervals
  PIs=TRUE, # prediction intervals 
  comparisons=TRUE, # comparison arrows”
  colors=c("black","dark grey","grey","red"),
  #  alpha=0.05,
  #  adjust="tukey",
  ylab = "",
  xlab = "plasma leucine [mcg/ml]"
  
  
) +
  theme_bw()



#emmip_lattice(pigs.lm1, conc~source*percent)


dat<-allEffects(pigs.lm1) %>% as.data.frame()




plot_grid(
  lattice::barchart(
    fit  ~ factor(percent)| source,
    dat[[1]],
    origin = 0,
    ylab = "plasma leucine [mcg/ml]",
    ylim = c(-2, 60),
    main = "lattice",
    layout= c(3,1)
  ),
  lattice::xyplot(
    fit  ~ percent| source,
    dat[[1]],
    origin = 0,
    ylab = "plasma leucine [mcg/ml]",
    ylim = c(-2, 60),
    main = "lattice",
    layout= c(3,1)
  ),
  plot(
    effects::effect("source*percent", pigs.lm1),
    main = "effect",
    factor.names = FALSE, rug =FALSE,
    #lty = 0,
    ylab = "plasma leucine [mcg/ml]",
    xlab = "",
    ylim = c(-2, 60),
    layout= c(3,1)
  ),
  
  plot(
    emmeans(pigs.lm1,  ~source|percent, 
            at=list(percent = c(11,14,17))
    ),
    main = "emmeans",
    ylab = "",
    xlab = "plasma leucine [mcg/ml]"
  ),
  
  plot(
    emmeans(pigs.lm1,  ~ percent | source, 
            at=list(percent = c(11,14,17))
    ),
    main = "emmeans",
    ylab = "",
    xlab = "plasma leucine [mcg/ml]"
  ),
  
  
  plot(  ggeffects::ggpredict(pigs.lm1, terms = c("percent", "source"))   )
)

#' Wenn lattice nicht geladen wird wird impliziet
#' lattice::trellis.par.set(effectsTheme())
#' aufgerufen


# effects -----------------------------------------------------------------


p1 <-
  plot(effects::effect("source*percent", pigs.lm1),
       main = "effect",
       rug = FALSE)
#update(p1, layout = c(1,3))

axes_setting <-
  list(
    x = list(
      cex = .75,
      source = list(lab = "Source of protein in the diet"),
      percent = list(lab = "Protein percentage in the diet")
      
    ),
    y = list(
      cex = .7,
      lab = list("Concentration of free\nplasma leucine, in mcg/m", cex = .8),
      ticks = list(at = c(20, 40, 60))
    )
  )
p2 <- plot(
  effect("source*percent", pigs.lm1),
  main = "effect + Update",
  rug = FALSE,
  axes = axes_setting,
  factor.names = FALSE
)
plot_grid(p1,
          update(
            p2,
            par.settings = bw_theme(),
            xlim = c(8, 19),
            ylim = c(15, 70)
          ),
          nrow = 1)

 
#' wenn lattice als erstes aufgerufen wird
#' library (lattice)
#' dann passiert folgendes

lattice::trellis.par.set(standard_theme())

p3 <- plot(effect("source*percent", pigs.lm1),
           main = "",  rug = FALSE)
p4 <- plot(
  effect("source*percent", pigs.lm1),
  main = "",
  rug = FALSE,
  axes = axes_setting,
  factor.names = FALSE
)

plot_grid(p3,
          update(
            p4,
            par.settings = bw_theme(),
            xlim = c(8, 19),
            ylim = c(15, 70)
          ),
          nrow = 1)

 
#' und endgueltig bekommt man die Grafik
#' in Schrarz/Weiss


lattice::trellis.par.set(bw_theme())

p6 <- plot(
  effect("source*percent", pigs.lm1),
  main = "",
  rug = FALSE,
  axes = axes_setting,
  layout =c(3,1),
  factor.names = FALSE
)

plot_grid(p1,
          p3,
          update(
            p6,
            par.settings = bw_theme(),
            xlim = c(8, 19),
            ylim = c(15, 70)
          ),
          labels = c('p1', 'p3', 'p6'),
          nrow = 2)



lim<- car::logit(c(0.05, 0.65 ))
# age + preferred.distance +running.expirience3+
pigs.lm2 <-
  glm(I(conc > 32) ~ source + percent,
      data = pigs,
      family = binomial())


dat <- effect("source", pigs.lm1) %>%  fix_to_df()
dat

# Effecte
emmeans(pigs.lm1, ~ source)



effect("source", pigs.lm1) %>%  fix_to_df()


inv.logit <-  function(x)
  1 / (1 + exp(-x))
car::logit(.1)


dat<-allEffects(pigs.lm1) %>% as.data.frame()
#stp25stat2::extract_effect(pigs.lm1)
plot_grid(
  lattice::barchart(
    fit  ~ factor(percent)| source,
    dat[[1]],
    origin = 0,
    ylab = "plasma leucine [mcg/ml]",
    ylim = c(-2, 60),
    main = "lattice",
    layout= c(3,1)
  ),
  lattice::xyplot(
    fit  ~ percent| source,
    dat[[1]],
    origin = 0,
    ylab = "plasma leucine [mcg/ml]",
    ylim = c(-2, 60),
    main = "lattice",
    layout= c(3,1)
  ),
  plot(
    effect("source*percent", pigs.lm1),
    main = "effect",
    factor.names = FALSE, rug =FALSE,
    #lty = 0,
    ylab = "plasma leucine [mcg/ml]",
    xlab = "",
    ylim = c(-2, 60),
    layout= c(3,1)
  ),
  
  emmip(
    pigs.lm1,
    ~  source+percent,
    CIs = TRUE,
    main = "emmip",
    ylab = "plasma leucine [mcg/ml]",
    CIarg = list(lwd = 4, alpha = 0.5),
    dotarg = list(pch = 15, cex = 3),
    linearg = list(linetype = 0)
  ),
  
  plot(
    emmeans(pigs.lm1,  ~  source),
    main = "emmeans",
    ylab = "",
    xlab = "plasma leucine [mcg/ml]"
  )
  
  
  
)

   




stop()

effectsTheme()
at <- c(1,3,5,7,9)/10
  plot_grid(
  plot(
    effect("source*percent", pigs.lm1),
    main = "",
    factor.names = FALSE,
    lty = 0,
    ylab = "plasma leucine [mcg/ml]",
    xlab = "",
    ylim = c(-2, 60)
  ),
  
  plot(
    effect("source", pigs.lm2),
    main = "",
    xlab="",
    lty = 0,
    ylim= range(car::logit(at)),
    axes = list(
      y = list(cex = .70,
               lab = "plasma leucine [mcg/ml]",
               ticks = list(
                 at = at
                   #round(inv.logit(-5:5), 2)
                 )
               ),
      x = list(cex = .70)
      )
    ),
  ncol=3
  )
  
  
 
 
 