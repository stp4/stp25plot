
require(magrittr)
require(tidyverse)
require(stp25plot)
#devtools::install_github("NightingaleHealth/ggforestplot")

require(stp25tools)
require(stp25plot)
 

require(ggplot2)
require(stp25plot)

# load("C:/Users/wpete/Dropbox/1_Projekte/002-NURMI/791_Martina_Gregori/dummy-forest-df.Rdata")
# ggplot_forest(data) +
#   ggforce::facet_col(
#     facets = ~group,
#     scales = "free_y",
#     space = "free"
#   )








require(emmeans)
head(pigs)

pigs2 <- pigs
pigs2$percent <- factor(pigs2$percent  )
pigs2$conc <- log(pigs2$conc  )
pigs.lm1 <- lm(conc ~ source + percent, data = pigs2)
summary(pigs.lm1)

require(effects)
plot(allEffects(pigs.lm1))
plot(emmeans(pigs.lm1,
             ~ percent +source))


# Effecte
emmeans(pigs.lm1, ~ percent)
stp25stat2::extract_effect(pigs.lm1)





# Estimate
prepare_forest(pigs.lm1)







stp25stat2::extract_effect(pigs.lm1) %>%
  ggplot(aes(
    x = level,
    y = fit,
    ymin = lower,
    ymax = upper
    #,col = group,fill = group
  )) +
  #specify position here
  geom_hline(yintercept = c(3,3.5,4), lty = 2) +
  geom_vline(xintercept = seq(0, 15) + .5, lty = 2, col = "gray") +
  geom_linerange(linewidth = 3, position = position_dodge(width = .7)) +
  #specify position here too
  geom_point(
    size = 3,
    shape = 21,
    colour = "white",
    stroke = 0.5,
    position = position_dodge(width = .7)
  ) +
  # scale_fill_manual(values = barCOLS) +
  # scale_color_manual(values = dotCOLS) + 
  
  # guides( 
  #   colour = guide_legend(reverse=TRUE),
  #   fill = guide_legend(reverse=TRUE)
  # ) +
  # 
  # scale_fill_manual(values = barCOLS) +
  # scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Estimate 95% CI", limits = c(3, 4)) +
  coord_flip() +
  # GGally::geom_stripped_cols()+
  #theme_void()
  theme_minimal() +
  theme( 
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())





# dummy-Daten -------------------------------------------------------------


set.seed(1)
n <- 10 * 2 * 3 *100
DF <- data.frame(
  y = rnorm(n),
  sex = gl(2, n / 2, labels = c("male", "female")) ,
  rx = gl(3, n / 3, labels = c("Obs",  "Tev", "Tev+5FU"))[sample.int(n)],
  age = 1:n,
  bmi = rnorm(n )
)
DF <- transform(DF,
                 y = y +
                   as.numeric(sex) / 2 +
                   as.numeric(rx)
)



# daten preparieren -------------------------------------------------------

fit1 <- lm(y ~ sex + rx + age + bmi,  DF)
forest_plot(fit1, plot=FALSE)

#'
#'Alternative broom

fit.male <- lm(y ~  rx + age + bmi,  DF, subset = sex=="male" )
fit.female <- lm(y ~  rx + age + bmi,  DF, subset = sex=="female" )

male<- broom::tidy(fit.male, conf.int =TRUE)
female<- broom::tidy(fit.female, conf.int =TRUE)

dat_broom <- Rbind2(male, female, .id = "group")
dat3 <- prepare_forest(fit.male, fit.female)
dat_broom
dat3

# forest_plot  ----------------------------------------------------------




fit2 <- update(fit1, .~. * sex)
forest_plot(fit2)



# arm::coefplot -----------------------------------------------------------


arm::coefplot(fit2,   intercept=TRUE)


# ggplot_forest -----------------------------------------------------------


prepare_forest(fit.male, fit.female,
               include.indercept = FALSE,
               include.referenze = FALSE) %>%
  ggplot_forest(
  groups = group,
  title = "Associations to metabolic traits",
  xlab = "1-SD increment in cardiometabolic trait
  per 1-SD increment in biomarker concentration",
  legend.title = "legend title using guide",
  cex = 1.1,
)



# ggplot + geom_pointrange ------------------------------------------------


prepare_forest(fit.male, fit.female,
               include.indercept = FALSE,
               include.referenze = FALSE) %>%
  ggplot(aes(
    x = term, y = estimate,
    ymin = conf.low, ymax = conf.high,
    color = group, shape = group
  )) +
  scale_x_discrete(limits = rev) +
  geom_pointrange(position = position_dodge(width = .5)) +
  labs(x = "",
       y = "Estimate",
       title = "Runs scored by Virat Kohli in ODI matches") +
  guides(
    colour = guide_legend(
      title = "legend title",
      override.aes =
        list(size =  1,  linetype = 0)),
    shape = guide_legend(
      title = "legend title",
    ),        
  ) +
  stp25plot:::theme_forest2() +
  coord_flip(ylim = c(-2, 2))


# ggplot + geom_linerange -------------------------------------------------



dotCOLS = c(  "#74C476","#BAE4B3","#f9b282")
barCOLS = c("#006D2C" , "#74C476","#A63603")

dat <- prepare_forest(fit.male, fit.female,
                      include.indercept = FALSE) 


ymax <- max(dat$conf.high, na.rm = TRUE)
ymin<- min(dat$conf.low, na.rm = TRUE)

dat
dat %>%
  ggplot(aes(
    x = term ,
    y = estimate,
    ymin = conf.low ,
    ymax = conf.high,
    col = group,
    fill = group
  )) +
  #specify position here
  
  geom_hline(yintercept = c(0, 20, 40, 60), lty = 2) +
  geom_vline(xintercept = seq(0, 15) + .5, lty = 2, col = "gray") +
  geom_linerange(linewidth = 3, position = position_dodge(width = .7)) +
  #specify position here too
  geom_point(
    size = 3,
    shape = 21,
    colour = "white",
    stroke = 0.5,
    position = position_dodge(width = .7)
  ) +
  # scale_fill_manual(values = barCOLS) +
  # scale_color_manual(values = dotCOLS) + 
  
  guides( 
    colour = guide_legend(reverse=TRUE),
    fill = guide_legend(reverse=TRUE)
  ) +
  
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Estimate 95% CI", limits = c(ymin, ymax)) +
  coord_flip() +
  # GGally::geom_stripped_cols()+
  #theme_void()
  theme_minimal() +
  theme( 
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


#' # Alternative
#' 
#'  ggplot(data = dat, aes(x = group  , y = estimate)) +
#' geom_pointrange(
#'   mapping = aes(
#'     ymin = conf.low ,
#'     ymax = conf.high,
#'     color = level,
#'     shape = level
#'   ),
#'   position = ggplot2::position_dodge(width =  0.5)
#' ) +
#'   GGally::geom_stripped_cols(odd = "#00000000", even = "#11111111") +
#'   ggplot2::guides(
#'     colour = guide_legend(
#'       reverse = TRUE,
#'       title = "legend.title",
#'       override.aes =  list(size = 1)
#'     ),
#'     shape = guide_legend(reverse = TRUE, title = "legend.title")
#'   ) +
#'   theme_forest2(base_size = 12) +
#'   coord_flip() 
#' 
#' 
#' #' # Alternative
#' 
#' 
#' ggplot(data = data, aes(x = group  , y = estimate)) +
#'   geom_point(
#'     mapping = aes(
#'       color = level,
#'       shape = level
#'     ),
#'     position = ggplot2::position_dodge(width =  0.5)
#'   ) +
#'   geom_errorbar(
#'     mapping = aes(
#'       ymin = conf.low ,
#'       ymax = conf.high,
#'       color = level,
#'       width = 0.2
#'     ),
#'     position = ggplot2::position_dodge(width =  0.5)
#'   ) +
#'   coord_flip()








require(cowplot)
plot_grid(p1, p2,
            rel_widths = c(1, 1.5)
          , rel_heights  = c(1, 1.5)
          , labels = c('A', 'B'))


#' https://waterdata.usgs.gov/blog/beyond-basic-plotting/
title <-
  ggdraw() + 
  draw_label("Conditions for site 05430175", fontface = 'bold')



legend <- get_legend(p2)
# 
# p1<- p1 + 
#   theme(legend.position = "none")
p2<- p2 + 
  theme(legend.position = "non")
   

left_row <-
  plot_grid(p2,
            legend,
            ncol = 1,
            rel_heights = c(1, .5 ) 
           )
 

plot_grid(
  title,NULL,
 NULL, # p1,
  left_row,
  nrow = 2,
  #labels = c("", "", "C"),
  rel_heights = c(0.2, 1 )
)



stop()
#detach("package:ggforestplot", unload = TRUE)






#' geht nicht
# pls::coefplot(fit1)
# remove.packages("pls", lib="C:/CustomR")

# type = c( "re", "", "emm", 
 
sjPlot::plot_model(fit2)


 
 

 
# 
# require(forestplot)
# data(dfHRQoL)
# dfHRQoL <-
#   dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = labeltext)
# head(dfHRQoL)
# dfHRQoL %>%
#   group_by(group) %>%
#   forestplot(
#     clip = c(-.1, 0.075),
#     shapes_gp = fpShapesGp(
#       box = c("blue", "darkred") %>% lapply(function(x)
#         gpar(fill = x, col = "#555555")),
#       default = gpar(vertices = TRUE)
#     ),
#     ci.vertices = TRUE,
#     ci.vertices.height = 0.05,
#     boxsize = .1,
#     xlab = "EQ-5D index"
#   )
# 
# 
# 
# detach("package:forestplot", unload = TRUE)
