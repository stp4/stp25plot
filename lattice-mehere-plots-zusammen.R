library(lattice)
data(mtcars)
# Create first plot and assign it to variable
p1 = xyplot(mpg ~ wt, data = mtcars,
            xlab = 'Car weight', ylab = 'Mileage')
# Create second plot and assign it to variable
p2 = xyplot(mpg ~ wt, group= factor(cyl), data = mtcars,
            xlab = 'Car weight', ylab = '',
            type=c('p', 'smooth'))
# print calls print.lattice, which takes a position
# argument.
print(p1, position = c(0, 0, 0.5, 1), more = TRUE)
print(p2, position = c(0.5, 0, 1, 1))


require(ggplot2)

# library(esquisse)
# esquisser(mtcars)

ggplot(mtcars) +
  aes(x = mpg) +
  geom_histogram(bins = 30L) +
  facet_wrap(vars(gear)) + theme(panel.grid.major = element_line(colour = "gray95"),
    panel.background = element_rect(fill = "gray81"),
    plot.background = element_rect(fill = "antiquewhite",
        colour = "aliceblue", linetype = "solid")) +labs(title = "titel", subtitle = "sub hallo",
    caption = "caption hallo")