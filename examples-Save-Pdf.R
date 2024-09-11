setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25plot")
require(cowplot)


ttl <- "Please revise the hyphen (-) into a minus sign"
ttl <- gsub("-" , "\u2212 -",  ttl)
ttl
title <-  cowplot::ggdraw() + 
  cowplot::draw_label(label=ttl, fontface = 'bold')

p1<- cowplot::plot_grid(
  title, NULL,
  function(x) plot(1),
  function(x) plot(1:10), 
  nrow = 2,
  #labels = c("", "", "C"),
  rel_heights = c(0.05, 1 )
)
p1

stp25output2::SavePlot("SavePlot-cairo", out.type = "cairo")
stp25output2::SavePlot("SavePlot-pdf" )
#  cowplot::save_plot(  "fig/test-save_plot.pdf", p1 )
  
  ggplot2::ggsave("fig/test-ggsave.pdf")
  ggplot2::ggsave("fig/test-ggsave+cairo.pdf", device = cairo_pdf)
  ggplot2::ggsave("fig/test-ggsave.eps")
  ggplot2::ggsave("fig/test-ggsave+cairo.eps", device = cairo_ps)
 # ggplot2::ggsave("test-ggsave.eps")

#  pdf("fig/test-pdfExample1.pdf")
#print(p1)
#dev.off()