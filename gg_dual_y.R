library(ggplot2)
library(dplyr)
library(scales)
library(gtable)

#' Example plot
ex.df <- data_frame(n.applied = rep(seq(0, 400, by = 50), 6),
                  condition = factor(rep(c("One", "Two"), each = 27)),
                  repl = rep(1:3, each = 9, times = 2),
                  noise = c(rnorm(27, sd = 0.4), 
                            rnorm(27, mean = 0.5, sd = 0.4)),
                  n2o.kg.ha = exp(0.005+noise)*exp(n.applied*(0.005-0.001*as.numeric(condition))))

ex.p1 <- ggplot(data=df., 
              aes(x=n.applied,y=n2o.kg.ha)) + 
  geom_point(alpha = 0.25) + 
  xytheme + facet_grid(~condition) + 
  geom_smooth(method = "glm", family = gaussian(link = log)) + 
  labs(x = "N applied", y = "N2O emissions")

ex.transfac <- 0.298*44/28
ex.newtitle <- expression("CO"[2]*" equivalents (Mg CO"[2]*italic("eq")*" ha"^-1*" y"^-1*")")

#' For making a two-axis L-R faceted plot, with a constant transforming factor 
#' "transfac", given a ggplot obj "p1." and a new title "newtitle"

gg_dual_y <- function(p1., transfac = transfac, newtitle = "newtitle", extraspace = 0) {
  require(gtable)
  require(ggplot2)
  require(gridExtra)
  require(scales)
  

p2. <- p1. +
  theme(axis.title.y = element_text(angle = -90, vjust = 0,
                                    hjust = 0.5),
        axis.text.y = element_text(hjust = 0)) + 
  ylab(newtitle) + 
  scale_y_continuous(breaks=
                       scale_y_continuous()$trans$breaks(
    ggplot_build(p1.)$panel$ranges[[1]]$y.range*transfac)/transfac,
                     labels=scale_y_continuous()$trans$breaks(
                       ggplot_build(p1.)$panel$ranges[[1]]$y.range*transfac))

#extract gtable
g1.<-ggplot_gtable(ggplot_build(p1.))
g2.<-ggplot_gtable(ggplot_build(p2.))

#overlap the panel of the 2nd plot on that of the 1st plot

pp.<-c(subset(g1.$layout, name=="panel", se=t:r))
ap.<-c(subset(g1.$layout, name=="ylab", se=t:r))
g.<-gtable_add_grob(g1., g2.$grobs[which(g2.$layout$name=="panel")], pp.$t, pp.$l, pp.$b, 
                   pp.$r)

ia. <- which(g2.$layout$name == "axis-l")
ga. <- g2.$grobs[[ia.]]
ax. <- ga.$children[[2]]
ax.$widths <- rev(ax.$widths)
ax.$grobs <- rev(ax.$grobs)
ax.$grobs[[1]]$x <- ax.$grobs[[1]]$x - unit(1, "npc") - unit(0.5*extraspace-0.003, "cm")
ax.$grobs[[2]]$x <- ax.$grobs[[2]]$x - unit(0.15, "cm")- unit(0.5*extraspace-0.003, "cm")

g. <- gtable_add_cols(g., g2.$widths[g2.$layout[ia., ]$l]+unit(extraspace+0.25, "cm"), 
                      length(g.$widths) - 1)

g. <- gtable_add_grob(g., ax., min(pp.$t), length(g.$widths)-1, max(pp.$b), z = 2)

g. <- gtable_add_grob(g., g2.$grob[which(g2.$layout$name=="ylab")], min(ap.$t), 
                      length(g.$widths), max(ap.$b), clip = "off")

#' Draw to viewer
grid.draw(g.)
return(arrangeGrob(g.))
}

ex.g1 <- gg_dual_y(ex.p1, ex.transfac, ex.newtitle)

#' Make a new ggsave clone appropriate for grobs
ggsavegrob <- ggplot2::ggsave
body(ggsavegrob) <- body(ggplot2::ggsave)[-2]

#' Example save
ggsavegrob(ex.g1, file = "test.png", height = 14, width = 21, unit = "cm", dpi = 600)

#' View how it's laid out by coords
gtable_show_layout(ex.g1)
