library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)

wade <- read.csv("wade.csv", stringsAsFactors = F)
wade2 <- wade %>% gather(key = stage, value = count, bud:pre.A)
wade2$stage <- factor(wade2$stage, levels = c("bud", "stage.D", "pre.A"),
                      labels = c("bud", "stage D", "pre-A"))


ggplot(wade2, aes(stage, count)) + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black"),
        legend.position = c(0.75,0.25),
        legend.key = element_rect(fill = "white"),
        legend.justification = c("center", "top"),
        legend.key.width = unit(2, "cm")) + 
  scale_shape_manual(values = c(0,1,2,5)) + 
  geom_point(aes(shape = species)) + 
  geom_line(aes(color = species, group = species)) + 
  facet_wrap(~gene, scales = "free", ncol = 2) + 
  labs(x = "")
ggsave("wade.png", width=25, height=25, unit="cm", dpi=600)
