library(ggplot2)
library(dplyr)
library(tidyr)

df <- data_frame(consensus = sample(LETTERS, 72, replace = T),
                 Ae = rep(NA, 72),
                 Ap = rep(NA, 72),
                 Am = rep(NA, 72),
                 Ac = rep(NA, 72),
                 position = c(1:40, 1:32),
                 gene = c(rep("gene1", 40), rep("gene2", 32)))
df$Ae[c(5,12,35,67,50)] <- sample(c(LETTERS, "-"), 5, replace = T)
df$Ap[c(5,12,22,67,44)] <- sample(c(LETTERS, "-"), 5, replace = T)
df$Am[c(28,20,35,70,50)] <- sample(c(LETTERS, "-"), 5, replace = T)
df$Ac[c(7,31,33,55,56)] <- sample(c(LETTERS, "-"), 5, replace = T)
df2 <- df %>% gather(key = species, value = protein, consensus:Ac) %>% na.omit
df2$species <- (factor(df2$species, 
                       levels = c("consensus", "Ap", "Am", "Ae", "Ac"),
                       labels = c("consensus", "A. patens\n'Major'", "A. misera", 
                                  "A. erecta", "A. cettoana")))

ggplot(df2, aes(position, (species))) + geom_text(aes(label = "")) + 
  theme(panel.background = element_blank(),
        axis.text.y = element_text(face = "italic"),
        strip.background = element_rect(fill = "white", colour = "black")) +
  facet_grid(gene~.) +
  scale_color_manual(values = c("grey50", "white", "white", "black", "white")) + 
  scale_fill_manual(values = c("white", "blue", "red", "white", "pink")) + 
  scale_x_continuous(breaks = c(10,20,30,40)) + 
  geom_rect(aes(xmax = position+0.5, 
                xmin = position-0.5, 
                ymin = as.numeric(species)-0.45, 
                ymax = as.numeric(species)+0.45, fill = species)) + 
  geom_rect(data=df2 %>% filter(species == "A. erecta"),
            aes(xmax = position+0.5, 
                xmin = position-0.5, 
                ymin = as.numeric(species)-0.45, 
                ymax = as.numeric(species)+0.45),
            color = "black", fill = "white",
            alpha = 0.75) + 
  geom_text(aes(label = protein, color = species)) + 
  guides(color = F, fill = F) + 
  labs(x = "", y = "")
ggsave("wade.png", width=25, height=10, unit="cm", dpi=600)
