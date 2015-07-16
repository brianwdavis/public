library(ggplot2)
library(dplyr)
library(tidyr)

df <- data_frame(consensus = sample(LETTERS, 272, replace = T),
                 Ae = rep(NA, 272),
                 Ap = rep(NA, 272),
                 Am = rep(NA, 272),
                 Ac = rep(NA, 272),
                 position = c(1:272))
samples <- sample(1:272, 40)
df$Ae[sample(samples, 15)] <- sample(c(LETTERS, "-"), 15, replace = T)
df$Ap[sample(samples, 15)] <- sample(c(LETTERS, "-"), 15, replace = T)
df$Am[sample(samples, 15)] <- sample(c(LETTERS, "-"), 15, replace = T)
df$Ac[sample(samples, 15)] <- sample(c(LETTERS, "-"), 15, replace = T)
df$wrap <- ((df$position-1) %/% 100)*100+1
df$pos <- df$position - df$wrap + 1
df2 <- df %>% gather(key = species, value = protein, consensus:Ac) %>% na.omit
df2$species <- (factor(df2$species, 
                       levels = rev(c("consensus", "Ap", "Am", "Ae", "Ac")),
                       labels = rev(c("consensus", "A. patens\n'Major'", "A. misera", 
                                  "A. erecta", "A. cettoana"))))

ggplot(df2, aes(pos, (species))) + geom_text(aes(label = "")) + 
  theme(panel.background = element_blank(),
        axis.text.y = element_text(face = "italic", size =12),
        strip.background = element_rect(fill = "white", colour = "black")) +
  facet_grid(wrap~., scales = "free_x") +
  scale_color_manual(values = rev(c("grey50", "white", "white", "black", "white"))) + 
  scale_fill_manual(values = rev(c("white", "blue", "red", "white", "magenta"))) + 
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90),
                     expand = c(0,1)) +
  geom_rect(aes(xmax = pos+0.5, 
                xmin = pos-0.5, 
                ymin = as.numeric(species)-0.45, 
                ymax = as.numeric(species)+0.45, fill = species)) + 
  geom_rect(data=df2 %>% filter(species == "A. erecta"),
            aes(xmax = pos+0.5, 
                xmin = pos-0.5, 
                ymin = as.numeric(species)-0.45, 
                ymax = as.numeric(species)+0.45),
            color = "black", fill = "white",
            alpha = 0.75) + 
  geom_text(aes(label = protein, color = species)) + 
  guides(color = F, fill = F) + 
  labs(x = "", y = "")
ggsave("wade.png", width=65, height=15, unit="cm", dpi=300)
