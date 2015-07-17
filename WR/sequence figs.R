library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)

#### Sequence F3'5'H ----
f3p5ph.string <- scan("F3prime5primeH.afa", what = "character", sep = "")
spnum <- sum(str_detect(f3p5ph.string, "[>]"))
strnum <- length(str_detect(f3p5ph.string, "[>]"))
spnames <- str_replace(f3p5ph.string[str_detect(f3p5ph.string, "[>]")], "[>]", "")
strdf <- data_frame(f3p5ph.string[2:(strnum/spnum)],
           f3p5ph.string[(strnum/spnum+2):(2*strnum/spnum)],
           f3p5ph.string[(2*strnum/spnum+2):(3*strnum/spnum)],
           f3p5ph.string[(3*strnum/spnum+2):(4*strnum/spnum)])
names(strdf) <- spnames
strdf$seg <- 1:(strnum/spnum-1)

strdfsep <- strdf %>% gather(species, seq, -seg) %>% 
  separate(seq, paste(rep("seqpos", 60), 1:60, sep = ""), sep = 1:59) %>% 
  gather(key = seqpos, value = let, seqpos1:seqpos60) %>% 
  mutate(position = as.numeric(str_replace(seqpos, "seqpos", "")) + 60*(seg-1)) %>% 
  filter(let != "")

f3p5ph <- strdfsep %>% group_by(position) %>% 
  mutate(noncons = n_distinct(let),
         noncons2 = ifelse(noncons>1, sort(-table(let))[[1]]==sort(-table(let))[[2]], F),
         consensus = ifelse(noncons2 == F, names(sort(-table(let)))[1], ""),
         sublet = ifelse(let == consensus, "", let)) 
f3p5ph$wrap <- ((f3p5ph$position-1) %/% 100)*100+1
f3p5ph$pos <- f3p5ph$position - f3p5ph$wrap + 1
f3p5ph$species <- factor(f3p5ph$species, levels = rev(sort(spnames)))
f3p5ph$splabel <- factor(f3p5ph$species, levels = rev(sort(spnames)),
                         labels = substr(rev(sort(spnames)), 1, 2))



ggplot(f3p5ph %>% filter(sublet != ""), aes(pos-0.5, (splabel))) + 
  geom_text(aes(label = "")) + 
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey70"),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face = "italic", size =12, color = "grey10"),
        strip.background = element_rect(fill = "white", colour = "black")) +
  facet_grid(wrap~., scales = "free_x") +
  scale_color_manual(values = rev(c("white", "white", "black", "white"))) + 
  scale_fill_manual(values = rev(c("blue", "red", "white", "magenta"))) + 
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90),
                     expand = c(0,1)) +
  geom_text(data = f3p5ph, 
            aes(x = pos-0.5, y = splabel, label = consensus), 
            alpha = 0.2) + 
  geom_rect(aes(xmax = pos, 
                xmin = pos-1, 
                ymin = as.numeric(splabel)-0.45, 
                ymax = as.numeric(splabel)+0.45, fill = splabel)) +
  geom_rect(data=f3p5ph %>% filter(splabel == "Am", sublet != ""),
            aes(xmax = pos, 
                xmin = pos-1, 
                ymin = as.numeric(splabel)-0.45, 
                ymax = as.numeric(splabel)+0.45),
            color = "black", fill = "white",
            alpha = 0.75) + 
  geom_text(aes(label = sublet, color = splabel)) + 
  guides(color = F, fill = F) + 
  labs(x = "", y = "")
ggsave("f3p5ph.png", width=45, height=20, unit="cm", dpi=300)


#### Sequence F3'H, variables reused ----
f3ph.string <- scan("F3primeH.afa", what = "character", sep = "")
spnum <- sum(str_detect(f3ph.string, "[>]"))
strnum <- length(str_detect(f3ph.string, "[>]"))
spnames <- str_replace(f3ph.string[str_detect(f3ph.string, "[>]")], "[>]", "")
strdf <- data_frame(f3ph.string[2:(strnum/spnum)],
                    f3ph.string[(strnum/spnum+2):(2*strnum/spnum)],
                    f3ph.string[(2*strnum/spnum+2):(3*strnum/spnum)],
                    f3ph.string[(3*strnum/spnum+2):(4*strnum/spnum)])
names(strdf) <- spnames
strdf$seg <- 1:(strnum/spnum-1)

strdfsep <- strdf %>% gather(species, seq, -seg) %>% 
  separate(seq, paste(rep("seqpos", 60), 1:60, sep = ""), sep = 1:59) %>% 
  gather(key = seqpos, value = let, seqpos1:seqpos60) %>% 
  mutate(position = as.numeric(str_replace(seqpos, "seqpos", "")) + 60*(seg-1)) %>% 
  filter(let != "")

f3ph <- strdfsep %>% group_by(position) %>% 
  mutate(noncons = n_distinct(let),
         noncons2 = ifelse(noncons>1, sort(-table(let))[[1]]==sort(-table(let))[[2]], F),
         consensus = ifelse(noncons2 == F, names(sort(-table(let)))[1], ""),
         sublet = ifelse(let == consensus, "", let)) 
f3ph$wrap <- ((f3ph$position-1) %/% 100)*100+1
f3ph$pos <- f3ph$position - f3ph$wrap + 1
f3ph$species <- factor(f3ph$species, levels = rev(sort(spnames)))
f3ph$splabel <- factor(f3ph$species, levels = rev(sort(spnames)),
                         labels = substr(rev(sort(spnames)), 1, 2))



ggplot(f3ph %>% filter(sublet != ""), aes(pos-0.5, (splabel))) + 
  geom_text(aes(label = "")) + 
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey70"),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face = "italic", size =12, color = "grey10"),
        strip.background = element_rect(fill = "white", colour = "black")) +
  facet_grid(wrap~., scales = "free_x") +
  scale_color_manual(values = rev(c("white", "white", "black", "white"))) + 
  scale_fill_manual(values = rev(c("blue", "red", "white", "magenta"))) + 
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90),
                     expand = c(0,1)) +
  geom_text(data = f3ph, 
            aes(x = pos-0.5, y = splabel, label = consensus), 
            alpha = 0.2) + 
  geom_rect(aes(xmax = pos, 
                xmin = pos-1, 
                ymin = as.numeric(splabel)-0.45, 
                ymax = as.numeric(splabel)+0.45, fill = splabel)) +
  geom_rect(data=f3ph %>% filter(splabel == "Am", sublet != ""),
            aes(xmax = pos, 
                xmin = pos-1, 
                ymin = as.numeric(splabel)-0.45, 
                ymax = as.numeric(splabel)+0.45),
            color = "black", fill = "white",
            alpha = 0.75) + 
  geom_text(aes(label = sublet, color = splabel)) + 
  guides(color = F, fill = F) + 
  labs(x = "", y = "")
ggsave("f3ph.png", width=45, height=20, unit="cm", dpi=300)
