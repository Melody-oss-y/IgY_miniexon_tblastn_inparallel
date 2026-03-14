library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
library(phytools)
library(ggnewscale)

setwd("~/Desktop/BIOL 398/Github")

x331 <- read.tree("pruned_Aves_331.tre")
metadata <- read.csv("working_BIOL398_spreadsheet(list_of_birds_plus_anseriforms).csv")

Anseriformes <- phytools::findMRCA(x331, tip=c("Chauna_torquata", "Aix_galericulata"))

p331 <- ggtree(x331, layout="circular", linewidth=0.1) %<+% metadata

g331 <- rotate_tree(p331, angle=155)

g331$data <- g331$data %>%
  mutate(
    norm_angle = angle > 450 | angle < 270,
    adj_angle  = ifelse(norm_angle, angle + 180, angle),
    adj_hjust  = ifelse(norm_angle, 1, 0)
  )

tip_data <- g331$data %>% filter(isTip)

c331 <- g331 +
  geom_highlight(node=Anseriformes, fill="#ffc94f", alpha=0.2) +
  xlim_expand(-7, NA) +
  
  # first tip point layer: mini_exon
  geom_tippoint(
    aes(color = mini_exon, x = x),
    size = 0.3
  ) +
  scale_colour_manual(
    name   = "Mini Exon",
    values = c("present (polyC)" = "#00FF00", "present (polyY)" = "#54c800", "absent" = "#e6446e"),
    na.translate = FALSE
  ) +
  
  new_scale_colour() +
  
  # second tip point layer: TAQGHARF_like
  geom_tippoint(
    aes(color = TAQGHARF_like, x = x + 1),
    size = 0.3
  ) +
  scale_colour_manual(
    name   = "Alignment to CH2-CH3 Intron",
    values = c("exists" = "blue", "lack" = "grey"),
    na.translate = FALSE
  ) +
  
  theme(legend.position = "left", legend.justification = "left") +
  
  geom_text(
    data  = tip_data,
    aes(x = x + 2, y = y, angle = adj_angle),
    hjust = tip_data$adj_hjust,
    label = tip_data$label,
    size  = 1
  ) +
  
  geom_nodepoint(aes(subset = !is.na(mini_exon)))

c331


