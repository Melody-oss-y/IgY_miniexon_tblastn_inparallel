library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
library(phytools)
library(ggnewscale)

setwd("~/Desktop/BIOL 398/Github")

xM <- read.tree("pruned_Aves_331.tre")
metadata <- read.csv("working_BIOL398_spreadsheet(list_of_birds_plus_anseriforms).csv")

filteredmd <- metadata %>% 
  dplyr::filter(mini_exon %in% c("absent", "present (polyC)", "present (polyY)"))

keep_tips <- filteredmd$X
tips_to_drop <- xM$tip.label[!xM$tip.label %in% keep_tips]
pruned_tree <- drop.tip(xM, tips_to_drop)

pM <- ggtree(pruned_tree, layout = "circular", linewidth = 0.1) %<+% filteredmd

gM <- rotate_tree(pM, angle=155)

gM$data <- gM$data %>%
  mutate(
    norm_angle = angle > 450 | angle < 270,
    adj_angle  = ifelse(norm_angle, angle + 180, angle),
    adj_hjust  = ifelse(norm_angle, 1, 0)
  )

tip_data_M <- gM$data %>% filter(isTip)

gM$data <- gM$data %>%
  mutate(
    adj_angle = ifelse(angle < 180, angle, angle + 180),
    adj_hjust = ifelse(angle < 180, 0, 1)
  )

cM <- gM +
  
  #first tip point layer: mini_exon
  geom_tippoint(
    aes(color = mini_exon, x = x),
    size = 1
  ) +
  scale_colour_manual(
    name   = "Mini Exon",
    values = c("present (polyC)" = "#00FF00", "present (polyY)" = "#54c800", "absent" = "#e6446e"),
    na.value = "grey"
  ) +
  xlim_expand(-5,NA) +
  geom_text(
    data  = tip_data_M,
    aes(x = x + 1, y = y, angle = adj_angle),
    hjust = tip_data_M$adj_hjust,
    label = tip_data_M$label,
    size  = 2,
    color = "lightblue"
  )

cM

