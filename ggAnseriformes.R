library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
library(phytools)
library(ggnewscale)

setwd("~/Desktop/BIOL 398/Github")

xA <- read.tree("pruned_Aves_331.tre")
metadata <- read.csv("working_BIOL398_spreadsheet(list_of_birds_plus_anseriforms).csv")
filteredmd <- metadata %>% dplyr::filter(Anseriformes == "y")
keep_tips <- metadata$X[metadata$Anseriformes == "y"]
tips_to_drop <- xA$tip.label[!xA$tip.label %in% keep_tips]
pruned_tree <- drop.tip(xA, tips_to_drop)

gA <- ggtree(pruned_tree, layout = "circular", linewidth = 0.1) %<+% filteredmd

gA$data <- gA$data %>%
  mutate(
    adj_angle = ifelse(angle < 180, angle, angle + 180),
    adj_hjust = ifelse(angle < 180, 0, 1)
  )

cA <- gA +
  
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
  geom_tiplab(aes(angle = adj_angle, hjust = adj_hjust, x = x + 1, color = "lightblue"), size = 2)

cA

