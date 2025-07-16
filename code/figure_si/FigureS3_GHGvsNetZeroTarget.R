########################Loading Library##########################################################
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(measurements)
library(progress)
library(zoo)
library(sde)
library(DiagrammeR)
library(rsvg)
library(ggplot2)
library(cowplot)
library(ggridges)
library(treemap)
library(RColorBrewer)
library(ggthemes)
library(latticeExtra)
library(lattice)
library(vcd)
library(stringr)
library(wesanderson)
####################Color Panel #############################################
plot_colors<- c(
  #"#1ba3c6",
  "#2cb5c0",
  #"#30bcad",
  #"#21B087",
  #"#33a65c",
  #"#57a337",
  #"#a2b627",
  #"#d5bb21",
  "#f8b620",
  #"#f89217",
  #"#f06719",
  "#e03426",
  #"#f64971",
  #"#fc719e",
  #"#eb73b3",
  "#ce69be",
  #"#a26dc2",
  #"#7873c0",
  "#4f7cba"
)


plot_colors_ps <-c(  
  "#30bcad",
  "#21B087",
  "#33a65c",
  "#57a337",
  "#a2b627",
  "#d5bb21",
  "#f89217",
  "#f06719",
  "#f64971",
  "#fc719e",
  "#eb73b3",
  "#ce69be",
  "#a26dc2",
  "#7873c0",
  "#4f7cba")


plot_colors_config <- c(
  "#440154",
  "#21908c",
  "#fde725"
)


df_emissions <- read.csv("C:/Users/file_location/Figure S3_GHGvsNetZeroTarget.csv")

df_emissions_long <- df_emissions %>%
  pivot_longer(-c(1:3),
               names_to = "Capture.Scenario",
               values_to = "Emissions") %>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "HD",
                           `MEDIUM` = "MD",
                           `HIGH` = "LD" ))

df_emissions_long$Scenario <- factor(df_emissions_long$Scenario, levels=c("LD","MD","HD"))

# Modify the line type based on Capture.Scenario
p2 <- ggplot(df_emissions_long, aes(x = Year, y = Emissions, group = Capture.Scenario, 
                                    color = Capture.Scenario, linetype = Capture.Scenario)) +
  geom_line(linewidth = 0.5) +
  geom_point() +
  facet_grid(Scenario ~ Crude.Quality) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50), oob = scales::oob_squish) +
  scale_color_manual(values = plot_colors) +
  scale_linetype_manual(values = c("Net.Zero.2035" = "solid", 
                                   "Net.Zero.2050" = "solid", 
                                   "No.Carbon.Capture" = "dashed",
                                   "Min.Carbon.Capture" = "dotted", 
                                   "Max.Carbon.Capture" = "dotdash")) +
  theme_classic() +
  labs(title = "Estimated Annual U.S. Refinery GHG Emissions against Net-Zero Target",
       x = "Year", color = "Capture.Scenario",
       y = expression("Annual Refinery GHG Emissions (million tonnes CO"[2]*"-eq yr"^{-1}*")")) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
        plot.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 6, hjust = 0.75),
        axis.text.y = element_text(size = 5.5),
        axis.title.x = element_text(size = 6, margin = margin(t = 15, unit = "pt")),
        axis.text.x = element_text(size = 5.5),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "lines"),
        legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.margin = margin(2, 2, 2, 2),
        legend.direction = "horizontal",
        legend.box = "vertical",
        strip.text.x = element_text(size = 6))
p2


save_directory <- "C:/Users/file_location/"

file_path <- file.path(save_directory, "FigureS3.png")

ggsave(file_path, p2, bg="white", width = 4.5, height = 5, units = "in", dpi = 600)