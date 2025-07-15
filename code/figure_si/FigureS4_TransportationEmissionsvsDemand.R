# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(grid)
library(ggthemes)
library(wesanderson)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(viridis)
# read csv file
# df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S4_Emissions_Transportation.csv")
df <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S4_Emissions_Transportation.csv")

df_select <- df %>%
  dplyr::select(Year, 
                Fuel.Demand.Scenario,  # Check for any typos in column names
                Total.Emissions,
                Total.Demands)
df_select <- df_select %>%
  mutate(Fuel.Demand.Scenario = factor(Fuel.Demand.Scenario,
                                       levels=c("BAS", "LD35", "LD50", "MD35", "MD50", "HD35", "HD50")))

x_coord <- 4714
x_coord_end <- 2103

p <- ggplot(df_select, aes(x=Total.Demands, y=Total.Emissions, color=Fuel.Demand.Scenario, group=Year)) +
  geom_point(size=2) + 
  geom_text_repel(aes(label=sprintf("%.0f", Total.Emissions)),
                  nudge_y = -5, size=2, 
                  color="black", segment.color = NA) +
  scale_x_reverse() +
  scale_color_viridis(discrete = TRUE) +
  labs(x=expression("Annual U.S. Transportation Fuel Demands, mmbbl yr "^{-1}*""),
       y=expression("Annual Estimated U.S. Transportation GHG Emissions, million tonnes CO"[2]*"-eq yr"^{-1}*" ")) +
  geom_vline(xintercept = x_coord, linetype="dashed", color="black") +
  geom_vline(xintercept = x_coord_end, linetype="dashed", color="black") +
  geom_vline(xintercept = 5539, linetype="dashed", color="darkred") +
  geom_segment(aes(x = x_coord - 750, y = 200,
                   xend = x_coord, yend = 200), 
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "black",
               show.legend = FALSE) +
  geom_segment(aes(x = x_coord_end + 750, y = 200,
                   xend = x_coord_end, yend = 200), 
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "black",
               show.legend = FALSE) +
  geom_segment(aes(x = 5539, y = 400,
                   xend = 5539, yend = 275), 
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "darkred",
               show.legend = FALSE) +
  geom_text(aes(x = 5539+400, y = 200, label = "Demand Estimated\nby EIA"), # Adjust 'x' and 'y' as needed
            hjust = 0, vjust = 0.5, size = 2, color = "darkred") +
  geom_text(aes(x = x_coord - 800, y = 200, label = "Demand Forecasted\nby NREL's EFS"), # Adjust 'x' and 'y' as needed
            hjust = 0, vjust = 0.5, size = 2, color = "black")+
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1.5,0.5), "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.title.y = element_text(size=7, hjust=0.75),
        axis.text.y = element_text(size=7),
        axis.title.x = element_text(size=7, margin = margin(t = 20, b = -15, unit = "pt"), vjust = 1),
        axis.text.x = element_text(size=7),
        legend.text = element_text(size=6), 
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t=5, unit="pt"), # Negative top margin to move legend up closer to the plot
        legend.box.margin = margin(t=5, b=-10, unit="pt"), # Adjust box margin if necessary
        # legend.justification = c("right", "top"),
        # legend.box.just = "left",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.4, "lines"), 
        legend.key.width = unit(0.8, "lines"),
        legend.key.height = unit(0.4, "lines")) +
  coord_cartesian(ylim = c(500, 2500),
                  clip = "off")+
  guides(fill = guide_legend(nrow = 1))

p <- p + 
  annotate("segment", x = x_coord, xend = x_coord, 
           y = -Inf, yend = 120,
           color = "black") +
  annotate("segment", x = x_coord_end, xend = x_coord_end, 
           y = -Inf, yend = 120,
           color = "black")
p
# # Specify the directory path where you want to save the figure

save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS4.png")


ggsave(file_path, p,bg="white", width = 4.5, height = 4.5, units = "in", dpi = 600)

