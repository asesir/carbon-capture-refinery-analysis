# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(tidyr)

# Your data
df<- read.csv("C:/Users/file_location/Figure S16_GHGbyEnergySource.csv")


# Pivot data to longer format
df_long <- df %>%
  dplyr::select(-c(Total.refinery.processes, Hydrogen.via.CNR)) %>%
  mutate(CrudeType = interaction(API, SUL, sep = "/"),
         Fuel.Demand.Scenario = factor(Fuel.Demand.Scenario, 
                                       levels=c("Low Fuel Demand","Medium Fuel Demand","High Fuel Demand"),
                                       ordered = TRUE))%>%
  pivot_longer(cols = c("Electricity","Heat","Steam","Hydrogen.via.SMR","Other.Emissions"),
               names_to = "Category", 
               values_to = "Value")%>%
  mutate(Category = recode(Category,
                           `Electricity` = "Electricity", 
                           `Heat` = "Heat", 
                           `Steam` = "Steam", 
                           `Hydrogen.via.SMR` = "Hydrogen via SMR", 
                           `Other.Emissions` = "Other Emissions"),
         Category = factor(Category,
                           levels = c("Electricity","Heat","Steam","Hydrogen via SMR","Other Emissions"),
                           ordered = TRUE),
         Year = as.factor(Year))


my_palette <- brewer.pal(n = length(unique(df_long$Category)), name = "Spectral")

# Plot
p <- ggplot(df_long, aes(x=Year)) +
  geom_bar(aes(y = Value, fill = Category), stat = 'identity', position = 'stack') +
  facet_grid(CrudeType ~ Fuel.Demand.Scenario)+
  scale_x_discrete()+
  scale_fill_manual(values = my_palette, name = "Refinery Energy Types")+
  labs(x = "Year",
       y = expression("Volume-weighted Average U.S. Refinery GHG Emissions Intensity (kg CO"[2]*"-eq bbl"^{-1}*")")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 270, hjust = 1, size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
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
    legend.box = "vertical" 
  )
print(p)

save_directory <- "C:/Users/file_location/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS16.png")


ggsave(file_path, p, bg = "white", width = 7, height = 5, units = "in", dpi = 600)

