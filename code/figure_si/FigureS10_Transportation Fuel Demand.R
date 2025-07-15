# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# read csv file
df <- read.csv("C:/Users/file_locatioin/Figure S10_TransportationFuelDemand.csv")


df <- df %>% 
  mutate_at(vars(3:8), ~ . / 1000000)

df_long<-df %>%
  pivot_longer(cols = c("Diesel.Fuel","Gasoline.Fuel","Jet.Fuel","LPG.Fuel","Lubricants","Residual.Fuel.Oil"),
               names_to = "Category", 
               values_to = "Value")

df_long <- df_long %>%
  mutate(
    Year = factor(Year, ordered = TRUE),
    Scenario = recode(Scenario,
                      `Low` = "High Fuel Demand (HD)",
                      `Medium` = "Med. Fuel Demand (MD)",
                      `High` = "Low Fuel Demand (LD)" ),
    Scenario = factor(Scenario, 
                           levels = c("Low Fuel Demand (LD)", "Med. Fuel Demand (MD)","High Fuel Demand (HD)"),
                           ordered = TRUE),
    Category = recode(Category,
                      `Gasoline.Fuel` = "Gasoline", 
                      `Jet.Fuel` = "Jet-A/AVTUR",
                      `Diesel.Fuel` = "ULSD", 
                      `Residual.Fuel.Oil` = "Residual Fuel Oil", 
                      `LPG.Fuel`="LPG", 
                      `Lubricants` = "Lube"),
    Category = factor(Category,
                      levels=c("Gasoline", "Jet-A/AVTUR", "ULSD", "Residual Fuel Oil", "LPG", "Lube"),
                      ordered = TRUE))





p <- df_long %>%
  ggplot(aes(x = Year,
             y = Value,
             color = Category,
             linetype = Category,
             group = Category)) +
  geom_point(aes(shape = Category,
                 color = Category),
             size = 1) +
  geom_line(size = 0.5) +
  facet_wrap(~ Scenario) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, size = 0.15) +
  labs(x = "Year",
       y = expression("Projected Annual Transportation Fuel Demand in U.S., (mmbbl y"^{-1}*")"),
       color = "Transportation Fuel",  # modify the legend title
       linetype = "Transportation Fuel",# modify the linetype legend title if needed
       shape = "Transportation Fuel") +  # modify the Shape legend title if needed
  scale_color_brewer(palette = "Spectral") +  # change the color scheme
  theme_minimal() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
        axis.title.y = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.line = element_line(size = 0.2),
        axis.ticks = element_line(size = 0.5),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.direction = "horizontal",
        legend.margin = margin(t = 0, unit = "cm")) +  # Reduced top margin of legend to bring it closer to x-axis
  guides(fill = guide_legend(nrow = 1))


print(p)

# # Specify the directory path where you want to save the figure

save_directory <- "C:/Users/file_locatioin/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS10.png")


ggsave(file_path, p,bg="white", width = 7, height = 4.5, units = "in", dpi = 600)

