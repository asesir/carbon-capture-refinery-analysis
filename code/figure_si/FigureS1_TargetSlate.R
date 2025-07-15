# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# read csv file
# df <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S1_TargetSlate.csv")
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S1_TargetSlate.csv")
# df <- read.csv("D:/PhD/PhD Programe UofC/LCA/Paper 01/Data/TargetSlate.csv")


df <- df %>%
  mutate(SCENARIO = recode(SCENARIO,
                                `LOW` = "High Fuel Demand (HD)",
                                `MEDIUM` = "Med. Fuel Demand (MD)",
                                `HIGH` = "Low Fuel Demand (LD)"))

df <- df %>%
  mutate(PADD = case_when(
    PADD %in% c("1", "2", "3", "4", "5") ~ paste("PADD", PADD), # Add "PADD" prefix if PADD is numeric
    TRUE ~ PADD # Keep original if it's "U.S."
  ))

df$SCENARIO <- factor(df$SCENARIO, levels=c("Low Fuel Demand (LD)","Med. Fuel Demand (MD)","High Fuel Demand (HD)"))

df$YEAR <- factor(df$YEAR, levels = c("2019", "2035", "2050"))

df <- df %>%
  mutate(Total = rowSums(across(Blended.Gasoline:Lube), na.rm = TRUE))


df_long<-df %>%
  pivot_longer(cols = c("Blended.Gasoline","Jet.A.AVTUR","ULSD","Fuel.Oil","Coke","Liquid.Heavy.Ends","Liquified.Petroleum.Gas..LPG.", "Lube"),
               names_to = "Category", 
               values_to = "Value")



# Change names for Electrification Scenarios column
AbbNames<-c(Blended.Gasoline = "Gasoline", Jet.A.AVTUR = "Jet-A/AVTUR",ULSD = "ULSD", Fuel.Oil = "Fuel Oil", Coke = "Coke", Liquid.Heavy.Ends = "LHE", Liquified.Petroleum.Gas..LPG.="LPG", Lube = "Lube" )
df_long$Category <- as.character(AbbNames[df_long$Category])
df_long$Category <- factor(df_long$Category, 
                           levels = c("Gasoline","Jet-A/AVTUR","ULSD","Fuel Oil","Coke","LHE","LPG","Lube"))


# Create a named vector for your custom color palette
my_palette <- brewer.pal(n = length(unique(df_long$Category)), name = "Spectral")


p <- ggplot(df_long, aes(x = YEAR)) +
  geom_bar(aes(y = Value, fill = Category),
           stat = 'identity', 
           position = 'stack') + 
  scale_fill_manual(values = my_palette) +  # Correctly apply palette to fill
  facet_grid(SCENARIO ~ PADD) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", 
       y = expression("Refinery Production (mmbbl y"^{-1}*")")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.y = element_text(size = 7), 
    axis.title.x = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 270),
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    strip.text.x = element_text(size = 7),
    strip.text.y = element_text(size = 7),
    strip.placement = "outside",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  )+
  guides(fill = guide_legend(nrow = 1))

p

# save_directory <- "D:/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS1.png")


ggsave(file_path, p, bg = "white", width = 7, height = 5, units = "in", dpi = 600)




