# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# # # read csv file
# df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/ProjCapacityReport.csv")
# df <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S2_ProjCapacity.csv")
df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S2_ProjCapacity.csv")
# df <- read.csv("D:/PhD/PhD Programe UofC/LCA/Paper 01/Data/ProjCapacityReport.csv")


# Change names for Electrification Scenarios column
AbbNames <- c(HIGH = "Low Fuel Demand (LD)", MEDIUM = "Med. Fuel Demand (MD)", LOW = "High Fuel Demand (HD)", REFERENCE ="Ref. Fuel Demand")

df$Electrification.Scenarios <- as.character(AbbNames[df$Electrification.Scenarios])

df<-subset(df,df$Electrification.Scenarios!="Ref. Fuel Demand")


df$Year <- as.factor(df$Year)
df$PADD <- as.factor(df$PADD)
df$Attribute <- as.factor(df$Attribute)
df$Attribute <- factor(df$Attribute,levels = rev(levels(df$Attribute)))
df$Electrification.Scenarios <- factor(df$Electrification.Scenarios, levels=c("Low Fuel Demand (LD)","Med. Fuel Demand (MD)","High Fuel Demand (HD)"))

# Create a named vector for your custom color palette
my_palette <- brewer.pal(n = length(unique(df$Attribute)), name = "Spectral")


df1 <- subset(df, (df$API.Level == "LIGHT" & df$Sul.Level == "SWEET"))

df2 <- subset(df, (df$API.Level == "HEAVY" & df$Sul.Level == "SOUR"))


p1 <- ggplot(df1, aes(x = Year, y = Value, fill = Attribute)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  facet_grid(Electrification.Scenarios ~ PADD, scales = "free", space = "free")+
  coord_cartesian(clip = "off")+
  scale_fill_manual(values = my_palette) +
  labs(title = "A. Projected Volume of Heavy/Sour Crude Processed by the U.S. Refining Sector",
    x = "Year", 
    y = expression("Refinery Throughput (mbbl bbl"^{-1}*")")) +
  theme_minimal() +
  theme(
      plot.title = element_text(size = 8, hjust =0, face = "bold"),
      axis.title.y = element_text(size = 8),
      axis.text.y = element_text(size = 8), 
      axis.title.x = element_text(size = 8),
      axis.text.x = element_text(size = 8, angle = 270),
      legend.position = "none",
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8),
      strip.placement = "outside",
      panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  )


p2 <- ggplot(df2, aes(x = Year, y = Value, fill = Attribute)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  facet_grid(Electrification.Scenarios ~ PADD, scales = "free", space = "free")+
  coord_cartesian(clip = "off")+
  scale_fill_manual(values = my_palette) +
  labs(title = "B. Projected Volume of Light/Sweet Crude Processed by the U.S. Refining Sector",
       x = "Year", 
       y = expression("Refinery Throughput (mbbl bbl"^{-1}*")")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8, hjust =0, face = "bold"),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 270),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  )+
  guides(fill = guide_legend(nrow = 1))

p_legend <- get_legend(p2)

p2<-p2+
  theme(legend.position = "none")


combined_plot <- plot_grid(
  p1,
  p2,
  p_legend,
  ncol = 1,
  rel_heights = c(1,1,0.1), 
  align = 'v',
  axis = 'tb'
)


print(combined_plot)


save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS2.png")


ggsave(file_path, combined_plot, bg = "white", width = 7, height = 9, units = "in", dpi = 600)



summarized_table_1 <- df1 %>%
  group_by(Electrification.Scenarios, Year, PADD) %>%  
  summarise(
    sum_cap = sum(Value),
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table_1)


summarized_table_2 <- df2 %>%
  group_by(Electrification.Scenarios, Year, PADD) %>%  
  summarise(
    sum_cap = sum(Value),
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table_2)


# Calculate percentage contribution
summarized_table_3 <- df2 %>%
  filter(PADD != "U.S.") %>% 
  group_by(Electrification.Scenarios, Year, PADD, Attribute) %>%
  summarise(
    sum_cap = sum(Value),
    .groups = 'drop'
  ) %>%
  group_by(Electrification.Scenarios, Year) %>%  # Group again by Scenarios and Year
  mutate(
    total_cap = sum(sum_cap),              # Calculate total for each group
    percentage = (sum_cap / total_cap) * 100  # Calculate percentage
  ) %>%
  ungroup()  # Remove grouping

category_summary <- summarized_table_3 %>%
  mutate(
    Category = case_when(
      Attribute == 0 ~ "Hydroskimming",
      Attribute %in% c(1, 2, 3) ~ "Medium Conversion",
      Attribute %in% c(4, 5, 6) ~ "Deep Conversion"
    )
  ) %>%
  group_by(Electrification.Scenarios, Year, Category) %>%
  summarise(
    category_sum = sum(sum_cap),  # Sum capacity for each category
    .groups = 'drop'
  ) %>%
  group_by(Electrification.Scenarios, Year) %>%
  mutate(
    total_cap = sum(category_sum),              # Calculate total for all categories
    percentage = (category_sum / total_cap) * 100  # Calculate percentage for each category
  ) %>%
  ungroup()  # Remove grouping for further processing