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

df_emissions <- read.csv("C:/Users/file_location/Figure S21_RO_Expected Emissions.csv")

df_ets <- read.csv("C:/Users/file_location/Figure S21_RO_EmissionsThreshold.csv")

df_emissions <- df_emissions %>%
  left_join(df_ets, by = c("Year"))

df_emissions$Ref_Emissions.Baseline <- df_emissions$Emissions.Baseline * df_emissions$Ref_Threshold
df_emissions$Low_Emissions.Baseline <- df_emissions$Emissions.Baseline * df_emissions$Low_Threshold
df_emissions$High_Emissions.Baseline <- df_emissions$Emissions.Baseline * df_emissions$High_Threshold

df_emissions <- df_emissions %>%
  group_by(Configuration, Config_cost, Scenario_cost, Scenario, EmissionsThreshold, OilPrices, Risk.Free.Interest.Rate, RefOpEx, Carbon.Capture.Operating.Cost.Variation, Carbon.Price.Variation, Project.Lifetime,Year) %>%
  # Add the count per RO_Options
  mutate(Total_Ref_EB = sum(Ref_Emissions.Baseline),
         Total_Low_EB = sum(Low_Emissions.Baseline),
         Total_High_EB = sum(High_Emissions.Baseline)) %>%
  # Ungroup the data frame
  ungroup()




df_emissions <- df_emissions %>%
  mutate(
    Year = factor(Year, ordered = TRUE),
    Scenario_cost = factor(Scenario_cost, 
                           levels = c("Low Fuel Demand", "Medium Fuel Demand","High Fuel Demand","Baseline Fueld Demand"),
                           ordered = TRUE),
    Config_cost = factor(Config_cost, 
                         levels = c("Hydroskimming","Medium Conversion","Deep Conversion"), 
                         ordered = TRUE))

plot_colors<- c(
  
  "#2cb5c0",
  
  "#f8b620",
  
  "#e03426"
  
)

# Now generate the plot with the net margin dot
p1 <- ggplot(df_emissions) +
  geom_bar(aes(x = Year, y = Expected.Total.Emissions, fill = RO_Options),stat = "identity", position = "stack") +
  geom_line(aes(x= Year, y = Total_Ref_EB, group = 2,color = "Net Zero by 2050 Emissions Redution Trajectory"))+ 
  geom_line(aes(x= Year, y = Total_Low_EB, group = 2,color = "Net Zero by 2100 Emissions Redution Trajectory"))+
  geom_line(aes(x= Year, y = Total_High_EB, group = 2,color = "Net Zero by 2035 Emissions Redution Trajectory"))+
  facet_grid(Scenario_cost ~ Config_cost , scales = "free_y", space = "fixed") +
  scale_fill_manual(values = plot_colors) +
  scale_color_manual(values = c("Net Zero by 2050 Emissions Redution Trajectory" = "red",
                                "Net Zero by 2100 Emissions Redution Trajectory" = "blue",
                                "Net Zero by 2035 Emissions Redution Trajectory" = "green")) + 
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  scale_x_discrete(limits = as.character(2019:2050), breaks = as.character(seq(2019, 2050, by = 5))) +
  labs(x = "Year",
       y = expression("Annual GHG Emissions, million tonnes CO"[2]*"-eq yr"^{-1}*""),
       fill = "Options",
       title = expression("Expected GHG Emissions by Options in 2035")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 270),
    legend.text = element_text(size = 6),
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

p1


save_directory <- "C:/Users/file_location/"

# # Create the full file path
file_path <- file.path(save_directory, "FigS21.png")


ggsave(file_path, p1, bg = "white", width = 7, height = 7, units = "in", dpi = 600)


#########summarize table#############

summarized_table_1 <- df_emissions %>%
  group_by(Scenario_cost, Config_cost, Year) %>%  
  summarise(Sum_Expected_Emissions = sum(Expected.Total.Emissions, na.rm = TRUE),
            Sum_Expected_Throughput = sum(Expected.Throughput, na.rm = TRUE), .groups = 'drop')



