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


df_crude <- read.csv("C:/Users/file_location/Figure S22_RO_Expected Throughput.csv")

# Filtering columns
df_crude_filtered <- df_crude %>%
  dplyr::select(-matches("Expected.Throughput|Target.Throughput"))

df_crude_long <- df_crude_filtered %>%
  pivot_longer(
    cols = -c(1:16),
    names_to = c(".value", "Variable"), # This will create two new columns
    names_pattern = "^(Expected|Target)_(.*)$" # This uses regex to separate the column names
  )

df_crude_long <- df_crude_long %>%
  mutate(
    Year = factor(Year, ordered = TRUE),
    Scenario_cost = factor(Scenario_cost, 
                           levels = c("Low Fuel Demand","Medium Fuel Demand","High Fuel Demand"),
                           ordered = TRUE),
    Config_cost = factor(Config_cost, 
                         levels = c("Hydroskimming","Medium Conversion","Deep Conversion"), 
                         ordered = TRUE),
    Variable = factor(Variable,
                      levels = c("Blended.Gasoline",
                                 "Jet.A.AVTUR",
                                 "ULSD",
                                 "Fuel.Oil",
                                 "Coke",
                                 "Liquid.Heavy.Ends",
                                 "Surplus.NCR.H2",
                                 "Refinery.Fuel.Gas..RFG.",
                                 "Liquified.Petroleum.Gas..LPG.",
                                 "Lube",
                                 "Naphtha",
                                 "VGO",
                                 "Crude",
                                 "Refinery.Operating.Cost")),
    ordered = TRUE
  )


# Calculate the sum of the Target values for each Year, Scenario_cost, and Config_cost combination
target_sums <- df_crude_long %>%
  group_by(Year, Scenario_cost, Config_cost) %>%
  summarise(Target_Sum = sum(Target, na.rm = TRUE)) %>%
  ungroup()

label_data <- target_sums %>%
  arrange(Year) %>%
  group_by(Scenario_cost, Config_cost) %>%
  filter(Year == max(Year)) %>%
  ungroup()



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



# Now, update your ggplot code to use midpoint_data for geom_text
p1 <- ggplot(df_crude_long, aes(x = Year, y = Expected, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(data = target_sums, aes(x = Year, y = Target_Sum, group = interaction(Scenario_cost, Config_cost), color = "Total Target Production"), size = 1, inherit.aes = FALSE) +
  facet_grid(Scenario_cost ~ Config_cost , scales = "free_y", space = "fixed") +
  #facet_grid(~ Config_cost , scales = "free_y", space = "fixed") +
  scale_fill_manual(values = plot_colors_ps) +
  scale_color_manual(values = c("Total Target Production" = "red")) + # Customize the color here
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 2000)) +
  scale_x_discrete(limits = as.character(2019:2050), breaks = as.character(seq(2019, 2050, by = 5))) +
  labs(x = "Year",
       y = expression("Annual Production, mmbbl yr"^{-1}*""),
       title = "Expected & Target Annual Refinery Production in 2035",
       fill = "Refinery Product",
       color = "Legend Title") +
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
file_path <- file.path(save_directory, "FigS22.png")


ggsave(file_path, p1, bg = "white", width = 7, height = 7, units = "in", dpi = 600)

