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
# df <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/FigureS23_Emissions_Transportation.csv")
df <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S5_TransportationEmissionsBreakdown.csv")

# df <- df %>%
#   mutate_at(vars(3:8), ~ . / 1000000)


df_long<-df %>%
  pivot_longer(cols = c("Diesel","Gasoline","Jet.Fuel","Fuel.Oil","Total.Emissions", "Total.Demands"),
               names_to = "Category",
               values_to = "Value")%>%
  mutate(Category = recode(Category, 
                           `Jet.Fuel` = "Jet Fuel",
                           `Fuel.Oil` = "Fuel Oil", 
                           `Total.Emissions` = "Total Emissions", 
                           `Total.Demands` = "Total Demands")) %>%
  mutate(Category = factor(Category,
                           levels = c("Gasoline",
                                      "Jet Fuel",
                                      "Diesel",
                                      "Fuel Oil",
                                      "Total Emissions", 
                                      "Total Demands")),
         Fuel.Demand.Scenario = recode(Fuel.Demand.Scenario, 
                                       `Baseline Fuel Demand`="Baseline Fuel Demand (BAS)",
                                       `Low Fuel Demand`="Low Fuel Demand (LD)", 
                                       `Medium Fuel Demand`="Medium Fuel Demand (MD)", 
                                       `High Fuel Demand`="High Fuel Demand (HD)"),
         Fuel.Demand.Scenario = factor(Fuel.Demand.Scenario,
                                       levels = c("Baseline Fuel Demand (BAS)",
                                                  "Low Fuel Demand (LD)", 
                                                  "Medium Fuel Demand (MD)", 
                                                  "High Fuel Demand (HD)")),
         Year = factor(Year,
                       levels = c("2019","2035","2050")))

baseline_2019 <- df_long %>%
  filter(Year==2019, Scenario=="BASELINE")


low_df <- mutate(baseline_2019, Scenario = "LOW", Fuel.Demand.Scenario = "Low Fuel Demand (LD)")
medium_df <- mutate(baseline_2019, Scenario = "MEDIUM", Fuel.Demand.Scenario = "Medium Fuel Demand (MD)")
high_df <- mutate(baseline_2019, Scenario = "HIGH", Fuel.Demand.Scenario = "High Fuel Demand (HD)")


df_long <- bind_rows(df_long, low_df, medium_df, high_df)

df_long <- df_long %>%
  filter(Scenario != "BASELINE")


# Calculate total emissions per group to use for percentage calculations
df_stacked <- df_long %>%
  filter(Category != "Total Demands") %>%
  group_by(Year, Fuel.Demand.Scenario) %>%
  mutate(Total_Emissions = sum(Value[Category == "Total Emissions"])) %>%
  ungroup()

df_stacked$Total_Emissions <- df_stacked$Total_Emissions / 1000
df_stacked$Value <- df_stacked$Value / 1000

# Calculate percentage for each category (except Total Emissions itself)
df_stacked <- df_stacked %>%
  mutate(Percentage = ifelse(Category == "Total Emissions", NA, Value / Total_Emissions * 100))


# Define the color palette based on the filtered categories for stacked bars
my_palette <- brewer.pal(n = length(unique(df_stacked$Category)), name = "Spectral")

# Create the plot
p2 <- ggplot() +
  geom_bar(data = df_stacked %>% filter(Category != "Total Emissions"), 
           aes(x = Year, y = Value, fill = Category), 
           stat = 'identity', 
           position = 'stack') +
  geom_point(data = df_stacked %>% filter(Category == "Total Emissions"), 
             aes(x = Year, y = Value, color = "Total Emissions", shape = "Total Emissions"), 
             size = 3) +
  geom_text(data = df_stacked %>% filter(Category == "Total Emissions"), 
            aes(x = Year, y = Value, label = sprintf("%.2f", Value)), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = my_palette) +
  scale_color_manual(values = c("Total Emissions" = "black")) +
  scale_shape_manual(values = c("Total Emissions" = 18)) +
  facet_wrap(~ Fuel.Demand.Scenario) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", 
       y = expression("Annual Estimated GHG Emissions by Fuel, giga tonnes CO"[2]*"-eq yr"^{-1}*" "),
       color = "Data Type",
       shape = "Data Type") +
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
    legend.key.size = unit(0.1, "cm"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  ) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(shape = NA)))

# Print the plot
print(p2)

# # Specify the directory path where you want to save the figure
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS5.png")


ggsave(file_path, p2,bg="white", width = 4.5, height = 4.5, units = "in", dpi = 600)