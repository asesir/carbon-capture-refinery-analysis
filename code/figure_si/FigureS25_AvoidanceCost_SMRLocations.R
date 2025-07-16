# Load required libraries
library(ggplot2)
library(dplyr)
library(grid)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(wesanderson)
library(ggnewscale)
library(RColorBrewer)

# read csv file
df <- read.csv("C:/Users/file_location/Figure S25_MACC_smrLocation.csv")


df <- df %>%
  mutate(
    Scenario = recode(Scenario,
                      `LOW` = "High Fuel Demand (HD)",
                      `MEDIUM` = "Med. Fuel Demand (MD)",
                      `HIGH` = "Low Fuel Demand (LD)",
                      `POTENTIAL` = "Extreme Low Fuel Demand (ELD)"),
    Scenario = factor(Scenario, levels = c("Extreme Low Fuel Demand (ELD)","Low Fuel Demand (LD)", "Med. Fuel Demand (MD)", "High Fuel Demand (HD)")),
    Year = factor(Year, levels = c("2019", "2035", "2050")),
    PADD = factor(PADD, levels = c("PADD1", "PADD2", "PADD3", "PADD4", "PADD5")),
    SMR.Capture.Locations = factor(SMR.Capture.Locations, levels = c("PSA inlet", "PSA outlet", "Furnance outlet"))
  )

df$Normalized.Avoidance.cost <- as.numeric(df$Normalized.Avoidance.cost)
df$Config <- as.factor(df$Config)

df1_filtered <- df %>% 
  filter(Normalized.Avoidance.cost != 0 & Normalized.Avoided.CO2 > 0.001 & API =="HEAVY" & Sul == "SOUR" & Scenario != "Extreme Low Fuel Demand (ELD)")

df2_filtered <- df %>% 
  filter(Normalized.Avoidance.cost != 0 & Normalized.Avoided.CO2 > 0.001 & API =="LIGHT" & Sul == "SWEET" & Scenario != "Extreme Low Fuel Demand (ELD)")

# # Compute mean values for each category
# df1_filtered_means <- df1_filtered %>%
#   group_by(Year, PADD, SMR.Capture.Locations) %>%
#   summarise(Mean_Avoidance_Cost = mean(`Normalized.Avoidance.cost`, na.rm = TRUE))

# --- Compute IQR bounds per group for df1_filtered ---
iqr_bounds_df1 <- df1_filtered %>%
  group_by(Year, PADD, SMR.Capture.Locations) %>%
  summarise(
    Q1 = quantile(Normalized.Avoidance.cost, 0.25, na.rm = TRUE),
    Q3 = quantile(Normalized.Avoidance.cost, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    Lower = Q1 - 1.5 * IQR,
    Upper = Q3 + 1.5 * IQR
  )

# --- Remove outliers from df1_filtered ---
df1_filtered_no_outliers <- df1_filtered %>%
  left_join(iqr_bounds_df1, by = c("Year", "PADD", "SMR.Capture.Locations")) %>%
  filter(Normalized.Avoidance.cost >= Lower & Normalized.Avoidance.cost <= Upper)

# --- Compute mean after removing outliers ---
df1_filtered_means <- df1_filtered_no_outliers %>%
  group_by(Year, PADD, SMR.Capture.Locations) %>%
  summarise(
    Mean_Avoidance_Cost = mean(Normalized.Avoidance.cost, na.rm = TRUE),
    .groups = "drop"
  )
# Define dot colors from Wes Anderson palette

padd_colors <- wes_palette("Zissou1", n = length(unique(df1_filtered$PADD)), type = "continuous")
config_colors <- brewer.pal(n = 7, name = "Set1")

# Plot
p_violin <- ggplot(df1_filtered_no_outliers, aes(x = SMR.Capture.Locations, y = `Normalized.Avoidance.cost`)) +
  facet_grid(Year ~ PADD, scales = "free", space = "free") +
  
  # Jitter colored by Config
  geom_jitter(aes(color = Config), width = 0.3, size = 0.5, alpha = 0.7) +
  scale_color_manual(name = "Config", values = config_colors) +
  
  # Box plot filled by PADD (outline in black)
  geom_boxplot(aes(fill = PADD), color = "black", width = 0.8, outlier.shape = NA, size = 0.3) +
  scale_fill_manual(name = "PADD", values = padd_colors) +
  
  # Mean points (legend via shape)
  geom_point(data = df1_filtered_means, 
             aes(x = SMR.Capture.Locations, y = Mean_Avoidance_Cost, shape = "Mean Avoidance Cost"), 
             color = "black", size = 2.5) +
  
  # Connecting line for mean
  geom_line(data = df1_filtered_means, 
            aes(x = SMR.Capture.Locations, y = Mean_Avoidance_Cost, group = interaction(Year, PADD)), 
            color = "blue", size = 0.4, alpha = 0.6, show.legend = FALSE) +
  
  # Shape legend
  scale_shape_manual(name = "", values = c("Mean Avoidance Cost" = 18)) +
  
  # Axes and labels
  scale_y_continuous(limits = c(0, 400)) +
  labs(x = "SMR Carbon Capture Locations", 
       y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")")) +
  
  # Theme and legend formatting
  theme_minimal() +
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1, byrow = TRUE),
    fill  = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1, byrow = TRUE),
    shape = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)
  ) +
  theme(
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size = 9),
    axis.text.x = element_text(size = 8, angle = 270),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = -5, b = 5, unit = "pt"),
    legend.key.size = unit(0.5, "lines"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

# Show plot
print(p_violin)







##################summarize table##############################
summarized_table_1 <- df1_filtered %>%
  group_by(Scenario, Year, SMR.Capture.Locations) %>%  
  summarise(
    Max_cost = max(Normalized.Avoidance.cost, na.rm = TRUE),
    Avoided_CO2_Max = Normalized.Avoided.CO2[which.max(Normalized.Avoidance.cost)],
    Min_cost = min(Normalized.Avoidance.cost, na.rm = TRUE),
    Avoided_CO2_Min = Normalized.Avoided.CO2[which.min(Normalized.Avoidance.cost)],
    .groups = 'drop')  # Summarize and drop grouping

# Display the resulting summarized table
print(summarized_table_1)


##############Save Figures############################################################
# # Specify the directory path where you want to save the figure

save_directory_figure <- "C:/Users/file_location/"
# # Create the full file path

file_path_1 <- file.path(save_directory_figure, "FigS25.png")
ggsave(file_path_1, p_violin, bg = "white", width = 7, height = 9, units = "in", dpi = 600)


save_directory_table <- "C:/Users/file_location/Table S33.csv"
write.csv(summarized_table_1,save_directory_table, row.names = FALSE)

