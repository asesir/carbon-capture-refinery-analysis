# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(wesanderson)
# Read the dataset
df_his <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S8_CrudeQuality_Hist.csv")
df_ets <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S8_CrudeQuality_ETS.csv")


# Reshape the data for easier plotting with ggplot2
df_ets_long <- df_ets %>%
  pivot_longer(
    cols = c(API_BAU, API_LB, API_UB, Sulfur_BAU, Sulfur_LB, Sulfur_UB),
    names_to = c("variable", "measure"), 
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  )

# Reshape the historical data to match the format of df_ets_long
df_his_long <- df_his %>%
  pivot_longer(cols = c(API, Sulfur),
               names_to = "variable",
               values_to = "value")

# You may want to add a 'measure' column to indicate these are historical data
df_his_long <- df_his_long %>%
  mutate(measure = "Historical")

# Combine historical and future data into one dataframe
df_combined <- bind_rows(df_ets_long, df_his_long)
# Adjust the 'variable' column values in df_combined
df_combined$variable <- gsub("API", "API Gravity", df_combined$variable)
df_combined$variable <- gsub("Sulfur", "Sulfur (wt%)", df_combined$variable)

df_combined$PADD <- paste("PADD",df_combined$PADD)

# Plotting with separate y-axes for API and Sulfur
new_colors <- c("red", "blue", "green", "orange")

p <- ggplot(df_combined, aes(x = Year, y = value, group = measure, color = measure)) +
  geom_line() +
  facet_grid(rows = vars(gsub(".*_", "", variable)), cols = vars(PADD), scales = "free_y", switch = "y") +
  scale_x_continuous(limits = c(1985, 2050), breaks = seq(1990, 2050, by = 20)) +
  theme_minimal() +
  labs(title = "API Gravity and Sulfur Content Trends by PADD",
       x = "Year",
       y = "") +  # Removing Y-axis label
  scale_color_manual(values = new_colors,
                     labels = c("Projected BAU Crude", "Historical Crude", 
                                "Projected Heavier or Sweeter Crude", "Projected Lighter or Sourer Crude"),
                     name = "Legend") +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_blank(), # Hide all y-axis titles
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 270),
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    strip.text.x = element_text(size = 8), # Clear the default strip labels on top
    strip.text.y = element_text(size = 8, face = "bold"), # Apply new settings for right side strip
    strip.placement = "outside", # Moves the strip to the right side
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5))

p

# save_directory <- "D:/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS8.png")


ggsave(file_path, p, bg = "white", width = 4.5, height = 4.5, units = "in", dpi = 600)