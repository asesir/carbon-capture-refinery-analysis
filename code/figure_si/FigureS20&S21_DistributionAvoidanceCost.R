library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggmacc)
library(cowplot)
library(RColorBrewer)

df1 <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S20_MACC_HS.csv")
df2 <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S21_MACC_LS.csv")

# df1 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S20_MACC_HS.csv")
# df2 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S21_MACC_LS.csv")

##############################First Figure ###################################################################
df1$Capture.Scenario <- factor(df1$Capture.Scenario)
df1$PADD <- paste("PADD", df1$PADD)
df1$PADD <- factor(df1$PADD)
df1$Config <- factor(df1$Config)
df1$Year <- factor(df1$Year)
df1 <- df1 %>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "High Fuel Demand (HD)",
                           `MEDIUM` = "Med. Fuel Demand (MD)",
                           `HIGH` = "Low Fuel Demand (LD)" ))
df1$Scenario <- factor(df1$Scenario, levels=c("Low Fuel Demand (LD)","Med. Fuel Demand (MD)","High Fuel Demand (HD)"))

# Convert the units in your dataframes
df1$Total.Avoided.CO2.kt.y <- df1$Total.Avoided.CO2.kt.y / 1000

df1_filtered <- df1 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)

# Generate individual plots for each combination of Scenario and Year
plots <- list()
years <- unique(df1_filtered$Year)

# Loop over scenarios and years to create individual plots
index <- 1  # To keep track of plot indices for titles
for (yr in years) {
    sub_data <- df1_filtered[df1_filtered$Year == yr,]
    p <- ggplot(sub_data) +
      geom_boxplot(aes(x = Config, y = Marginal.Abatement.Cost....t, fill = PADD),size = 0.1, outlier.size = 0.1) +
      facet_grid(Scenario ~ PADD)+
      scale_y_continuous(limits = c(0, 600)) +
      scale_fill_brewer(palette = "Spectral", name = "PADD") +
      labs(x = "Refinery Configuration", 
           y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")"),
           title = paste(LETTERS[index], ".Distribution of CO2 Avoidance Cost in", yr, sep = " ")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 7), # Hide all y-axis titles
        axis.text.y = element_text(size = 6), 
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        legend.position = "none",
        strip.text.x = element_text(size = 7), # Clear the default strip labels on top
        strip.text.y = element_text(size = 7), # Apply new settings for right side strip
        strip.placement = "outside", # Moves the strip to the right side
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
        )
    plots[[paste(yr)]] <- p
    index <- index + 1
}


# Combine plots using patchwork and display a single legend
combined_plot_1 <- wrap_plots(plots, ncol = 1)


##############################Second Figure ###################################################################
# Replace '#DIV/0!' with 0 or any other value deemed appropriate
df2$Total.Avoided.CO2.kt.y <- gsub("#DIV/0!", "0", df2$Total.Avoided.CO2.kt.y)

# Convert to numeric
df2$Total.Avoided.CO2.kt.y <- as.numeric(df2$Total.Avoided.CO2.kt.y)

df2$Capture.Scenario <- factor(df2$Capture.Scenario)
df2$PADD <- paste("PADD", df2$PADD)
df2$PADD <- factor(df2$PADD)
df2$Config <- factor(df2$Config)
df2$Year <- factor(df2$Year)
df2 <- df2 %>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "High Fuel Demand (HD)",
                           `MEDIUM` = "Med. Fuel Demand (MD)",
                           `HIGH` = "Low Fuel Demand (LD)" ))
df2$Scenario <- factor(df2$Scenario, levels=c("Low Fuel Demand (LD)","Med. Fuel Demand (MD)","High Fuel Demand (HD)"))


# Convert the units in your dataframes
df2$Total.Avoided.CO2.kt.y <- df2$Total.Avoided.CO2.kt.y / 1000

df2_filtered <- df2 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)

# Generate individual plots for each combination of Scenario and Year
plots <- list()
years <- unique(df2_filtered$Year)

# Loop over scenarios and years to create individual plots
index <- 1  # To keep track of plot indices for titles
for (yr in years) {
  sub_data <- df2_filtered[df2_filtered$Year == yr,]
  p <- ggplot(sub_data) +
    geom_boxplot(aes(x = Config, y = Marginal.Abatement.Cost....t, fill = PADD),size = 0.1, outlier.size = 0.1) +
    facet_grid(Scenario ~ PADD)+
    scale_y_continuous(limits = c(0, 600)) +
    scale_fill_brewer(palette = "Spectral", name = "PADD") +
    labs(x = "Refinery Configuration", 
         y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")"),
         title = paste(LETTERS[index], ".Distribution of CO2 Avoidance Cost in", yr, sep = " ")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, face = "bold"),
      axis.title.y = element_text(size = 7), # Hide all y-axis titles
      axis.text.y = element_text(size = 6), 
      axis.title.x = element_text(size = 7),
      axis.text.x = element_text(size = 6),
      legend.position = "none",
      strip.text.x = element_text(size = 7), # Clear the default strip labels on top
      strip.text.y = element_text(size = 7), # Apply new settings for right side strip
      strip.placement = "outside", # Moves the strip to the right side
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    )
  plots[[paste(yr)]] <- p
  index <- index + 1
}


# Combine plots using patchwork and display a single legend
combined_plot_2 <- wrap_plots(plots, ncol = 1)


###########Mock figure to generate a legend##########################################
sub_data_mock <- df1[df1$Scenario == "Low Fuel Demand (LD)" & df1$Year == "2035",]

p_mock <- ggplot(sub_data_mock) +
  geom_boxplot(aes(x = Config, y = Marginal.Abatement.Cost....t, fill = PADD),size = 0.1, outlier.size = 0.1) +
  facet_grid(Scenario ~ PADD)+
  scale_y_continuous(limits = c(0, 600)) +
  scale_fill_brewer(palette = "Spectral", name = "PADD") +
  labs(x = "Refinery Configuration", 
       y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")"),
       title = expression ("C.Distribution of CO"[2]*" Avoidance Cost for Carbon Capture in Different Refinery Configurations")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_blank(), # Hide all y-axis titles
    axis.text.y = element_text(size = 6), 
    axis.title.x = element_text(size = 7),
    axis.text.x = element_text(size = 6, angle = 270),
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    strip.text.x = element_text(size = 8), # Clear the default strip labels on top
    strip.text.y = element_text(size = 7), # Apply new settings for right side strip
    strip.placement = "outside", # Moves the strip to the right side
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5))

p_mock
components <- get_plot_component(p_mock, "guide-box", return_all = TRUE)
p_legends <- components[[3]] 


#######################Combine all Plots##########################################

combined_plot_1 <- plot_grid(
  combined_plot_1,
  p_legends,
  ncol = 1,
  rel_heights = c(1,0.05),# adjust as needed
  align = 'v'
)

combined_plot_2 <- plot_grid(
  combined_plot_2,
  p_legends,
  ncol = 1,
  rel_heights = c(1,0.05),# adjust as needed
  align = 'v'
)




##############Save Figures############################################################
# # Specify the directory path where you want to save the figure
# save_directory <- "D:/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path_1 <- file.path(save_directory, "FigS20.png")
ggsave(file_path_1, combined_plot_1, bg = "white", width = 7, height = 9, units = "in", dpi = 600)

file_path_2 <- file.path(save_directory, "FigS21.png")
ggsave(file_path_2, combined_plot_2, bg = "white", width = 7, height = 9, units = "in", dpi = 600)



# Assuming your dataframe is named df_1
summarized_table_1 <- df1_filtered %>%
  group_by(Scenario, Year, PADD) %>%  
  summarise(
    Mean_cost = mean(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Max_cost = max(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Config_Max = Config[which.max(Marginal.Abatement.Cost....t)],
    Min_cost = min(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Config_Min = Config[which.min(Marginal.Abatement.Cost....t)],
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table_1)


summarized_table_2 <- df2_filtered %>%
  group_by(Scenario, Year, PADD) %>%  
  summarise(
    Mean_cost = mean(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Max_cost = max(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Config_Max = Config[which.max(Marginal.Abatement.Cost....t)],
    Min_cost = min(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Config_Min = Config[which.min(Marginal.Abatement.Cost....t)],
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table_2)

