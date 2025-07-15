library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggmacc)
library(cowplot)
library(RColorBrewer)

df1 <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S18_MACC_HS.csv")
df2 <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S19_MACC_LS.csv")

# df1 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S18_MACC_HS.csv")
# df2 <- read.csv("C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S19_MACC_LS.csv")

colors <- brewer.pal(8, "BrBG")

##############################First Figure ###################################################################
df1$Capture.Scenario <- factor(df1$Capture.Scenario)
df1$PADD <- paste("PADD", df1$PADD)
df1$PADD <- factor(df1$PADD)
df1$Year <- factor(df1$Year)
df1 <- df1 %>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "HD",
                           `MEDIUM` = "MD",
                           `HIGH` = "LD" ))
df1$Scenario <- factor(df1$Scenario, levels=c("LD","MD","HD"))

# Convert the units in your dataframes
df1$Total.Avoided.CO2.kt.y <- df1$Total.Avoided.CO2.kt.y / 1000


df1_filtered <- df1 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)

# Generate individual plots for each combination of Scenario and Year
plots <- list()
scenarios <- unique(df1$Scenario)
years <- unique(df1$Year)

# Loop over scenarios and years to create individual plots
index <- 1  # To keep track of plot indices for titles
for (sc in scenarios) {
  for (yr in years) {
    # Extract last two digits of the year
    yr_short <- substr(as.character(yr), 3, 4)
    
    # Subset the data for the current scenario and year
    sub_data <- df1[df1$Scenario == sc & df1$Year == yr, ]
    
    # Generate the plot
    p <- ggmacc(sub_data, abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
                zero_line = FALSE, threshold_line = FALSE) +
      scale_x_continuous(labels = scales::number_format(), breaks = seq(0, 150, 50), limits = c(0, 150)) +
      scale_y_continuous(limits = c(0, 300)) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste(LETTERS[index], ". CO2 Avoidance Cost by Process Unit in ", sc, yr_short, sep = ""),
        fill = "Captured Units",
        x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
        y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
      ) +
      theme_minimal() +
      theme(
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        axis.title.x = element_text(size = 6),
        plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
        legend.position = "none"
      )
    
    # Save the plot in the list
    plots[[paste(sc, yr)]] <- p
    
    # Increment index for the next plot
    index <- index + 1
  }
}

# Combine plots using patchwork and display a single legend
combined_plot_1 <- wrap_plots(plots, ncol = 2) + 
  plot_layout(guides = "collect")
print(combined_plot_1)


###################Second Figure##################################################################
# Replace '#DIV/0!' with 0 or any other value deemed appropriate
df2$Total.Avoided.CO2.kt.y <- gsub("#DIV/0!", "0", df2$Total.Avoided.CO2.kt.y)

# Convert to numeric
df2$Total.Avoided.CO2.kt.y <- as.numeric(df2$Total.Avoided.CO2.kt.y)

df2$Capture.Scenario <- factor(df2$Capture.Scenario)
df2$PADD <- paste("PADD", df2$PADD)
df2$PADD <- factor(df2$PADD)
df2$Year <- factor(df2$Year)
df2 <- df2 %>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "HD",
                           `MEDIUM` = "MD",
                           `HIGH` = "LD" ))
df2$Scenario <- factor(df2$Scenario, levels=c("LD","MD","HD"))

# Convert the units in your dataframes
df2$Total.Avoided.CO2.kt.y <- df2$Total.Avoided.CO2.kt.y / 1000


df2_filtered <- df2 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)

# Generate individual plots for each combination of Scenario and Year
plots <- list()
scenarios <- unique(df2$Scenario)
years <- unique(df2$Year)

# Loop over scenarios and years to create individual plots
index <- 1  # To keep track of plot indices for titles
for (sc in scenarios) {
  for (yr in years) {
    # Extract last two digits of the year
    yr_short <- substr(as.character(yr), 3, 4)
    
    # Subset the data for the current scenario and year
    sub_data <- df2[df2$Scenario == sc & df2$Year == yr, ]
    
    # Generate the plot
    p <- ggmacc(sub_data, abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
                zero_line = FALSE, threshold_line = FALSE) +
      scale_x_continuous(labels = scales::number_format(), breaks = seq(0, 150, 50), limits = c(0, 150)) +
      scale_y_continuous(limits = c(0, 300)) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste(LETTERS[index], ". CO2 Avoidance Cost by Process Unit in ", sc, yr_short, sep = ""),
        fill = "Captured Units",
        x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
        y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
      ) +
      theme_minimal() +
      theme(
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.title.y = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        axis.title.x = element_text(size = 6),
        plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
        legend.position = "none"
      )
    
    # Save the plot in the list
    plots[[paste(sc, yr)]] <- p
    
    # Increment index for the next plot
    index <- index + 1
  }
}

# Combine plots using patchwork and display a single legend
combined_plot_2 <- wrap_plots(plots, ncol = 2) + 
  plot_layout(guides = "collect")
print(combined_plot_2)

###########Mock figure to generate a legend##########################################

sub_data_mock <- df1[df1$Scenario == "LD" & df1$Year == "2035",]

p_mock <- ggmacc(sub_data_mock, abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
            zero_line = FALSE, threshold_line = FALSE) +
  scale_x_continuous(labels = scales::number_format(), breaks = seq(0, 140, 35), limits = c(0, 140)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_manual(values = colors) +
  labs(fill = "Captured Units",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.y = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    axis.title.x = element_text(size = 6),
    plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.2, "cm")
  )+
  guides(
    fill = guide_legend(order = 2, title = NULL, nrow = 1, byrow = TRUE))
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
# save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path_1 <- file.path(save_directory, "FigS18.png")
ggsave(file_path_1, combined_plot_1, bg = "white", width = 7, height = 9, units = "in", dpi = 600)

file_path_2 <- file.path(save_directory, "FigS19.png")
ggsave(file_path_2, combined_plot_2, bg = "white", width = 7, height = 9, units = "in", dpi = 600)


##########TOTAL AVOIDED co2#######################################
summarized_table_1 <- df1_filtered %>%
  group_by(Scenario, Year, Capture.Scenario) %>%  
  summarise(Sum_Emissions_Reduction_Potential = sum(Total.Avoided.CO2.kt.y, na.rm = TRUE), .groups = 'drop')

################AVG COST##############################################
summarized_table_1 <- df1_filtered %>%
  group_by( PADD, Config) %>%  
  summarise(Avg_Avoidance_Cost = mean(Marginal.Abatement.Cost....t, na.rm = TRUE),
            .groups = 'drop')

AvgCost_ConfigNot6_PAD3 <- mean(subset(summarized_table_1, Config != 6 & PADD =="PADD 3")$Avg_Avoidance_Cost)

AvgCost_PADNot3 <- mean(subset(summarized_table_1, PADD !="PADD 3")$Avg_Avoidance_Cost)

##################Total Avoided CO2 by $93 to $170#####################

summarized_table_1 <- df1_filtered %>%
  filter(Marginal.Abatement.Cost....t < 400)%>%
  group_by(Scenario, Year) %>% 
  summarise(Sum_Emissions_Reduction_Potential = sum(Total.Avoided.CO2.kt.y, na.rm = TRUE), .groups = 'drop')


###############

summarized_table_1 <- df1_filtered %>%
  group_by(Config, Scenario, Year, PADD) %>%  
  summarise(
    Max_cost = max(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Avoided_CO2_Max = Total.Avoided.CO2.kt.y[which.max(Marginal.Abatement.Cost....t)],
    Capture_Scenario_Max = Capture.Scenario[which.max(Marginal.Abatement.Cost....t)],
    Min_cost = min(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Avoided_CO2_Min = Total.Avoided.CO2.kt.y[which.max(Marginal.Abatement.Cost....t)],
    Capture_Scenario_Min = Capture.Scenario[which.min(Marginal.Abatement.Cost....t)],
    .groups = 'drop')  # Summarize and drop grouping

# Display the resulting summarized table
print(summarized_table_1)


# Total SMR Emissions

puEmissions <- df1_filtered %>%
  group_by(Scenario, Year, Capture.Scenario) %>%  
  summarise(
    smrEmissions = sum(Total.Avoided.CO2.kt.y),
    .groups = 'drop')  # Summarize and drop grouping




# Count occurrences of FCC/RFCC and calculate the percentage
fcc_rfcc_count <- summarized_table_1 %>%
  summarise(
    Total = n(),  # Total number of rows
    FCC_RFCC_Count = sum(Capture_Scenario_Min == "FCC/RFCC"),  # Count of FCC/RFCC occurrences
    FCC_RFCC_Percentage = (FCC_RFCC_Count / Total) * 100  # Percentage of FCC/RFCC occurrences
  )


SMR_count <- summarized_table_1 %>%
  summarise(
    Total = n(),  # Total number of rows
    SMR_Count = sum(Capture_Scenario_Min == "SMR"),  # Count of FCC/RFCC occurrences
    SMR_Percentage = (SMR_Count / Total) * 100  # Percentage of FCC/RFCC occurrences
  )

HT_count <- summarized_table_1 %>%
  summarise(
    Total = n(),  # Total number of rows
    HT_Count = sum(Capture_Scenario_Min == "Hydrotreater"),  # Count of FCC/RFCC occurrences
    HT_Percentage = (HT_Count / Total) * 100  # Percentage of FCC/RFCC occurrences
  )

SB_count <- summarized_table_1 %>%
  summarise(
    Total = n(),  # Total number of rows
    SB_Count = sum(Capture_Scenario_Max == "Steam Boiler"),  # Count of FCC/RFCC occurrences
    SB_Percentage = (SB_Count / Total) * 100  # Percentage of FCC/RFCC occurrences
  )

summarized_table_2 <- df2_filtered %>%
  group_by(Config, Scenario, Year, PADD) %>%  
  summarise(
      Max_cost = max(Marginal.Abatement.Cost....t, na.rm = TRUE),
      Capture_Scenario_Max = Capture.Scenario[which.max(Marginal.Abatement.Cost....t)],
      Min_cost = min(Marginal.Abatement.Cost....t, na.rm = TRUE),
      Capture_Scenario_Min = Capture.Scenario[which.min(Marginal.Abatement.Cost....t)],
      .groups = 'drop')  # Summarize and drop grouping

# Display the resulting summarized table
print(summarized_table_2)
