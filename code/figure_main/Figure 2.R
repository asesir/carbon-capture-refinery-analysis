library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggmacc)
library(cowplot)
library(RColorBrewer)

df1 <- read.csv("C:/Users/file_location/Figure2_AvoidanceCostCurve.csv")

df1$Capture.Scenario <- factor(df1$Capture.Scenario)
df1$PADD_factor <- factor(df1$PADD)


# Convert the units in your dataframes
df1$Total.Avoided.CO2.kt.y <- df1$Total.Avoided.CO2.kt.y / 1000


df1_filtered <- df1 %>% filter(Marginal.Abatement.Cost....t != 0 & Total.Avoided.CO2.kt.y > 0.001)



full_macc_2050_HS_Unit <- df1 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = Capture.Scenario,
         zero_line = FALSE, threshold_line = FALSE)

full_macc_2050_HS_PADD <- df1 %>%
  ggmacc(abatement = Total.Avoided.CO2.kt.y, mac = Marginal.Abatement.Cost....t, fill = PADD_factor,
         zero_line = FALSE, threshold_line = FALSE)

# colors <- pal(8)
colors <- brewer.pal(8, "BrBG")

p1 <- full_macc_2050_HS_Unit +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,100,25),limits = c(0, 105)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_manual(values = colors) +
  labs(title = expression("A. CO "[2]*" Avoidance Cost by Process Unit"),
       fill = "Captured Units",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0,size = 6, face = "bold"),
        axis.line = element_line(size = 0),
        axis.ticks = element_line(size = 0.2),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        legend.key.size = unit(0.3, "cm"))

p2 <- full_macc_2050_HS_PADD +
  scale_x_continuous(labels = scales::number_format(),breaks=seq(0,100,25),limits = c(0, 105)) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = expression("B. CO "[2]*" Avoidance Cost by PADD"),
       fill = "PADD",
       x = expression("Avoided CO"[2]*" (million tonnes CO"[2]*"-eq yr"^{-1}*")"),
       y = expression("CO"[2]*" avoidance cost ($ tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0,size = 6, face = "bold"),
        axis.line = element_line(size = 0),
        axis.ticks = element_line(size = 0.2),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        legend.key.size = unit(0.4, "cm"))





# Extract the legend
p_legend_Unit <- get_legend(p1)
p_legend_PADD <- get_legend(p2)


# Remove legend for each plot
p1 <- p1 + 
  theme(axis.title.x = element_text(size = 5), axis.text.x = element_text(size = 4),axis.title.y = element_text(size = 5), axis.text.y = element_text(size = 4),legend.position = "none") 



p2 <- p2 + 
  theme(axis.title.x = element_text(size = 5), axis.text.x = element_text(size = 4),axis.title.y = element_text(size = 5), axis.text.y = element_text(size = 4),legend.position = "none") 



# Extract the 'Config' value from the interaction variable for x-axis labels
labels <- sapply(levels(interaction(df1_filtered$Config, df1_filtered$PADD)), function(x) strsplit(x, "\\.")[[1]][1])


# Compute the x-intercepts for the vertical lines
vlines <- df1_filtered %>% 
  group_by(PADD) %>% 
  summarise(max_config = max(as.numeric(Config))) %>% 
  mutate(cumulative_max_config = cumsum(max_config))

# Create the new box plot
box_plot_PADD <- ggplot(df1_filtered, aes(x = interaction(Config, PADD), y = Marginal.Abatement.Cost....t, fill = interaction(PADD))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  scale_y_continuous(limits = c(0, 500)) +
  scale_x_discrete(labels = labels) +
  scale_fill_brewer(palette = "Spectral", name = "PADD") +
  labs(x = "Refinery Configuration", 
       y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")"),
       title = expression ("C.Distribution of CO"[2]*" Avoidance Cost for Carbon Capture in Different Refinery Configurations")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 5),
        axis.text.y = element_text(size = 4),
        axis.title.x = element_text(size = 5),
        axis.text.x = element_text(size = 4),
        axis.line = element_line(size = 0),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 4),
        legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(hjust = 0,size = 6, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.4)) 

# Set the midpoints manually based on the information you provided
midpoints <- c(5.5, 9.5, 16.5, 21.5)

# Add the vertical lines to the plot at these midpoints
for (i in midpoints) {
  box_plot_PADD <- box_plot_PADD + geom_vline(xintercept = i, linetype = "dashed", linewidth = 0.2)
}

# Add the PADD labels
padd_labels <- paste0("PADD ", 1:4)
for (i in seq_along(midpoints)) {
  box_plot_PADD <- box_plot_PADD + 
    annotate("text", x = midpoints[i]-1.5, y = 10, label = padd_labels[i], size = 1)
}


box_plot_PADD <- box_plot_PADD +
  annotate("text", x = 27, y = 10, label = "PADD 5", size = 1)

box_plot_PADD_legend<-get_legend(box_plot_PADD)

box_plot_PADD <- box_plot_PADD +
  theme(axis.title.x = element_text(), axis.text.x = element_text(size = 4),axis.title.y = element_text(size = 5), axis.text.y = element_text(size = 4),legend.position = "none")


# # Remove y-axis title from individual plots
# p1 <- p1 +
#   theme(axis.title.y=element_blank())
# p2 <- p2 + 
#   theme(axis.title.y=element_blank())
# box_plot_PADD <- box_plot_PADD + 
#   theme(axis.title.y=element_blank())
# 
# # Create a plot that only contains the y-axis label
# y_label <- ggplot() + 
#   theme_void() + 
#   labs(y = expression("CO "[2]*" Avoidance Cost ($ tonne CO"[2]*"-eq"^{-1}*")")) + 
#   theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size = 4))


# Combine the legends vertically and left aligned
legends <- plot_grid(p_legend_Unit, p_legend_PADD, box_plot_PADD_legend, ncol = 1, align = 'v',rel_heights = c(0.8,1,1))

# Arrange the MAC plots
mac_plots <- plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(1, 1))

# Combine all the plots with the common y-axis label
fig2 <- plot_grid(
  # y_label,
  plot_grid(mac_plots, box_plot_PADD, nrow = 2, rel_heights = c(2, 1)),
  legends,
  ncol = 2,
  rel_widths = c(0.85, 0.15),# adjust as needed
  align = 'h'
)

print(fig2)

# # Specify the directory path where you want to save the figure

save_directory <- "C:/Users/file_location/Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "Figure_2.png")


ggsave(file_path, fig2, bg = "white", width = 4.5, height = 4.5, units = "in", dpi = 600)

##########TOTAL AVOIDED co2#######################################
summarized_table_1 <- df1_filtered %>%
  group_by(Scenario, Year, Capture.Scenario) %>%  
  summarise(Sum_Emissions_Reduction_Potential = sum(Total.Avoided.CO2.kt.y, na.rm = TRUE), .groups = 'drop')

################AVG COST##############################################
summarized_table_1 <- df1_filtered %>%
  group_by( PADD, Config) %>%  
  summarise(Avg_Avoidance_Cost = mean(Marginal.Abatement.Cost....t, na.rm = TRUE),
            .groups = 'drop')

AvgCost_ConfigNot6_PAD3 <- mean(subset(summarized_table_1, Config != 6 & PADD ==3)$Avg_Avoidance_Cost)

AvgCost_PADNot3 <- mean(subset(summarized_table_1, PADD !=3)$Avg_Avoidance_Cost)

##################Total Avoided CO2 by $93 to $170#####################

summarized_table_1 <- df1_filtered %>%
  filter(Marginal.Abatement.Cost....t < 100)%>%
  group_by(Scenario, Year) %>% 
  summarise(Sum_Emissions_Reduction_Potential = sum(Total.Avoided.CO2.kt.y, na.rm = TRUE), .groups = 'drop')


###############

summarized_table_1 <- df1_filtered %>%
  group_by(Config, Scenario, Year, PADD) %>%  
  summarise(
    Max_cost = max(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Capture_Scenario_Max = Capture.Scenario[which.max(Marginal.Abatement.Cost....t)],
    Min_cost = min(Marginal.Abatement.Cost....t, na.rm = TRUE),
    Capture_Scenario_Min = Capture.Scenario[which.min(Marginal.Abatement.Cost....t)],
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table_1)

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
