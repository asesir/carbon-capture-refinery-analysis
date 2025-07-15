library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggmacc)
library(cowplot)
library(RColorBrewer)
library(wesanderson) 
df1 <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S24_MACC_Sensitivity_Dimensions.csv")

##############################First Figure ###################################################################
df1$Capture.Scenario <- factor(df1$Capture.Scenario)
df1$PADD <- factor(df1$PADD)
df1$Config <- factor(df1$Config)
df1$Year <- factor(df1$Year)
df1 <- df1 %>%
  filter(Scenario != "POTENTIAL")%>%
  mutate(Scenario = recode(Scenario,
                           `LOW` = "High Fuel Demand (HD)",
                           `MEDIUM` = "Med. Fuel Demand (MD)",
                           `HIGH` = "Low Fuel Demand (LD)" )) %>%
  pivot_longer(
    cols = -c(1:10),
    names_to = "Category", 
    values_to = "Avoidance Cost")%>%
  mutate(Category = recode(Category,
                `Avoidance.Cost.Ref....t`="Ref Unit Height",
                `Avoidance.Cost.0.5Ref....t`="50% Ref Unit Height",
                `Avoidance.Cost.1.5Ref....t`="150% Ref Unit Height"))

df1$Scenario <- factor(df1$Scenario, levels=c("Low Fuel Demand (LD)","Med. Fuel Demand (MD)","High Fuel Demand (HD)"))
df1$Category <- factor(df1$Category, levels=c("50% Ref Unit Height","Ref Unit Height","150% Ref Unit Height"))

# Convert the units in your dataframes
df1$Total.Avoided.CO2_Avg.kt.y <- df1$Total.Avoided.CO2_Avg.kt.y / 1000

df1_filtered <- df1 %>% filter(`Avoidance Cost` != 0 & Total.Avoided.CO2_Avg.kt.y > 0.001)

# Generate individual plots for each combination of Scenario and Year
plots <- list()
years <- unique(df1_filtered$Year)

# Compute mean values for each category
df_means <- df1_filtered %>%
  group_by(Category) %>%
  summarise(Mean_Avoidance_Cost = mean(`Avoidance Cost`, na.rm = TRUE))

# Define dot colors from Wes Anderson palette
dot_colors <- wes_palette("Zissou1", n = length(unique(df1_filtered$PADD)), type = "continuous")  

# Violin + Box Plot + Sample Dots + Mean Values
p_violin <- ggplot(df1_filtered, aes(x = Category, y = `Avoidance Cost`)) +
  # Violin plot with black outline (no fill)
  geom_violin(trim = FALSE, fill = NA, color = "black", size = 0.3) +
  # Larger box plot inside the violin
  geom_boxplot(width = 0.8, fill = NA, color = "black", outlier.shape = NA, size = 0.3) +
  # Jittered sample dots (spread out, colored, more visible)
  geom_jitter(aes(color = PADD), width = 0.3, size = 0.3, alpha = 0.3) +
  # Mean value markers (black diamonds)
  geom_point(data = df_means, aes(x = Category, y = Mean_Avoidance_Cost), 
             color = "darkred", shape = 18, size = 3) +
  # Light pink line connecting mean values
  geom_line(data = df_means, aes(x = Category, y = Mean_Avoidance_Cost, group = 1), 
            color = "blue", size = 0.5, alpha = 0.7) +
  # Mean value labels inside a bubble with proper mathematical formatting
  geom_label(data = df_means, aes(x = Category, 
                                  y = Mean_Avoidance_Cost, 
                                  label = paste0(" ", "\u03BC", " = ", round(Mean_Avoidance_Cost, 1))),  
             fill = "white", color = "black", size = 2, label.size = 0.3, vjust = -1.4) +
  # Apply Zissou1 color palette to dots
  scale_color_manual(values = dot_colors) +  
  # Y-axis limit and labels
  scale_y_continuous(limits = c(0, 600)) +
  labs(x = "Absorber & Stripper Height", 
       y = expression("CO"[2]*" avoidance cost ($ tonne CO "[2]*"-eq"^{-1}*")")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 6),
    axis.text.y = element_text(size = 5), 
    axis.title.x = element_text(size = 6),
    axis.text.x = element_text(size = 5),
    legend.position = "bottom",
    legend.title = element_text(size = 4),
    legend.spacing.y = unit(0.1, 'cm'),
    legend.margin = margin(-10, 0, 0, 0),
    legend.text = element_text(size = 4),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

# Print the figure
print(p_violin)



##############Save Figures############################################################
# # Specify the directory path where you want to save the figure
# save_directory <- "D:/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path_1 <- file.path(save_directory, "FigS24.png")
ggsave(file_path_1, p_violin, bg="white", width = 3.42, height = 3.42, units = "in", dpi = 600)




