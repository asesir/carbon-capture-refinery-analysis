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
####################Loading Data Frame#########################################

df_decision_count<-read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S6_RO_decisioncount_Sensitivity.csv")

prepare_data <- function(df_decision_count, main_var, level, other_vars) {
  if (level == "Ref") {
    df_decision_count %>% filter(across(all_of(main_var), ~.x == "Ref"))
  } else {
    df_decision_count %>% filter(!!sym(main_var) == level, across(all_of(other_vars), ~.x == "Ref"))
  }
}

# Create a list of variable names for the first 5 columns
variables <- names(df_decision_count)[3:7]

# Prepare the data for the "Ref" scenario, which will be the same for all facets
data_ref <- prepare_data(df_decision_count, variables, "Ref", variables)

# Initialize an empty list to store data for each facet and level
facet_list <- list()

# Loop through each variable and level to create a subset of the data
for (var in variables) {
  facet_list[[var]] <- list(
    Low = prepare_data(df_decision_count, var, "Low", setdiff(variables, var)),
    Ref = data_ref, # Reuse the data_ref for the "Ref" condition
    High = prepare_data(df_decision_count, var, "High", setdiff(variables, var))
  )
}

# Combine the list into a single dataframe for plotting
plot_data <- map_dfr(facet_list, ~bind_rows(.x, .id = "Level"), .id = "Variable")

# Calculate the percentages
plot_data <- plot_data %>%
  group_by(Year, Variable, Level) %>%
  mutate(TotalCount = sum(Count)) %>%
  ungroup() %>%
  mutate(Percentage = Count / TotalCount)

# Reorder the levels of the factor for Variable and Level
plot_data <- plot_data %>%
  mutate(Variable = recode(Variable,
                           'Risk.Free.Interest.Rate' = "Risk-free Interest Rate",
                           'OilPrices' = "Crack Spread",
                           'Carbon.Capture.Operating.Cost.Variation'="CCS Utility Cost",
                           'Carbon.Price.Variation' = "Carbon Price",
                           'Carbon.Capture.CapEx' = "CCS CapEx"),
         Level = recode(Level,
                        `High` = "High Scenario",
                        `Ref` = "Reference Scenario",
                        `Low` = "Low Scenario"))

plot_data$Variable <- factor(plot_data$Variable, 
                             levels = c("Crack Spread",
                                        "Carbon Price", 
                                        "CCS CapEx",
                                        "CCS Utility Cost", 
                                        "Risk-free Interest Rate"))

plot_data$Level <- factor(plot_data$Level, levels = c("High Scenario", "Reference Scenario", "Low Scenario"))

plot_colors<- c(
  
  "#2cb5c0",
  
  "#f8b620",
  
  "#e03426"
  
)

# plot_data <- plot_data %>%
#   filter(Variable != "Refinery Lifetime")

# Generate the plot
p1 <- ggplot(plot_data, aes(x = Year, y = Percentage, fill = RO_Options)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(rows = vars(Variable), cols = vars(Level)) +
  scale_fill_manual(values = plot_colors) +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,0.5,1)) +
  scale_x_discrete(limits = (2019:2050), breaks = (seq(2019, 2050, by = 5)))+
  labs(x = "Year", y = "Count", fill = "RO_Options") +
  labs(x = "Year",
       y = expression("Probability of Continue Operation, Deploy Carbon Capture and Shutdown Refinery, %"),
       fill = "Options",
       title = expression ("Possibility of Different Options for a Medium Refinery with Baseline Fuel Demand in 2019")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 270),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        strip.text.x = element_text(size = 6, face = "bold"),
        strip.text.y = element_text(size = 6, face = "bold"),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  theme(plot.title = element_blank())+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))


print(p1)

# # Specify the directory path where you want to save the figure
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# save_directory <- "C:/Users/fang.li/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS6.png")
ggsave(file_path, p1, bg = "white", width = 7, height = 7, units = "in", dpi = 600)
