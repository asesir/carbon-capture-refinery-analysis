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


df_ys_long_decisioncount <- read.csv("C:/Users/file_location/Figure3_RealOption.csv")
# Load data
carbon_data <- read.csv("C:/Users/file_location/Figure S13_CarbonPrice_stats.csv")


plot_colors<- c(
  
  "#2cb5c0",
  
  "#f8b620",
  
  "#e03426"
  
)


df_ys_long_decisioncount <- df_ys_long_decisioncount %>%
  mutate(
    Year = factor(Year, ordered = TRUE)
  )%>%
  mutate(Scenario = recode(Scenario,
                                `LOW` = "High Fuel Demand",
                                `MEDIUM` = "Medium Fuel Demand",
                                `HIGH` = "Low Fuel Demand",
                                `BASELINE` = "Baseline Fuel Demand"),
         Carbon.Capture.Operating.Cost.Variation = recode(Carbon.Capture.Operating.Cost.Variation, `1` = "RefCCOpEx"),
         Carbon.Price.Variation = recode(Carbon.Price.Variation, `1` = "RefCPS")) %>%
  mutate(Config = factor(Config, levels = c("Hydroskimming", "Medium Conversion", "Deep Conversion")),
         Scenario = factor(Scenario,
                                levels = c("Baseline Fuel Demand", "Low Fuel Demand", "Medium Fuel Demand", "High Fuel Demand")),
         Risk.Free.Interest.Rate = factor(Risk.Free.Interest.Rate, levels = c("LowRFRate", "RefRFRate", "HighRFRate")),
         Carbon.Capture.CapEx = factor(Carbon.Capture.CapEx,levels = c("LowCapEx", "RefCapEx", "HighCapEx")))





# Prepare the data
carbon_data <- carbon_data %>%
  mutate(Year = as.numeric(substring(Date, 1, 4))) %>%
  group_by(Year) %>%
  summarise(
    HistoricalPrice = mean(Mean_price[Year <= 2022], na.rm = TRUE),  # Average historical price
    MinProjected = if_else(Year > 2022, min(Min_price, na.rm = TRUE), NA_real_),  # Minimum projected price
    MaxProjected = if_else(Year > 2022, max(Max_price, na.rm = TRUE), NA_real_),  # Maximum projected price
    MeanProjected = if_else(Year > 2022, mean(Mean_price, na.rm = TRUE), NA_real_)  # Mean projected price
  ) %>%
  ungroup()

# Insert a new row for 2023 as historical data
additional_row <- carbon_data %>%
  filter(Year == 2023) %>%
  mutate(HistoricalPrice = MeanProjected, MinProjected = NA, MaxProjected = NA, MeanProjected = NA)

# Bind the new row to the original dataframe and arrange by year
carbon_data <- bind_rows(carbon_data, additional_row) %>%
  arrange(Year, HistoricalPrice)

# Define colors from wesanderson palette
historical_color <- wes_palette("Zissou1")[1]  # Deep blue
projected_color <- wes_palette("Darjeeling1")[1]  # Strong red
variation_color <- scales::alpha(projected_color, 0.3)  # Lighter shade of the projected color


# Assuming plot_carbonprice and fig3 are already created using your existing scripts
fig3 <- ggplot(df_ys_long_decisioncount, aes(fill = RO_Options, x = Year, y = Count)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(Scenario ~ Config, scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = as.character(2019:2050), breaks = as.character(seq(2019, 2050, by = 5))) +
  scale_fill_manual(values = plot_colors) +
  labs(x = "Year",
       y = expression("Probability of Continue Operation, Deploy Carbon Capture and Shutdown Refinery, %"),
       fill = "Options",
       title = expression ("Possibility of Different Options for Selected Refineries")) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, angle = 270),
        axis.line = element_line(size = 0),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        strip.placement = "outside",
        # strip.background = element_rect(colour = "black", fill = NA, size = 0.3),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.25)) +
  theme(plot.title = element_blank())+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))


plot_carbonprice <- ggplot(carbon_data) +
  geom_line(aes(x = Year, y = HistoricalPrice, color = "Historical Carbon Prices"), size = 0.5, na.rm = TRUE) +
  geom_ribbon(aes(x = Year, ymin = MinProjected, ymax = MaxProjected, fill = "Variation of Projected Carbon Prices"), alpha = 0.3, na.rm = TRUE) +
  geom_line(aes(x = Year, y = MeanProjected, color = "Projected Mean Carbon Prices"), size = 0.5, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(min(carbon_data$Year, na.rm = TRUE), 2050, by = 5), limits = c(min(carbon_data$Year, na.rm = TRUE), 2050)) +
  scale_y_continuous(breaks = seq(0, 600, by = 200), limits = c(0, 600)) +
  scale_color_manual(values = c("Historical Carbon Prices" = historical_color, "Projected Mean Carbon Prices" = projected_color)) +
  scale_fill_manual(values = variation_color) +
  theme_minimal() +
  labs(
    x = "Year",
    y = expression("Carbon Price, ($ tCO"[2]*"-eq"^{-1}*")"),
    title = "Projected Carbon Prices"
  ) +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title.y = element_text(size = 10, margin = margin(r = 1)),
    axis.text.y = element_text(size = 8, margin = margin(r = 1)),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 270),
    axis.ticks = element_line(size = 0.2),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  guides(
    fill = guide_legend(order = 2, title = NULL, nrow = 1, byrow = TRUE),
    color = guide_legend(order = 1, title = NULL, nrow = 1, byrow = TRUE)
  )

plot_carbonprice <- plot_carbonprice +
  theme(plot.margin = unit(c(1, 8, 1, 1.3), "mm"),
        axis.title.y = element_text(margin = margin(r = 10, unit = "pt"))
  )

# Combine the plots
combined_plot <- plot_grid(fig3, plot_carbonprice, ncol = 1, rel_heights = c(3, 1))

# Using `cowplot::draw_label` to manually place labels on the combined plot
combined_plot_with_labels <- cowplot::ggdraw(combined_plot) +
  draw_label("A)", x = 0.02, y = 0.99, hjust = -0.5, vjust = 1, fontface = "bold", size = 15, color = "black") +
  draw_label("B)", x = 0.02, y = 0.29, hjust = -0.5, vjust = 1, fontface = "bold", size = 15, color = "black")

# Print the final combined plot with labels
print(combined_plot_with_labels)




# # Specify the directory path where you want to save the figure

save_directory <- "C:/Users/file_location/"
# # Create the full file path
file_path <- file.path(save_directory, "Figure_3.png")


ggsave(file_path, combined_plot_with_labels,bg="white", width = 7, height = 9, units = "in", dpi = 600)
