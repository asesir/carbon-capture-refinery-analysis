########################Loading Library##########################################################
library(dplyr)
library(tidyverse)
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

#####################Loading DataFrame################
df <- read.csv("C:/Users/file_locatioin/Figure S12_CarbonPriceTrajectory.csv")

df_long <- df %>% 
  pivot_longer(
               cols = starts_with("Run"), 
               names_to = "Iteration", 
               values_to = "Prices")

summarized_table <- df_long %>%
  group_by(Date) %>%  
  summarise(
    Max_price = max(Prices, na.rm = TRUE),
    Iteration_Max = Iteration[which.max(Prices)],
    Min_price = min(Prices, na.rm = TRUE),
    Iteration_Min = Iteration[which.max(Prices)],
    Mean_price = mean(Prices, na.rm = TRUE),
    .groups = 'drop')  # Summarize and drop grouping
# Display the resulting summarized table
print(summarized_table)

write.csv(summarized_table, file = 'C:/Users/file_locatioin/FigureS5_CarbonPrice_stats.csv', row.names = FALSE)

#########################Plot############################
library(tidyverse)
library(wesanderson) # Ensure this package is installed for custom color palettes

# Load data
carbon_data <- read.csv("C:/Users/file_locatioin/Figure S13_CarbonPrice_stats.csv")

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

# Plotting
plot_carbonprice <- ggplot(carbon_data) +
  geom_line(aes(x = Year, y = HistoricalPrice, color = "Historical Carbon Prices"), size = 0.5, na.rm = TRUE) +
  geom_ribbon(aes(x = Year, ymin = MinProjected, ymax = MaxProjected, fill = "Variation of Projected Carbon Prices"), alpha = 0.3, na.rm = TRUE) +
  geom_line(aes(x = Year, y = MeanProjected, color = "Projected Carbon Prices"), size = 0.5, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(min(carbon_data$Year, na.rm = TRUE), 2050, by = 5), limits = c(min(carbon_data$Year, na.rm = TRUE), 2050)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 250), limits = c(0, 1000)) +
  scale_color_manual(values = c("Historical Carbon Prices" = historical_color, "Projected Carbon Prices" = projected_color)) +
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
    axis.text.x = element_text(angle = 270, hjust = 1, size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  guides(
    fill = guide_legend(order = 2, title = NULL, nrow = 1, byrow = TRUE),
    color = guide_legend(order = 1, title = NULL, nrow = 1, byrow = TRUE)
  )

# Print the plot
print(plot_carbonprice)


save_directory <- "C:/Users/file_locatioin/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS12.png")


ggsave(file_path, p, bg = "white", width = 4.5, height = 4.5, units = "in", dpi = 600)
