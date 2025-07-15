# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(wesanderson)
# Read the dataset
df_io <- read.csv("C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/Data/File/Figure Plotting File/SI/Figure S11_RefineryInputOutput_LinearRegression.csv")
# df_io <- read.csv("D:/PhD/PhD Programe UofC/LCA/Paper 01/Data/RefineryInputOutput_LinearRegression.csv")

# Create a new column for updated PADD labels
df_io$PADD_label <- paste("PADD", df_io$PADD)

# Proceed with your existing regression analysis code
regression_results <- df_io %>%
  group_by(PADD_label) %>%
  summarize(
    Intercept = coef(lm(input_var ~ 0 + production_var))[1],
    R2 = summary(lm(input_var ~ 0 + production_var ))$r.squared,
    .groups = 'drop'
  )

regression_results$label <- paste("italic(y) == ", round(regression_results$Intercept, 3),
                                  "*italic(x)~~~R^2 == ", round(regression_results$R2, 3))

# Plotting
p <- ggplot(df_io, aes(x = production_var, y = input_var)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, linetype = "dotted", color = "red", se = FALSE,size = 1) +
  facet_wrap(~ PADD_label, scales = "free") +  # Updated to use PADD_label
  geom_text(data = regression_results, aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.1, size = 2, parse = TRUE, color = "blue") +
  labs(title = "Relationship between Refinery Production and Crude Input by PADD",
       x = expression("Total Volume of Refinery Production, mbbl d"^{-1}*""),
       y = expression("Total Volume of Refinery Crude Input, mbbl d"^{-1}*"")) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 6),
    axis.text.y = element_text(size = 6), 
    axis.title.x = element_text(size = 6),
    axis.text.x = element_text(size = 6, angle = 270),
    legend.text = element_text(size = 4),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    strip.text.x = element_text(size = 8, face = "bold"),
    strip.text.y = element_text(size = 8, face = "bold"),
    strip.placement = "outside",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

print(p)


# save_directory <- "D:/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
save_directory <- "C:/Users/Asesi/OneDrive/PhD/PhD Programe UofC/LCA/Paper 01/SI Figures/"
# # Create the full file path
file_path <- file.path(save_directory, "FigS11.png")


ggsave(file_path, p, bg = "white", width = 4.5, height = 4.5, units = "in", dpi = 600)
