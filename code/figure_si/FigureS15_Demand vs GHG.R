# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(grid)
library(ggthemes)
library(wesanderson)
library(scales)
library(ggrepel)


#####################SI figure for light/sweet crude#######################

df<- read.csv("C:/Users/file_location/Figure S15_DemandvsEmissions.csv")


# # Specify the directory path where you want to save the figure
save_directory <- "C:/Users/file_location/"



# 
# # Create the full file path
file_path <- file.path(save_directory, "FigS15.png")

df_long <- df%>% 
  pivot_longer(cols = -c("Demand..MMbbl.year", "Crude.Type"),
               names_to = "Category",
               values_to = "Emissions")

line_types <- c("No.Carbon.Capture" = "solid", 
                "Max.Carbon.Capture" = "twodash", 
                "Min.Carbon.Capture" = "dotted")

colors <- wes_palette("Rushmore", 3, type = "continuous")


# Calculate the midpoint for the vertical line
# x_coord <- mean(c(6101,5334))
x_coord <- 4714
x_coord_end <- 2103

p1 <- ggplot(df_long, aes(x=Demand..MMbbl.year, y=Emissions, color=Category, group=Category, linetype=Category)) +
  geom_point(size=2) + # Increase the size of the points
  # geom_line(size=0.5) + 
  geom_text_repel(data = df_long[df_long$Category == "Min.Carbon.Capture",],
                  aes(label=sprintf("%.0f", Emissions)),
                  nudge_y = -5, size=2, 
                  color="black", segment.color = NA) +
  geom_text_repel(data = df_long[df_long$Category != "Min.Carbon.Capture",],
                  aes(label=sprintf("%.0f", Emissions)),
                  nudge_y = 5, size=2, 
                  color="black", segment.color = NA) +
  facet_wrap( ~ Crude.Type)+
  scale_x_reverse() +
  scale_linetype_manual(values=line_types) +
  scale_color_manual(values=colors) +
  labs(x=expression("Annual Estimated U.S. Refining Products Demand, MMbbl yr "^{-1}*""),
       y=expression("Annual Estimated U.S. Refinery GHG Emissions, million tonnes CO"[2]*"-eq yr"^{-1}*" ")) +
  guides(color=guide_legend(override.aes=list(linetype=c("solid", "twodash", "dotted")))) +
  geom_vline(xintercept = x_coord, linetype="dashed", color="black") +
  geom_vline(xintercept = x_coord_end, linetype="dashed", color="black") +
  geom_vline(xintercept = 5539, linetype="dashed", color="darkred") +
  geom_segment(aes(x = x_coord - 750, y = 5,
                   xend = x_coord, yend = 5), # Adjust 'xend' as needed
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "black",
               show.legend = FALSE) +
  geom_segment(aes(x = x_coord_end + 750, y = 5,
                   xend = x_coord_end, yend = 5), # Adjust 'xend' as needed
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "black",
               show.legend = FALSE) +
  geom_segment(aes(x = 5539, y = 20, 
                   xend = 5539, yend = 10), # Adjust 'yend' as needed
               arrow = arrow(length = unit(0.1, "inches"), type = "open", ends = "last"),
               size = 0.3,
               color = "darkred",
               show.legend = FALSE) +
  geom_text(aes(x = 5539+400, y = 5, label = "Demand Estimated\nby EIA"), # Adjust 'x' and 'y' as needed
            hjust = 0, vjust = 0.5, size = 1.5, color = "darkred") +
  geom_text(aes(x = x_coord - 800, y = 5, label = "Demand Forecasted\nby NREL's EFS"), # Adjust 'x' and 'y' as needed
            hjust = 0, vjust = 0.5, size = 1.5, color = "black")+
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.title.y = element_text(size=6, hjust=0.75),
        axis.text.y = element_text(size=5.5),
        axis.title.x = element_text(size=6, margin = margin(t = 15, unit = "pt")),
        axis.text.x = element_text(size=5.5),
        legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6), 
        legend.key.size = unit(0.5, "lines"), 
        legend.key.width = unit(0.5, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.spacing.x = unit(0.1, "cm"), 
        legend.spacing.y = unit(0.1, "cm"), 
        legend.margin = margin(2, 2, 2, 2), 
        legend.direction = "horizontal", 
        legend.box = "vertical",
        strip.text.x = element_text(size = 6)) +
  coord_cartesian(ylim = c(30, 240),
                  clip = "off")

p1 <- p1 + 
  annotate("segment", x = x_coord, xend = x_coord, 
           y = -Inf, yend = 7, 
           linetype = "dashed", color = "black")

print(p1)


ggsave(file_path, p1, bg="white", width = 4.5, height = 5, units = "in", dpi = 600)

