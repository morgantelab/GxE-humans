

# Load necessary packages
library(ggplot2)
library(cowplot)

# Define the order of variables for the x-axis
variable_order <- c("Townsend", "walk.d", "act0.d", "act1.d", "TVtime", "PCtime", "DRtime", 
                    "getup", "sleep.d", "smoking.now", "smoked.past", "veg.cook", "fish.oily", 
                    "fish.lean", "meat.proc", "poultry", "beef", "lamb", "pork", "cheese", 
                    "salt", "tea", "coffee", "alc1", "waist", "BFP", "BMR")

# Create the dot plot with CI and reordered x-axis variables
p <- ggplot(table1_dataset, aes(x = factor(Variable, levels = variable_order), y = Estimate, color = Component)) +
  geom_point(position = position_dodge(width = 0.9), size = 4.5) + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), position = position_dodge(width = 0.9), width = 0.25) + 
  scale_color_manual(values = c("Cohort" = "#E69F00", "Genetics" = "#56B4E9", "Lifestyle" = "#009E73")) +
  labs(title = "", 
       x = "", y = "Variance") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
#        axis.text.x = element_text(vjust = 0.5, size = 30),
        axis.text.y = element_text(size = 30),
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 30 , face = "plain")) +
  guides(color = guide_legend(title = NULL))  # Remove legend title

# Increase the space between variables on x-axis
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Save the plot to the specified directory
output_directory <- "plots_0524"
output_file <- file.path(output_directory, "table1_plot_reordered.png")
ggsave(output_file, plot = p, width = 20, height = 12)

cat("Plot saved at:", output_file)
