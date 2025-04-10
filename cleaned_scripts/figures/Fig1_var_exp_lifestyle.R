rm(list=ls()); gc()

# Load necessary packages
library(ggplot2)
library(cowplot)

#set wd
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs/")

# Load the data not using this
Fig_1_without_residual <- read.csv("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs/Fig_1_dataset_without_residual.csv")


# Define the order of variables for the x-axis
variable_order <- c("Townsend", "walk.d", "act0.d", "act1.d", "TVtime", "PCtime", "DRtime", 
                    "getup", "sleep.d", "smoking.now", "smoked.past", "veg.cook", "fish.oily", 
                    "fish.lean", "meat.proc", "poultry", "beef", "lamb", "pork", "cheese", 
                    "salt", "tea", "coffee", "alc1", "waist", "BFP", "BMR")

# Create the dot plot with CI and reordered x-axis variables
p <- ggplot(Fig_1_without_residual, aes(x = factor(Variable, levels = variable_order), y = Estimate, color = Component)) +
  geom_point(position = position_dodge2(width = 1, preserve = "single"), size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 1, position = position_dodge2(width = 1, preserve = "single")) +
  scale_color_manual(name = NULL, values = c("Cohort" = "#E69F00", "Genetics" = "#56B4E9", "Lifestyle" = "#009E73"))+
  scale_y_continuous(limits = c(0, max(Fig_1_without_residual$CI_upper) * 1.1)) +  # Adjust Y range
  labs(title = "", x = "", y = "Variance") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 35),  # Adjusted size
        axis.text.y = element_text(size = 35),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text(size = 35),  # Adjusted legend size
        legend.key.size = unit(3, "cm"),
        axis.title.y = element_text(size = 35, face = "plain"),
        panel.grid.major = element_blank(),  # Remove grid lines for a cleaner look
        panel.grid.minor = element_blank()) 

# Increase the space between variables on x-axis
p <- p + theme(axis.text.x = element_text(margin = margin(t = 10))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# # Save the plot as TIFF instead of PNG
# output_directory <- "/data2/morgante_lab/ukbiobank_projects/GxE/figures/"
# output_file <- file.path(output_directory, "Fig_1_var_exp_lifestyle.tiff")
# 
# tiff(filename = output_file, width = 20, height = 12, units = "in", res = 300, compression = "lzw")
# print(p)
# dev.off()

# Save the plot as a PDF instead of TIFF
output_directory <- "/data2/morgante_lab/ukbiobank_projects/GxE/figures/"
output_file <- file.path(output_directory, "Fig1_var_exp_lifestyle_giant.pdf")

ggsave(output_file, plot = p, width = 24, height = 16, dpi = 300, device = "pdf")



cat("Plot saved at:", output_file)
