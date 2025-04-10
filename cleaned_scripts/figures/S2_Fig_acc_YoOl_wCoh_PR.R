rm(list=ls())

# Set working directory
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs")

# Load necessary libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(readr)
library(grid)
library(gridExtra)
library(gridGraphics)

# Read new datasets
PR_acc_YoOl <- read_csv("PR.acc.YoOl.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
PR_acc_wCoh <- read_csv("PR.acc.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

# Remove second row twice as per previous processing
PR_acc_YoOl <- PR_acc_YoOl[-2, ]
PR_acc_YoOl <- PR_acc_YoOl[-2, ]
PR_acc_wCoh <- PR_acc_wCoh[-2, ]
PR_acc_wCoh <- PR_acc_wCoh[-2, ]

# Define model labels with subscripts
Lables <- c(expression(M[0]), expression(M[1]), expression(M[2]), expression(M[3]),
            expression(M[11]), expression(M[12]), expression(M[13]),
            expression(M[21]), expression(M[22]), expression(M[23]))

# Define colors for category
color_map <- c("M0" = "#56B4E9", # Genetics
               "M1" = "#009E73", "M2" = "#009E73", "M3" = "#009E73", # Lifestyle
               "M11" = "#F39B7F", "M12" = "#F39B7F", "M13" = "#F39B7F", # GxL
               "M21" = "#CC79A7", "M22" = "#CC79A7", "M23" = "#CC79A7") # GxL Interaction

# Convert Model column to factor with specified levels
convert_model_factor <- function(df) {
  df %>%
    mutate(Model = factor(Model, levels = c("M0", "M1", "M2", "M3",
                                            "M11", "M12", "M13",
                                            "M21", "M22", "M23")))
}

# Apply factor conversion to both datasets
PR_acc_YoOl <- convert_model_factor(PR_acc_YoOl)
PR_acc_wCoh <- convert_model_factor(PR_acc_wCoh)

# Create plot for YoOl (Left Side, a)
plot_YoOl <- ggplot(PR_acc_YoOl, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  labs(x = "", y = expression(italic(r))) + 
  scale_y_continuous(limits = c(0.1, 0.3)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

# Create plot for wCoh (Right Side, b)
plot_wCoh <- ggplot(PR_acc_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.1, 0.3)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

# Combine the plots into a 1x2 grid
final_plot <- plot_grid(plot_YoOl, plot_wCoh, ncol = 2, labels = c("a", "b"), label_size = 20, label_fontface = "bold")

# Save the plot in TIFF format
# ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S2_Fig_acc_YoOl_wCoh_PR.tiff",
#       final_plot,
#       width = 24,
#       height = 10,
#       dpi = 300,
#       device = "tiff",
#       compression = "lzw")

# Save the plot in PDF format
ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S2_Fig_acc_YoOl_wCoh_PR.pdf", 
       final_plot, 
       width = 24, 
       height = 10, 
       dpi = 300, 
       device = "pdf")

