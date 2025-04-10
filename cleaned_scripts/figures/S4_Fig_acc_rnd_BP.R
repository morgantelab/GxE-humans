rm(list=ls())

#set wd
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs")


# Load necessary libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(readr)
library(grid)
library(dplyr)
library(gridExtra)
library(gridGraphics)

DP_acc_wCoh <- read_csv("DP.acc.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
SP_acc_wCoh <- read_csv("SP.acc.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
PP_acc_wCoh <- read_csv("PP.acc.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

DP_acc_wCoh <- DP_acc_wCoh[-2, ]
DP_acc_wCoh <- DP_acc_wCoh[-2, ]
SP_acc_wCoh <- SP_acc_wCoh[-2, ]
SP_acc_wCoh <- SP_acc_wCoh[-2, ]
PP_acc_wCoh <- PP_acc_wCoh[-2, ]
PP_acc_wCoh <- PP_acc_wCoh[-2, ]

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

# Apply factor conversion to all datasets
DP_acc_wCoh <- convert_model_factor(DP_acc_wCoh)
SP_acc_wCoh <- convert_model_factor(SP_acc_wCoh)
PP_acc_wCoh <- convert_model_factor(PP_acc_wCoh)

# Create individual plots for intercept
plot_dp_acc <- ggplot(DP_acc_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  labs(x = "", y = expression(italic(r))) + 
  scale_y_continuous(limits = c(0.26, 0.48)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_sp_acc <- ggplot(SP_acc_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.26, 0.48)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

plot_pp_acc <- ggplot(PP_acc_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.26, 0.48)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

# Combine the plots into a 2x3 grid
top_plots <- plot_grid(plot_dp_acc, plot_sp_acc, plot_pp_acc, ncol = 3, labels = c("a", "b", "c"), label_size = 20, label_fontface = "bold")

# Add the legend below the combined plot
final_plot <- plot_grid(top_plots, ncol = 1, rel_heights = c(1, 0.1))

# Save the plot
#ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S1_Fig_acc_rnd_BP.png", final_plot, width = 24, height = 10, dpi = 300)

# # Save plot in tiff Feb 28 2025
# ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S1_Fig_acc_rnd_BP.tiff", 
#        final_plot, 
#        width = 24, 
#        height = 10, 
#        dpi = 300, 
#        device = "tiff", 
#        compression = "lzw")

# Save plot in tiff Feb 28 2025
ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S1_Fig_acc_rnd_BP.pdf", 
       final_plot, 
       width = 24, 
       height = 10, 
       dpi = 300, 
       device = "pdf")
