#Supplementary figs
rm(list=ls())

#set wd
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs")

# Load necessary libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(readr)

# Read in new datasets
PR_int_YoOl <- read_csv("PR.int.YoOl.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

PR_slp_YoOl <- read_csv("PR.slp.YoOl.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

PR_int_wCoh <- read_csv("PR.int.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

PR_slp_wCoh <- read_csv("PR.slp.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

# Remove second row twice as per previous processing
PR_int_YoOl <- PR_int_YoOl[-2, ]
PR_int_YoOl <- PR_int_YoOl[-2, ]
PR_int_wCoh <- PR_int_wCoh[-2, ]
PR_int_wCoh <- PR_int_wCoh[-2, ]

PR_slp_YoOl <- PR_slp_YoOl[-2, ]
PR_slp_YoOl <- PR_slp_YoOl[-2, ]
PR_slp_wCoh <- PR_slp_wCoh[-2, ]
PR_slp_wCoh <- PR_slp_wCoh[-2, ]

# Define model labels with subscripts
Lables <- c(expression(M[0]), expression(M[1]), expression(M[2]), expression(M[3]),
            expression(M[11]), expression(M[12]), expression(M[13]),
            expression(M[21]), expression(M[22]), expression(M[23]))

# Define colors for each model (color-blind friendly)
color_map <- c("M0" = "#56B4E9",  # Genetics
               "M1" = "#009E73", "M2" = "#009E73", "M3" = "#009E73",  # Lifestyle
               "M11" = "#F39B7F", "M12" = "#F39B7F", "M13" = "#F39B7F",  # GxL
               "M21" = "#CC79A7", "M22" = "#CC79A7", "M23" = "#CC79A7")  # GxL Interaction

# Convert Model column to factor with specified levels
convert_model_factor <- function(df) {
  df %>%
    mutate(Model = factor(Model, levels = c("M0", "M1", "M2", "M3",
                                            "M11", "M12", "M13",
                                            "M21", "M22", "M23")))
}

# Apply factor conversion to all datasets
PR_int_YoOl <- convert_model_factor(PR_int_YoOl)
PR_int_wCoh <- convert_model_factor(PR_int_wCoh)
PR_slp_YoOl <- convert_model_factor(PR_slp_YoOl)
PR_slp_wCoh <- convert_model_factor(PR_slp_wCoh)

# Create individual plots for intercept
plot_YoOl_int <- ggplot(PR_int_YoOl, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = expression(beta[0])) + 
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_wCoh_int <- ggplot(PR_int_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

# Create individual plots for slope
plot_YoOl_slp <- ggplot(PR_slp_YoOl, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "", y = expression(beta[1])) + 
  scale_y_continuous(limits = c(0.6, 1.00)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_wCoh_slp <- ggplot(PR_slp_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.6, 1.00)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

# Align left plots (YoOl) and right plots (wCoh)
left_plots_aligned <- plot_grid(plot_YoOl_int, plot_YoOl_slp, nrow = 2, align = "v", labels = c("a", "c"), label_size = 20)
right_plots_aligned <- plot_grid(plot_wCoh_int, plot_wCoh_slp, nrow = 2, align = "v", labels = c("b", "d"), label_size = 20)

# Combine left and right plots into a single figure
combined_plots_aligned <- plot_grid(left_plots_aligned, right_plots_aligned, ncol = 2, align = "h")

# Save the final plot as TIFF with LZW compression
# ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S3_Fig_int_slp_YoOl_wCoh_PR.tiff", 
#        combined_plots_aligned, 
#        width = 24, 
#        height = 16, 
#        dpi = 300, 
#        device = "tiff", 
#        compression = "lzw")

ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S3_Fig_int_slp_YoOl_wCoh_PR.pdf", 
       combined_plots_aligned, 
       width = 24, 
       height = 16, 
       dpi = 300, 
       device = "pdf")
