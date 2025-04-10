#Supplementary figs
rm(list=ls())

#set wd
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/datasets/datasets_figs")

# Load necessary libraries
library(ggplot2)
library(cowplot)
library(dplyr)
library(readr)

DP_int_wCoh <- read_csv("DP.int.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

DP_slp_wCoh <- read_csv("DP.slp.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

SP_int_wCoh <- read_csv("SP.int.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

SP_slp_wCoh <- read_csv("SP.slp.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

PP_int_wCoh <- read_csv("PP.int.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

PP_slp_wCoh <- read_csv("PP.slp.wCoh.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))

DP_int_wCoh <- DP_int_wCoh[-2, ]
DP_int_wCoh <- DP_int_wCoh[-2, ]
SP_int_wCoh <- SP_int_wCoh[-2, ]
SP_int_wCoh <- SP_int_wCoh[-2, ]
PP_int_wCoh <- PP_int_wCoh[-2, ]
PP_int_wCoh <- PP_int_wCoh[-2, ]

DP_slp_wCoh <- DP_slp_wCoh[-2, ]
DP_slp_wCoh <- DP_slp_wCoh[-2, ]
SP_slp_wCoh <- SP_slp_wCoh[-2, ]
SP_slp_wCoh <- SP_slp_wCoh[-2, ]
PP_slp_wCoh <- PP_slp_wCoh[-2, ]
PP_slp_wCoh <- PP_slp_wCoh[-2, ]

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
DP_int_wCoh <- convert_model_factor(DP_int_wCoh)
SP_int_wCoh <- convert_model_factor(SP_int_wCoh)
PP_int_wCoh <- convert_model_factor(PP_int_wCoh)
DP_slp_wCoh <- convert_model_factor(DP_slp_wCoh)
SP_slp_wCoh <- convert_model_factor(SP_slp_wCoh)
PP_slp_wCoh <- convert_model_factor(PP_slp_wCoh)

# Create individual plots for intercept
plot_dp_int <- ggplot(DP_int_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = expression(beta[0])) + 
  scale_y_continuous(limits = c(-2, 1.5)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_sp_int <- ggplot(SP_int_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(-2, 1.5)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        #        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_pp_int <- ggplot(PP_int_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(-2, 1.5)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        #        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

# Create individual plots for slope
plot_dp_slp <- ggplot(DP_slp_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "", y = expression(beta[1])) + 
  scale_y_continuous(limits = c(0.75, 1.25)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_sp_slp <- ggplot(SP_slp_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.75, 1.25)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        #        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_pp_slp <- ggplot(PP_slp_wCoh, aes(x = Model, y = Val, colour = Model)) + 
  geom_point(size = 8, position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "", y = "") + 
  scale_y_continuous(limits = c(0.75, 1.25)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        #        axis.title.y = element_text(size = 30, vjust = 1),
        legend.position = "none")

# Combine the plots into a 2x3 grid
#middle_plots <- plot_grid(plot_dp_int, plot_sp_int, plot_pp_int, ncol = 3, labels = c("a", "b", "c"), label_size = 20, label_fontface = "bold")
#bottom_plots <- plot_grid(plot_dp_slp, plot_sp_slp, plot_pp_slp, ncol = 3, labels = c("d", "e", "f"), label_size = 20, label_fontface = "bold")

# Combine top and bottom plots
#combined_plot <- plot_grid(middle_plots, bottom_plots, ncol = 1, rel_heights = c(1, 1))

# Save the plot
#ggsave("Supp_Figure_int_slp_Combined.png", combined_plot, width = 24, height = 16, dpi = 300)

# Align left plots first based on the y-axis line
left_plots_aligned <- plot_grid(plot_dp_int, plot_dp_slp, nrow=2, align = "v", labels = c("a", "d"), label_size = 20)

# Align middle plots based on the y-axis line
middle_plots_aligned <- plot_grid(plot_sp_int, plot_sp_slp, ncol = 1, align = "v", labels = c("b", "e"), label_size = 20, hjust = 0.5)

# Align right plots based on the y-axis line
right_plots_aligned <- plot_grid(plot_pp_int, plot_pp_slp, ncol = 1, align = "v", labels = c("c", "f"), label_size = 20, hjust = 0.5)

# Combine the aligned left, middle, and right plots
combined_plots_aligned <- plot_grid(left_plots_aligned, middle_plots_aligned, right_plots_aligned, ncol = 3, align = "h")

# Save the final plot
#ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S2_Fig_int_slp_rnd_BP.png", combined_plots_aligned, width = 24, height = 16, dpi = 300)

# save plot as tiff Feb 28 2025
# Save the final plot as a compressed TIFF under 5MB
# ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S2_Fig_int_slp_rnd_BP.tiff", 
#        combined_plots_aligned, 
#        width = 24, 
#        height = 16, 
#        dpi = 300, 
#        device = "tiff", 
#        compression = "lzw")

ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/S2_Fig_int_slp_rnd_BP.pdf", 
       combined_plots_aligned, 
       width = 24, 
       height = 16, 
       dpi = 300, 
       device = "pdf")
