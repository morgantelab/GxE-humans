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
library(ggpubr)
library(patchwork)

DP_acc_YoOl <- read_csv("DP.acc.YoOl.csv",
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
DP_acc_YoOl <- DP_acc_YoOl[-2, ]
DP_acc_YoOl <- DP_acc_YoOl[-2, ]

SP_acc_YoOl <- read_csv("SP.acc.YoOl.csv",
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
SP_acc_YoOl <- SP_acc_YoOl[-2, ]
SP_acc_YoOl <- SP_acc_YoOl[-2, ]

PP_acc_YoOl <- read_csv("PP.acc.YoOl.csv", 
                        col_names = c("Model", "Val"),
                        col_types = cols(Val = col_number()))
PP_acc_YoOl <- PP_acc_YoOl[-2, ]
PP_acc_YoOl <- PP_acc_YoOl[-2, ]

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
DP_acc_YoOl <- convert_model_factor(DP_acc_YoOl)
SP_acc_YoOl <- convert_model_factor(SP_acc_YoOl)
PP_acc_YoOl <- convert_model_factor(PP_acc_YoOl)

# Create the plots
plot_dp <- ggplot(DP_acc_YoOl, aes(x = Model, y = Val, colour=Model)) +
  geom_point(size=8, position=position_dodge(0)) +
  labs(x="", y = expression(italic(r))) +
  scale_y_continuous(limits=c(0, 0.35), breaks = seq(0, 0.3, 0.1)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, color = 'black'),
        axis.title = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_sp <- ggplot(SP_acc_YoOl, aes(x = Model, y = Val, colour=Model)) +
  geom_point(size=8, position=position_dodge(0)) +
  labs(x="", y = "") +
  scale_y_continuous(limits=c(0, 0.35), breaks = seq(0, 0.3, 0.1)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20, colour = 'black'),
        axis.text.y = element_text(size = 20, colour = 'black'),
        legend.position = "none")

plot_pp <- ggplot(PP_acc_YoOl, aes(x = Model, y = Val, colour=Model)) +
  geom_point(size=8, position=position_dodge(0)) +
  labs(x="", y = "") +
  scale_y_continuous(limits=c(0, 0.35), breaks = seq(0, 0.3, 0.1)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20, colour = 'black'),
        axis.text.y = element_text(size = 20, colour = 'black'),
        legend.position = "none")

# Combine the plots into a 2x3 grid
#top_plots <- plot_grid(plot_dp, plot_sp, plot_pp, ncol = 3, labels = c("a", "b", "c"), label_size = 20, label_fontface = "bold")

# Add the legend below the combined plot
#final_plot <- plot_grid(top_plots, ncol = 1, rel_heights = c(1, 1))

# # # Arrange plots side by side with large labels
#  final_plot <- (plot_dp | plot_sp | plot_pp) + 
#    plot_annotation(tag_levels = "a")

# Create the plot layout using patchwork (ensures even spacing)
plot_grid <- plot_dp | plot_sp | plot_pp

# Manually add a, b, c with `cowplot::draw_plot_label()`
final_plot <- ggdraw() + 
  draw_plot(plot_grid, 0, 0, 1, 1) +  # Place the patchwork grid
  draw_plot_label(c("a", "b", "c"), x = c(0.0, 0.33, 0.66), y = 1, size = 22, fontface = "bold")






# Save the plot
#ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/Fig4_pred_cor_BP.png", final_plot, width = 24, height = 10, dpi = 300)

# # Save the plot for tiff format Feb 28 2025
# ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/Fig4_pred_cor_BP.tiff", 
#        final_plot, 
#        width = 24, 
#        height = 10, 
#        dpi = 300, 
#        device = "tiff", 
#        compression = "lzw")

# Save the plot for tiff format Feb 28 2025
ggsave("/data2/morgante_lab/ukbiobank_projects/GxE/figures/Fig4_pred_cor_BP.pdf", 
       final_plot, 
       width = 24, 
       height = 10, 
       dpi = 300, 
       device = "pdf")
