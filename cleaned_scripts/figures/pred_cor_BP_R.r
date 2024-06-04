# script to plot pred_cor_BP.png
# Load necessary libraries
library(ggplot2)
library(grid)
library(cowplot)
library(dplyr)
library(gridExtra)
library(gridGraphics)

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


# Convert Model column to factor with specified levels
DP_acc_YoOl$Model <- factor(DP_acc_YoOl$Model, levels = names(color_map))
PP_acc_YoOl$Model <- factor(PP_acc_YoOl$Model, levels = names(color_map))
SP_acc_YoOl$Model <- factor(SP_acc_YoOl$Model, levels = names(color_map))

# Create the plots
plot_dp <- ggplot(DP_acc_YoOl, aes(x = Model, y = VC, colour=Model)) +
  geom_point(size=8, position=position_dodge(0.5)) +
  labs(x="", y = expression(italic(r))) +
  scale_y_continuous(limits=c(0, 0.3)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 30, vjust = 1),
        legend.position = "none")

plot_sp <- ggplot(SP_acc_YoOl, aes(x = Model, y = VC, colour=Model)) +
  geom_point(size=8, position=position_dodge(0.5)) +
  labs(x="", y = "") +
  scale_y_continuous(limits=c(0, 0.3)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

plot_pp <- ggplot(PP_acc_YoOl, aes(x = Model, y = VC, colour=Model)) +
  geom_point(size=8, position=position_dodge(0.5)) +
  labs(x="", y = "") +
  scale_y_continuous(limits=c(0, 0.3)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_map) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = "none")

# Combine the plots
combined_plot <- plot_grid(plot_dp,
                           plot_sp,
                           plot_pp,
                           ncol=3, align = "h", labels = c("a", "b", "c"), label_size = 20, label_fontface = "bold")

# Save the plot
ggsave("Figure_2.png", combined_plot, width = 24, height = 10, dpi = 300)
