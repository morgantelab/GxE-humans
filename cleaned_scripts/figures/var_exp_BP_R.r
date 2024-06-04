# Load necessary libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)

# Labels with subscripts
Lables <- c(expression(M[0]), expression(M[1]), expression(M[2]), expression(M[3]),
            expression(M[11]), expression(M[12]), expression(M[13]),
            expression(M[21]), expression(M[22]), expression(M[23]))

# Color-blind-friendly palette (Okabe-Ito) with abbreviated VC values
color_blind_palette <- c(
  "D" = "#E69F00",        # orange
  "G" = "#56B4E9",        # sky blue
  "GxL" = "#F39B7F",      # reddish orange
  "L" = "#009E73"       # green
)

# Legend labels
legend_labels <- c("D" = "Cohort", "G" = "Genetics", "GxL" = "Genetics x Lifestyle", "L" = "Lifestyle")

# Ensure the Model column is a factor and ordered according to Lables
df_long$Model <- factor(df_long$Model, levels = c("M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23"))

# Ensure the VC column is a factor and ordered according to the desired legend order
df_long$VC <- factor(df_long$VC, levels = c("D", "G", "L", "GxL"))

# Filter out rows with 0 values
df_long <- df_long %>% filter(val != 0)

# Create individual plots
plot_dp <- ggplot(df_long %>% filter(Trait == "DP"), aes(x = Model, y = val, color = VC)) +
  geom_point(position = position_dodge(width = 0.5), size = 7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.4, position = position_dodge(0.5)) +
  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )

plot_sp <- ggplot(df_long %>% filter(Trait == "SP"), aes(x = Model, y = val, color = VC)) +
  geom_point(position = position_dodge(width = 0.5), size = 7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.4, position = position_dodge(0.5)) +
  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )

plot_pp <- ggplot(df_long %>% filter(Trait == "PP"), aes(x = Model, y = val, color = VC)) +
  geom_point(position = position_dodge(width = 0.5), size = 7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.4, position = position_dodge(0.5)) +
  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 25),  # Adjust the size of the text in the legend
    legend.key.size = unit(3, "cm"),  # Adjust the size of the legend keys
    axis.text.x = element_text(size = 35),
    axis.text.y = element_text(size = 35),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )

# Begin creating the PNG output
png(filename = "Figure_1_resize.png", height = 1800, width = 1400, bg = "white")

# Create layout for plots with panel letters
grid.newpage()
pushViewport(viewport(layout = grid.layout(6, 2, heights = unit(c(0, 1, 0, 1, 0, 1), "null"))))

# Add panel letters
grid.text("a", x = unit(0.02, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
print(plot_dp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))

grid.text("b", x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
print(plot_sp, vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))

grid.text("c", x = unit(0.02, "npc"), y = unit(0.31, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
print(plot_pp, vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))

dev.off()  # Close the PNG device
