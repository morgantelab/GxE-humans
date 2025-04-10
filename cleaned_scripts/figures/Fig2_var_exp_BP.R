rm(list=ls()); gc()

# Load necessary libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggpubr)

#original file dir. not using now. 
#setwd("/data2/morgante_lab/ukbiobank_projects/kgoda_practice/gwas_analysis/cohort_batch_effects/plots_manuscript/sep_run_2024")
setwd("/data2/morgante_lab/ukbiobank_projects/GxE/figures/")

# Load the data not using this
#VCEval_20240823 <- read_excel("/data2/morgante_lab/ukbiobank_projects/kgoda_practice/gwas_analysis/cohort_batch_effects/plots_manuscript/sep_run_2024/VCEval_20240823.xlsx", col_types = c("text", "text", "numeric", "text", "text", "text", "numeric"))

# original dataframe
df <- data.frame(
  trait = c("DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", 
            "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", 
            "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP"),
  Model = c("M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23", 
            "M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23", 
            "M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23"),
  D = c("4.80 (4.55, 5.07)", "5.85 (5.18, 6.48)", "5.23 (4.47, 5.87)", "4.78 (4.55, 5.05)", "5.74 (5.07, 6.45)", "5.21 (4.49, 5.94)", "4.78 (4.51, 5.02)", "5.76 (5.16, 6.48)", "5.19 (4.46, 5.85)", "4.79 (4.57, 5.04)", 
        "14.82 (14.35, 15.25)", "12.27 (11.72, 12.89)", "11.97 (11.37, 12.55)", "14.75 (14.35, 15.22)", "12.23 (11.65, 12.8)", "11.94 (11.38, 12.49)", "14.8 (14.33, 15.2)", "12.26 (11.71, 12.92)", "11.96 (11.43, 12.5)", 
        "14.8 (14.41, 15.19)", 
        "17.43 (16.9, 17.85)", "14.77 (14.28, 15.29)", "15 (14.52, 15.48)", "17.4 (16.91, 17.88)", "14.81 (14.28, 15.31)", "15.01 (14.48, 15.5)", "17.43 (16.94, 17.82)", "14.84 (14.33, 15.32)", "15.02 (14.48, 15.51)", "17.43 (17.02, 17.92)"),
  G = c("13.98 (13.21, 14.68)", NA, NA, NA, "12.64 (11.99, 13.21)", "12.64 (12.04, 13.3)", "12.95 (12.36, 13.61)", "12.66 (11.97, 13.26)", "12.65 (12.04, 13.35)", "12.96 (12.36, 13.59)", 
        "12.37 (11.73, 12.97)", NA, NA, NA, "11.49 (10.87, 12.04)", "11.5 (10.93, 12.13)", "11.62 (10.99, 12.19)", "11.52 (10.92, 12.07)", "11.52 (10.99, 12.16)", "11.62 (11.07, 12.18)", 
        "10.19 (9.62, 10.76)", NA, NA, NA, "9.87 (9.23, 10.41)", "9.85 (9.28, 10.4)", "9.88 (9.32, 10.46)", "9.86 (9.26, 10.43)", "9.85 (9.29, 10.44)", "9.92 (9.36, 10.47)"),
  L = c(NA, "11.44 (11, 11.85)", "11.25 (10.82, 11.66)", "10.66 (10.29, 11.01)", "11.52 (11.02, 11.92)", "11.34 (10.92, 11.81)", "10.28 (9.91, 10.64)", "11.59 (11.19, 12.03)", "11.41 (10.95, 11.8)", "10.33 (9.97, 10.65)", 
        NA, "5.66 (5.36, 5.94)", "5.58 (5.29, 5.85)", "5.2 (4.97, 5.51)", "5.57 (5.28, 5.84)", "5.49 (5.21, 5.73)", "4.97 (4.7, 5.2)", "5.62 (5.31, 5.91)", "5.54 (5.28, 5.81)", "4.99 (4.76, 5.25)", 
        NA, "1.25 (1.12, 1.37)", "1.21 (1.07, 1.34)", "1.09 (0.99, 1.21)", "1.17 (1.05, 1.31)", "1.14 (1.02, 1.28)", "1.02 (0.91, 1.13)", "1.19 (1.05, 1.3)", "1.16 (1.03, 1.3)", "1.03 (0.92, 1.15)"),
  GxL = c(NA, NA, NA, NA, NA, NA, NA, "2.43 (1.94, 2.89)", "1.74 (1.43, 2.18)", "2.49 (1.93, 2.92)", 
          NA, NA, NA, NA, NA, NA, NA, "1.59 (1.23, 1.98)", "1.33 (1.06, 1.61)", "1.73 (1.3, 2.17)", 
          NA, NA, NA, NA, NA, NA, NA, "1.64 (1.31, 1.98)", "1.41 (1.15, 1.72)", "1.81 (1.47, 2.14)")
)

# Convert the dataframe to long format
df_long <- df %>%
  pivot_longer(cols = c(D, G, L, GxL), 
               names_to = "VC", 
               values_to = "val")

# Updated extract_ci() function to handle values consistently
extract_ci <- function(x) {
  if (is.na(x) || x == "") {
    return(c(NA, NA, NA))  # Return NA if value is missing or empty
  }
  
  # Extract mean value before the parentheses
  mean_val <- as.numeric(str_extract(x, "^\\d+\\.\\d+"))
  
  # Extract lower and upper CI using regex pattern inside the parentheses
  ci_vals <- str_match(x, "\\((\\d+\\.\\d+),\\s*(\\d+\\.\\d+)\\)")
  
  # If no CI is found, handle it correctly
  if (is.null(ci_vals)) {
    return(c(mean_val, NA, NA))  # Return mean with NAs for CI if not found
  }
  
  ci_lower <- as.numeric(ci_vals[2])
  ci_upper <- as.numeric(ci_vals[3])
  
  return(c(mean_val, ci_lower, ci_upper))
}

# Apply the function to extract values for mean, CI lower, and CI upper
df_long_ci <- df_long %>%
  mutate(
    mean = map_dbl(val, ~extract_ci(.x)[1]),      # Extract mean
    ci_lower = map_dbl(val, ~extract_ci(.x)[2]),  # Extract lower CI
    ci_upper = map_dbl(val, ~extract_ci(.x)[3])   # Extract upper CI
  ) %>%
  select(-val)  # Remove the old 'val' column with combined mean and CI

#fixing the dataset
df_long_ci <- df_long_ci %>%
  mutate(ci_lower = if_else(Model == "M1" & trait == "DP" & VC == "L", 11, ci_lower))
df_long_ci <- df_long_ci %>%
  mutate(ci_upper = if_else(Model == "M1" & trait == "DP" & VC == "L", 11.85, ci_upper))
df_long_ci <- df_long_ci %>%
  mutate(ci_lower = if_else(Model == "M2" & trait == "SP" & VC == "L", 5.3, ci_lower))
df_long_ci <- df_long_ci %>%
  mutate(mean = if_else(Model == "M2" & trait == "PP" & VC == "D", 15, mean))



## this was without CI not using in final plot ##
# convert dataframe to represent long format #
#df <- data.frame(
#  trait = c("DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", "DP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP", "PP"),
#  Model = c("M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23", "M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23", "M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23"),
#  D = c(4.80, 5.85, 5.23, 4.78, 5.74, 5.21, 4.78, 5.76, 5.19, 4.79, 14.82, 12.27, 11.97, 14.75, 12.23, 11.94, 14.8, 12.26, 11.96, 14.8, 17.43, 14.77, 15, 17.4, 14.81, 15.01, 17.43, 14.84, 15.02, 17.43),
#  G = c(13.98, NA, NA, NA, 12.64, 12.64, 12.95, 12.66, 12.65, 12.96, 12.37, NA, NA, NA, 11.49, 11.5, 11.62, 11.52, 11.52, 11.62, 10.19, NA, NA, NA, 9.87, 9.85, 9.88, 9.86, 9.85, 9.92),
#  L = c(NA, 11.44, 11.25, 10.66, 11.52, 11.34, 10.28, 11.59, 11.41, 10.33, NA, 5.66, 5.58, 5.2, 5.57, 5.49, 4.97, 5.62, 5.54, 4.99, NA, 1.25, 1.21, 1.09, 1.17, 1.14, 1.02, 1.19, 1.16, 1.03),
#  GxL = c(NA, NA, NA, NA, NA, NA, NA, 2.43, 1.74, 2.49, NA, NA, NA, NA, NA, NA, NA, 1.59, 1.33, 1.73, NA, NA, NA, NA, NA, NA, NA, 1.64, 1.41, 1.81)
#)

#Labels with subscripts
Lables <- c(expression(M[0]), expression(M[1]), expression(M[2]), expression(M[3]),
            expression(M[11]), expression(M[12]), expression(M[13]),
            expression(M[21]), expression(M[22]), expression(M[23]))

# Color-blind-friendly palette (Okabe-Ito) with abbreviated VC values
color_blind_palette <- c(
  "D" = "#E69F00",        # orange
  "G" = "#56B4E9",        # sky blue
  "L" = "#009E73",       # green
  "GxL" = "#F39B7F"     # reddish orange
)

# Legend labels
legend_labels <- c("D" = "Cohort", "G" = "Genetics", "L" = "Lifestyle", "GxL" = "Genetics x Lifestyle")

# Ensure the Model column is a factor and ordered according to Lables
df_long_ci$Model <- factor(df_long_ci$Model, levels = c("M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23"))

# Ensure the VC column is a factor and ordered according to the desired legend order
df_long_ci$VC <- factor(df_long_ci$VC, levels = c("D", "G", "L", "GxL"))

# Filter out rows with 0 values
df_long_ci <- df_long_ci %>% filter(mean != 0)

# Create individual plots
plot_dp <- ggplot(df_long_ci %>% filter(trait == "DP"), aes(x = Model, y = mean, color = VC)) +
  geom_point(position = position_dodge2(width = 1, preserve = "single"), size = 9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, position = position_dodge2(width = 1, preserve = "single")) +  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 18)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 35),  # Adjust the size of the text in the legend
    legend.key.size = unit(2, "cm"),
    axis.text.x = element_text(size = 35, color = "black"),
    axis.text.y = element_text(size = 35, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )

plot_sp <- ggplot(df_long_ci %>% filter(trait == "SP"), aes(x = Model, y = mean, color = VC)) +
  geom_point(position = position_dodge2(width = 1, preserve = "single"), size = 9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, position = position_dodge2(width = 1, preserve = "single")) +  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 18)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 30),  # Adjust the size of the text in the legend
    legend.key.size = unit(2, "cm"),
    axis.text.x = element_text(size = 35, color = "black"),
    axis.text.y = element_text(size = 35, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )

plot_pp <- ggplot(df_long_ci %>% filter(trait == "PP"), aes(x = Model, y = mean, color = VC)) +
  geom_point(position = position_dodge2(width = 1, preserve = "single"), size = 9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, position = position_dodge2(width = 1, preserve = "single")) +  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 18)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 30),  # Adjust the size of the text in the legend
    legend.key.size = unit(2, "cm"),  # Adjust the size of the legend keys
    axis.text.x = element_text(size = 35, color = "black"),
    axis.text.y = element_text(size = 35, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm")
  )


# Begin creating the PNG output
#png(filename = "Fig2_var_exp_BP.png", height = 1800, width = 1400, bg = "white")

# Create layout for plots with panel letters
#grid.newpage()
#pushViewport(viewport(layout = grid.layout(6, 2, heights = unit(c(0, 1, 0, 1, 0, 1), "null"))))

# Add panel letters
#grid.text("a", x = unit(0.02, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
#print(plot_dp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))

#grid.text("b", x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
#print(plot_sp, vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))

#grid.text("c", x = unit(0.02, "npc"), y = unit(0.31, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
#print(plot_pp, vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))

#dev.off()  # Close the PNG device

#noah suggested to fix legend but worked after adding common.legend
final <- ggarrange(plot_dp, plot_sp, plot_pp, ncol=1, common.legend = 1, legend = "bottom")
ggsave("Fig2_var_exp_BP_noah_sugg.pdf", final, height = 32, width = 24, dpi = 300, device = "pdf")

# # Begin creating the TIFF output
# tiff(filename = "Fig2_var_exp_BP.tiff", height = 1800, width = 1400, bg = "white")
# 
# # Create layout for plots with panel letters
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(6, 2, heights = unit(c(0, 1, 0, 1, 0, 1), "null"))))
# 
# # Add panel letters
# grid.text("a", x = unit(0.02, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
# print(plot_dp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
# 
# grid.text("b", x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
# print(plot_sp, vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
# 
# grid.text("c", x = unit(0.02, "npc"), y = unit(0.31, "npc"), gp = gpar(fontsize = 30, fontface = "bold"), just = "left")
# print(plot_pp, vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
# 
# dev.off()  # Close the TIFF device

# # Create layout for plots with panel letters
# pdf(file = "Fig2_var_exp_BP_noah_eb.pdf", height = 15.24, width = 11.85, bg = "white")
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(6, 2, heights = unit(c(0, 0.8, 0, 0.8, 0, 1), "null"))))
# 
# # Add panel letters
# grid.text("a", x = unit(0.02, "npc"), y = unit(0.95, "npc"), gp = gpar(fontsize = 15, fontface = "bold"), just = "left")
# print(plot_dp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))
# 
# grid.text("b", x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 15, fontface = "bold"), just = "left")
# print(plot_sp, vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
# 
# grid.text("c", x = unit(0.02, "npc"), y = unit(0.33, "npc"), gp = gpar(fontsize = 15, fontface = "bold"), just = "left")
# print(plot_pp, vp = viewport(layout.pos.row = 6, layout.pos.col = 1:2))
# 
# dev.off()  # Close the PDF device
