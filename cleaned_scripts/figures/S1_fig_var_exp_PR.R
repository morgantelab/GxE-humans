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

setwd("/data2/morgante_lab/ukbiobank_projects/GxE/figures/")

# original dataframe
df <- data.frame(
  trait = c("PR", "PR", "PR", "PR", "PR", "PR", "PR", "PR", "PR", "PR"),
  Model = c("M0", "M1", "M2", "M3", "M11", "M12", "M13", "M21", "M22", "M23"),
  D = c("0.91 (0.79, 1.02)", "1.12 (0.79, 1.44)", "1.64 (1.3, 2.01)", "0.91 (0.8, 1.03)", "1.36 (0.98, 1.69)", "1.88 (1.44, 2.23)", "0.9 (0.78, 1)", "1.34 (0.99, 1.75)", "1.9 (1.52, 2.31)", "0.92 (0.8, 1.03)"),
  G = c("10.86 (10.15, 11.46)", NA, NA, NA, "10.31 (9.64, 10.95)", "10.4 (9.79, 11.08)", "10.46 (9.8, 11.09)", "10.36 (9.73, 10.98)", "10.41 (9.75, 11)", "10.46 (9.8, 11.08)"),
  L = c(NA, "6.66 (6.32, 6.97)", "6.69 (6.33, 6.99)", "6.41 (6.1, 6.71)", "6.72 (6.4, 7.03)", "6.8 (6.48, 7.16)", "6.31 (6, 6.59)", "6.73 (6.41, 7.08)", "6.83 (6.47, 7.16)", "6.31 (6.03, 6.65)"),
  GxL = c(NA, NA, NA, NA, NA, NA, NA, "4.32 (3.72, 5.08)", "3.01 (2.54, 3.57)", "3.84 (3.08, 4.5)")
)

# Convert the dataframe to long format
df_long <- df %>%
  pivot_longer(cols = c(D, G, L, GxL), 
               names_to = "VC", 
               values_to = "val")

extract_ci <- function(x) {
  if (is.na(x) || x == "") {
    return(c(NA, NA, NA))  # Return NA if value is missing or empty
  }
  
  # Extract mean value (before the parentheses)
  mean_val <- as.numeric(str_extract(x, "^[0-9]+\\.?[0-9]*"))
  
  # Extract CI values inside the parentheses
  ci_vals <- str_match(x, "\\(([0-9]+\\.?[0-9]*),\\s*([0-9]+\\.?[0-9]*)\\)")
  
  # Check if CI extraction worked
  if (is.na(ci_vals[1])) {
    return(c(mean_val, NA, NA))  # Return mean with NA CIs if extraction failed
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

plot_PR <- ggplot(df_long_ci %>% filter(trait == "PR"), aes(x = Model, y = mean, color = VC)) +
  geom_point(position = position_dodge2(width = 1, preserve = "single"), size = 7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, position = position_dodge2(width = 1, preserve = "single")) +
  labs(title = "", x = "", y = "Variance") +
  scale_y_continuous(limits = c(0, 18)) +
  scale_x_discrete(labels = Lables) +
  scale_color_manual(values = color_blind_palette, labels = legend_labels) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 35),  # Adjust the size of the text in the legend
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


# Save plot as PDF
ggsave("FigS1_var_exp_PR.pdf", plot = plot_PR, width = 24, height = 16, device = "pdf")
