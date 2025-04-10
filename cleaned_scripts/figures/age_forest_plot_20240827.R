
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)

# Set the working directory
setwd("/data2/morgante_lab/ukbiobank_projects/GxE")

# Function to read data from a log file
read_log_data <- function(file_path) {
  data <- readLines(file_path)
  return(data)
}

# Function to extract rG and its SE from the log data
extract_rG <- function(data, file_path) {
  # Extract the line containing rG and its SE
  rG_line <- grep("rG", data, value = TRUE)
  # Extract rG and SE values from the line
  rG_value <- as.numeric(str_extract(rG_line, "\\b\\d+\\.\\d+\\b"))
  SE_value <- as.numeric(str_extract(str_extract(rG_line, "\\b\\d+\\.\\d+\\s+\\d+\\.\\d+\\b"), "\\d+\\.\\d+$"))
  
  # Determine trait from file name for y-axis label
  trait <- ifelse(grepl("dp", file_path), "DP", 
                  ifelse(grepl("pp", file_path), "PP", "SP"))
  
  return(data.frame(
    Trait = trait,
    rG = rG_value,
    SE = SE_value,
    File = trait  # Use the simplified trait names
  ))
}

# Function to create a forest plot for rG values
create_forest_plot <- function(rG_data, title) {
  # Calculate lower and upper bounds for error bars
  rG_data <- rG_data %>%
    mutate(lower = rG - 2 * SE, upper = rG + 2 * SE)
  
  # Print the exact CI values for each trait
  print("Confidence Intervals (using 2 * SE) for each trait:")
  print(rG_data %>% select(Trait, rG, lower, upper))
  
  # Ensure the correct order for Trait factor levels: DP, SP, PP
  rG_data$Trait <- factor(rG_data$Trait, levels = c("PP", "SP", "DP"))
  
  # Create the plot
  p <- ggplot(rG_data, aes(y = Trait, x = rG, xmin = lower, xmax = upper)) +
    geom_point(shape = 18, size = 3, color = "black") +
    geom_errorbarh(height = 0.3, color = "black", size = 1.0) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.text.y = element_text(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.line = element_line(color = "black"),  # Add axis lines
      panel.border = element_blank(),  # Remove the border around the plot
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.3, "cm"),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1), "lines")  # Adjust margins; top, right, bottom, left
    ) +
    labs(title = title, y = "", x = "Genetic Correlation (rG)") +
    labs(x = expression("Genetic correlation (r"[G]~")")) +
    coord_cartesian(xlim = c(0.7, 1.3), clip = "off") +  # Extended xlim and turn clipping off
    geom_vline(xintercept = 1, linetype = "dotted", color = "black")
  
  return(p)
}

# Explicitly define the file paths for the required files
file_paths_age <- c("/data2/morgante_lab/ukbiobank_projects/GxE/run/run_gen_cor_output/gc_age_dp_20240823.log", "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_gen_cor_output/gc_age_sp_20240823.log", "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_gen_cor_output/gc_age_pp_20240823.log")

# Read data from each file and extract rG values for age
rG_data_list_age <- lapply(file_paths_age, function(file_path) {
  data <- read_log_data(file_path)
  extract_rG(data, file_path)
})

# Combine all data into a single data frame for age
combined_rG_data_age <- do.call(rbind, rG_data_list_age)

# Generate plot for age
plot_age <- create_forest_plot(combined_rG_data_age, "")

# Display the plot
#print(plot_age)

# Save the plot
#ggsave("figures/gen_cor/Fig3_age_gen_cor_BP_order.png", plot_age, width = 8, height = 6, dpi = 300)

# Save the plot as PDF instead of PNG
ggsave("figures/gen_cor/Fig3_age_gen_cor_BP_order.pdf", 
       plot_age, 
       width = 8, 
       height = 6, 
       device = "pdf")


