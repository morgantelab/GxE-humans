# Load necessary libraries
library(dplyr)

# Set working directory
setwd("rerun_feb2024")

# Read the .txt file into a data frame
grouped_pheno <- read.table("grouped_pheno_age.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Define a custom function to merge columns
merge_columns <- function(df, cols) {
  apply(df[cols], 1, function(x) {
    # If all values are NA, return NA
    if (all(is.na(x))) {
      return(NA)
    }
    # Otherwise, return the first non-NA value found
    return(x[!is.na(x)][1])
  })
}

# Merge columns for each group and create new columns
grouped_pheno$DP0a_young <- merge_columns(grouped_pheno, c("DP0a01", "DP0a02", "DP0a11", "DP0a12"))
grouped_pheno$DP0a_old <- merge_columns(grouped_pheno, c("DP0a03", "DP0a04", "DP0a13", "DP0a14"))

grouped_pheno$PP0a_young <- merge_columns(grouped_pheno, c("PP0a01", "PP0a02", "PP0a11", "PP0a12"))
grouped_pheno$PP0a_old <- merge_columns(grouped_pheno, c("PP0a03", "PP0a04", "PP0a13", "PP0a14"))

grouped_pheno$SP0a_young <- merge_columns(grouped_pheno, c("SP0a01", "SP0a02", "SP0a11", "SP0a12"))
grouped_pheno$SP0a_old <- merge_columns(grouped_pheno, c("SP0a03", "SP0a04", "SP0a13", "SP0a14"))

# Remove the original columns
grouped_pheno <- grouped_pheno %>%
  select(FID, IID, ends_with("_young"), ends_with("_old"))

# Write the updated data frame back to a new text file
write.table(grouped_pheno, "updated_grouped_pheno_age.txt", sep = "\t", na = "NA", quote = FALSE, row.names = FALSE)

