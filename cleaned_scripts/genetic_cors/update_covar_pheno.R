# Load libraries
library(data.table)

# Load the main data from the RData file
load("data4_20240209.RData")

# Read the covariate/phenotype file
covar_pheno <- fread("covar_pheno.txt")

# Assuming 'ID' in main_data corresponds to 'FID' in covar_pheno
# And that the columns to be updated are 'PP0a', 'DP0a', and 'SP0a'

# Loop through the rows in covar_pheno to update values
for (i in 1:nrow(covar_pheno)) {
  # Find the matching row in main_data
  match_row <- which(dt$ID == covar_pheno$FID[i])
  
  # Check if there is a matching row
  if (length(match_row) == 1) {
    # Update the values for 'PP0a', 'DP0a', and 'SP0a'
    covar_pheno$PP0a[i] <- dt$PP0a[match_row]
    covar_pheno$DP0a[i] <- dt$DP0a[match_row]
    covar_pheno$SP0a[i] <- dt$SP0a[match_row]
  }
}

# Write the updated data frame back to a new text file
# Write the updated data frame back to a new text file using write.table
write.table(covar_pheno, "updated_covar_pheno.txt", row.names = FALSE, sep = "\t", na = "NA", quote = FALSE)


