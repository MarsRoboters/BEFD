# ---------------------------------------------------
# Packages
# ---------------------------------------------------

library("readr")

# ---------------------------------------------------
# Data Exploration
# ---------------------------------------------------

sample_submission <- read.csv("sampleSubmission.csv") 
summary(sample_submission)

cat("\nMissing Values:\n")
print(colSums(is.na(sample_submission)))

