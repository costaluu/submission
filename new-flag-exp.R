#!/usr/bin/env Rscript

library(readr)
library(ggplot2)
library(nortest, pos=17) # ad.test
library(pgirmess) # kruskalmc
library(agricolae) # kruskal with tukey groups
library(RColorBrewer)
library(rcompanion) # multiVDA
library(effsize) # for Cohen's d and Cliff's Delta
library(rstatix) # (no longer used for effect size)

#==================================================
# Preparation
#==================================================
approaches <- c("ORIG", "FLAG")
v_factor_levels <- unique(approaches)

# LOAD RESULTS FILE
results_loc <- "c#-results.csv"
raw_results <- read_delim(results_loc, ",", escape_double = FALSE, trim_ws = TRUE)
raw_results <- subset(raw_results, Approach %in% approaches)

# Reordering and renaming
raw_results$Approach <- factor(raw_results$Approach,
                               levels = c("ORIG", "FLAG"),
                               labels = c("Original", "FLAG"))

raw_results <- raw_results %>% 
  arrange(method_function, Approach)  # Critical for alignment

print(raw_results %>% filter(method_function == "UseSqlDatabaseStatus"))
print(raw_results)

#==================================================
# Normality Test (COG), Paired Test, and Magnitude
#==================================================
cog_orig <- subset(raw_results, Approach == "Original")$COG
cog_flag <- subset(raw_results, Approach == "FLAG")$COG

print(cog_orig)

# Using Shapiro-Wilk for normality in this example
shapiro_orig_cog <- shapiro.test(cog_orig)
shapiro_flag_cog <- shapiro.test(cog_flag)

cat("Shapiro-Wilk Test for COG (Original):\n")
print(shapiro_orig_cog)
cat("\nShapiro-Wilk Test for COG (FLAG):\n")
print(shapiro_flag_cog)

alpha <- 0.05
norm_orig_cog <- shapiro_orig_cog$p.value > alpha
norm_flag_cog <- shapiro_flag_cog$p.value > alpha

if (norm_orig_cog && norm_flag_cog) {
  cat("\nCOG data appears to be normally distributed in both groups. Performing paired t-test.\n")
  paired_test_cog <- t.test(cog_orig, cog_flag, paired = TRUE)
  magnitude_cog <- cohen.d(cog_orig, cog_flag, paired = TRUE)
  cat("\nMagnitude (Cohen's d):\n")
  print(magnitude_cog)
} else {
  cat("\nCOG data appears to be NOT normally distributed in at least one group. Performing Wilcoxon signed-rank test.\n")
  paired_test_cog <- wilcox.test(cog_orig, cog_flag, paired = TRUE, exact = FALSE, conf.int = TRUE, conf.level = 0.95)
  magnitude_cog <- wilcox_effsize(raw_results, COG ~ Approach, paired = TRUE, ci = TRUE, conf.level = 0.95)
  
  # ∣r∣≈0.1: Small effect
  # ∣r∣≈0.3: Medium effect
  # ∣r∣≈0.5: Large effect
  
  cat("\nMagnitude (Rank-Biserial Correlation):\n")
  print(magnitude_cog)
}

print(paired_test_cog)
if (paired_test_cog$p.value < alpha) {
  cat("\nCOG paired test result is statistically significant.\n")
} else {
  cat("\nCOG paired test result is NOT statistically significant.\n")
}

#==================================================
# Normality Test (CYC), Paired Test, and Magnitude
#==================================================
cyc_orig <- subset(raw_results, Approach == "Original")$CYC
cyc_flag <- subset(raw_results, Approach == "FLAG")$CYC

shapiro_orig_cyc <- shapiro.test(cyc_orig)
shapiro_flag_cyc <- shapiro.test(cyc_flag)

cat("\nShapiro-Wilk Test for CYC (Original):\n")
print(shapiro_orig_cyc)
cat("\nShapiro-Wilk Test for CYC (FLAG):\n")
print(shapiro_flag_cyc)

norm_orig_cyc <- shapiro_orig_cyc$p.value > alpha
norm_flag_cyc <- shapiro_flag_cyc$p.value > alpha

if (norm_orig_cyc && norm_flag_cyc) {
  cat("\nCYC data appears to be normally distributed in both groups. Performing paired t-test.\n")
  paired_test_cyc <- t.test(cyc_orig, cyc_flag, paired = TRUE)
  magnitude_cyc <- cohen.d(cyc_orig, cyc_flag, paired = TRUE)
  cat("\nMagnitude (Cohen's d):\n")
  print(magnitude_cyc)
} else {
  cat("\nCYC data appears to be NOT normally distributed in at least one group. Performing Wilcoxon signed-rank test.\n")
  paired_test_cyc <- wilcox.test(cyc_orig, cyc_flag, paired = TRUE, exact = FALSE, conf.int = TRUE, conf.level = 0.95)
  magnitude_cyc <- wilcox_effsize(raw_results, CYC ~ Approach, paired = TRUE, ci = TRUE, conf.level = 0.95)
  cat("\nMagnitude (Rank-Biserial Correlation):\n")
  
  # ∣r∣≈0.1: Small effect
  # ∣r∣≈0.3: Medium effect
  # ∣r∣≈0.5: Large effect
  
  print(magnitude_cyc)
}

print(paired_test_cyc)
if (paired_test_cyc$p.value < alpha) {
  cat("\nCYC paired test result is statistically significant.\n")
} else {
  cat("\nCYC paired test result is NOT statistically significant.\n")
}
