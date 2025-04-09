#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(ggplot2)
library(effsize) # for Cohen's d
library(rstatix) # for wilcox_effsize

#==================================================
# Preparation
#==================================================
# Load and prepare data with pair_id
raw_results <- read_csv("java-results.csv") %>%
  filter(Approach %in% c("ORIG", "FLAG")) %>%
  mutate(
    Approach = factor(Approach,
                      levels = c("ORIG", "FLAG"),
                      labels = c("Original", "FLAG")
    ),
    pair_id = as.factor(pair_id)
  ) %>%
  arrange(pair_id, Approach) # Critical for proper pairing

# Verify pairing structure
cat("Pairing verification:\n")
print(raw_results %>% 
        select(pair_id, method_function, Approach, COG, CYC) %>% 
        arrange(pair_id, Approach))

pairing_check <- raw_results %>%
  group_by(pair_id) %>%
  summarise(n = n(), approaches = list(Approach)) %>%
  filter(n != 2 | !all(c("Original", "FLAG") %in% unlist(approaches)))
if (nrow(pairing_check) > 0) {
  cat("Pairing issues detected:\n")
  print(pairing_check)
  stop("Each pair_id must have exactly one 'Original' and one 'FLAG' entry!")
}

#==================================================
# Improved Paired Analysis Function
#==================================================
analyze_paired <- function(data, metric) {
  # Test normality of differences
  diffs <- data %>%
    group_by(pair_id) %>%
    summarise(diff = diff(!!sym(metric))) %>%
    pull(diff)
  
  shapiro_test <- shapiro.test(diffs)
  norm_test <- shapiro_test$p.value > 0.05
  
  # Paired test
  if(norm_test) {
    test <- t.test(
      data %>% filter(Approach == "Original") %>% pull(metric),
      data %>% filter(Approach == "FLAG") %>% pull(metric),
      paired = TRUE
    )
    effsize <- cohen.d(
      data %>% filter(Approach == "Original") %>% pull(metric),
      data %>% filter(Approach == "FLAG") %>% pull(metric),
      paired = TRUE
    )
  } else {
    test <- wilcox.test(
      data %>% filter(Approach == "Original") %>% pull(metric),
      data %>% filter(Approach == "FLAG") %>% pull(metric),
      paired = TRUE, exact = FALSE, conf.int = TRUE, conf.level = 0.97
    )
    effsize <- wilcox_effsize(
      data, 
      reformulate("Approach", metric), 
      paired = TRUE, ci = TRUE, conf.level = 0.97
    )
  }
  
  list(
    test = test,
    effsize = effsize,
    shapiro = shapiro_test
  )
}

#==================================================
# COG Analysis
#==================================================
cat("\n===== COG Analysis =====\n")
cog_results <- analyze_paired(raw_results, "COG")

cat("\nNormality of Differences:\n")
print(cog_results$shapiro)

cat("\nTest Results:\n")
print(cog_results$test)

cat("\nEffect Size:\n") 
print(cog_results$effsize)

#==================================================
# CYC Analysis
#==================================================
cat("\n===== CYC Analysis =====\n")
cyc_results <- analyze_paired(raw_results, "CYC")

cat("\nNormality of Differences:\n")
print(cyc_results$shapiro)

cat("\nTest Results:\n")
print(cyc_results$test)

cat("\nEffect Size:\n")
print(cyc_results$effsize)