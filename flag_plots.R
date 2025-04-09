#!/usr/bin/env Rscript

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_wider

#==================================================
# Preparation: Combine data from all languages
#==================================================
files <- c("java-results.csv", "c++-results.csv", "c#-results.csv",
           "golang-results.csv", "python-results.csv")
languages <- c("Java", "C++", "C#", "Golang", "Python")

all_results <- do.call(rbind, lapply(seq_along(files), function(i) {
  df <- read_delim(files[i], ",", escape_double = FALSE, trim_ws = TRUE)
  df$Language <- languages[i]
  return(df)
}))

# Keep only the two approaches and recode them
approaches <- c("ORIG", "FLAG")
all_results <- subset(all_results, Approach %in% approaches)
all_results$Approach <- factor(all_results$Approach,
                               levels = c("ORIG", "FLAG"),
                               labels = c("Original", "FLAG"))
all_results$Language <- factor(all_results$Language, levels = languages)

# Create a new factor variable for the x-axis ordered as: Language Before, Language After
all_results$LangApproach <- with(all_results, paste(Language, ifelse(Approach == "Original", "Before", "After")))
desired_levels <- c("Java Before", "Java After",
                    "C++ Before", "C++ After",
                    "C# Before", "C# After",
                    "Golang Before", "Golang After",
                    "Python Before", "Python After")
all_results$LangApproach <- factor(all_results$LangApproach, levels = desired_levels)

#==================================================
# Customization Parameters
#==================================================
# Colors for boxplots:
colors <- c("Original" = "#f8c6c6", "FLAG" = "#c8f8c6")

# Theme for plots with adjustments:
custom_theme <- theme_light() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(margin = margin(r = 10)), # extra space between y-axis label and axis
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position = "none")

#==================================================
# Plot for Cognitive Complexity (COG)
#==================================================
p_cog <- ggplot(data = subset(all_results, !is.na(COG)),
                aes(x = LangApproach, y = COG, fill = Approach)) +
  geom_boxplot(outlier.size = 3, size = 0.5, lwd = 0.5) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "", y = "Cognitive Complexity") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  custom_theme

pdf("COG_BoxPlot_combined.pdf", width = 12, height = 4)
print(p_cog)
dev.off()

#==================================================
# Plot for Cyclomatic Complexity (CYC)
#==================================================
p_cyc <- ggplot(data = subset(all_results, !is.na(CYC)),
                aes(x = LangApproach, y = CYC, fill = Approach)) +
  geom_boxplot(outlier.size = 3, size = 0.5, lwd = 0.5) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = "", y = "Cyclomatic Complexity") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  custom_theme

pdf("CYC_BoxPlot_combined.pdf", width = 12, height = 4)
print(p_cyc)
dev.off()

#==================================================
# Calculate and Print Mean, Median, IQR, and Reduction for COG and CYC
#==================================================
calculate_reduction <- function(data, metric) {
  data %>%
    filter(!is.na({{ metric }})) %>%
    group_by(Language, Approach) %>%
    summarise(
      mean_val = mean({{ metric }}, na.rm = TRUE),
      median_val = median({{ metric }}, na.rm = TRUE),
      iqr_val = IQR({{ metric }}, na.rm = TRUE),
      sd_val = sd({{ metric }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Approach, values_from = c(mean_val, median_val, iqr_val, sd_val)) %>%
    mutate(
      Absolute_Mean = mean_val_Original - mean_val_FLAG,
      Relative_Mean = 100 * (mean_val_FLAG / mean_val_Original),
      Absolute_Median = median_val_Original - median_val_FLAG,
      Relative_Median = 100 * (median_val_FLAG / median_val_Original),
      Absolute_IQR = iqr_val_Original - iqr_val_FLAG,
      Relative_IQR = 100 * (iqr_val_FLAG / iqr_val_Original),
      Absolute_SD = sd_val_Original - sd_val_FLAG
    ) %>%
    select(
      Language,
      Mean_Original = mean_val_Original, Mean_FLAG = mean_val_FLAG,
      Absolute_Mean, Relative_Mean,
      Median_Original = median_val_Original, Median_FLAG = median_val_FLAG,
      Absolute_Median, Relative_Median,
      IQR_Original = iqr_val_Original, IQR_FLAG = iqr_val_FLAG,
      Absolute_IQR, Relative_IQR,
      SD_Original = sd_val_Original, SD_FLAG = sd_val_FLAG,
      Absolute_SD
    )
}

# For Cognitive Complexity (COG)
reduction_cog <- calculate_reduction(all_results, COG)
cat("\nMean, Median, IQR, and Reduction for Cognitive Complexity (COG):\n")
print(reduction_cog)

# For Cyclomatic Complexity (CYC)
reduction_cyc <- calculate_reduction(all_results, CYC)
cat("\nMean, Median, IQR, and Reduction for Cyclomatic Complexity (CYC):\n")
print(reduction_cyc)

#==================================================
# Compute Standard Deviation of the Absolute Differences (Revised)
#==================================================

# For each pair (Language and pair_id), explicitly pick the Original and FLAG values.
paired_results <- all_results %>%
  group_by(Language, pair_id) %>%
  summarise(
    COG_Original = first(COG[Approach == "Original"]),
    COG_FLAG     = first(COG[Approach == "FLAG"]),
    CYC_Original = first(CYC[Approach == "Original"]),
    CYC_FLAG     = first(CYC[Approach == "FLAG"]),
    .groups = "drop"
  ) %>%
  mutate(
    diff_COG = abs(COG_Original - COG_FLAG),
    diff_CYC = abs(CYC_Original - CYC_FLAG)
  )

# Now calculate the standard deviation of these differences for each language.
std_differences <- paired_results %>%
  group_by(Language) %>%
  summarise(
    sd_diff_COG = sd(diff_COG, na.rm = TRUE),
    sd_diff_CYC = sd(diff_CYC, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nStandard Deviation of Absolute Differences for Each Language:\n")
print(std_differences)