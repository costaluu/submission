#!/usr/bin/env Rscript

library(readr)
library(ggplot2)
library(nortest, pos=17) # ad.test
library(pgirmess) # kruskalmc
library(agricolae) # kruskal with tukey groups
library(RColorBrewer)

library(rcompanion)  # multiVDA
library(effsize) # VD.A

#==================================================
# Preparation
#==================================================
approaches <- c(
  "ORIG",
  #"DEV",
  "FLAG")
v_factor_levels <- unique(approaches)

#LOAD RESULTS FILE
#results_loc <- "python-results.csv"
#results_loc <- "c++-results.csv"
#results_loc <- "c#-results.csv"
#results_loc <- "golang-results.csv"
results_loc <- "java-results.csv"
#results_loc <- "global-results.csv"
raw_results <- read_delim(results_loc, ",", escape_double = FALSE, trim_ws = TRUE)

raw_results <- subset(raw_results, Approach %in% approaches)

pdf_w <- 9
pdf_h <- 6
my.cols = brewer.pal(n = 3, name = "Dark2")

#reordering and renaming
raw_results$Approach <- factor(raw_results$Approach, 
                               levels=c("ORIG","FLAG"),
                               labels=c("Original","FLAG"))


#==================================================
# Boxplots (COG)
#==================================================
pdf("COG_BoxPlot.pdf", width=pdf_w, height=pdf_h)

BP <- ggplot(data=subset(raw_results, !is.na(COG)), aes(Approach, COG, fill=Approach))+ 
  geom_boxplot(outlier.size = 3, size=1) +
  scale_fill_manual(values = my.cols) +
  labs(y = "Cognitive Complexity")

#BP + labs(y = "Cognitive Complexity")

BP + theme_light() + theme(axis.text = element_text(size = 20),
                           axis.title.y = element_text(size = 24, margin = margin(r=10)),
                           legend.position="none",
                           axis.title.x = element_blank())

dev.off()

#==================================================
# Boxplots (CYC)
#==================================================
pdf("CYC_BoxPlot.pdf", width=pdf_w, height=pdf_h)

BP <- ggplot(data=subset(raw_results, !is.na(CYC)), aes(Approach, CYC, fill=Approach))+ 
  geom_boxplot(outlier.size = 3, size=1) +
  scale_fill_manual(values = my.cols) +
  labs(y = "Cyclomatic Complexity")

BP + theme_light() + theme(axis.text = element_text(size = 20),
                           axis.title.y = element_text(size = 24, margin = margin(r=10)),
                           legend.position="none",
                           axis.title.x = element_blank())

dev.off()

#==================================================
# Normality Test (COG)
#==================================================

# Separar os dados de COG por Approach para o teste de Shapiro-Wilk
cog_orig <- subset(raw_results, Approach == "Original")$COG
cog_flag <- subset(raw_results, Approach == "FLAG")$COG

# Realizar o teste de Shapiro-Wilk para cada Approach
shapiro_orig_cog <- shapiro.test(cog_orig)
shapiro_flag_cog <- shapiro.test(cog_flag)

# Imprimir os resultados
cat("Teste de Shapiro-Wilk para COG (Original):\n")
print(shapiro_orig_cog)
cat("\nTeste de Shapiro-Wilk para COG (FLAG):\n")
print(shapiro_flag_cog)

is_cog_orig_normal <- FALSE
is_cog_flag_normal <- FALSE

# Determine normality based on p-value
alpha <- 0.05 # Significance level

if (shapiro_orig_cog$p.value > alpha) {
  is_cog_orig_normal <- TRUE
  cat("\nCOG (Original) appears to be normally distributed.\n")
} else {
  cat("\nCOG (Original) appears to be NOT normally distributed.\n")
}

if (shapiro_flag_cog$p.value > alpha) {
  is_cog_flag_normal <- TRUE
  cat("COG (FLAG) appears to be normally distributed.\n")
} else {
  cat("COG (FLAG) appears to be NOT normally distributed.\n")
}


#==================================================
# Normality Test (COG)
#==================================================

#NORMALITY TEST (AD)

#The null hypothesis for the A-D test is that the data does follow a normal distribution. 
#If p-value < significance level, the data does not follow a normal distribution.
with(raw_results, ad.test(COG))

#==================================================
# COG
#==================================================
#Kruskal-Wallis rank sum test

with(raw_results, tapply(COG, Approach, median, na.rm=TRUE))
kruskal.test(COG ~ Approach, data=raw_results)

#Kruskal-Wallis rank sum test (MULTIPLE COMPARISON)
kruskalmc(raw_results$COG, raw_results$Approach)

# Pairwise comparisons using Wilcoxon’s test
#wilcox_test(raw_results, COG ~ Approach, p.adjust.method = "bonferroni")

out <- kruskal(raw_results$COG, raw_results$Approach)
out

#==================================================
# COG - Pairwise Vargha and Delaney's A and Cliff's delta
#==================================================
multiVDA(COG ~ Approach, data=raw_results)

#==================================================
# COG - VD.A: Vargha and Delaney A measure (small, >= 0.56; medium, >= 0.64; large, >= 0.71)
#==================================================
#orig <- subset(raw_results, Approach == "Original")
dev <- subset(raw_results, Approach == "Developer")
flag <- subset(raw_results, Approach == "FLAG")

cat("# DEV vs FLAG")
VD.A(dev$COG,flag$COG)






#==================================================
# Normality Test (CYC)
#==================================================
#NORMALITY TEST (AD)
#The null hypothesis for the A-D test is that the data does follow a normal distribution. 
#If p-value < significance level, the data does not follow a normal distribution.
with(raw_results, ad.test(CYC))

#==================================================
# CYC
#==================================================
#Kruskal-Wallis rank sum test
with(raw_results, tapply(CYC, Approach, median, na.rm=TRUE))
kruskal.test(CYC ~ Approach, data=raw_results)

#Kruskal-Wallis rank sum test (MULTIPLE COMPARISON)
kruskalmc(raw_results$CYC, raw_results$Approach)

# Pairwise comparisons using Wilcoxon’s test
#wilcox_test(raw_results, CYC ~ Approach, p.adjust.method = "bonferroni")

out <- kruskal(raw_results$CYC, raw_results$Approach)
out

#==================================================
# CYC - Pairwise Vargha and Delaney's A and Cliff's delta
#==================================================
multiVDA(CYC ~ Approach, data=raw_results)

#==================================================
# CYC - VD.A: Vargha and Delaney A measure (small, >= 0.56; medium, >= 0.64; large, >= 0.71)
#==================================================
orig <- subset(raw_results, Approach == "Original")
#dev <- subset(raw_results, Approach == "Developer")
flag <- subset(raw_results, Approach == "FLAG")

cat("# DEV vs FLAG")
VD.A(dev$CYC,flag$CYC)
