#Factorial design 
library(DeclareDesign)
# install.packages("dplyr")
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)

setwd("/home/santi/Dropbox/Research/Methods/EGAP/project")
source("DeclareDesign/templates file.R")

factorial_design <- factorial_template(
  N = 120000,                            # Arbitrary Population size
  n = 300,                             # Sample size, defaults to 50 per treatment cond. 
  cond_means = c(4, 3.6, 4.4, 4.6),       # Expectation of PO in each treatment cond.
  noise_scale = 1,                     # Noise constant across groups
  block_var_probs = rep(1/6, 6),       # 6 blocks of equal size
  blocked_RA = TRUE)                    # Standard deviation of error term

diagnosis <- diagnose_design(factorial_design, population_draws = 50)

df <- data.frame(Est = colnames(diagnosis$diagnosis), 
                 Power = diagnosis$diagnosis["Mean, Power",],
                 SD = diagnosis$diagnosis["S.D., Power",]) %>%
      mutate(upper_ci = Power + 1.96 * SD,
             upper_ci = ifelse(upper_ci > 1, 1, upper_ci),
             lower_ci = Power - 1.96 * SD,
             lower_ci = ifelse(lower_ci < 0, 0, lower_ci),
             Estimand = factor(Est, levels(Est)[c(3, 2, 1, 7, 6, 5, 4)]))

pdf(file = "power300_bra.pdf")
ggplot(df, aes(x = Power, y = Estimand, col = Estimand)) + geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, col = Estimand), height = 0, size = 1.4) +
  theme_bw() + theme(legend.position = "none", text = element_text(size=20)) + xlim (c(0, 1))
dev.off()


factorial_design <- factorial_template(
  N = 120000,                            # Arbitrary Population size
  n = 400,                             # Sample size, defaults to 50 per treatment cond. 
  cond_means = c(4, 3.6, 4.4, 4.6),       # Expectation of PO in each treatment cond.
  noise_scale = 1,
   block_var_probs = rep(1/6, 6),       # 6 blocks of equal size
  blocked_RA = TRUE)                    # Standard deviation of error term

diagnosis <- diagnose_design(factorial_design, population_draws = 50)

df <- data.frame(Est = colnames(diagnosis$diagnosis), 
                 Power = diagnosis$diagnosis["Mean, Power",],
                 SD = diagnosis$diagnosis["S.D., Power",]) %>%
      mutate(upper_ci = Power + 1.96 * SD,
             upper_ci = ifelse(upper_ci > 1, 1, upper_ci),
             lower_ci = Power - 1.96 * SD,
             lower_ci = ifelse(lower_ci < 0, 0, lower_ci),
             Estimand = factor(Est, levels(Est)[c(3, 2, 1, 7, 6, 5, 4)]))

pdf(file = "power400_bra.pdf")
ggplot(df, aes(x = Power, y = Estimand, col = Estimand)) + geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, col = Estimand), height = 0, size = 1.4) +
  theme_bw() + theme(legend.position = "none", text = element_text(size=20)) + xlim (c(0, 1))
dev.off()



factorial_design <- factorial_template(
  N = 120000,                            # Arbitrary Population size
  n = 500,                             # Sample size, defaults to 50 per treatment cond. 
  cond_means = c(4, 3.6, 4.4, 4.6),       # Expectation of PO in each treatment cond.
  noise_scale = 1,
    block_var_probs = rep(1/6, 6),       # 6 blocks of equal size
  blocked_RA = TRUE)                    # Standard deviation of error term

diagnosis <- diagnose_design(factorial_design, population_draws = 50)

df <- data.frame(Est = colnames(diagnosis$diagnosis), 
                 Power = diagnosis$diagnosis["Mean, Power",],
                 SD = diagnosis$diagnosis["S.D., Power",]) %>%
      mutate(upper_ci = Power + 1.96 * SD,
             upper_ci = ifelse(upper_ci > 1, 1, upper_ci),
             lower_ci = Power - 1.96 * SD,
             lower_ci = ifelse(lower_ci < 0, 0, lower_ci),
             Estimand = factor(Est, levels(Est)[c(3, 2, 1, 7, 6, 5, 4)]))

pdf(file = "power500_bra.pdf")
ggplot(df, aes(x = Power, y = Estimand, col = Estimand)) + geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, col = Estimand), height = 0, size = 1.4) +
  theme_bw() + theme(legend.position = "none", text = element_text(size=20)) + xlim (c(0, 1))
dev.off()


factorial_design <- factorial_template(
  N = 120000,                            # Arbitrary Population size
  n = 600,                             # Sample size, defaults to 50 per treatment cond. 
  cond_means = c(4, 3.6, 4.4, 4.6),       # Expectation of PO in each treatment cond.
  noise_scale = 1,
    block_var_probs = rep(1/6, 6),       # 6 blocks of equal size
  blocked_RA = TRUE)                    # Standard deviation of error term

diagnosis <- diagnose_design(factorial_design, population_draws = 50)

df <- data.frame(Est = colnames(diagnosis$diagnosis), 
                 Power = diagnosis$diagnosis["Mean, Power",],
                 SD = diagnosis$diagnosis["S.D., Power",]) %>%
      mutate(upper_ci = Power + 1.96 * SD,
             upper_ci = ifelse(upper_ci > 1, 1, upper_ci),
             lower_ci = Power - 1.96 * SD,
             lower_ci = ifelse(lower_ci < 0, 0, lower_ci),
             Estimand = factor(Est, levels(Est)[c(3, 2, 1, 7, 6, 5, 4)]))

pdf(file = "power600_bra.pdf")
ggplot(df, aes(x = Power, y = Estimand, col = Estimand)) + geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci, col = Estimand), height = 0, size = 1.4) +
  theme_bw() + theme(legend.position = "none", text = element_text(size=20)) + xlim (c(0, 1))
dev.off()







