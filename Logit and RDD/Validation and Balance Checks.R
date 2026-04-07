##################################################
#######  Validation and Balance Checks    ########
##################################################

#========================== 1. McCrary Density Test ===========================

mccrary <- rddensity(X = data$running_age, c = 0)
summary(mccrary)
# Density plot
rdplotdensity(mccrary, data$running_age,
              title = "Density of running_age = age − 67 around c = 0",
              xlabel = "Age − 67",
              ylabel = "Density")

#============================= 2. Balance Tests ================================

balance_vars <- c("SAU_TOT", "PBSTOT_COEF17", "UGBTA.TOT", "BIO_FIL",
                  "EXTERNALISFIL", "CERTIFICATION_BINARY", "RDEV", "REGL")

balance_results <- do.call(rbind, lapply(balance_vars, function(var) {
  tryCatch({
    rd_bal <- rdrobust(
      y      = data[[var]],
      x      = data$running_age,
      c      = 0,
      masspoints = "adjust"
    )
    data.frame(
      variable = var,
      estimate = rd_bal$Estimate[1],
      se       = rd_bal$se[3],
      tstat    = rd_bal$Estimate[1] / rd_bal$se[3],
      ci_low   = rd_bal$ci[3, 1],
      ci_high  = rd_bal$ci[3, 2],
      pval     = rd_bal$pv[3]
    )
  }, error = function(e) {
    message(paste("Skipping", var, ":", e$message))
    NULL
  })
}))

print(balance_results)

# Clean variable labels for the plot
var_labels <- c(
  SAU_TOT              = "Farm size (SAU)",
  PBSTOT_COEF17        = "Gross output (PBS)",
  UGBTA.TOT            = "Livestock units (UGB)",
  BIO_FIL              = "Organic farming",
  EXTERNALISFIL        = "Externalisation",
  CERTIFICATION_BINARY = "Certification",
  RDEV                 = "Rural development (RDEV)",
  REGL                 = "Disadvantages location (REGL)"
)

balance_results$label <- var_labels[balance_results$variable]
balance_results$label <- ifelse(is.na(balance_results$label),
                                balance_results$variable,
                                balance_results$label)

balance_results$significant <- ifelse(
  balance_results$pval < 0.05, "Significant (5%)", "Not significant"
)

ggplot(balance_results, aes(x = reorder(label, abs(tstat)), y = tstat)) +
  geom_hline(yintercept = c(-1.96, 1.96),
             linetype = "dashed", color = "steelblue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_col(aes(fill = significant), width = 0.6, alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Significant (5%)" = "#D95F02",   # Orange = concern
    "Not significant"  = "#1B9E77"    # Green  = OK
  )) +
  annotate("text", x = 0.6, y = 2.15, label = "±1.96", 
           color = "steelblue", size = 3.2) +
  labs(
    x     = NULL,
    y     = "|t|-statistic (robust)",
    title = "Covariate Balance at the Age-67 Cutoff",
    subtitle = "Farms around the cutoff differ significantly on most pre-treatment characteristics",
    fill  = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line        = element_line(color = "black", linewidth = 0.8),
    axis.ticks       = element_line(color = "black"),
    axis.text        = element_text(color = "black"),
    legend.position  = "top",
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 11, color = "grey30"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

#======================= 3. Bandwidth Sensitivity =============================

bw_seq <- seq(3, 20, by = 1)   
bw_sensitivity <- do.call(rbind, lapply(bw_seq, function(h_try) {
  tryCatch({
    rd_try <- rdrobust(
      y          = data$Y,
      x          = data$running_age,
      c          = 0,
      h          = h_try,
      fuzzy      = data$D,
      masspoints = "adjust"
    )
    data.frame(
      h        = h_try,
      estimate = rd_try$Estimate[2],
      se       = rd_try$se[3],
      tstat    = abs(rd_try$Estimate[2] / rd_try$se[3]),
      ci_low   = rd_try$ci[3, 1],
      ci_high  = rd_try$ci[3, 2],
      pval     = rd_try$pv[3],
      n_left   = rd_try$N_h[1],
      n_right  = rd_try$N_h[2]
    )
  }, error = function(e) {
    message(paste("Failed at h =", h_try, ":", e$message))
    NULL
  })
}))
#MSE-optimal bandwidth
h_opt <- 6.62   

# t-stat by bandwidth 
ggplot(bw_sensitivity, aes(x = h, y = tstat)) +
  geom_hline(yintercept = 1.96, linetype = "dashed",
             color = "steelblue", linewidth = 0.7) +
  geom_vline(xintercept = h_opt, linetype = "dotted",
             color = "grey40", linewidth = 0.8) +
  geom_line(color = "#D95F02", linewidth = 1.1) +
  geom_point(color = "#D95F02", size = 2) +
  annotate("text", x = h_opt + 0.4, y = max(bw_sensitivity$tstat, na.rm = TRUE) * 0.97,
           label = paste0("h* = ", h_opt), color = "grey30", size = 3.5, hjust = 0) +
  annotate("text", x = 3.2, y = 2.1,
           label = "1.96 (5% threshold)", color = "steelblue", size = 3.2, hjust = 0) +
  labs(
    x        = "Bandwidth h (years)",
    y        = "|t|-statistic",
    title    = "Bandwidth Sensitivity of the RD Estimate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line      = element_line(color = "black", linewidth = 0.8),
    axis.ticks     = element_line(color = "black"),
    axis.text      = element_text(color = "black"),
    plot.title     = element_text(face = "bold", size = 15),
    plot.subtitle  = element_text(size = 11, color = "grey30"),
    panel.grid.minor = element_blank()
  )
