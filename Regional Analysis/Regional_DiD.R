# This R file contains code to replicate the regional DiD estimations of all model specifications as well as the comparative CI plots.

library(dplyr)
library(tidyverse)
library(arrow)
library(sf)
library(ggplot2)
library(readr)
library(stringi)
library(fixest)
library(car)
library(scales)

#=============================== Load the files  ===============================
base_path <- getwd()
file_path <- file.path(base_path, "francetransfert-1325119685PiA_panel_clean_coef.parquet")
df <- read_parquet(file_path)

colnames(df) %>%
  sort()

#========================== Select relevant variables ==========================
categorical_variables <- c(
  "OVER_67", 
  'IS_MICRO' ,
  'BENEF_PAC', 
  'SIEGE_LIB_REG', 
  'OTEFDA_COEF17'
)

numerical_variables <- c("SAU_TOT",
                         "AGE",
                         "PBSTOT_COEF17", 
                         "UGBTA.TOT",
                         'farm_id', 
                         'year') 



df <- df %>%
  select(all_of(c(numerical_variables,categorical_variables )))

df <- df %>%
  relocate(farm_id, year)

df <- df %>%
  drop_na()

colSums(is.na(df))

df <- df %>%
  mutate(AGE = if_else(year == 2023, AGE + 3, AGE))

# Create baseline covariates
baseline_data <- df[df$year == 2020, c("SAU_TOT", "UGBTA.TOT", 'IS_MICRO','OTEFDA_COEF17', 'farm_id')]

# Merge the baseline covariates back to your original dataset
df <- merge(df, baseline_data, by = "farm_id", suffixes = c("", "_t0"))

#===================== Define treatment and control group ======================
#Indicator for post-treatment year
df$POST <- ifelse(df$year==2023, 1, 0)

#Treatment indicator whether a farm lost CAP eligibility in 2023
df <- df %>%
  group_by(farm_id) %>%
  mutate(treated_group = as.integer(any(year == 2023 & BENEF_PAC == 0))) %>%
  ungroup() %>%
  mutate(Lost_eligibility = treated_group * POST)

#Outcome variable
df <- df %>% 
  rename(Std_gross_output = PBSTOT_COEF17 )

categorical_variables_did <- c(
  "OVER_67", 
  'OTEFDA_COEF17'
)
df <- df %>%
  mutate(across(all_of(categorical_variables_did), ~ as.factor(.)))

# Mapping of factors to integer codes
level_maps <- lapply(df[categorical_variables_did], function(x) {
  f <- factor(x)
  data.frame(code = seq_along(levels(f)), level = levels(f), row.names = NULL)
})

# View the mappings 
mapping_df <- dplyr::bind_rows(
  lapply(names(level_maps), function(v) {
    cbind(variable = v, level_maps[[v]])
  })
)

#================== Fixed-effect equation for DiD regression ===================
regions <- df%>%
  pull(SIEGE_LIB_REG)%>%
  unique()

models_by_region <- vector("list", length(regions))
names(models_by_region) <- regions

for (region in regions) {
  df_region <- df%>%
    filter(SIEGE_LIB_REG == region)
  
  model_FE <- feols( Std_gross_output ~ Lost_eligibility + POST + treated_group 
                     +  UGBTA.TOT_t0 + OVER_67 + SAU_TOT_t0 + OTEFDA_COEF17_t0,
                     data = df_region, cluster = ~farm_id )
  
  models_by_region[[region]] <- summary(model_FE)
}

models_by_region[['CENTRE VAL DE LOIRE']]

#========== Plot of regional DiD estimates with 95% CI with controls ===========
did_region <- do.call(rbind, lapply(names(models_by_region), function(region) {
  m <- models_by_region[[region]]
  ct <- coeftable(m)
  
  est <- ct["Lost_eligibility", "Estimate"]
  se  <- ct["Lost_eligibility", "Std. Error"]
  
  data.frame(
    region = region,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se,
    n = nobs(m)
  )
}))

did_region_plot <- did_region %>%
  mutate( significant = ifelse(ci_low > 0 | ci_high < 0, "Significant (5%)", "Not significant"),
          region = reorder(region, estimate) )

ggplot(did_region_plot, aes(x = region, y = estimate)) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 0.8,
             color = "grey40") +
  
  geom_errorbar(aes(ymin = ci_low,
                    ymax = ci_high,
                    color = significant),
                width = 0.15,
                linewidth = 1) +
  
  geom_point(aes(color = significant), size = 3) +
  
  coord_flip() +
  
  scale_y_continuous(
    breaks = seq(
      floor(min(did_region_plot$estimate) / 100000) * 100000,
      ceiling(max(did_region_plot$estimate) / 100000) * 100000,
      by = 100000
    ),
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant" = "#D95F02"
  )) +
  
  labs(
    x = 'Regions',
    y = "DiD estimate (Lost eligibility effect on standard gross output)",
    title = "Regional DiD Estimates With Covariates",
    color = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#========== Fixed-effect equation for DiD regression with no controls ==========
models_by_region_no_controls <- vector("list", length(regions))

names(models_by_region_no_controls) <- regions
for (region in regions) {
  df_region <- df%>%
    filter(SIEGE_LIB_REG == region)
  
  model_no_controls <- feols(
    Std_gross_output ~ Lost_eligibility + POST + treated_group,
    data = df_region,
    cluster = ~farm_id
  )
  
  models_by_region_no_controls[[region]] <- summary(model_no_controls)
}

models_by_region_no_controls[['BRETAGNE']]

#========= Plot of regional DiD estimates with 95% CI with no controls =========
did_no_controls_region <- do.call(rbind, lapply(names(models_by_region_no_controls), function(region) {
  m <- models_by_region_no_controls[[region]]
  ct <- coeftable(m)
  
  est <- ct["Lost_eligibility", "Estimate"]
  se  <- ct["Lost_eligibility", "Std. Error"]
  
  data.frame(
    region = region,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se,
    n = nobs(m)
  )
}))

did_no_controls_region_plot <- did_no_controls_region %>%
  mutate( significant = ifelse(ci_low > 0 | ci_high < 0, "Significant (5%)", "Not significant"),
          region = reorder(region, estimate) )

ggplot(did_no_controls_region_plot, aes(x = region, y = estimate)) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 0.8,
             color = "grey40") +
  
  geom_errorbar(aes(ymin = ci_low,
                    ymax = ci_high,
                    color = significant),
                width = 0.15,
                linewidth = 1) +
  
  geom_point(aes(color = significant), size = 3) +
  
  coord_flip() +
  
  scale_y_continuous(
    breaks = seq(
      floor(min(did_no_controls_region_plot$estimate) / 100000) * 100000,
      ceiling(max(did_no_controls_region_plot$estimate) / 100000) * 100000,
      by = 100000
    ),
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant" = "#D95F02"
  )) +
  
  labs(
    x = 'Regions',
    y = "DiD estimate (Lost eligibility effect on standard gross output)",
    title = "Regional DiD Estimates Without Covariates",
    color = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
