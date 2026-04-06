# Load libraries
library(dplyr)
library(tidyverse)
library(haven)
library(arrow)
library(ggplot2)
library(margins)
library(rdrobust)
library(sandwich)
library(lmtest)
library(fixest)

path_data <- "francetransfert-1325119685"

filetest <- file.path(path_data, "RA2020_ESEA2023_PAC.parquet")
test <- read_parquet(filetest)

#========================= Preparation of the analyses =========================

# Load data file
path_data <- "/Users/monabnis/Desktop/Polytechnique/Polytechnique M2/Policy-in-action/Data/francetransfert-1325119685"

file <- file.path(path_data, "PiA_panel_clean_coef.parquet")
panel_data <- read_parquet(file)

# Create treatment variable (1 if treated, meaning loss of subsidies)
panel_data <- panel_data %>%
  filter(!is.na(BENEF_PAC)) %>%
  group_by(farm_id) %>%
  mutate(treated = if_else(any(year == 2020 & BENEF_PAC == 1) & any(year == 2023 & BENEF_PAC == 0), 1, 0)) %>%
  mutate(treated = if_else(year == 2023, treated, 0))

# Create "Post" variable
panel_data <- panel_data %>%
  mutate(Post = if_else(year == 2023, 1, 0))

# Create centered age variable
panel_data <- panel_data %>%
  mutate(AGE = if_else(year == 2023, AGE +3, AGE)) %>%
  mutate(running_age = AGE - 67)

#================ Fuzzy diff-in-disc (full sample and controls) ================

# Choose an optimal bandwidth for the analysis
panel_data2020 <- panel_data %>%
  filter(year == 2020)

bw <- rdbwselect(
  y = panel_data2020$PBSTOT_COEF17,
  x = panel_data2020$running_age,
  c = 0,
  fuzzy = panel_data2020$treated,
  masspoints = "adjust"
)

summary(bw)

panel_bw <- panel_data %>%
  filter(abs(running_age) <= bw$bws["mserd", "h (left)"])

# Effect of treatment on gross production
didisc <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age +
    SAU_TOT +
    UGBTA.TOT |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
)  

summary(didisc)  
 
#================ Fuzzy diff-in-disc (full sample, no controls) ================

# Choose an optimal bandwidth for the analysis
bw <- rdbwselect(
  y = panel_data2020$PBSTOT_COEF17,
  x = panel_data2020$running_age,
  c = 0,
  fuzzy = panel_data2020$treated,
  masspoints = "adjust"
)

summary(bw)

panel_bw <- panel_data %>%
  filter(abs(running_age) <= bw$bws["mserd", "h (left)"])

# Effect of treatment on gross production
didisc_nc <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
)  

summary(didisc_nc) 
#================ Restrict the dataset to BENEF_PAC = 1 in 2020 ================

# Restrict the sample to farms for were eligible in 2020
data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1))

data_restricted2020 <- data_restricted %>%
  filter (year == 2020)

# Choose an optimal bandwidth for the analysis
bw <- rdbwselect(
  y = data_restricted2020$PBSTOT_COEF17,
  x = data_restricted2020$running_age,
  c = 0,
  fuzzy = data_restricted2020$treated,
  masspoints = "adjust"
)

summary(bw)

panel_bw <- data_restricted %>%
  filter(abs(running_age) <= bw$bws["mserd", "h (left)"])

# Effect of treatment on gross production (with controls)
didisc_rest <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age +
    SAU_TOT +
    UGBTA.TOT |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
) 

summary(didisc_rest) 

# Effect of treatment on gross production (without controls)
didisc_rest_nc <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
)  

summary(didisc_rest_nc) 
#=========================== Exclude the micro farms ===========================

# Restrict the sample to farms for were eligible in 2020
data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0)

data_exclusion2020 <- data_exclusion %>%
  filter(year == 2020)

# Choose an optimal bandwidth for the analysis
bw <- rdbwselect(
  y = data_exclusion2020$PBSTOT_COEF17,
  x = data_exclusion2020$running_age,
  c = 0,
  fuzzy = data_exclusion2020$treated,
  masspoints = "adjust"
)

summary(bw)

panel_bw <- data_exclusion %>%
  filter(abs(running_age) <= bw$bws["mserd", "h (left)"])

# Effect of treatment on gross production (with controls)
didisc_excl <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age +
    SAU_TOT +
    UGBTA.TOT |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
) 

summary(didisc_excl) 

# Effect of treatment on gross production (without controls)
didisc_excl_nc <- feols(
  PBSTOT_COEF17 ~ Post +
    running_age +
    OVER_67 +
    Post:running_age |
    treated ~ Post:OVER_67,
  data = panel_bw,
  cluster = ~ farm_id
)  

summary(didisc_excl_nc) 
#============ Comparative Plot of different Diff-in-Disc Estimates =============

models_list <- list(
  "Controls (full)"     = didisc,
  "No controls (full)"         = didisc_nc,
  "Controls (BENEF_PAC=1 in 2020)"   = didisc_rest,
  "No Controls (BENEF_PAC=1 in 2020)" = didisc_rest_nc,
  "Controls (exclude micro)"    = didisc_excl,
  "No controls (exclude micro)" = didisc_excl_nc
)

didisc_specs <- do.call(rbind, lapply(names(models_list), function(spec) {
  
  m <- models_list[[spec]]
  
  # For feols IV objects, use coeftable() to extract safely
  ct <- coeftable(m)
  
  # The instrumented 'treated' coefficient row
  est <- ct["fit_treated", "Estimate"]
  se  <- ct["fit_treated", "Std. Error"]
  
  data.frame(
    spec     = spec,
    estimate = est,
    se       = se,
    ci_low   = est - 1.96 * se,
    ci_high  = est + 1.96 * se
  )
  
})) %>%
  mutate(significant = ifelse(ci_low > 0 | ci_high < 0,
                              "Significant (5%)",
                              "Not significant"))

didisc_specs$spec <- factor(didisc_specs$spec, levels = names(models_list))

plot_didisc_specs <- ggplot(didisc_specs, aes(x = spec, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3) +
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "Diff-in-Disc estimate (Lost eligibility effect on standard gross output)",
    title = "Difference-in-Discontinuity Estimates Across Model Specifications",
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

plot_didisc_specs

