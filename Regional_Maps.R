# This R file contains code to replicate the regional point maps and the regional summary statistics. 

library(dplyr)
library(tidyverse)
library(arrow)
library(sf)
library(ggplot2)
library(readr)
library(stringi)
library(knitr)

#=============================== Load the files  ==============================

base_path <- getwd()
file_path <- file.path(base_path, "francetransfert-1325119685/RA2020_ESEA2023_PAC.parquet")
df <- read_parquet(file_path)

colnames(df) %>%
  sort()


french_regions_sf <- st_read(
  "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson",
  quiet = TRUE
)

#=================== Create a proportional point map of 2020 ===================

# 1) Count farms per region 
total_counts_2020 <- df %>%
  mutate(SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)) %>%
  count(SIEGE_LIB_REG, name = "n_farms")

# 2) Count of beneficiary vs non beneficiay farms per region
beneficiary_count_2020 <- df %>%
  group_by(SIEGE_LIB_REG, BENEF_PAC_2020) %>%
  summarise(n_farms = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = BENEF_PAC_2020,
    values_from = n_farms,
    names_prefix = "n_farm_",
    values_fill = 0
  ) %>%
  rename(
    n_farm_non_beneficiary = n_farm_0,
    n_farm_beneficiary     = n_farm_1
  )

# 3) Merge the counts by the region 
counts_2020 <- total_counts_2020 %>%
  left_join(beneficiary_count_2020, by = "SIEGE_LIB_REG")

# 4) Join counts to regions 
french_regions_sf_2020 <- french_regions_sf %>%
  rename(SIEGE_LIB_REG = nom) %>%
  select(-code) %>%
  mutate(
    SIEGE_LIB_REG = stri_trans_general(SIEGE_LIB_REG, "Latin-ASCII"),
    SIEGE_LIB_REG = toupper(SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("'", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("-", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("[^A-Z ]", "", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("\\s+", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)
  ) %>%
  left_join(counts_2020, by = "SIEGE_LIB_REG")


# 5) Create one POINT geometry per region 
regions_pts <- french_regions_sf_2020 %>%
  st_point_on_surface() %>%
  select(SIEGE_LIB_REG, n_farm_beneficiary, n_farm_non_beneficiary, geometry)

# 6) Two points per region (beneficiary vs non-beneficiary) 
pts_long_sf <- regions_pts %>%
  pivot_longer(
    cols = c(n_farm_beneficiary, n_farm_non_beneficiary),
    names_to = "status",
    values_to = "n_farms"
  ) %>%
  mutate(
    status = recode(
      status,
      n_farm_beneficiary     = "CAP Beneficiary",
      n_farm_non_beneficiary = "CAP Non-Beneficiary"
    )
  ) %>%
  filter(!is.na(n_farms))

# 7) Plot 
ggplot() +
  geom_sf(data = french_regions_sf_2020, fill = "white", color = "grey40", linewidth = 0.3) +
  geom_sf(data = pts_long_sf, aes(size = n_farms, color = status), alpha = 0.75) +
  scale_size_area(max_size = 18) +
  scale_color_manual(values = c("CAP Beneficiary" = "blue", "CAP Non-Beneficiary" = "red")) +
  guides(
    size  = guide_legend(override.aes = list(alpha = 1)),
    color = guide_legend(override.aes = list(alpha = 1))
  ) +
  theme_void() +
  labs(
    title = "Farms by French Region (2020)",
    size  = "Number of Farms",
    color = NULL
  )

#==================- Create a proportional point map of 2023 ===================

# 1) Count farms per region 
counts_2023 <- df %>%
  mutate(
    SIEGE_LIB_REG = trimws(SIEGE_LIB_REG),
    ETAT_ESEA2023 = trimws(ETAT_ESEA2023)
  ) %>%
  group_by(SIEGE_LIB_REG, ETAT_ESEA2023) %>%
  summarise(n_farms = n(), .groups = "drop")

counts_2023_wide <- counts_2023 %>%
  tidyr::pivot_wider(
    names_from  = ETAT_ESEA2023,
    values_from = n_farms,
    values_fill = 0
  ) %>%
  rename(
    n_active = ACTIF,
    n_ceased = CESSE,
    n_na     = `NA`
  )

# 2) Among active farms, count beneficiary vs non beneficiay farms per region
active_benef_2023 <- df %>%
  mutate(SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)) %>%
  filter(ETAT_ESEA2023 == "ACTIF") %>%     
  group_by(SIEGE_LIB_REG, BENEF_PAC_2023) %>%
  summarise(n_farms = n(), .groups = "drop")


active_benef_2023_wide <- active_benef_2023 %>%
  tidyr::pivot_wider(
    names_from  = BENEF_PAC_2023,
    values_from = n_farms,
    values_fill = 0
  ) %>%
  rename(
    n_active_non_beneficiary = `0`,
    n_active_beneficiary     = `1`
  )

# 3) Merge the counts by the region 
total_counts_2023 <- counts_2023_wide %>%
  left_join(active_benef_2023_wide, by = "SIEGE_LIB_REG")

# 4) Join counts to regions 
french_regions_sf_2023 <- french_regions_sf %>%
  rename(SIEGE_LIB_REG = nom) %>%
  select(-code) %>%
  mutate(
    SIEGE_LIB_REG = stri_trans_general(SIEGE_LIB_REG, "Latin-ASCII"),
    SIEGE_LIB_REG = toupper(SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("'", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("-", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("[^A-Z ]", "", SIEGE_LIB_REG),
    SIEGE_LIB_REG = gsub("\\s+", " ", SIEGE_LIB_REG),
    SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)
  ) %>%
  left_join(total_counts_2023, by = "SIEGE_LIB_REG")

# 5) Create one POINT geometry per region 
regions_pts_2023 <- french_regions_sf_2023 %>%
  st_point_on_surface() %>%
  select(-n_na)

# 6) Two points per region (beneficiary vs non-beneficiary) 
pts_long_sf_2023 <- regions_pts_2023 %>%
  pivot_longer(
    cols = c(n_active_beneficiary, n_active_non_beneficiary),
    names_to = "status",
    values_to = "n_farms"
  ) %>%
  mutate(
    status = recode(
      status,
      n_active_beneficiary     = "CAP Beneficiary",
      n_active_non_beneficiary = "CAP Non-Beneficiary"
    )
  ) %>%
  filter(!is.na(n_farms))

# 7) Plot 
ggplot() +
  geom_sf(data = french_regions_sf_2023,
          fill = "white", color = "grey40", linewidth = 0.3) +
  geom_sf(data = pts_long_sf_2023,
          aes(size = n_farms, color = status), alpha = 0.75) +
  
  scale_size_continuous(
    range = c(1, 12),      
    breaks = c(50,100,200,500, 1000, 2000, 5000),
    labels = scales::comma
  ) +
  
  scale_color_manual(
    values = c("CAP Beneficiary" = "blue",
               "CAP Non-Beneficiary" = "red")
  ) +
  
  guides(
    size  = guide_legend(override.aes = list(alpha = 1)),
    color = guide_legend(override.aes = list(alpha = 1))
  ) +
  theme_void() +
  labs(
    title = "Active Farms by French Region (2023)",
    size  = "Number of Farms",
    color = NULL
  )


#=================== Dominant farming type and economic size per region ===================

# 1) Dominant farming type (OTEFDA) per region
dominant_farming_type <- df %>%
  mutate(SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)) %>%
  filter(!is.na(OTEFDA_COEF17)) %>%
  group_by(SIEGE_LIB_REG, OTEFDA_COEF17) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SIEGE_LIB_REG) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(dominant_farming_type = OTEFDA_COEF17,
         n_dominant = n)

# 2) Percentage of micro farms per region
micro_farm_pct <- df %>%
  mutate(SIEGE_LIB_REG = trimws(SIEGE_LIB_REG)) %>%
  filter(!is.na(DIMECO_COEF2017)) %>%
  group_by(SIEGE_LIB_REG) %>%
  summarise(
    n_total = n(),
    n_micro  = sum(DIMECO_COEF2017 ==  "1-micros", na.rm = TRUE),
    pct_micro = round(100 * n_micro / n_total, 1),
    .groups = "drop"
  )

# 3) Join both tables
region_summary <- dominant_farming_type %>%
  left_join(micro_farm_pct, by = "SIEGE_LIB_REG") %>%
  mutate(pct_dominant = round(100 * n_dominant / n_total, 1)) %>%
  select(SIEGE_LIB_REG, dominant_farming_type, n_dominant, pct_dominant, n_total, n_micro, pct_micro) %>%
  arrange(desc(pct_micro))

