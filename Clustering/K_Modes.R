# This R file contains code to replicate the K Modes clustering results and the elbow plot. 

library(dplyr)
library(arrow)
library(sf)
library(ggplot2)
library(readr)
library(stringi)
library(tidyverse)
library(klaR)  
library(cluster)

#=============================== Load the files ================================
base_path <- getwd()
file_path <- file.path(base_path, "francetransfert-1325119685PiA_panel_clean_coef.parquet")
df <- read_parquet(file_path)

colnames(df) %>%
  sort()

#============================== Select variables ===============================
#Couldn't really categories by farming type, organic farming, quality certification, 

variables <- c(
  "REGL",
  "STATUT",
  "DIMECO_COEF2017",
  'BENEF_PAC',
  "RDEV", 
  'year',
  "SAU_TOT_CAT", 
  "PBSTOT_CAT", 
  'farm_id'
)
 

df_2023 <- df %>%
  dplyr::select(farm_id, year, OVER_67) %>%
  dplyr::filter(year == 2023) %>%
  dplyr::select(-year)

#=============================== Data processing ===============================
#Filter the data for farms that lost CAP eligibility in 2023. 
#Keep the baseline data of 2020. 
df_2020 <- df %>%
  dplyr::select(dplyr::all_of(variables)) %>%
  dplyr::relocate(farm_id, year) %>%
  dplyr::group_by(farm_id) %>%
  dplyr::mutate(
    lost_eligibility = as.integer(
      any(year == 2020 & BENEF_PAC == 1) &
        any(year == 2023 & BENEF_PAC == 0)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year == 2020, lost_eligibility == 1) %>%
  dplyr::select(-lost_eligibility, -BENEF_PAC, -year)  

#Merge with the 2023 data for active/ceased status of farms
df_merged <- df_2020 %>%
  dplyr::left_join(df_2023, by = "farm_id")%>%
  dplyr::select(-farm_id)%>%
  dplyr ::rename(OVER_67_2023 = OVER_67)


colSums(is.na(df_merged))


#================================== K-modes ====================================

# Convert all columns to factors explicitly
df_kmodes <- as.data.frame(lapply(df_merged, as.factor))

set.seed(123)
ks <- 2:6
kmodes_results <- lapply(ks, function(k) {
  klaR::kmodes(df_kmodes, modes = k, iter.max = 100, weighted = FALSE)
})
names(kmodes_results) <- paste0("k", ks)

table(kmodes_results[["k3"]]$cluster)
kmodes_results[["k3"]]$modes

# Summaries across k
kmodes_table <- data.frame(
  k = ks,
  withinss = sapply(kmodes_results, function(m) sum(m$withindiff)),
  size_min = sapply(kmodes_results, function(m) min(m$size)),
  size_max = sapply(kmodes_results, function(m) max(m$size))
)

kmodes_table$pct_decrease <- c(NA, -diff(kmodes_table$withinss) /
                                 head(kmodes_table$withinss, -1) * 100)

print(kmodes_table)

diss_matrix <- daisy(df_kmodes, metric = "gower")
silhouette_means <- sapply(kmodes_results, function(model) {
  sil <- silhouette(model$cluster, diss_matrix)
  mean(sil[, 3])
})

data.frame(
  k = ks,
  mean_silhouette = silhouette_means
)
#================================ Elbow plot ===================================
plot(kmodes_table$k, kmodes_table$withinss,
     type = "b", pch = 19,
     col = "blue",
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Distance",
     #main = "K-modes Elbow Plot"
     )
grid()

