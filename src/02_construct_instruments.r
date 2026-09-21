# ==============================================================================
# STAGE 02: NACE SPLICING & BARTIK INSTRUMENT GENERATOR (EXACT THESIS REPLICATION)
# ------------------------------------------------------------------------------
# Author: Charalambos Balachamis
# Mechanics: 2008 NACE Splice -> 2001-03 LOO Bartik -> t-1 Lags -> Z-Scores
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

cat("=======================================================================\n")
cat(" [STAGE 02] NACE SPLICING & BARTIK SHIFT-SHARE CONSTRUCTION            \n")
cat("=======================================================================\n\n")

# ------------------------------------------------------------------------------
# 1. LOAD STAGE 1 MACRO PANEL & RAW DATA
# ------------------------------------------------------------------------------
cat("[1/4] Loading Stage 1 panel and raw worker counts...\n")

macro_panel <- readRDS("data/processed/nuts2_macro_panel.rds")
universal_regions <- sort(unique(macro_panel$geo))

clean_eurostat_headers <- function(df) {
  df %>%
    rename_with(~"geo", any_of(c("geo", "REF_AREA", "NUTS_ID", "geopolitical entity (reporting)", "GEOPOL"))) %>%
    rename_with(~"Year", any_of(c("TIME_PERIOD", "time", "TIME", "Year"))) %>%
    rename_with(~"values", any_of(c("OBS_VALUE", "values", "Value", "VALUE")))
}

raw_dir <- if (dir.exists("data/raw")) "data/raw" else "data/raw_data"
df_htc1 <- read_csv(file.path(raw_dir, "htc_rev1_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_htc2 <- read_csv(file.path(raw_dir, "htc_rev2_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()

# ------------------------------------------------------------------------------
# 2. EXTRACT TOTAL WORKERS & HTC PERCENTAGE SHARES
# ------------------------------------------------------------------------------
get_workers <- function(df, nace_col, target, min_yr, max_yr) {
  df %>%
    mutate(geo = str_trim(as.character(geo))) %>%
    filter(geo %in% universal_regions, !!sym(nace_col) == target, unit == "THS_PER") %>%
    select(geo, Year, Workers_THS = values) %>%
    mutate(Year = as.numeric(Year), Workers_THS = as.numeric(Workers_THS)) %>%
    filter(!is.na(Workers_THS), Year >= min_yr, Year <= max_yr)
}

total_pre08  <- get_workers(df_htc1, "nace_r1", "TOTAL", 2000, 2008)
total_post08 <- get_workers(df_htc2, "nace_r2", "TOTAL", 2008, 2023)

htc_pre08 <- get_workers(df_htc1, "nace_r1", "HTC", 2000, 2008) %>%
  inner_join(total_pre08, by = c("geo", "Year"), suffix = c("_HTC", "_Total")) %>%
  mutate(HTC_Share = (Workers_THS_HTC / Workers_THS_Total) * 100) %>%
  select(geo, Year, HTC_Workers = Workers_THS_HTC, HTC_Old = HTC_Share)

htc_post08 <- get_workers(df_htc2, "nace_r2", "HTC", 2008, 2023) %>%
  inner_join(total_post08, by = c("geo", "Year"), suffix = c("_HTC", "_Total")) %>%
  mutate(HTC_Share = (Workers_THS_HTC / Workers_THS_Total) * 100) %>%
  select(geo, Year, HTC_Workers = Workers_THS_HTC, HTC_Modern = HTC_Share)

# 2008 Ratio Splice on the Shares
ratios_2008 <- inner_join(
  htc_pre08 %>% filter(Year == 2008) %>% select(geo, Old_2008 = HTC_Old),
  htc_post08 %>% filter(Year == 2008) %>% select(geo, Mod_2008 = HTC_Modern),
  by = "geo"
) %>% mutate(Ratio = Mod_2008 / Old_2008)

htc_spliced <- full_join(htc_pre08, htc_post08, by = c("geo", "Year")) %>%
  left_join(ratios_2008 %>% select(geo, Ratio), by = "geo") %>%
  mutate(
    HTC_Spliced = if_else(Year >= 2008, HTC_Modern, HTC_Old * Ratio),
    HTC_Workers_Merge = if_else(Year >= 2008, HTC_Workers.y, HTC_Workers.x),
    Country_Code = substr(geo, 1, 2)
  )

# ------------------------------------------------------------------------------
# 3. LEAVE-ONE-OUT (LOO) BARTIK INSTRUMENT (2001-2003 BASELINE)
# ------------------------------------------------------------------------------
cat("[2/4] Constructing Leave-One-Out Bartik IV (2001-2003 Baseline)...\n")

loo_calc <- htc_spliced %>%
  group_by(Country_Code, Year) %>%
  mutate(
    Nat_HTC_Workers = sum(HTC_Workers_Merge, na.rm = TRUE),
    Rest_of_Country = Nat_HTC_Workers - HTC_Workers_Merge
  ) %>%
  ungroup() %>%
  group_by(Country_Code) %>%
  mutate(n_regions = n_distinct(geo)) %>%
  ungroup() %>%
  mutate(Exogenous_Pool = if_else(n_regions == 1, Nat_HTC_Workers, Rest_of_Country))

# Baseline Pool & Share (2001-2003 Mean with na.rm = TRUE)
base_metrics <- loo_calc %>%
  filter(Year %in% c(2001, 2002, 2003)) %>%
  group_by(geo) %>%
  summarise(
    Base_Pool_0103 = mean(Exogenous_Pool, na.rm = TRUE),
    Base_Share_0103 = mean(HTC_Spliced, na.rm = TRUE),
    .groups = "drop"
  )

bartik_panel <- loo_calc %>%
  left_join(base_metrics, by = "geo") %>%
  mutate(
    LOO_Growth = if_else(is.na(Base_Pool_0103) | Base_Pool_0103 <= 0, NA_real_, Exogenous_Pool / Base_Pool_0103),
    Bartik_HTC_IV = Base_Share_0103 * LOO_Growth,
    Bartik_HTC_IV = if_else(is.infinite(Bartik_HTC_IV) | is.nan(Bartik_HTC_IV), NA_real_, Bartik_HTC_IV)
  ) %>%
  select(geo, Year, HTC_Spliced, Bartik_HTC_IV)

# ------------------------------------------------------------------------------
# 4. FINAL MERGE, t-1 LAGGING & GLOBAL Z-SCORES
# ------------------------------------------------------------------------------
cat("[3/4] Applying t-1 lags and standardizing global Z-scores...\n")

final_regression_panel <- macro_panel %>%
  inner_join(bartik_panel, by = c("geo", "Year")) %>%
  arrange(geo, Year) %>%
  group_by(geo) %>%
  mutate(
    lag_HTC    = dplyr::lag(HTC_Spliced, n = 1),
    lag_Bartik = dplyr::lag(Bartik_HTC_IV, n = 1)
  ) %>%
  ungroup() %>%
  mutate(
    zlag_HTC    = as.numeric(scale(lag_HTC)),
    zlag_Bartik = as.numeric(scale(lag_Bartik))
  )

cat("[4/4] Generating first-stage correlation report...\n")

valid_iv <- final_regression_panel %>% filter(!is.na(zlag_HTC), !is.na(zlag_Bartik))
iv_corr <- cor(valid_iv$zlag_HTC, valid_iv$zlag_Bartik)

saveRDS(final_regression_panel, "data/processed/final_regression_panel.rds")
write_csv(final_regression_panel, "data/processed/final_regression_panel.csv")

cat("\n=======================================================================\n")
cat("STAGE 02 COMPLETE: 'data/processed/final_regression_panel.rds' generated.\n")
cat(sprintf("Total Observations: %d across %d regions.\n", nrow(final_regression_panel), n_distinct(final_regression_panel$geo)))
cat(sprintf("FIRST-STAGE PREVIEW: HTC to Bartik IV Correlation: r = %.3f\n", iv_corr))
cat("=======================================================================\n")