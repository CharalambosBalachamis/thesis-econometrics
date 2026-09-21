# ==============================================================================
# STAGE 01: MACRO DATASET BUILDER (EXACT 232-REGION THESIS REPLICATION)
# ------------------------------------------------------------------------------
# Author: Charalambos Balachamis
# Mechanics: 1999-2023 extraction, t-1 lagging, global Z-score standardization
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

cat("=======================================================================\n")
cat(" [STAGE 01] ASSEMBLING MASTER MACRO PANEL (232-REGION EXACT MATCH)     \n")
cat("=======================================================================\n\n")

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. LOAD RAW DATA & STANDARDIZE HEADERS
# ------------------------------------------------------------------------------
cat("[1/5] Loading raw datasets...\n")

clean_eurostat_headers <- function(df) {
  df %>%
    rename_with(~"geo", any_of(c("geo", "REF_AREA", "NUTS_ID", "geopolitical entity (reporting)", "GEOPOL"))) %>%
    rename_with(~"Year", any_of(c("TIME_PERIOD", "time", "TIME", "Year"))) %>%
    rename_with(~"values", any_of(c("OBS_VALUE", "values", "Value", "VALUE")))
}

# Auto-detect raw folder
raw_dir <- if (dir.exists("data/raw")) "data/raw" else "data/raw_data"

df_prod     <- read_csv(file.path(raw_dir, "productivity_per_worker_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_deflator <- read_csv(file.path(raw_dir, "gdp_deflator_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_gfcf     <- read_csv(file.path(raw_dir, "gfcf_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_edu      <- read_csv(file.path(raw_dir, "education_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_htc1     <- read_csv(file.path(raw_dir, "htc_rev1_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()
df_htc2     <- read_csv(file.path(raw_dir, "htc_rev2_raw.csv"), show_col_types = FALSE) %>% clean_eurostat_headers()

# ------------------------------------------------------------------------------
# 2. EXTRACT EXACT 232-REGION SAMPLE (THE MODERN CORE)
# ------------------------------------------------------------------------------
cat("[2/5] Filtering to the strict 232 NUTS-2 region sample...\n")

eu27_codes <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

extract_clean_regions <- function(df) {
  df %>%
    mutate(geo = str_trim(as.character(geo))) %>%
    filter(nchar(geo) == 4, substr(geo, 1, 2) %in% eu27_codes, !grepl("Z|Y|X", substr(geo, 3, 4))) %>%
    pull(geo) %>% unique()
}

# The exact intersection that locked in your 232 regions
universal_regions <- reduce(list(extract_clean_regions(df_prod), extract_clean_regions(df_gfcf), extract_clean_regions(df_htc2)), intersect)

cat(sprintf("  -> Locked sample: %d EU-27 regions identified.\n", length(universal_regions)))

# ------------------------------------------------------------------------------
# 3. COMPUTE NATIONAL DEFLATOR & REGIONAL DENOMINATOR (1999-2023)
# ------------------------------------------------------------------------------
cat("[3/5] Computing national deflators and regional employment base...\n")

clean_deflator <- df_deflator %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(nchar(geo) == 2, geo %in% eu27_codes) %>%
  select(Country_Code = geo, Year, Deflator = values) %>%
  mutate(Year = as.numeric(Year), Deflator = as.numeric(Deflator)) %>%
  filter(!is.na(Deflator), Year >= 1999, Year <= 2023)

emp_total_pre08 <- df_htc1 %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(geo %in% universal_regions, grepl("TOTAL|total", nace_r1, ignore.case = TRUE)) %>%
  select(geo, Year, Workers_THS = values) %>%
  mutate(Year = as.numeric(Year), Workers_THS = as.numeric(Workers_THS)) %>%
  filter(Year >= 1999, Year < 2008)

emp_total_post08 <- df_htc2 %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(geo %in% universal_regions, grepl("TOTAL|total", nace_r2, ignore.case = TRUE)) %>%
  select(geo, Year, Workers_THS = values) %>%
  mutate(Year = as.numeric(Year), Workers_THS = as.numeric(Workers_THS)) %>%
  filter(Year >= 2008, Year <= 2023)

regional_employment <- bind_rows(emp_total_pre08, emp_total_post08) %>% arrange(geo, Year)

# ------------------------------------------------------------------------------
# 4. COMPUTE MACRO VARIABLES & APPLY LAGGING
# ------------------------------------------------------------------------------
cat("[4/5] Computing Real Productivity, Capital, and Education (Applying t-1 Lags)...\n")

# A. Real Productivity (2000-2023)
panel_prod <- df_prod %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(geo %in% universal_regions) %>%
  select(geo, Year, Nominal_Prod_EUR = values) %>%
  mutate(Year = as.numeric(Year), Nominal_Prod_EUR = as.numeric(Nominal_Prod_EUR), Country_Code = substr(geo, 1, 2)) %>%
  filter(Year >= 2000, Year <= 2023) %>%
  left_join(clean_deflator, by = c("Country_Code", "Year")) %>%
  mutate(
    Real_Prod_EUR = (Nominal_Prod_EUR / Deflator) * 100,
    log_prod = log(Real_Prod_EUR)
  )

# B. Real Capital Investment (GFCF) - Starts 1999 for lagging
panel_gfcf <- df_gfcf %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(geo %in% universal_regions) %>%
  select(geo, Year, Nominal_GFCF_MIO = values) %>%
  mutate(Year = as.numeric(Year), Nominal_GFCF_MIO = as.numeric(Nominal_GFCF_MIO)) %>%
  mutate(Country_Code = substr(geo, 1, 2)) %>%
  filter(Year >= 1999, Year <= 2023) %>%
  left_join(clean_deflator, by = c("Country_Code", "Year")) %>%
  left_join(regional_employment, by = c("geo", "Year")) %>%
  mutate(
    Real_GFCF_MIO = (Nominal_GFCF_MIO / Deflator) * 100,
    Real_GFCF_per_Worker = (Real_GFCF_MIO * 1000) / Workers_THS,
    log_gfcf_pw = log(Real_GFCF_per_Worker)
  ) %>%
  arrange(geo, Year) %>%
  group_by(geo) %>%
  mutate(laglog_gfcf_pw = dplyr::lag(log_gfcf_pw, n = 1)) %>%
  ungroup() %>%
  filter(Year >= 2000)

# C. Education - Starts 1999 for lagging
panel_edu <- df_edu %>%
  mutate(geo = str_trim(as.character(geo))) %>%
  filter(geo %in% universal_regions) %>%
  select(geo, Year, Raw_Edu = values) %>%
  mutate(Year = as.numeric(Year), Raw_Edu = as.numeric(Raw_Edu)) %>%
  filter(!is.na(Raw_Edu), Year >= 1999, Year <= 2023) %>%
  group_by(geo, Year) %>%
  summarise(Raw_Edu = mean(Raw_Edu, na.rm = TRUE), .groups = "drop") %>%
  arrange(geo, Year) %>%
  group_by(geo) %>%
  mutate(lag_Edu = dplyr::lag(Raw_Edu, n = 1)) %>%
  ungroup() %>%
  filter(Year >= 2000)

# ------------------------------------------------------------------------------
# 5. ASSEMBLE PANEL & COMPUTE Z-SCORES
# ------------------------------------------------------------------------------
cat("[5/5] Merging final dataset and computing global Z-scores...\n")

master_stage1 <- panel_prod %>%
  select(geo, Country_Code, Year, log_prod) %>%
  left_join(panel_gfcf %>% select(geo, Year, laglog_gfcf_pw), by = c("geo", "Year")) %>%
  left_join(panel_edu %>% select(geo, Year, lag_Edu), by = c("geo", "Year")) %>%
  mutate(
    zlog_prod       = as.numeric(scale(log_prod)),
    zlaglog_gfcf_pw = as.numeric(scale(laglog_gfcf_pw)),
    zlag_Edu        = as.numeric(scale(lag_Edu))
  ) %>%
  arrange(geo, Year)

saveRDS(master_stage1, "data/processed/nuts2_macro_panel.rds")

cat("\n=======================================================================\n")
cat("STAGE 01 COMPLETE: 'data/processed/nuts2_macro_panel.rds' generated.\n")
cat(sprintf("Total Observations: %d across %d regions.\n", nrow(master_stage1), n_distinct(master_stage1$geo)))
cat("=======================================================================\n")