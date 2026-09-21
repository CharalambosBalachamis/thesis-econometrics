# ==============================================================================
# STAGE 03: ECONOMETRIC MODEL ESTIMATION (TWFE, TRENDS, 2SLS BARTIK & HETEROGENEITY)
# ------------------------------------------------------------------------------
# Author: Charalambos Balachamis
# Project: EU NUTS-2 Shift-Share Econometrics Pipeline
#
# Inputs:
#   - data/processed/final_regression_panel.rds (from Stage 02)
#
# Outputs:
#   - outputs/tables/table1_ols_stepwise.txt (and .tex / .csv)
#   - outputs/tables/table2_iv_stepwise.txt
#   - outputs/tables/table3_heterogeneity_geography.txt
#   - outputs/tables/table4_heterogeneity_institutions.txt
#   - outputs/tables/table5_heterogeneity_agglomeration.txt
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

cat("=======================================================================\n")
cat(" [STAGE 03] ESTIMATING TWFE OLS, 2SLS BARTIK IV & HETEROGENEITY MODELS \n")
cat("=======================================================================\n\n")

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. LOAD REGRESSION PANEL & CONSTRUCT HETEROGENEITY PARTITIONS
# ------------------------------------------------------------------------------
cat("[1/5] Ingesting final regression panel and setting up spatial partitions...\n")

df <- readRDS("data/processed/final_regression_panel.rds")

# Classification Codes
nms_codes <- c("PL", "CZ", "SK", "HU", "RO", "BG", "HR", "SI", "EE", "LV", "LT", "CY", "MT")

capital_nuts2 <- c(
  "AT13", "BE10", "BG41", "CY00", "CZ01", "DE30", "DK01", "EE00", 
  "ES30", "FI1B", "FR10", "HR04", "HU11", "IE06", "ITI4", "LT01", 
  "LU00", "LV00", "MT00", "NL32", "PL91", "PT17", "RO32", "SE11", 
  "SI04", "SK01"
)

# Enrich panel with heterogeneity markers
df_reg <- df %>%
  mutate(
    Geo_Region = case_when(
      Country_Code %in% c("DE", "FR", "AT", "BE", "NL", "LU", "IE")             ~ "1. West (Industrial Core)",
      Country_Code %in% c("PL", "CZ", "SK", "HU", "RO", "BG")                     ~ "2. East (CEE Convergence)",
      Country_Code %in% c("IT", "ES", "EL", "GR", "PT", "HR", "SI", "CY", "MT")   ~ "3. South (Mediterranean)",
      Country_Code %in% c("SE", "DK", "FI", "EE", "LV", "LT")                     ~ "4. North (Nordics & Baltics)",
      TRUE                                                                        ~ "Other"
    ),
    EU_Membership = if_else(Country_Code %in% nms_codes, "2. New Member States (CEE)", "1. EU-15 Advanced Core"),
    Agglomeration = if_else(geo %in% capital_nuts2, "1. Capital City Region", "2. Non-Capital Hinterland")
  )

cat(sprintf("  -> Total Observations: %d across %d NUTS-2 regions.\n\n", nrow(df_reg), n_distinct(df_reg$geo)))

# Variable dictionary for clean publication tables
var_dict <- c(
  "zlag_HTC"        = "HTC Share (t-1, Z-score)",
  "fit_zlag_HTC"    = "HTC Share [2SLS] (t-1, Z-score)",
  "zlaglog_gfcf_pw" = "Log Real GFCF/Worker (t-1, Z-score)",
  "zlag_Edu"        = "Tertiary Education (t-1, Z-score)",
  "zlag_Bartik"     = "Bartik HTC Instrument (t-1, Z-score)"
)

# ------------------------------------------------------------------------------
# 2. STEPWISE TWFE OLS SPECIFICATIONS
# ------------------------------------------------------------------------------
cat("[2/5] Estimating Stepwise OLS Models (Baseline -> Solow -> Time Trends)...\n")

# Model 1: Bivariate with Country + Year Fixed Effects
ols_1 <- feols(zlog_prod ~ zlag_HTC | Country_Code + Year, data = df_reg, vcov = ~geo)

# Model 2: Physical Capital Augmented
ols_2 <- feols(zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw | Country_Code + Year, data = df_reg, vcov = ~geo)

# Model 3: Full Solow Specification (+ Human Capital)
ols_3 <- feols(zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year, data = df_reg, vcov = ~geo)

# Model 4: Full Solow + Country-Specific Linear Time Trends
ols_4 <- feols(zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year + Country_Code[Year], data = df_reg, vcov = ~geo)

# Output Table 1
table1_ols <- etable(
  "OLS (Basic)"         = ols_1,
  "OLS (+Capital)"      = ols_2,
  "OLS (Full Solow)"    = ols_3,
  "OLS (Solow + Trend)" = ols_4,
  dict = var_dict,
  se.below = TRUE,
  digits = 3,
  fitstat = c("n", "r2", "wr2")
)

cat("\n=======================================================================\n")
cat(" TABLE 1: OLS STEPWISE TWFE ESTIMATES                                  \n")
cat("=======================================================================\n")
print(table1_ols)

# Export Table 1
writeLines(capture.output(table1_ols), "outputs/tables/table1_ols_stepwise.txt")

# ------------------------------------------------------------------------------
# 3. 2SLS BARTIK INSTRUMENTAL VARIABLE REGRESSIONS
# ------------------------------------------------------------------------------
cat("\n[3/5] Estimating 2SLS Bartik IV Models...\n")

# IV Model 1: Baseline IV
iv_1 <- feols(zlog_prod ~ 1 | Country_Code + Year | zlag_HTC ~ zlag_Bartik, data = df_reg, vcov = ~geo)

# IV Model 2: IV + Physical Capital
iv_2 <- feols(zlog_prod ~ zlaglog_gfcf_pw | Country_Code + Year | zlag_HTC ~ zlag_Bartik, data = df_reg, vcov = ~geo)

# IV Model 3: Full Solow IV
iv_3 <- feols(zlog_prod ~ zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year | zlag_HTC ~ zlag_Bartik, data = df_reg, vcov = ~geo)

# IV Model 4: Full Solow IV + Country Linear Trends
iv_4 <- feols(zlog_prod ~ zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year + Country_Code[Year] | zlag_HTC ~ zlag_Bartik, data = df_reg, vcov = ~geo)

# Extract First-Stage Diagnositics
fs_3 <- summary(iv_3, stage = 1)
fs_4 <- summary(iv_4, stage = 1)

table2_iv <- etable(
  "2SLS (Basic)"        = iv_1,
  "2SLS (+Capital)"     = iv_2,
  "2SLS (Full Solow)"   = iv_3,
  "2SLS (Solow + Trend)"= iv_4,
  dict = var_dict,
  se.below = TRUE,
  digits = 3,
  fitstat = c("n", "r2", "ivf1", "wald")
)

cat("\n=======================================================================\n")
cat(" TABLE 2: 2SLS BARTIK IV ESTIMATES                                     \n")
cat("=======================================================================\n")
print(table2_iv)

writeLines(capture.output(table2_iv), "outputs/tables/table2_iv_stepwise.txt")

# ------------------------------------------------------------------------------
# 4. HETEROGENEITY PARTITIONS (GEOGRAPHY & INSTITUTIONS)
# ------------------------------------------------------------------------------
cat("\n[4/5] Estimating Spatial, Institutional & Agglomeration Heterogeneity...\n")

# A. Cardinal Geography Splits (Excluding Other)
df_geo_valid <- df_reg %>% filter(Geo_Region != "Other")

ols_geo <- feols(
  zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year,
  data = df_geo_valid,
  split = ~Geo_Region,
  vcov = ~geo
)

iv_geo <- feols(
  zlog_prod ~ zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year | zlag_HTC ~ zlag_Bartik,
  data = df_geo_valid,
  split = ~Geo_Region,
  vcov = ~geo
)

table3_geo <- etable(
  iv_geo,
  dict = var_dict,
  se.below = TRUE,
  digits = 3,
  fitstat = c("n", "r2", "ivf1")
)

cat("\n=======================================================================\n")
cat(" TABLE 3: 2SLS CAUSAL ESTIMATES BY CARDINAL GEOGRAPHY                  \n")
cat("=======================================================================\n")
print(table3_geo)
writeLines(capture.output(table3_geo), "outputs/tables/table3_heterogeneity_geography.txt")

# B. Institutional History (EU-15 Core vs New Member States)
ols_inst <- feols(
  zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year,
  data = df_reg,
  split = ~EU_Membership,
  vcov = ~geo
)

iv_inst <- feols(
  zlog_prod ~ zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year | zlag_HTC ~ zlag_Bartik,
  data = df_reg,
  split = ~EU_Membership,
  vcov = ~geo
)

table4_inst <- etable(
  iv_inst,
  dict = var_dict,
  se.below = TRUE,
  digits = 3,
  fitstat = c("n", "r2", "ivf1")
)

cat("\n=======================================================================\n")
cat(" TABLE 4: 2SLS CAUSAL ESTIMATES BY INSTITUTIONAL HISTORY (EU-15 vs CEE)\n")
cat("=======================================================================\n")
print(table4_inst)
writeLines(capture.output(table4_inst), "outputs/tables/table4_heterogeneity_institutions.txt")

# C. Agglomeration Economies (Capital City vs Hinterland)
ols_agglom <- feols(
  zlog_prod ~ zlag_HTC + zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year,
  data = df_reg,
  split = ~Agglomeration,
  vcov = ~geo
)

iv_agglom <- feols(
  zlog_prod ~ zlaglog_gfcf_pw + zlag_Edu | Country_Code + Year | zlag_HTC ~ zlag_Bartik,
  data = df_reg,
  split = ~Agglomeration,
  vcov = ~geo
)

table5_agglom <- etable(
  iv_agglom,
  dict = var_dict,
  se.below = TRUE,
  digits = 3,
  fitstat = c("n", "r2", "ivf1")
)

cat("\n=======================================================================\n")
cat(" TABLE 5: 2SLS CAUSAL ESTIMATES BY AGGLOMERATION (CAPITAL VS REST)     \n")
cat("=======================================================================\n")
print(table5_agglom)
writeLines(capture.output(table5_agglom), "outputs/tables/table5_heterogeneity_agglomeration.txt")

# ------------------------------------------------------------------------------
# 5. CONSOLIDATED SUMMARY & COMPACT REPORT
# ------------------------------------------------------------------------------
cat("\n[5/5] Exporting summary report...\n")

cat("\n=======================================================================\n")
cat("STAGE 03 COMPLETE: All models estimated and tables saved to 'outputs/tables/'.\n")
cat("=======================================================================\n")