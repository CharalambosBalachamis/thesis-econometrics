# ==============================================================================
# MASTER EXECUTION SCRIPT
# Project: EU NUTS-2 Shift-Share Econometrics Pipeline
# Author: Charalambos Balachamis
# ==============================================================================

# 1. Install required packages if missing
required_packages <- c("tidyverse", "fixest")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. Run the pipeline sequentially
cat("Starting Full Thesis Econometric Pipeline...\n\n")

source("src/01_build_panel.R")
source("src/02_construct_instruments.R")
source("src/03_estimate_models.R")

cat("\n=======================================================================\n")
cat("PIPELINE COMPLETE: All data processed and tables exported to 'outputs/'.\n")
cat("=======================================================================\n")