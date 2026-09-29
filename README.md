Tech Employment & Regional Productivity in the EU (2000–2023)
An econometric pipeline built in R to analyze the causal impact of high-technology (HTC) employment on regional labor productivity across 232 European NUTS-2 regions.

Executive Summary & Policy Impact

While standard correlations suggest tech employment drives regional wealth, this project proves that standard models are upwardly biased by spatial sorting (tech firms migrating to already-wealthy hubs). Using an instrumental variable approach, the findings reveal:

    The Tech Dividend is Real, but Smaller: An exogenous tech shock causes a 0.111 SD increase in local productivity (smaller than the biased OLS estimate of 0.122 SD).

    The Impact is Highly Conditional: In mature Western European markets, tech spillovers are efficiently absorbed. In Eastern/Southern peripheral regions, the causal premium becomes statistically insignificant.

    Policy Recommendation: European Cohesion Policy should prioritize structural convergence (physical capital and institutional quality) in peripheral regions before allocating subsidies for advanced innovation clusters.

Methodology & Econometric Identification

To address endogeneity and reverse causality, the empirical strategy utilizes a Shift-Share (Bartik) Instrumental Variable framework.

    Instrument: A leave-one-out exogenous national growth shock interacted with pre-crisis (2001-2003) regional historical employment shares.

    Model: Estimated using Two-Way Fixed Effects (TWFE) with national linear time trends to absorb unobserved regional traits and pan-European shocks.

    Standard Errors: Cluster-robust at the NUTS-2 regional level.

Core Variables

    Target (Y): Labor Productivity (Regional GDP / total regional employment).

    Treatment (X): Technological Employment (Sectoral employment shares isolating the High-Technology Sector).

    Controls: Physical Capital (Gross Fixed Capital Formation per worker) and Human Capital (Tertiary education attainment rates).

    (Continuous variables are standardized into Z-scores).

Detailed Findings

1. The Aggregate Tech Dividend & Sorting Bias
An exogenous 1 standard deviation increase in local HTC employment directly causes a 0.111 SD increase in regional labor productivity. The difference between this 2SLS estimate and the TWFE OLS estimate (0.122 SD) quantifies the spatial sorting bias.

2. The Core-Periphery Divergence

    EU-15 Consistency: In the mature labor markets of the Advanced Core, national tech shocks diffuse efficiently (IV: +0.130 SD).

    CEE Periphery: In the New Member States, the causal premium drops to a statistically insignificant -0.446 SD. The initial positive OLS correlation is revealed to be driven entirely by endogenous spatial sorting.

3. The Absorptive Constraint
In the West, mature industrial networks internalize spillovers (IV: +0.172 SD). In the East and South, positive correlations disappear under 2SLS, demonstrating that tech workers merely chased existing growth. Peripheral growth currently relies almost entirely on physical capital deepening (IV: +0.671 SD in CEE).
Repository Structure

    /src/: Contains the modular R scripts for data cleaning, splicing, instrument construction, and fixest model estimation.

    /data/raw/: Original Eurostat SDMX CSVs (Productivity, GFCF, Education, NACE Rev 1.1/2.0).

    /outputs/tables/: Publication-ready regression tables.

How to Run

    Clone this repository.

    Open thesis.Rproj in RStudio.

    Run the master script to execute the pipeline from start to finish:

## How to Run
1. Clone this repository.
2. Open `thesis.Rproj` in RStudio.
3. Run the master script to execute the pipeline from start to finish:
   ```R
   source("run_all.R")
