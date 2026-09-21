# Tech Employment & Regional Productivity in the EU

This repository contains the data engineering and econometric pipeline for analyzing the causal impact of high-technology (HTC) employment on regional labor productivity across 232 European NUTS-2 regions from 2000 to 2023.

## Motivation & Research Question
While economic theory often assumes tech workers drive local growth through knowledge spillovers, regional disparities across Europe continue to widen. A core endogeneity problem persists: highly productive regions inherently attract tech firms, creating severe reverse causality. 

This project asks: **Is tech employment a true catalyst for regional productivity, or do tech firms simply migrate to already-wealthy hubs?**

## Methodology
The empirical strategy addresses spatial sorting endogeneity using a **Shift-Share (Bartik) Instrumental Variable** framework. It leverages a leave-one-out exogenous national growth shock interacted with pre-crisis (2001-2003) regional historical employment shares. 

Models are estimated using Two-Way Fixed Effects (TWFE) with national linear time trends, absorbing unobserved regional traits, aggregate pan-European shocks, and divergent national trends. Standard errors are cluster-robust at the NUTS-2 regional level.

## Core Variables
* **Labor Productivity (Dependent Variable):** Regional GDP divided by total regional employment.
* **Technological Employment:** Sectoral employment shares utilized to isolate the High-Technology Sector (HTC).
* **Macroeconomic Controls:** Physical Capital (Gross Fixed Capital Formation per worker) and Human Capital (Tertiary education attainment rates).
* Continuous variables are standardized into Z-scores.

## Key Findings

### 1. The Aggregate Tech Dividend & Sorting Bias
An exogenous 1 standard deviation increase in local HTC employment directly causes a **0.111 SD** increase in regional labor productivity. However, this causal 2SLS estimate is smaller than the TWFE OLS estimate (0.122 SD), proving standard correlations are upwardly biased by the spatial sorting of tech firms into already-productive hubs.

### 2. The Core-Periphery Divide
Does the technological premium accrue uniformly, or is it constrained by regional institutional maturity?
* **EU-15 Consistency:** In the mature labor markets of the Advanced Core (EU-15), national tech shocks diffuse efficiently (IV: +0.130 SD).
* **CEE Periphery Collapse:** In the New Member States (CEE), the causal premium collapses entirely to a statistically insignificant -0.446 SD. The initial positive OLS correlation is revealed to be an illusion driven by endogenous spatial sorting. 

### 3. The Absorptive Constraint
In the West, mature industrial networks efficiently internalize spillovers (IV: +0.172 SD). Conversely, in the East and South, positive OLS correlations evaporate under 2SLS, demonstrating that tech workers merely chased existing growth. Without deep institutional scaffolding, peripheral growth relies almost entirely on physical capital deepening (IV: +0.671 SD in CEE).

## Policy Implications
High-technology employment is a powerful engine for regional productivity, but its efficacy is strictly conditional. Blindly injecting innovation and tech subsidies into peripheral regions lacking institutional maturity represents a severe policy misallocation. European Cohesion Policy must prioritize basic structural convergence (Solow physical capital and institutional quality) before attempting to subsidize advanced innovation clusters.

## Repository Structure
* `/src/`: Contains the modular R scripts for data cleaning, splicing, instrument construction, and `fixest` model estimation.
* `/data/raw/`: Original Eurostat SDMX CSVs (Productivity, GFCF, Education, NACE Rev 1.1/2.0).
* `/outputs/tables/`: Publication-ready regression tables.

## How to Run
1. Clone this repository.
2. Open `thesis.Rproj` in RStudio.
3. Run the master script to execute the pipeline from start to finish:
   ```R
   source("run_all.R")
