# Spatial Analysis of Rent Burden and Demographic Change on Oʻahu

Cole Yanagihara  
University of Illinois Urbana-Champaign  
UP 317: Introduction to Urban Data Science  
Professor Irene Farah  
May 2026

## Overview

This project examines spatial patterns of housing affordability pressure and demographic change across Oʻahu, Hawaiʻi from 2010–2024.

Using American Community Survey (ACS) data, spatial analysis, and Local Indicators of Spatial Association (LISA), this project investigates whether patterns of rent burden and Hawaiʻi-born population concentration align with expected indicators of gentrification.

Due to Hawaiʻi's unique housing market, constrained geography, and demographic context, this analysis explores whether traditional gentrification frameworks developed in mainland U.S. cities accurately describe neighborhood change on Oʻahu.

## Methods

- ACS 5-Year Estimates (2010–2024)
- Census tract-level spatial analysis
- Geographic data processing using `sf`
- Spatial autocorrelation analysis using `spdep`
- LISA cluster analysis
- Data visualization using `tmap` and `ggplot2`
- Final map refinement and graphic design using Adobe Illustrator

## Key Findings

- Rent burden showed statistically significant but relatively weak spatial clustering.
- Hawaiʻi-born population patterns showed much stronger and more persistent spatial clustering.
- Overall, the analysis found limited evidence of classic gentrification patterns on Oʻahu.

## Setup

This project uses the U.S. Census Bureau API through the `tidycensus` R package.

### 1. Obtain a Census API Key

Request a free API key:

https://api.census.gov/data/key_signup.html

### 2. Install Your API Key Locally

Run once in R:

```r
library(tidycensus)

census_api_key(
  "YOUR_KEY",
  install = TRUE
)
```

This stores your API key locally in your `.Renviron` file.

The API key is not included in this repository. After restarting R, `tidycensus` will automatically retrieve the key when running the analysis scripts.

## Future Improvements

Potential extensions include incorporating additional housing datasets and predictive modeling approaches to better estimate neighborhood housing pressure beyond ACS median rent measurements.
