 This repository contains data and code for analyses and figures related to:

**Early life adversity shapes the relationship between growth and reproduction in free-ranging female rhesus macaques**
Rachel M Petersen , Sam K Patterson , Anja Widdig , Cassandra M Turcotte , Susan C. Antón , Scott A Williams , Ashly N Romero , Samuel E Bauman Surratt , Angelina V. Ruiz-Lambides , Cayo Biobank Research Unit , Michael J Montague , Noah Snyder-Mackler , Lauren J.N. Brent , James P. Higham , Amanda J. Lea. Proceedings of the National Academy of Sciences. 2026

The analyses assess effects of early life adversity (ELA) on patterns of growth, reproduction, and fitness using long-term data from the rhesus macaques of Cayo Santiago.

---

## Requirements

- **R version:** 4.5.1
- Package versions are recorded using renv (see renv.lock)

---

## Setting Up the Environment

Place all of the provided files in the **same working directory**. No additional folder structure is required. 

Start a clean R session and restore the package environment by running the following:

```r
install.packages("renv")
renv::restore()
```

---

## Analysis pipeline

1. `ELA_LifeHistory.R`: Runs models testing the effects of six individual sources of ELA, as well as cumulative ELA, on early-life growth, age at first birth, adult body size, and reproductive rate. Creates Figure 2 plots.

   **Data requirements:**
   - `Reproduction_ELA_data.rds`: reproduction and associated ELA data
   - `Morph_ELA_data.rds`: body size and associated ELA data

2. `Repro_growth_tradeoffs.R`: Runs models testing for trade-offs between growth and reproduction, and whether these trade-offs are modified by ELA.

   **Data requirements:**
   - `Subadult_repro_growth.rds`: paired age at first birth and sub-adult body weight data
   - `Adult_repro_growth.rds`: paired reproductive rate and adult body weight data

3. `Lifehistory_fitness.R`: Runs models testing the effects of life history variation on lifetime reproductive success.

   **Data requirements:**
   - `Reproduction_ELA_data.rds`: reproduction and associated ELA data
   - `Subadult_residualsize.rds`: subadult residual body sizes (weight corrected for age)
   - `Adult_residualsize.rds`: adult residual body sizes
   - `natural_death_moms.rds`: list of females to be included in fitness tests

---

## Data Access

The associated data are housed here (on GitHub) and on Zenodo (https://doi.org/10.5281/zenodo.17095401)
