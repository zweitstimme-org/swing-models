# How Swing Model Assumptions Shape Vote-to-Seat Predictions

Repository for the paper **"How Swing Model Assumptions Shape Vote-to-Seat Predictions"**, comparing uniform and proportional swing models (and variants) for translating national voting trends into district-level outcomes.

## Repository structure

```
swing-models/
├── code/                    # R scripts (run from project root)
│   ├── 0_utils.R            # Utility functions (DLM, Wahlrecht XML, etc.)
│   ├── 1_dataset.R         # Build dataset: candidates 1983–2025, swing variables
│   ├── 2_evaluation.R       # Model evaluation (accuracy, MAE, RMSE)
│   ├── 3_maps.R            # District maps and geographic visualizations
│   ├── 4_closest_races_comparison.R
│   ├── 6_swing_simulation.R # Simulation: uniform vs proportional swing
│   ├── 7_election_volatility_analysis.R
│   ├── 8_party_volatility_analysis.R
├── data/                    # Input data and derived outputs
│   ├── btw_candidates_1983-2025.RData   # Produced by 1_dataset.R
│   ├── clea/                # CLEA data
│   ├── district-shapefiles/ # Geographic boundaries (Bundestag elections)
│   ├── out/                 # Evaluation and other outputs (CSV, RDS)
│   └── ...
├── figures/                 # Plots (PDF/PNG)
├── tables/                  # LaTeX tables for the paper
└── README.md
```

## Requirements

- **R** (tested with R 4.x)
- R packages (install as needed):
  - Core: `tidyverse`, `lubridate`, `dlm`, `xml2`, `xtable`, `ggplot2`, `scales`, `gt`
  - Maps: `sf`, `patchwork`
  - Simulation: `MASS`, `patchwork`, `future`, `furrr`, `progressr`, `broom`, `stargazer`

## Reproduction workflow

Scripts are intended to be run from the **project root** (directory containing `swing-models.Rproj`). Paths in the code assume that working directory.

1. **Data**: Run `1_dataset.R` to build the candidate-level dataset and save `btw_candidates_1983-2025.RData` (and related outputs). This script expects input files in `data/` (e.g. `btw_candidates_1983-2025_full.csv`, 2025 results, shapefiles as used in the repo).
2. **Evaluation**: Run `2_evaluation.R` to compare swing models and write results to `data/out/` and figures to `figures/`.
3. **Maps**: Run `3_maps.R` for district maps (uses `data/district-shapefiles/`).
4. **Closest races**: Run `4_closest_races_comparison.R`.
5. **Simulation**: Run `6_swing_simulation.R` for the uniform vs proportional swing simulation (figures and regression outputs).
6. **Volatility**: Run `7_election_volatility_analysis.R` and `8_party_volatility_analysis.R` for volatility analyses.

## Data

- **Candidate/results data**: German federal election candidate and district-level results (1983–2025), including Zweitstimme (party list) and Erststimme (constituency) vote shares.
- **Shapefiles**: District boundaries for Bundestag elections (sources indicated in `data/district-shapefiles/`).
- **CLEA**: Comparative study data in `data/clea/`.

External data (e.g. Wahlrecht.de XML for polls) may be loaded by `0_utils.R` and related scripts; see script headers and comments for URLs and usage.

## Outputs

- **Figures** (`figures/`): Coefficient plots, first-difference plots, performance heatmaps, maps of mispredicted districts, etc.
- **Tables** (`tables/`): LaTeX files for the paper (e.g. evaluation aggregate, regression, volatility).

## Funding

This research was funded by the Deutsche Forschungsgemeinschaft (DFG, German Research Foundation) — Project No. 529544178.

## License

See repository license file (if present). For data and shapefiles, respect the terms of the original data providers.