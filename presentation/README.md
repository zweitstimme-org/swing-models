# DVPW AK Wahlen Presentation

## Overview
This LaTeX beamer presentation is prepared for the DVPW AK Wahlen conference on September 18, 2025. It presents the key findings from the paper "How Swing Model Assumptions Shape Vote-to-Seat Predictions: Evidence from Recent German Elections."

## Key Changes Highlighted
The presentation emphasizes several important updates to the research:

1. **New Simulation Parameter**: The `uniform_share` parameter (0-1) replaces the previous `swing_concentration` parameter
2. **Updated Methodology**: Models now use single-coefficient swing variables instead of interaction terms
3. **Enhanced Simulation**: 5 simulations per scenario (instead of 2) for more robust results
4. **Focus on Small Parties**: Simulation specifically designed to test proportional swing performance with clustered small parties

## Files
- `dvpw_presentation.tex` - Main presentation file
- `README.md` - This file

## Compilation
To compile the presentation, run:

```bash
pdflatex dvpw_presentation.tex
```

## Dependencies
The presentation requires:
- LaTeX with beamer class
- Standard packages (amsmath, graphicx, etc.)
- Access to the figures directory (`../figures/`)

## Structure
1. **Introduction** - Research question and motivation
2. **Empirical Analysis** - German election results and model performance
3. **Simulation Study** - Key innovations and findings
4. **Discussion** - Theoretical contributions and practical implications
5. **Conclusion** - Summary and future research directions

## Key Figures
The presentation includes all major figures from the paper:
- Model accuracy comparisons
- Geographical distribution of mispredictions
- Simulation parameter grids and heatmaps
- Regression coefficient plots

## Contact
For questions about the presentation or research:
- Cornelius Erfort: cornelius.erfort@uni-wh.de
- University Witten/Herdecke
