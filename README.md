# UAV-Based Monitoring of Cereal–Medicinal and Aromatic Plant Strip-Intercropping Systems Using Machine Learning and Explainable AI

## Overview

This repository contains the R scripts used in the development and evaluation of a UAV-based remote sensing (RS), machine learning (ML), and explainable artificial intelligence (XAI) framework for monitoring cereal–medicinal and aromatic plant (C-MAP) strip-intercropping systems.

The study was conducted at four field sites in Hesse, Germany, in 2024. Spring wheat was grown in strip-intercropping systems with caraway or coriander. UAV-based multispectral imagery and field measurements were collected at three growth stages to estimate dry biomass (DB) and leaf area index (LAI).

Spectral and structural variables derived from UAV data were used to develop and evaluate three regression models:

- Random Forest Regression (RFR)
- Support Vector Regression (SVR)
- Partial Least Squares Regression (PLSR)

Explainable AI techniques were applied to investigate the contribution of spectral and structural variables to model predictions. Predicted dry biomass was also used to generate spatial maps for assessing intercropping performance and supporting preliminary land equivalent ratio (LER) analysis.

## Repository Structure

```text
uav-cmap-intercropping-ml-xai/
│
├── 01_data_preprocessing/
│   └── Scripts for preparing input data
│
├── 02_feature_extraction/
│   └── Scripts for spectral and structural feature extraction
│
├── 03_model_development/
│   └── Scripts for cereal and medicinal and aromatic plant data analysis
│
├── 04_model_evaluation/
│   └── Scripts for model validation and performance comparison
│
├── 05_xai_analysis/
│   └── Scripts for explainable AI and feature importance analysis
│
├── 06_spatial_mapping/
│   └── Scripts for biomass mapping and spatial analysis
│
└── README.md
```

*Note: The folder structure is illustrative. Folder contents should be adjusted to match the scripts included in the repository.*

## Data Analysis Workflow

The analysis was performed separately for the two crop groups:

- **Cereal (C):** Spring wheat.
- **Medicinal and aromatic plants (MAP):** Caraway and coriander.

Separate analyses were conducted to account for differences between the cereal and medicinal and aromatic plant components of the strip-intercropping systems.

The general workflow comprises:

1. Preparation and organization of UAV-derived variables and field measurements.
2. Extraction and processing of spectral and structural features.
3. Development of RFR, SVR, and PLSR models for DB and LAI estimation.
4. Evaluation and comparison of model performance.
5. Application of XAI techniques to interpret model predictions and identify important variables.
6. Generation of spatial dry biomass maps and preliminary LER analysis.

## Data Availability

The datasets used in this study are not publicly available in this repository. The data supporting the findings of this study are available from the corresponding author upon reasonable request, subject to applicable data-sharing conditions.

## Software and Dependencies

The analyses were performed in R. The required packages depend on the individual scripts and may include packages for data processing, spatial analysis, machine learning, model interpretation, and visualization.

Users should consult the relevant scripts for package dependencies and configuration details.

## Reproducibility

The scripts are provided to document the analytical workflow and support reproducibility of the methods described in the associated scientific publication.

Reproducing the complete analysis requires access to the underlying datasets and any additional input files referenced by the scripts. Data availability is described in the Data Availability section above.

## Citation

If you use these scripts in your research, please cite the associated publication once the manuscript has been published.

**Manuscript:** *[Insert the final published article title]*

**Authors:** [Insert author names]

**Journal:** [Insert journal name]

**DOI:** [Insert DOI when available]

## Contact

For questions regarding the scripts, methodology, or data availability, please contact the corresponding author of the associated publication.