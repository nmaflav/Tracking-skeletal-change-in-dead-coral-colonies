# Tracking skeletal change in dead coral colonies

Code and data for the manuscript: **"Tracking skeletal change in dead coral
colonies: A non-invasive 3D photogrammetry workflow and case-study
application"** (under review).

## Overview

The breakdown of coral structures by bioeroding organisms is a key reef
process. This repository supports a workflow that quantifies structural change
from surficial bioerosion and overgrowth in coral skeletons using 3D
reconstructions from large-area photogrammetry.

The workflow:

1. Identifies recently dead colonies in large-area photogrammetry surveys
2. Extracts the corresponding image subsets
3. Reconstructs sub-millimetre-resolution 3D models of individual skeletons
4. Aligns models from consecutive surveys and compares them in Geomagic
   (3D compare tool) to estimate erosion and deposition

**Precision assessment:** 10 randomly selected skeletons were each
reconstructed 10 independent times, and variation in volume and surface area
was summarised as relative standard error (RSE) by morphotaxon.

**Case study:** 60 skeletons from the southern Great Barrier Reef, tracked
between May 2022 and May 2023.

## Repository structure

```
├── analysis/
│   ├── 01_precision_assessment.R
│   ├── 02_data_clean_exploration.R
│   └── 03_data_analysis.R
├── data/
│   ├── metadata.csv               # colony IDs, morphotaxon, survey info, 
│   ├── data.csv                   # change outputs per skeleton
│   └── precision.csv              # outputs from repeated reconstructions
├── outputs/
│   ├── 3D compare heatmaps               
│   ├── diagrams                  
│   └── plots
└── README.md
```

## Data availability

The files in `data/` are the erosion and deposition outputs, colony metadata,
and precision-assessment outputs. The raw photogrammetry imagery and the 3D
models of the 60 skeletons are held in an institutional repository (name
withheld during peer review) and are available on reasonable request.

## Requirements

- R (version 4.4.2) and the packages listed in `scripts/` 
- Geomagic was used for model comparison; its outputs are provided in `outputs/3D compare heatmaps`.

## How to reproduce the analysis

1. Clone this repository and open the `.Rproj` file in RStudio
2. Install the required packages
3. Run the scripts in `scripts/` in numerical order
