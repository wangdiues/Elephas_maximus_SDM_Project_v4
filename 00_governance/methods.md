# Methods — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
ODMAP protocol: v2.0 (Zurell et al. 2020)
Study area: Bhutan (national)

---

## O — Overview

| Item | Value |
|------|-------|
| Taxon | *Elephas maximus* (Asian elephant) |
| Study area | Bhutan (national) |
| Modelling objective | Potential habitat suitability mapping; conservation planning |
| SDM algorithms | GLM, Random Forest, BRT (GBM), MaxEnt (maxnet) |
| Ensemble method | Weighted mean by AUC; algorithms below quality gates excluded |
| Projection period | Present (1986–2015) + future (2041–2060, 2061–2080, 2081–2100) |
| Future scenarios | CMIP6 SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5 × 8 GCMs |

---

## D — Data

### Occurrence data
- Source: station-level presence-absence survey data (`station_survey`, `book1_survey`)
- Raw records: 1,089 total (252 presence, 837 absence)
- Coordinate system: decimal degrees (WGS 84)
- Thinning: exact duplicate removal; spatial thinning to ≥ 0.1 km grid

### Background / pseudo-absence data
- Method: random sampling within accessible area (M = Bhutan national boundary)
- N background: 10,000
- Bias correction: optional kernel density bias layer if configured

### Environmental predictors
- Baseline climate: 19 CHELSA bioclimatic variables (BIO1–BIO19), 1986–2015, 30 arcsec
- DEM: Bhutan 12.5 m NASADEM
- Vegetation: NDVI, EVI (Bhutan 2023, 10 m, Sentinel-2)
- Land cover: ESA landcover (primary), lulc2020.shp (auxiliary vector)
- Human pressure: Human Influence Index (HII) Bhutan 2020, 1 km
- Water: major rivers, streams, watershed water sources (vector distance layers)
- Protected areas: Bhutan PA boundary 2023 (overlay only)

### Collinearity filtering
- Pearson |r| ≤ 0.7 (pairwise)
- VIF ≤ 5 (R²-based, standalone implementation)
- Maximum predictors: 25

---

## M — Model

### Accessible area (M)
- Defined as: Bhutan national boundary (bhutan.shp)
- Rationale: conservative operational boundary reflecting known elephant range country

### Algorithms
| Algorithm | Key settings |
|-----------|-------------|
| GLM | Binomial family, logit link, stepwise AIC selection |
| Random Forest | ranger; 500 trees, mtry = floor(sqrt(p)), importance = permutation |
| BRT | gbm; interaction.depth = 3, learning.rate = 0.01, bag.fraction = 0.75, CV = 5-fold |
| MaxEnt | maxnet; feature classes = lqp, regularization multiplier optimised by cross-validation |

### Cross-validation
- Method: spatial block CV (blockCV)
- Folds: 5
- Block size: determined by spatial autocorrelation range

### Ensemble
- Method: weighted mean (AUC weights)
- Inclusion: algorithm must pass all FATAL quality gates (AUC ≥ 0.65, calibration slope 0.5–2.0)
- Minimum members: 2

---

## A — Assessment

### Discrimination
- AUC (ROC)
- True Skill Statistic (TSS)

### Calibration
- Calibration slope (logit-scale regression)
- Brier score

### Habitat association
- Boyce continuous index (adaptive binning for n < 50 presences)

### Spatial independence
- Moran's I on model residuals

### Novel environment
- MESS (Multivariate Environmental Similarity Surface)

---

## P — Prediction

- Present-day suitability: continuous [0, 1] and binary (optimal threshold by TSS)
- Future suitability: per GCM × SSP × time period × algorithm; ensemble across GCMs
- Change metrics: habitat area change, range shift, stability
- Uncertainty: SD across GCMs per scenario; ensemble uncertainty

---

## References

- Allouche O, Tsoar A, Kadmon R (2006) Assessing the accuracy of species distribution models: prevalence, kappa and the true skill statistic (TSS). *Journal of Applied Ecology* 43:1223–1232.
- Fielding AH, Bell JF (1997) A review of methods for the assessment of prediction errors in conservation presence/absence models. *Environmental Conservation* 24:38–49.
- Zurell D et al. (2020) A standard protocol for reporting species distribution models. *Ecography* 43:1261–1277.
