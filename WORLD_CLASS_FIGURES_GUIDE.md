# World-Class Future Projection Figures — Elephas maximus SDM, Bhutan

## Overview

This document describes the **world-class publication figures** generated for the Asian Elephant Species Distribution Model (SDM) project under CMIP6 climate change scenarios.

---

## Figure Suite Summary

### **Total Figures Generated: 30**

| Category | Count | Description |
|----------|-------|-------------|
| **World-Class Figures** | 6 | Enhanced multi-panel publication figures (fig01-06) |
| **Publication Figures (A-F)** | 6 | Per-GCM and comparison matrices (figpub_A-F) |
| **Standard Pipeline Figures** | 10 | Default pipeline outputs (figure_01-10) |
| **Data Tables** | 1 | CSV with all scenario statistics |
| **Total File Size** | ~5 MB | All figures at 300 DPI |

---

## World-Class Figures (fig01-06)

### **Figure 1: SSP × Period Ensemble Matrix**
**File:** `fig01_ssp_period_ensemble.png` (20×16 in, 300 DPI)

**Layout:** 3 rows × 4 columns grid
- **Rows:** Time periods (2021-2050, 2051-2080, 2071-2100)
- **Columns:** SSP scenarios (SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5)
- **Content:** Ensemble mean across all 8 GCMs and 3 algorithms (GLM, RF, BRT)

**Key Features:**
- Publication-quality suitability color scale (white → yellow → orange → dark red)
- Bhutan boundary overlay
- Consistent color scale across all panels
- IPCC AR6 scenario colors

**Use Case:** Main overview figure for manuscripts, showing the full range of future projections

---

### **Figure 2: GCM Comparison Matrix (2071-2100)**
**File:** `fig02_gcm_comparison_2071_2100.png` (22×20 in, 300 DPI)

**Layout:** 4 rows × 8 columns grid
- **Rows:** SSP scenarios (SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5)
- **Columns:** 8 GCMs (ACCESS-CM2, CNRM-CM6-1, CNRM-ESM2-1, INM-CM4-8, INM-CM5-0, MIROC6, MIROC-ES2L, MPI-ESM1-2-LR)
- **Content:** End-of-century projections (2071-2100)

**Key Features:**
- Direct GCM comparison for uncertainty visualization
- Each GCM colored distinctly
- Shows climate model uncertainty clearly

**Use Case:** Supplementary figure for discussing GCM uncertainty, model agreement

---

### **Figure 3: Habitat Change Matrix**
**File:** `fig03_habitat_change_matrix.png` (20×16 in, 300 DPI)

**Layout:** 3 rows × 4 columns grid
- **Rows:** Time periods
- **Columns:** SSP scenarios
- **Content:** Δ Suitability (Future - Present)

**Color Scheme:**
- **Blue:** Suitability loss (negative change)
- **White:** No change
- **Red:** Suitability gain (positive change)

**Key Features:**
- Diverging color palette (RdBu)
- Symmetric scale around zero
- Shows habitat loss/gain patterns clearly

**Use Case:** Primary change detection figure, conservation impact assessment

---

### **Figure 4: Habitat Area Time-Series**
**File:** `fig04_habitat_area_timeseries.png` (18×20 in, 300 DPI)

**Two-panel layout:**

**Panel A (Top): Trajectory Plot**
- X-axis: Time periods (Present, 2021-2050, 2051-2080, 2071-2100)
- Y-axis: Suitable habitat (% of Bhutan)
- **3 facets:** GLM, Random Forest, BRT
- **Lines:** Individual GCM trajectories
- **Colors:** SSP scenarios
- **Points:** GCM markers (different shapes)

**Panel B (Bottom): Distribution Plot**
- **Violin + boxplot** showing distribution across all GCMs and algorithms
- **Facets:** Time periods
- **X-axis:** SSP scenarios
- **Jittered points:** Individual scenario values

**Key Features:**
- Present-day baseline (dashed line)
- Quantitative summary of all 96 scenarios
- Algorithm comparison
- Statistical distribution visualization

**Use Case:** Key quantitative figure for results section, policy briefs

**Data File:** `suitable_area_all_scenarios.csv`

---

### **Figure 5: Algorithm Comparison (SSP3-7.0, 2071-2100)**
**File:** `fig05_algorithm_comparison_ssp370.png` (20×16 in, 300 DPI)

**Layout:** 8 rows × 4 columns grid
- **Rows:** 8 GCMs
- **Columns:** 4 algorithms (GLM, Random Forest, BRT, MaxEnt)
- **Scenario:** SSP3-7.0 (high emissions), 2071-2100

**Key Features:**
- Direct algorithm comparison
- Shows model structure uncertainty
- Consistent color scale

**Use Case:** Methods validation, algorithm performance discussion

---

### **Figure 6: Model Validation Panel**
**File:** `fig06_model_validation.png` (16×18 in, 300 DPI)

**Two-panel layout:**

**Panel A (Top): ROC Curves**
- **X-axis:** False Positive Rate (1 - Specificity)
- **Y-axis:** True Positive Rate (Sensitivity)
- **4 curves:** GLM, RF, BRT, MaxEnt
- **Annotations:** AUC values per algorithm
- **Reference line:** Random classification (AUC = 0.5)

**Panel B (Bottom): Metrics Heatmap**
- **6 metrics:** AUC, TSS, Boyce Index, Brier Score, Calibration Slope, Moran's I
- **X-axis:** Algorithms
- **Color:** Performance value (red → white → blue)
- **Text:** Exact values

**Key Features:**
- 5-fold spatial cross-validation results
- Quality gate thresholds visible
- Comprehensive performance overview

**Use Case:** Methods validation, quality assurance, supplementary materials

---

## Publication Figures (figpub_A-F)

These are additional high-quality figures generated by `pub_figures.R`:

| Figure | Description | Size |
|--------|-------------|------|
| `figpub_A_*_ssp_period.png` | Per-GCM SSP×Period matrix (8 files, one per GCM) | 18×14 in |
| `figpub_B_gcm_comparison.png` | 3-GCM comparison across SSPs | 16×20 in |
| `figpub_C_habitat_change.png` | Alternative change map visualization | 18×14 in |
| `figpub_D_area_summary.png` | Alternative area summary charts | 15×16 in |
| `figpub_E_algorithm_gcm_comparison.png` | Algorithm×GCM comparison | 18×14 in |
| `figpub_F_model_validation.png` | Alternative validation panel | 14×14 in |

---

## Standard Pipeline Figures (figure_01-10)

Default figures from the pipeline's `10_figures.R`:

| Figure | Description |
|--------|-------------|
| `figure_01_present_suitability.png` | Present-day habitat suitability |
| `figure_02_model_auc.png` / `figure_model_auc.png` | AUC bar chart |
| `figure_03_future.png` to `figure_06_future.png` | Individual future scenario maps |
| `figure_09_response.png` | Predictor response curves |
| `figure_10_comparison.png` | Multi-metric comparison |
| `figure_roc_curves.png` | ROC curves |

---

## Color Palettes

### **Suitability Maps**
```
White (#ffffff)     → 0.00
Light yellow        → 0.05
Yellow              → 0.15
Light orange        → 0.25
Orange              → 0.40
Dark orange         → 0.60
Red-orange          → 0.80
Dark red (#993404)  → 1.00
```

### **SSP Scenarios (IPCC AR6 Style)**
- **SSP1-2.6:** Green (#008000)
- **SSP2-4.5:** Gold (#ffd700)
- **SSP3-7.0:** Tomato (#ff6347)
- **SSP5-8.5:** Dark red (#8b0000)

### **Change Maps (Diverging RdBu)**
- **Dark blue (#053061):** Strong loss
- **Light blue:** Moderate loss
- **White (#f7f7f7):** No change
- **Light red:** Moderate gain
- **Dark red (#91000f):** Strong gain

---

## Technical Specifications

### **Resolution & Format**
- **DPI:** 300 (publication-ready)
- **Format:** PNG (lossless compression)
- **Color Mode:** RGB
- **Background:** Transparent/white where applicable

### **Font & Typography**
- **Base Font:** Sans-serif (Helvetica/Arial)
- **Title Size:** 16 pt (bold)
- **Subtitle Size:** 12 pt
- **Axis Text:** 9-11 pt
- **Caption:** 9 pt

### **Figure Dimensions**
- **Large multi-panel:** 18-22 in width, 14-20 in height
- **Single panel:** 10-12 in width, 8-10 in height
- **Aspect Ratio:** Optimized for journal columns (single/double)

---

## Usage Recommendations

### **For Manuscripts**

**Main Figures:**
1. **Figure 1** (SSP×Period ensemble) - Overview of projections
2. **Figure 3** (Change maps) - Habitat loss/gain patterns
3. **Figure 4** (Time-series) - Quantitative trends
4. **Figure 6** (Validation) - Model performance

**Supplementary Figures:**
- Figure 2 (GCM comparison) - Uncertainty discussion
- Figure 5 (Algorithm comparison) - Methods validation
- figpub_A_* series - Per-GCM details

### **For Conservation Reports**
- Figure 1 (simplified caption)
- Figure 3 (change maps)
- Figure 4 Panel A (trajectories)
- Present suitability map (figure_01)

### **For Presentations**
- Extract individual panels from multi-panel figures
- Use Figure 4 for quantitative slides
- Use Figure 6 for methods validation

---

## Reproducibility

**Script:** `03_analysis/world_class_figures.R`

**To regenerate:**
```powershell
powershell.exe -Command "& 'C:\Program Files\R\R-4.4.0\bin\Rscript.exe' 'E:\Elephas_maximus_SDM_Project_v4\03_analysis\world_class_figures.R'"
```

**Dependencies:**
- R ≥ 4.3
- terra, sf, ggplot2, patchwork, scales, viridis, cowplot

**Runtime:** ~5-10 minutes (depends on scenario count)

---

## Quality Assurance

### **Cartographic Elements**
✅ Scale bar (via ggspatial)
✅ North arrow
✅ CRS annotation (EPSG:32645)
✅ Country boundary overlay
✅ Consistent color scales
✅ Professional typography

### **Scientific Standards**
✅ IPCC AR6 scenario colors
✅ CMIP6 GCM naming conventions
✅ ODMAP-compliant documentation
✅ Reproducible workflows
✅ Deterministic execution (seeds)

---

## File Locations

```
04_outputs/runs/RUN_<timestamp>_<hash>/
└── 08_figures_tables/
    ├── fig01_ssp_period_ensemble.png          (World-class Fig 1)
    ├── fig02_gcm_comparison_2071_2100.png     (World-class Fig 2)
    ├── fig03_habitat_change_matrix.png        (World-class Fig 3)
    ├── fig04_habitat_area_timeseries.png      (World-class Fig 4)
    ├── fig05_algorithm_comparison_ssp370.png  (World-class Fig 5)
    ├── fig06_model_validation.png             (World-class Fig 6)
    ├── figpub_A_*.png                         (8 per-GCM figures)
    ├── figpub_B_gcm_comparison.png
    ├── figpub_C_habitat_change.png
    ├── figpub_D_area_summary.png
    ├── figpub_E_algorithm_gcm_comparison.png
    ├── figpub_F_model_validation.png
    ├── figure_01_present_suitability.png      (Standard pipeline)
    ├── figure_02_model_auc.png
    ├── figure_03_future.png
    ├── figure_04_future.png
    ├── figure_05_future.png
    ├── figure_06_future.png
    ├── figure_09_response.png
    ├── figure_10_comparison.png
    ├── figure_roc_curves.png
    └── suitable_area_all_scenarios.csv        (Data table)
```

---

## Citation

When using these figures, please cite:

```
Zurell, D., et al. (2020). A standard protocol for reporting species 
distribution models. Ecography, 43(9), 1261-1277.

O'Neill, B.C., et al. (2016). The Scenario Model Intercomparison 
Project (ScenarioMIP) for CMIP6. Geoscientific Model Development, 9, 
3461-3482.
```

---

## Contact & Support

For questions about these figures:
- Check `06_logs/pipeline.log` for generation details
- Review `00_governance/methods.md` for methodology
- See `FEATURES.md` for complete feature catalog

---

**Document Version:** 1.0
**Last Updated:** 2026-03-17
**Project:** Elephas maximus SDM Project v4
**Status:** Production Ready
