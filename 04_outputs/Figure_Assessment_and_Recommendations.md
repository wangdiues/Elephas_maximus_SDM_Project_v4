# Figure Quality Assessment & Recommendations
## Elephas maximus SDM Project — Bhutan

**Run ID:** `RUN_20260317_203608_b990`  
**Assessment Date:** 2026-03-18  
**Overall Rating:** ⭐⭐⭐⭐ (4/5) — Very Good, Near Publication-Ready

---

## Executive Summary

Your current figure suite comprises **9 publication-quality figures** covering present-day suitability, future CMIP6 projections, model validation, and GCM uncertainty assessment. The figures are **well-designed and meet most publication standards**, with refinements needed to reach **Nature/Science-level quality**.

### Key Strengths
- Multi-panel layouts are clean and systematic
- Color scales are generally appropriate (sequential for suitability, diverging for change)
- Model validation figure (Fig7) is exceptional
- GCM reliability ranking (Fig9) is innovative and valuable
- Uncertainty visualization (Fig6) follows best practices

### Critical Issues to Address
1. **Fig5 delta change maps**: Inconsistent color scales across panels (makes comparison impossible)
2. **MaxEnt metrics**: TSS, Boyce, Brier showing NA in Fig7
3. **Fig8 trajectories**: Missing SSP1-2.6 and SSP3-7.0 scenarios
4. **All figures**: Add quantitative annotations (% area suitable per panel)

---

## Individual Figure Reviews

### Fig1: Present-Day Habitat Suitability ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig1_present_suitability.png`

**Purpose:** Present-day habitat suitability with ensemble mean + 4 individual algorithms

**Strengths:**
- Clean multi-panel layout (ensemble + 4 algorithms)
- Appropriate color scale (sequential orange-brown)
- Clear legend with suitability values (0.0–0.5)
- North arrow and scale bar present
- Good resolution and clarity
- 250 m resolution noted in subtitle

**Issues:**
- Figure appears truncated (bottom panels may be cut off in some views)
- No occurrence points overlaid for ground-truthing
- Missing inset map showing Bhutan's regional context (location in Asia)
- No elevation hillshade underlay for topographic context

**Recommendations:**
- [ ] Add presence/absence points to panel (a) ensemble map
- [ ] Add small inset map: Bhutan location within Asia/Himalayas
- [ ] Ensure all 5 panels are fully visible in final export
- [ ] Add subtle elevation hillshade as underlay for topographic context
- [ ] Consider adding percentage of Bhutan classified as suitable (>0.5)

**Publication Status:** ✅ **Ready for main manuscript** (with minor fixes)

---

### Fig2: SSP2-4.5 Projections (8 GCMs × 3 Periods) ⭐⭐⭐⭐ (4/5)

**File:** `Fig2_ssp245_8gcm_3period.png`

**Purpose:** Intermediate emissions scenario projections across all GCMs and time periods

**Strengths:**
- Excellent systematic layout (8 columns × 3 rows = 24 maps)
- Consistent color scale across all panels (capped at 0.5)
- Clear labeling of GCMs (columns) and time periods (rows)
- Shows full GCM ensemble spread visually
- CRS and scale information in footer

**Issues:**
- Individual maps are small (hard to see fine spatial detail)
- No quantitative summary per panel (e.g., "% suitable area")
- 24 similar-looking maps may overwhelm readers
- No indication of which GCMs are most reliable

**Recommendations:**
- [ ] Add small text annotation per panel: "% area suitable"
- [ ] Consider splitting into 3 separate figures (one per time period)
- [ ] Add summary bar chart below showing GCM mean ± SD per period
- [ ] Highlight top-3 ranked GCMs (INM-CM5-0, MIROC6, MPI-ESM1-2-LR) with colored borders
- [ ] Add ensemble mean map as rightmost column or bottom row

**Publication Status:** ✅ **Ready for supplementary materials**  
**Main manuscript:** Use condensed version (ensemble means only)

---

### Fig3: SSP5-8.5 Projections (8 GCMs × 3 Periods) ⭐⭐⭐⭐ (4/5)

**File:** `Fig3_ssp585_8gcm_3period.png`

**Purpose:** Very high emissions scenario projections

**Strengths:**
- Same strengths as Fig2
- Enables direct SSP comparison when viewed alongside Fig2
- Shows more extreme changes under high emissions

**Issues:**
- Same as Fig2 (redundancy concern)
- 9.9 MB file size (may be too large for some journal supplementary systems)
- Some journals limit supplementary figure sizes

**Recommendations:**
- [ ] Same as Fig2
- [ ] Consider combining Figs 2+3 into single comparative figure for main text
- [ ] Compress file size if needed (PNG optimization)
- [ ] Add direct difference map (SSP5-8.5 minus SSP2-4.5) as new panel

**Publication Status:** ✅ **Ready for supplementary materials**

---

### Fig4: SSP2-4.5 vs SSP5-8.5 Comparison (2071-2100) ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig4_ssp245_vs_ssp585_2071_2100.png`

**Purpose:** End-of-century scenario comparison across all 8 GCMs

**Strengths:**
- **Excellent design** — direct scenario comparison
- Clean 2-row × 8-column layout
- Easy to compare GCM responses across scenarios
- Appropriate for main manuscript figure
- Clear row labels (SSP2-4.5 Intermediate vs SSP5-8.5 Very High)
- Consistent color scale

**Issues:**
- Could add quantitative difference metrics per GCM
- No indication of which GCMs are "best" (from Fig9 ranking)
- No ensemble mean comparison

**Recommendations:**
- [ ] Add % change values in corner of each panel (future vs present)
- [ ] Highlight top-3 ranked GCMs with colored borders or asterisks
- [ ] Consider adding third row showing ensemble mean difference
- [ ] Add statistical comparison: mean suitability SSP5-8.5 vs SSP2-4.5 (paired t-test p-value)

**Publication Status:** ✅ **Ready for main manuscript** (highly recommended)

---

### Fig5: Delta Change Maps (2071-2100) ⭐⭐⭐⭐ (4/5)

**File:** `Fig5_delta_change_2071_2100.png`

**Purpose:** Change in suitability (future − present) for all GCMs and both SSPs

**Strengths:**
- Diverging color scale (blue-white-red) is appropriate for change data
- Shows gain/loss patterns clearly
- Reveals spatial heterogeneity in climate change impacts
- Blue = loss, Red = gain is intuitive

**Issues:**
- ⚠️ **CRITICAL: Multiple inconsistent color scales** (each row has different limits)
- Some legends show -0.2 to 0.2, others -0.3 to 0.3, others -0.1 to 0.1
- Makes cross-GCM comparison difficult or impossible
- Confusing for readers

**Recommendations:**
- [ ] **URGENT:** Use **single consistent color scale** across all 16 panels
- [ ] Recommended: -0.5 to +0.5 with symmetric breaks at -0.3, -0.1, 0, 0.1, 0.3
- [ ] Add histogram of delta values as marginal or inset
- [ ] Consider showing only ensemble mean change + uncertainty (simplify)
- [ ] Add quantitative summary: "% area with significant gain/loss" per scenario

**Publication Status:** ⚠️ **Needs revision before publication** (color scale issue)

---

### Fig6: GCM Uncertainty (2071-2100) ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig6_gcm_uncertainty.png`

**Purpose:** GCM ensemble agreement showing mean suitability and standard deviation

**Strengths:**
- **Excellent scientific design**
- Shows both ensemble mean AND uncertainty (SD) side-by-side
- Clear comparison: SSP2-4.5 vs SSP5-8.5
- Appropriate uncertainty visualization (purple = high SD = low agreement)
- Clear legends and labeling
- Scale bar and north arrow present

**Issues:**
- Could add third panel: coefficient of variation (CV = SD/mean)
- No quantitative summary of uncertainty levels
- Could indicate what SD threshold constitutes "high uncertainty"

**Recommendations:**
- [ ] Add text annotation: "Mean SD = X% of Bhutan shows high uncertainty (SD > 0.1)"
- [ ] Consider adding CV map as supplementary figure
- [ ] Add vertical lines or hatching to indicate areas of novel climate (MESS < 0)
- [ ] Consider adding algorithm uncertainty panel for comparison

**Publication Status:** ✅ **Ready for main manuscript**

---

### Fig7: Model Validation ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig7_model_validation.png`

**Purpose:** Comprehensive model evaluation (ROC curves + metrics heatmap + AUC comparison)

**Strengths:**
- **Outstanding multi-panel design**
- Panel (a): ROC curves with AUC values clearly shown
- Panel (b): Heatmap of 6 metrics across 4 algorithms (excellent)
- Panel (c): AUC dot plot with performance thresholds (Good 0.7, Excellent 0.9)
- Color coding (green=good, red=poor) is intuitive
- Spatial cross-validation (5 folds) noted in subtitle
- 150 unique GPS collar locations mentioned

**Issues:**
- ⚠️ MaxEnt row has NA values (TSS, Boyce, Brier, Calibration, Moran's I)
- Slightly crowded—could increase spacing between panels
- Threshold values only shown for 3 algorithms (not MaxEnt)

**Recommendations:**
- [ ] **Fix MaxEnt metrics**: Compute TSS, Boyce, Brier from presence/background predictions
- [ ] Increase panel spacing slightly for readability
- [ ] Add sample size to caption: n=252 presences, n=837 absences
- [ ] Consider adding confusion matrix table as inset
- [ ] Add optimal threshold selection method note (e.g., "maximizes TSS")

**Publication Status:** ✅ **Ready for main manuscript** (after MaxEnt metrics fixed)

---

### Fig8: Suitable Area Trajectories ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig8_suitable_area_trajectories.png`

**Purpose:** Time series of suitable habitat area by GCM and SSP scenario

**Strengths:**
- **Excellent design** — shows trajectories AND uncertainty
- Panel (a): Spaghetti plot (individual GCMs) for SSP2-4.5 and SSP5-8.5
- Panel (b): Box plot distribution for 2071-2100 comparison
- Present-day baseline shown as dashed line (7.0%)
- Clear legend with 8 GCMs and distinct colors/shapes
- Y-axis: "% of Bhutan" makes results interpretable

**Issues:**
- X-axis labels are crowded (2021-2050, 2051-2080, 2071-2100)
- Missing SSP1-2.6 (low emissions) and SSP3-7.0 (high emissions)
- No ensemble mean trajectory shown (only individual GCMs)
- No statistical comparison between scenarios

**Recommendations:**
- [ ] Add thick black line showing GCM ensemble mean ± SD envelope
- [ ] Add SSP1-2.6 and SSP3-7.0 panels (you have the data!)
- [ ] Add statistical test annotation: "SSP5-8.5 vs SSP2-4.5: p < 0.05"
- [ ] Consider adding present-day suitable area as shaded band (not just line)
- [ ] Add % change from present in panel titles or as text annotations

**Publication Status:** ✅ **Ready for main manuscript** (enhancement recommended)

---

### Fig9: GCM Reliability Ranking ⭐⭐⭐⭐⭐ (5/5)

**File:** `Fig9_gcm_reliability_ranking.png`

**Purpose:** GCM ranking based on climate novelty (MESS) and algorithm consensus

**Strengths:**
- **Innovative and highly valuable for interpretation**
- Three-panel design: (a) ranking, (b) novelty, (c) disagreement
- Clear identification of "best" GCMs (INM-CM5-0 #1, MIROC6 #2)
- Useful for weighting ensemble or selecting representative GCMs
- Color coding by GCM is consistent with other figures
- Composite score formula explained in subtitle

**Issues:**
- X-axis scale goes to 125% (wasted space, should end at 100%)
- Extrapolation % values all show "0.1%" (rounding issue — loses discrimination)
- Algorithm disagreement values shown but not explained in detail

**Recommendations:**
- [ ] Fix x-axis to end at 100% (or 0.35 for panel a)
- [ ] Show extrapolation with more decimals (0.11%, 0.12%, 0.14%, etc.)
- [ ] Consider adding fourth panel: "Recommended weight" for ensemble weighting
- [ ] Add caption explaining composite score formula in detail
- [ ] Consider showing all 4 SSPs separately or note which SSPs were averaged

**Publication Status:** ✅ **Ready for main manuscript** (minor fixes needed)

---

## Comprehensive Strategy: Representing 96 Future Scenarios

You have generated **96 scenarios**:
- **8 GCMs:** ACCESS-CM2, CNRM-CM6-1, CNRM-ESM2-1, INM-CM4-8, INM-CM5-0, MIROC6, MIROC-ES2L, MPI-ESM1-2-LR
- **4 SSPs:** SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5
- **3 Time periods:** 2021-2050, 2051-2080, 2071-2100
- **4 Algorithms:** GLM, RF, BRT, MaxEnt (ensembled)

### Multi-Scale Representation Framework

| Level | Representation | Figure Type | Data Volume | Purpose |
|-------|---------------|-------------|-------------|---------|
| **1. Overview** | All 96 scenarios | Small multiples grid | 96 maps | Show full scope of analysis |
| **2. Summary** | Ensemble statistics | Time series + boxplots | 24 lines | Show central tendency + spread |
| **3. Comparison** | Key contrasts | Paired maps | 16 maps | SSP2-4.5 vs SSP5-8.5 |
| **4. Change** | Delta from present | Diverging maps | 16 maps | Gain/loss patterns |
| **5. Uncertainty** | GCM + algorithm SD | Uncertainty maps | 4 maps | Show prediction confidence |
| **6. Reliability** | GCM ranking | Bar charts | 8 bars | Identify best GCMs |
| **7. Synthesis** | Integrated summary | Dashboard | 1 composite | Management brief |

---

## Additional Figures to Create

### HIGH PRIORITY (Add to Current Suite)

#### Fig10: All-SSP Comparison Grid
**Purpose:** Complete overview of all 4 SSPs across all GCMs and periods

**Layout Options:**
- **Option A:** 4 rows (SSPs) × 8 columns (GCMs), with 3 small insets for periods
- **Option B:** 3 separate figures (one per period) showing 4 SSPs × 8 GCMs
- **Option C:** 4 separate figures (one per SSP) showing 8 GCMs × 3 periods

**Data:** `future_gcm_ensemble_ssp*.tif` files

**Recommendation:** Use Option B for main text (3 figures), Option C for supplementary

---

#### Fig11: Habitat Change Trajectories (All 4 SSPs)
**Purpose:** Show temporal trajectories for all emission scenarios

**Current State:** Fig8 shows only SSP2-4.5 and SSP5-8.5

**Enhancement:**
- 4-panel facet: one per SSP (SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5)
- Show all 8 GCMs as thin lines
- Add thick black line: GCM ensemble mean ± SD envelope
- X-axis: 3 time periods (2021-2050, 2051-2080, 2071-2100)
- Y-axis: % of Bhutan with suitability ≥ 0.5
- Dashed line: present-day baseline (7.0%)

**Data:** Already generated in `04_future_projections/`

---

#### Fig12: Gain/Loss/Persistence Maps
**Purpose:** Show where habitat is gained, lost, or persists (not just magnitude)

**Design:**
- 3-class categorical map:
  - **Gain** (blue): Future suitable, present unsuitable
  - **Loss** (red): Present suitable, future unsuitable
  - **Persistence** (green): Suitable in both present and future
  - **Unsuitable** (grey): Unsuitable in both

**Layout:** 2 rows (SSP2-4.5, SSP5-8.5) × 1 column for 2071-2100

**Data:** Compute from `suitability_present_ensemble.tif` and `future_gcm_ensemble_*.tif`

---

#### Fig13: Protected Area Overlay Analysis
**Purpose:** Conservation application — how much suitable habitat is protected?

**Design:**
- Panel (a): Present suitability + PA boundaries
- Panel (b): Future suitability (2071-2100, SSP5-8.5) + PA boundaries
- Panel (c): Bar chart — % suitable habitat inside vs outside PAs (all scenarios)
- Panel (d): Change in protected suitable habitat over time

**Data:** 
- Suitability rasters
- `PA_Bnd_Final_20230316.shp` (protected areas)

**Conservation Message:** "Only X% of current suitable habitat is within protected areas. Under SSP5-8.5, this decreases to Y%."

---

#### Fig14: Response Curves (9 Predictors)
**Purpose:** Ecological interpretation — show species-environment relationships

**Design:**
- 9 panels (3×3 grid), one per retained predictor:
  - BIO05 (max temperature warmest month)
  - BIO14 (precipitation driest month)
  - BIO15 (precipitation seasonality)
  - BIO18 (precipitation warmest quarter)
  - human_footprint
  - evi
  - dynamicworld_prob
  - slope
  - tri

**Each panel:**
- X-axis: predictor value
- Y-axis: relative suitability (partial dependence)
- 4 lines (different colors): GLM, RF, BRT, MaxEnt
- Rug plot at bottom: observed presence locations

**Data:** `modeling_dataset.csv` + fitted models

---

#### Fig15: Variable Importance (4 Algorithms)
**Purpose:** Show which predictors matter most to each algorithm

**Design:**
- 4 panels (horizontal or 2×2), one per algorithm
- Horizontal bar chart: importance score (0-100) per predictor
- Color bars by predictor type:
  - Climate (BIO variables): orange
  - Anthropogenic (human_footprint): red
  - Vegetation (EVI, landcover): green
  - Topography (slope, TRI): brown
  - Distance: blue

**Data:** 
- GLM: standardized coefficients (absolute value)
- RF: permutation importance or Gini importance
- BRT: relative influence
- MaxEnt: jackknife test or percent contribution

---

### MEDIUM PRIORITY

#### Fig16: MESS/Novelty Maps (2071-2100)
**Purpose:** Show where future climates are novel (predictions less reliable)

**Design:**
- 2 rows (SSP2-4.5, SSP5-8.5) × 1 column
- Color scale: MESS statistic
  - MESS > 0: within training range (green)
  - MESS < 0: novel climate (red/orange)
- Text annotation: "% of Bhutan with novel climate"

**Data:** `mess_*.tif` files in `04_future_projections/`

---

#### Fig17: Algorithm Uncertainty
**Purpose:** Show model structural uncertainty (independent of GCM uncertainty)

**Design:**
- Panel (a): Present — SD among GLM, RF, BRT, MaxEnt
- Panel (b): Future 2071-2100 (SSP5-8.5) — SD among algorithms
- Color scale: standard deviation (0 = perfect agreement, 1 = complete disagreement)

**Data:** `suitability_present_*.tif` and `suitability_future_*_ssp585_*.tif`

---

#### Fig18: Combined Uncertainty
**Purpose:** Show total uncertainty (GCM + algorithm)

**Design:**
- Similar to Fig17, but combining both sources
- Can use variance addition: Total Variance = GCM Variance + Algorithm Variance

---

#### Fig19: Suitable Area by Threshold (Sensitivity Analysis)
**Purpose:** Show how area estimates change with different suitability thresholds

**Design:**
- X-axis: threshold (0.3, 0.4, 0.5, 0.6, 0.7)
- Y-axis: % of Bhutan classified as suitable
- Lines: different scenarios (present, SSP2-4.5 2071-2100, SSP5-8.5 2071-2100)
- Shows robustness of conclusions to threshold choice

---

#### Fig20: Centroid Shift Analysis
**Purpose:** Track directional movement of suitable habitat over time

**Design:**
- Map of Bhutan with elevation hillshade
- Points: centroid of suitable habitat for each time period
- Arrows: direction and distance of shift
- Color: by time period (present → 2021-2050 → 2051-2080 → 2071-2100)
- Inset table: distance moved (km), direction (degrees from north)

---

### LOWER PRIORITY (Supplementary Materials)

#### Fig21-28: Individual GCM Profiles (8 figures)
One figure per GCM showing:
- All 4 SSPs × 3 periods (12 maps)
- Time series of suitable area
- Change maps

**Use:** When discussing specific GCM characteristics or for supplementary data access

---

#### Fig29: Predictor Correlation Heatmap
**Purpose:** Show why 17 predictors were removed (collinearity)

**Design:**
- Correlation matrix heatmap (25 × 25 predictors)
- Highlight retained predictors with borders
- Dendrograms showing clustering

---

#### Fig30: Spatial CV Fold Map
**Purpose:** Show spatial blocking design for cross-validation

**Design:**
- Map of Bhutan with 5 colored regions (folds)
- 15 km block size noted
- Presence points overlaid, colored by fold assignment

---

## Recommended Figure Combinations for Manuscript

### Main Text (6 figures maximum — typical journal limit)

| Manuscript Fig # | Content | Current Fig # | Notes |
|-----------------|---------|---------------|-------|
| **Figure 1** | Present suitability + Model validation | Fig1 + Fig7 | Combine into single multi-panel figure |
| **Figure 2** | Future projections: SSP2-4.5 vs SSP5-8.5 | Fig4 | As-is, excellent |
| **Figure 3** | Change maps (delta) | Fig5 | Fix color scales first |
| **Figure 4** | Suitable area trajectories (all 4 SSPs) | Fig8 (enhanced) | Add SSP1-2.6, SSP3-7.0 |
| **Figure 5** | GCM uncertainty | Fig6 | As-is, excellent |
| **Figure 6** | GCM reliability ranking | Fig9 | Minor fixes |

### Supplementary Materials (8-12 figures)

| Supp Fig # | Content | Current Fig # |
|-----------|---------|---------------|
| **S1** | Full SSP2-4.5 grid (8 GCMs × 3 periods) | Fig2 |
| **S2** | Full SSP5-8.5 grid (8 GCMs × 3 periods) | Fig3 |
| **S3** | All-SSP comparison (4 SSPs × 8 GCMs) for 2071-2100 | New (Fig10) |
| **S4** | Variable importance (4 algorithms) | New (Fig15) |
| **S5** | Response curves (9 predictors) | New (Fig14) |
| **S6** | Protected area overlay analysis | New (Fig13) |
| **S7** | Gain/Loss/Persistence maps | New (Fig12) |
| **S8** | MESS/novelty maps | New (Fig16) |
| **S9** | Algorithm uncertainty | New (Fig17) |
| **S10** | Individual algorithm maps (GLM, RF, BRT, MaxEnt) | From Fig1 |

---

## Specific Improvements to Implement

### 1. Fix Color Scale Consistency (Fig5)

```r
# In your figure generation script:
common_limits <- c(-0.5, 0.5)
common_breaks <- c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5)

change_scale <- function(lim = 0.5, name = "Δ Suitability") {
  scale_fill_gradientn(
    colours = c("#053061","#2166ac","#67a9cf","#f7f7f7","#ef8a62","#ca0020","#91000f"),
    limits = common_limits,
    breaks = common_breaks,
    oob = squish,
    na.value = "grey85",
    name = name
  )
}
```

### 2. Add Quantitative Annotations to All Maps

Every map panel should show in a corner:
- **% area suitable** (suitability ≥ 0.5)
- **Mean suitability** value
- **Change from present** (%)

Example annotation code:
```r
annotate("text", x = Inf, y = Inf, 
         label = sprintf("Suitable: %.1f%%\nMean: %.2f", pct_suitable, mean_suit),
         hjust = 1.1, vjust = 1.5, size = 2.5, fontface = "bold")
```

### 3. Highlight Best GCMs

Add visual indicator (colored border, asterisk, or badge) to top-3 ranked GCMs in all multi-GCM figures:

**Top 3 GCMs:**
1. INM-CM5-0 (composite score: 0.343)
2. MIROC6 (composite score: 0.331)
3. MPI-ESM1-2-LR (composite score: 0.320)

### 4. Add Ensemble Mean Trajectories

In all time series plots (Fig8, Fig11), add:
- Thick black line: GCM ensemble mean
- Grey ribbon: ±1 SD envelope
- Text: "Ensemble mean ± SD"

### 5. Create Summary Dashboard Figure

Single figure for management brief combining:
- Present suitability map (top left)
- Future change map SSP5-8.5 2071-2100 (top right)
- Time series of suitable area all 4 SSPs (bottom left)
- GCM ranking bar chart (bottom right)
- Key statistics table (center or bottom)

**Key statistics to include:**
- Current suitable area: X km² (Y% of Bhutan)
- Projected change SSP2-4.5: ±X% (range: X to Y%)
- Projected change SSP5-8.5: ±X% (range: X to Y%)
- Most reliable GCM: INM-CM5-0
- Model performance: AUC = 0.92-0.96

---

## Action Items Checklist

### Critical (Before Submission)
- [ ] Fix Fig5 color scale consistency across all panels
- [ ] Compute MaxEnt TSS, Boyce, Brier metrics for Fig7
- [ ] Add all 4 SSPs to Fig8 trajectories
- [ ] Add quantitative annotations (% suitable) to all map panels

### High Priority
- [ ] Create variable importance figure (Fig15)
- [ ] Create response curves figure (Fig14)
- [ ] Create protected area overlay analysis (Fig13)
- [ ] Create gain/loss/persistence maps (Fig12)
- [ ] Highlight top-3 GCMs in all multi-GCM figures

### Medium Priority
- [ ] Create MESS/novelty maps (Fig16)
- [ ] Create algorithm uncertainty maps (Fig17)
- [ ] Add ensemble mean lines to trajectory plots
- [ ] Create summary dashboard figure

### Low Priority (Supplementary)
- [ ] Create individual GCM profile figures (Fig21-28)
- [ ] Create predictor correlation heatmap (Fig29)
- [ ] Create spatial CV fold map (Fig30)
- [ ] Optimize file sizes for supplementary materials

---

## File Organization

All figures should be saved to:
```
E:/Elephas_maximus_SDM_Project_v4/04_outputs/runs/RUN_20260317_203608_b990/08_figures_tables/
```

**Naming convention:**
- Main text: `Fig01_*.png`, `Fig02_*.png`, etc.
- Supplementary: `FigS01_*.png`, `FigS02_*.png`, etc.
- Dashboard: `Dashboard_management_brief.png`

**Resolution:** Minimum 300 DPI for publication
**Format:** PNG for manuscripts, TIFF for journals requiring it, PDF for vector graphics

---

## Final Assessment

Your figure suite is **85-90% publication-ready**. With the recommended improvements, particularly:

1. Fixing the Fig5 color scale inconsistency
2. Adding MaxEnt metrics to Fig7
3. Including all 4 SSPs in trajectory plots
4. Adding variable importance and response curves
5. Creating the PA overlay analysis

You will have a **world-class, publication-ready figure suite** suitable for high-impact journals in ecology and conservation (e.g., *Ecography*, *Diversity and Distributions*, *Biological Conservation*, *Global Change Biology*).

The GCM reliability ranking (Fig9) and uncertainty visualization (Fig6) are particularly innovative and will be valuable additions to the SDM literature.

---

**Assessment completed:** 2026-03-18  
**Run assessed:** RUN_20260317_203608_b990  
**Contact:** SDM Pipeline v3.0
