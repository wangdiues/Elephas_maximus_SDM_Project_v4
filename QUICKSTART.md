# Quick Start Guide

## Prerequisites

- R version 4.3 or higher
- 8GB+ RAM recommended
- 10GB+ free disk space

---

## Step 1: Install Dependencies

Run the setup script to install all required R packages:

```bash
Rscript 03_analysis/setup.R
```

This will:
- Check R version
- Install required packages (terra, sf, yaml, ggplot2, viridis, ranger, maxnet, gbm)
- Verify data files exist
- Create required directories

---

## Step 2: Verify Setup

Check that all required files are in place:

```
✓ occurrence input file referenced in 00_governance/config.yaml
✓ required AOI/vector layers referenced in 00_governance/config.yaml
✓ required baseline climate rasters referenced in 00_governance/config.yaml
```

---

## Step 3: Run Pipeline

```bash
Rscript 03_analysis/run_pipeline.R 00_governance/config.yaml
```

On Windows PowerShell, use:

```powershell
.\rerun.ps1
```

The pipeline will:
1. Validate all inputs
2. Process occurrence data
3. Build predictor stack
4. Train models (GLM, RF, BRT, MaxEnt)
5. Evaluate models
6. Generate future projections
7. Create all figures
8. Generate reports

---

## Step 4: Check Results

Results are saved to:
```
04_outputs/runs/RUN_YYYYMMDD_HHMMSS_<id>/
├── 00_manifest/
├── 01_processed_data/
├── 02_models/
├── 03_present_suitability/
├── 04_future_projections/
├── 05_change_metrics/
├── 06_uncertainty/
├── 07_overlays/
├── 08_figures_tables/
└── 09_reports/
```

Logs are saved to:
```
06_logs/
├── pipeline.log
├── errors.log
└── warnings.log
```

---

## Using Docker (Alternative)

If you prefer Docker:

```bash
# Build image
docker build -t elephas-sdm:2.1 .

# Run pipeline
docker run -v ${PWD}:/app elephas-sdm:2.1

# Or with docker-compose
docker-compose --profile production up
```

---

## Troubleshooting

### Package installation fails

Try installing system dependencies first:

**Windows:**
- Install Rtools from https://cran.r-project.org/bin/windows/Rtools/

**Linux:**
```bash
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libudunits2-dev
```

**macOS:**
```bash
brew install gdal geos proj udunits
```

### Data files missing

Download required data files:
- Occurrence data: station-level presence-absence CSV (`elephant_PA_data.csv`)
- Climate data: WorldClim or provided rasters
- Vector data: Provided shapefiles

Large raw data and generated run outputs are intentionally excluded from the
GitHub repository. See `DATA_ACCESS.md` for the publication and access model.

### Memory errors

Reduce `n_background` in config.yaml or increase RAM.

---

## Configuration

Edit `00_governance/config.yaml` to customize:
- Thinning distance
- Number of background points
- Cross-validation folds
- Future projection scenarios
- Random seeds

---

## Output

The pipeline generates:
- 42 deliverables
- 13 publication figures
- ODMAP-compliant reports
- Management brief

See `FEATURES.md` for complete list.

---

## Support

For issues:
1. Check `06_logs/errors.log`
2. Review `06_logs/warnings.log`
3. See `README.md` for project documentation
