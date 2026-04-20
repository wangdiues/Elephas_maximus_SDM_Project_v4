# Data Context Specification — Elephas maximus SDM Project v4

Version: 3.0-bhutan-national
Updated: 2026-03-15

---

## Occurrence Data

### occurrence_bhutan.csv (ACTIVE — v3.0)
| Field | Value |
|-------|-------|
| Path | 01_data_raw/01_occurrences/occurrence_bhutan.csv |
| Records | 123 |
| Coordinate columns | decimalLongitude, decimalLatitude |
| CRS | WGS 84 (EPSG:4326) |
| Source | GPS collar telemetry, Bhutan-wide |
| Temporal coverage | Post-1986 (matching baseline climate window) |
| Quality notes | No obvious spatial duplicates in first review; deduplication applied in Phase 3 |
| Registry ID | occ_raw_003 |

### occurrence_south_central_bhutan.csv (DEPRECATED)
| Field | Value |
|-------|-------|
| Path | 01_data_raw/01_occurrences/occurrence_south_central_bhutan.csv |
| Records | 82,684 raw (150 unique GPS locations) |
| Status | DEPRECATED — superseded by occurrence_bhutan.csv for v3.0 |
| Registry ID | occ_raw_002 |

---

## AOI / Study Area

### bhutan.shp (ACTIVE — v3.0)
| Field | Value |
|-------|-------|
| Path | 01_data_raw/03_vector/shapefiles/Bhutan/bhutan.shp |
| Coverage | Bhutan national boundary |
| Source CRS | WGS 84 (EPSG:4326) per bhutan.prj |
| Pipeline CRS | EPSG:32645 (WGS 84 / UTM Zone 45N) after reprojection |
| Area | ~38,394 km² |
| Registry ID | aoi_002 |

### South_Central_Bhutan.shp (DEPRECATED)
| Field | Value |
|-------|-------|
| Path | 01_data_raw/03_vector/shapefiles/South Central Bhutan/South_Central_Bhutan.shp |
| Coverage | South Central Bhutan sub-region (~2,607 km²) |
| Status | DEPRECATED — superseded by bhutan.shp for v3.0 |
| Registry ID | aoi_001 |

---

## Climate / Raster Predictors

### Baseline bioclimate (ACTIVE)
| Field | Value |
|-------|-------|
| Source | CHELSA v2.1 historical bioclimatic variables |
| Variables | BIO1–BIO19 (19 variables) |
| Period | 1986–2015 |
| Resolution | 30 arcsec (~1 km at equator) |
| CRS | Custom TM (lon_0=90) — pipeline reprojects to EPSG:32645 |
| Path glob | 01_data_raw/02_rasters/present/Historical_bioclims/data/Historical/1986-2015/Historical_1986-2015_bio*.tif |

### DEM (ACTIVE)
| Field | Value |
|-------|-------|
| Source | NASADEM 12.5 m |
| Path | 01_data_raw/02_rasters/dem/DEM_Bhutan_12.5NG.tif |
| Coverage | Bhutan |

### Human footprint (ACTIVE)
| Field | Value |
|-------|-------|
| Source | Human Influence Index (HII) |
| Path | 01_data_raw/02_rasters/present/HII_Bhutan_2020.tif |
| Resolution | 1 km |
| Coverage | Bhutan |

### ESA Land cover (ACTIVE — primary for v3.0)
| Field | Value |
|-------|-------|
| Source | ESA WorldCover |
| Path | 01_data_raw/02_rasters/present/esa_landcover.tif |
| Resolution | 10 m |
| Coverage | Bhutan |
| Notes | `primary_landcover_source: "esa"` in config |

### NDVI / EVI (ACTIVE)
| Path | 01_data_raw/02_rasters/vegetation_indices/NDVI_Bhutan_2023.tif |
| Path | 01_data_raw/02_rasters/vegetation_indices/EVI_Bhutan_2023.tif |
| Source | Sentinel-2 2023 |
| Resolution | 10 m |
| CRS | EPSG:32645 |

### Future climate (ACTIVE)
| GCM | Alias in repo |
|-----|--------------|
| MRI-ESM2-0 | MRI-ESM2-0 |
| ACCESS-CM2 | ACCES_CM2 |
| CNRM-CM6-1 | CNRM_CM6_1 |
| CNRM-ESM2-1 | CNRM-ESM2-1 |
| INM-CM4-8 | INM_CM4_8 |
| INM-CM5-0 | INM-CM5-0 |
| MIROC6 | MIROC6 |
| MPI-ESM1-2-LR | MPI-ESM1-2-LR |

Scenarios: SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP5-8.5
Periods: 2041–2060, 2061–2080, 2081–2100

---

## Vector Overlays

| Layer | Path | Coverage | Status |
|-------|------|----------|--------|
| Protected areas | PA_Bhutan/PA_Bnd_Final_20230316.shp | Bhutan | ACTIVE |
| Major rivers | rivers_streams/Major_Rivers.shp | Bhutan | ACTIVE |
| Streams | rivers_streams/Stream_above3.shp | Bhutan | ACTIVE |
| Water sources | water_sources/watershed_water_source.shp | Bhutan | ACTIVE |
| Roads | Roads/road_network.shp | Bhutan (verify) | ACTIVE |
| LULC 2020 vector | lulc/lulc2020.shp | Coverage TBD | ACTIVE (auxiliary) |
| Settlements | Settlements/MS_Building_Foot_Prints_SCB.shp | South Central Bhutan ONLY | DISABLED — SCB only |
| Private land | Pvt land/Private_Public_Registered_Lands_SCB.shp | South Central Bhutan ONLY | DISABLED — SCB only |
| Conflict zones | — | — | NOT CONFIGURED |

---

## Data Quality Notes

1. `bhutan.shp` loaded with EPSG:4326 from bhutan.prj — no CRS ambiguity (unlike old SCB file).
2. Bioclim rasters carry a custom TM CRS (lon_0=90, k=1, x_0=250000) — pipeline explicitly reprojects to EPSG:32645 via `get_unified_template()`.
3. NDVI/EVI are already in EPSG:32645 — no reprojection needed.
4. `road_network.shp` coverage should be verified against Bhutan national extent before including road-distance predictor.
5. `lulc2020.shp` — Bhutan-wide coverage not yet verified; treated as auxiliary only.
