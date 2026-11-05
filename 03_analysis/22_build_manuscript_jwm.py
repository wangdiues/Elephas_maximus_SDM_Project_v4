"""Build the Journal of Wildlife Management submission from the accepted run.

Every {{key}} in 05_manuscripts/source/*_jwm.md is replaced from the claim ledgers
(17_manuscript_numbers.R, 21_jwm_numbers_figures.R); a missing key is an error.
Writes to "05_manuscripts/Journal of Wildlife Management"; the Ecology and Evolution
files are not touched.

Usage (repo root): python 03_analysis/22_build_manuscript_jwm.py 04_outputs/runs/RUN_<id>
"""
import csv
import re
import shutil
import subprocess
import sys
from pathlib import Path

from docx import Document
from docx.enum.text import WD_COLOR_INDEX
from docx.oxml import OxmlElement, parse_xml
from docx.oxml.ns import nsdecls, qn
from docx.shared import Inches, Pt, RGBColor
from PIL import Image

run = Path(sys.argv[1])
ms_dir = Path("05_manuscripts")
src = ms_dir / "source"
out_dir = ms_dir / "Journal of Wildlife Management"
fig_in = ms_dir / "figures"
fig_work = out_dir / "figures"
fig_out = out_dir / "Figures_for_upload"
tab_dir = run / "08_figures_tables"
TITLE = "Broad-scale climate refugia for Asian elephants in Bhutan lie mostly outside protected areas"
DATE = "5 November 2026"


def rows(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


L = {r["key"]: r["value"] for r in rows(ms_dir / "manuscript_claim_ledger.csv")}
if {r["run_id"] for r in rows(ms_dir / "manuscript_claim_ledger.csv")} != {run.name}:
    sys.exit("Ledger run does not match " + run.name)
L.update({r["key"]: r["value"] for r in rows(out_dir / "jwm_extra_numbers.csv")})


def num(k):
    return float(L[k])


occ = rows(Path("01_data_raw/01_occurrences/elephant_PA_data.csv"))
L["n_stations"] = str(len(occ))
L["n_presences"] = str(sum(r["presence"] == "1" for r in occ))
L["n_absences"] = str(sum(r["presence"] == "0" for r in occ))
L["prevalence"] = f"{int(L['n_presences']) / int(L['n_stations']):.3f}"
L["domain_excluded_km2"] = str(round(num("bhutan_km2_on_grid") - num("domain_km2")))
L["present_suitable_outside_pa_pct"] = f"{100 - num('present_suitable_in_pa_pct'):.1f}"
L["present_suitable_outside_pa_km2"] = str(round(num("present_suitable_km2") - num("present_suitable_in_pa_km2")))
L["refugia_core_outside_pa_pct"] = f"{100 - num('refugia_core_in_pa_pct'):.1f}"
sens_dir = ms_dir / "sensitivity_no_dist_pa"
sens = rows(sens_dir / "claim_ledger_no_dist_pa.csv")
L["sens_run_id"] = sens[0]["run_id"]
L.update({"sens_" + r["key"]: r["value"] for r in sens})
L["sens_present_suitable_outside_pa_pct"] = f"{100 - num('sens_present_suitable_in_pa_pct'):.1f}"
L["sens_refugia_core_outside_pa_pct"] = f"{100 - num('sens_refugia_core_in_pa_pct'):.1f}"
L.update({r["key"]: r["value"] for r in rows(sens_dir / "map_agreement.csv")})
changes = [num(k) for k in L if k.startswith("future_change_pct_")]
L["future_mean_change_min"] = f"{min(changes):.1f}"
L["future_mean_change_max"] = f"{max(changes):.1f}"
L["run_id"] = run.name


def fmt(v):
    try:
        x = float(v)
    except ValueError:
        return v
    if x.is_integer() and abs(x) >= 1000:
        out = f"{int(x):,}"
    elif x.is_integer():
        out = str(int(x))
    else:
        out = v
    return out.replace("-", "−", 1) if out.startswith("-") else out


def fill(text):
    missing = sorted(set(k for k in re.findall(r"\{\{(\w+)\}\}", text) if k not in L))
    if missing:
        sys.exit("Missing ledger keys: " + ", ".join(missing))
    text = re.sub(r"\{\{(\w+)\}\}", lambda m: L[m.group(1)] if "year" in m.group(1) else fmt(L[m.group(1)]), text)
    return text.replace("+-", "−").replace("%–-", "% to −")


PAGE = '\n```{=openxml}\n<w:p><w:r><w:br w:type="page"/></w:r></w:p>\n```\n'


def table(header, body, caption, notes=None):
    out = [PAGE, f'::: {{custom-style="TableTitle"}}\n{caption}\n:::', "",
           "| " + " | ".join(header) + " |", "|" + "|".join(["---"] * len(header)) + "|"]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in body]
    if notes:
        out += ["", '::: {custom-style="TableNote"}'] + ["\n" + n for n in notes] + [":::"]
    return "\n".join(out) + "\n"


def c(v, d=3):
    return f"{float(v):.{d}f}"


def signed(v):
    return f"{float(v):+.1f}".replace("-", "−")


SSP = {"ssp126": "SSP1-2.6", "ssp245": "SSP2-4.5", "ssp370": "SSP3-7.0", "ssp585": "SSP5-8.5"}
PER = {"2021_2050": "2021–2050", "2051_2080": "2051–2080", "2071_2100": "2071–2100"}
GCM = {"acces_cm2": "ACCESS-CM2", "cnrm_cm6_1": "CNRM-CM6-1", "inm_cm5_0": "INM-CM5-0",
       "miroc6": "MIROC6", "mpi_esm1_2_lr": "MPI-ESM1-2-LR"}
BIO = {"BIO01": "Annual mean temperature", "BIO02": "Mean diurnal range", "BIO03": "Isothermality",
       "BIO04": "Temperature seasonality", "BIO05": "Maximum temperature of warmest month",
       "BIO06": "Minimum temperature of coldest month", "BIO07": "Temperature annual range",
       "BIO08": "Mean temperature of wettest quarter", "BIO09": "Mean temperature of driest quarter",
       "BIO10": "Mean temperature of warmest quarter", "BIO11": "Mean temperature of coldest quarter",
       "BIO12": "Annual precipitation", "BIO13": "Precipitation of wettest month",
       "BIO14": "Precipitation of driest month", "BIO15": "Precipitation seasonality",
       "BIO16": "Precipitation of wettest quarter", "BIO17": "Precipitation of driest quarter",
       "BIO18": "Precipitation of warmest quarter", "BIO19": "Precipitation of coldest quarter"}
PLAIN = {**BIO, "human_footprint": "Human Impact Index", "dist_to_major_rivers": "Distance to major rivers",
         "dist_to_private_land": "Distance to registered private land",
         "dist_to_protected_areas": "Distance to protected areas", "dist_to_roads": "Distance to roads",
         "dist_to_settlements": "Distance to buildings", "dist_to_streams": "Distance to streams",
         "dist_to_water_sources": "Distance to water sources", "aspect": "Aspect", "slope": "Slope",
         "tri": "Terrain ruggedness", "elevation": "Elevation", "dynamicworld_prob": "Land-cover class probability",
         "evi": "Enhanced vegetation index", "landcover": "Land-cover class",
         "ndvi": "Normalized difference vegetation index"}


def plain(p):
    return PLAIN.get(p, p)


def plain_text(s):
    return re.sub(r"\bBIO\d\d\b|\b(?:dist_to_\w+|dynamicworld_prob|human_footprint|evi|ndvi|elevation|tri)\b",
                  lambda m: plain(m.group(0)).lower() if m.group(0) in PLAIN else m.group(0), s).replace("r=", "r = ")


AUC_DEF = "AUC = area under the receiver operating characteristic curve; TSS = true skill statistic"
PLACE = "Asian elephants in Bhutan"
SURVEYS = "National Elephant Survey (2016) and National Tiger Survey (2021–2022)"

# ── Main tables ──────────────────────────────────────────────────────────
t1 = {r["Algorithm"]: r for r in rows(tab_dir / "table_01_model_performance.csv")}
body = []
for a, name in [("GLM", "Generalized linear model"), ("RF", "Random forest"), ("BRT", "Boosted regression trees")]:
    r = t1[a]
    body.append([name, f"{c(r['AUC_fold_mean'])} ± {c(r['AUC_fold_SD'])}", f"{c(r['TSS_fold_mean'])} ± {c(r['TSS_fold_SD'])}",
                 c(r["Boyce"]), c(r["Brier_fold_mean"]), c(r["Calibration_slope"], 2), c(r["Morans_I"])])
body.append(["Ensemble^a^", L["ensemble_auc_mean"], L["ensemble_tss_mean"], c(L["ensemble_boyce"]),
             c(L["ensemble_brier"]), c(L["ensemble_calibration_slope"], 2), c(L["ensemble_moran_i"])])
T1 = table(["Model", "AUC", "TSS", "Boyce index", "Brier score", "Calibration slope", "Moran's *I*"], body,
           f"Table 1. Spatially blocked cross-validation performance of models predicting occurrence of {PLACE}, "
           f"from {fmt(L['n_stations'])} stations of the {SURVEYS}. Values for AUC, TSS, and Brier score of individual algorithms "
           f"are means ± SD across 5 folds (15-km blocks); other values use pooled held-out predictions. {AUC_DEF}.",
           [f"^a^Equal-weight mean of the 3 algorithms, calculated on pooled held-out predictions. Habitat threshold = {L['threshold']}."])

T2rows = [("All stations", "all"), ("All stations, dung records removed", "no_dung"),
          ("National Tiger Survey stations", "tiger"), ("National Elephant Survey stations", "elephant"),
          (f"Stations at or below highest detection ({fmt(L['station_elev_max_presence_m'])} m)", "below_max_presence"),
          ("Stations ≤1,000 m", "below_1000m")]
T2 = table(["Stations scored", "Stations", "Presences", "Prevalence", "AUC", "TSS at habitat threshold^a^", "Best TSS^b^"],
           [[n, fmt(L[f"eval_{k}_n"]), L[f"eval_{k}_presences"], L[f"eval_{k}_prevalence"], L[f"eval_{k}_auc"],
             L[f"eval_{k}_tss_at_threshold"], L[f"eval_{k}_tss_best"]] for n, k in T2rows],
           f"Table 2. Discrimination of the equal-weight ensemble for {PLACE} on subsets of stations from the {SURVEYS}, "
           f"using the same pooled, spatially blocked held-out predictions in every row. {AUC_DEF}.",
           [f"^a^Threshold = {L['threshold']}, set on all stations.", "^b^TSS at the threshold that is optimal for that subset."])

body = []
for s in SSP:
    for p in PER:
        k = f"{s}_{p}"
        body.append([SSP[s], PER[p], fmt(L[f"future_area_km2_{k}"]), signed(L[f"future_change_pct_{k}"]),
                     signed(L[f"future_gcm_median_change_pct_{k}"]),
                     f"{fmt(L[f'future_gcm_area_min_km2_{k}'])}–{fmt(L[f'future_gcm_area_max_km2_{k}'])}",
                     L[f"future_gcm_n_loss_{k}"]])
T3 = table(["Pathway", "Period", "GCM-mean map (km²)", "Change, GCM mean (%)", "Change, GCM median (%)",
            "Range across GCMs (km²)", "GCMs with net loss (of 5)"], body,
           f"Table 3. Projected suitable habitat for {PLACE} under 4 shared socioeconomic pathways (SSP) and 3 periods, 2021–2100. "
           f"GCM = general circulation model.",
           [f"Present suitable habitat = {fmt(L['present_suitable_km2'])} km². The GCM-mean map averages the 5 per-GCM ensemble "
            f"projections before thresholding; the median and range are taken over the 5 per-GCM habitat areas."])

T4 = table(["Category", "Definition", "Area (km²)", "% of present habitat", "% inside network", "Outside network (km²)",
            "% in top quarter of human pressure"],
           [["Present habitat", f"Ensemble suitability ≥{L['threshold']}", fmt(L["present_suitable_km2"]), "100",
             L["present_suitable_in_pa_pct"], fmt(L["present_suitable_outside_pa_km2"]), L["present_suitable_in_top25_human_pressure_pct"]],
            ["High-stability habitat", "Suitable in ≥45 of 60 projections", fmt(L["refugia_high_km2"]), L["refugia_high_pct_present"],
             L["refugia_high_in_pa_pct"], fmt(L["refugia_high_outside_pa_km2"]), "–"],
            ["Core refugia", "Suitable in all 60 projections", fmt(L["refugia_core_km2"]), L["refugia_core_pct_present"],
             L["refugia_core_in_pa_pct"], fmt(L["refugia_core_outside_pa_km2"]), L["refugia_core_in_top25_human_pressure_pct"]]],
           f"Table 4. Present habitat and climate refugia for {PLACE}, with coverage by the protected-area network and overlap "
           f"with human pressure. Projections are 5 general circulation models × 4 pathways × 3 periods, 2021–2100.",
           ["The network comprises national parks, wildlife sanctuaries, the strict nature reserve, and biological corridors."])

SPAN = f"{SURVEYS}"
FIGS = [
    ("main_figure_01_spatial_cv", "Figure_1_study_area_cross_validation",
     f"Study area and spatial cross-validation design for Asian elephants in Bhutan. Points are stations of the {SPAN} (filled circles, presences; open gray circles, absences). Shaded squares are the 15-km blocks used for 5-fold spatial cross-validation, colored by fold."),
    ("Figure_2_predictor_importance", "Figure_2_predictor_importance",
     f"Leading predictors of Asian elephant occurrence in Bhutan for each algorithm in the ensemble, fitted to {fmt(L['n_stations'])} stations surveyed in 2016 and 2021–2022. Importance is rescaled within each algorithm to a maximum of 100 (generalized linear model, absolute coefficient × predictor standard deviation; random forest, permutation importance; boosted regression trees, relative influence) and is not comparable across algorithms."),
    ("Figure_3_response_curves", "Figure_3_response_curves",
     f"Partial dependence of predicted Asian elephant occurrence on 6 leading predictors in Bhutan, from {fmt(L['n_stations'])} stations surveyed in 2016 and 2021–2022. Lines show the mean prediction across all stations when the predictor is set to each value, for each algorithm and their equal-weight ensemble. Axes span the 2nd–98th percentiles of station values. Precipitation seasonality is the coefficient of variation of monthly precipitation."),
    ("main_figure_03_present_suitability", "Figure_4_present_suitability",
     f"Present habitat suitability for Asian elephants in Bhutan (equal-weight ensemble of generalized linear model, random forest, and boosted regression trees; 1986–2015 climate). Yellow lines are protected-area and biological-corridor boundaries; blank cells lie outside the modeled domain. The habitat threshold is {L['threshold']}."),
    ("main_figure_04_future_habitat_area", "Figure_5_future_habitat_area",
     f"Projected suitable habitat for Asian elephants in Bhutan by shared socioeconomic pathway (SSP) and period, 2021–2100. Colored lines are the 5 general circulation models (GCM); the black line is the median across GCMs; the dotted line is present habitat ({fmt(L['present_suitable_km2'])} km²)."),
    ("main_figure_05_refugia_stability", "Figure_6_refugia_stability",
     "Stability of present Asian elephant habitat in Bhutan across 60 future projections (5 general circulation models × 4 shared socioeconomic pathways × 3 periods, 2021–2100): the fraction of projections in which each currently suitable cell remains suitable. Cells with value 1 are core refugia. Green lines are protected-area and biological-corridor boundaries."),
    ("main_figure_06_interaction_exposure", "Figure_7_interaction_exposure",
     f"Potential human–elephant interaction exposure in Bhutan at present: the geometric mean of rescaled habitat suitability and human pressure. The index describes co-occurrence and is not a fitted or validated conflict-risk model. White points are {L['hwc_incidents_in_domain']} geolocated elephant-conflict incidents recorded in the Spatial Monitoring and Reporting Tool by the Department of Forests and Park Services in {L['hwc_year_min']}–{L['hwc_year_max']}, shown for context only. Gray lines are district (Dzongkhag) boundaries; green lines are protected areas."),
]

ms = fill((src / "manuscript_jwm.md").read_text(encoding="utf-8"))
head, body_txt = ms.split("The Asian elephant (*Elephas maximus*) is listed", 1)
body_main, refs = body_txt.split("# REFERENCES", 1)
for item in sorted(set(re.findall(r"\b(?:Table|Figure|Appendix) S\d+\b", body_main))):
    body_main = re.sub(rf"\b{item}\b(?!\d)", f"{item}, available in Supporting Information", body_main, count=1)
ms = '::: {custom-style="Front"}\n' + head + "\n:::\n\nThe Asian elephant (*Elephas maximus*) is listed" + body_main + "# REFERENCES" + refs
caps = "\n\n".join(f"Figure {i}. {f[2]}" for i, f in enumerate(FIGS, 1))
ms += "\n" + PAGE + '\n::: {custom-style="Heading 1"}\nFigure Captions\n:::\n\n::: {custom-style="Front"}\n' + caps + "\n:::\n"
ms += "\n" + T1 + "\n" + T2 + "\n" + T3 + "\n" + T4
(src / "manuscript_jwm_filled.md").write_text(ms, encoding="utf-8")

# ── Figures for upload ───────────────────────────────────────────────────
fig_out.mkdir(exist_ok=True)
Image.MAX_IMAGE_PIXELS = None
for stem, name, _ in FIGS:
    p = (fig_work / f"{stem}.png") if (fig_work / f"{stem}.png").exists() else (fig_in / f"{stem}.png")
    Image.open(p).convert("RGB").save(fig_out / f"{name}.tif", compression="tiff_lzw", dpi=(600, 600))
Image.open(fig_work / "Graphical_abstract.png").convert("RGB").save(fig_out / "Graphical_abstract.tif", compression="tiff_lzw", dpi=(600, 600))
shutil.copy(fig_work / "Graphical_abstract.pdf", fig_out / "Graphical_abstract.pdf")
im = Image.open(tab_dir / "figure_s10_mess_map.png").convert("RGB")
fig_s1 = fig_work / "Figure_S1_climatic_novelty.png"
im.crop((0, 540, im.width, 1750)).save(fig_s1, dpi=(300, 300))

# ── Supporting Information ───────────────────────────────────────────────
S = ['::: {custom-style="SIHeading"}\nSupporting Information\n:::', DATE,
     f"Wangdi, and L. Sagar. 2026. {TITLE}. Journal of Wildlife Management.",
     f"All tables and figures derive from 1 accepted analysis run (`{run.name}`), except the predictor sensitivity analysis in Table S11."]

exc = rows(Path("01_data_raw/01_occurrences/occurrence_exclusions.csv"))
n_out = [r for r in exc if r["exclusion_reason"] == "outside_bhutan_boundary"]
n_unv = [r for r in exc if r["exclusion_reason"] == "unverified_coordinate"]
tn = [r for r in occ if r["source"] == "station_survey"]
en = [r for r in occ if r["source"] == "book1_survey"]
pres = lambda z: sum(r["presence"] == "1" for r in z)
absn = lambda z: sum(r["presence"] == "0" for r in z)
S.append(table(["Step", "Records", "Presences", "Absences"], [
    ["Merged and de-duplicated stations", fmt(len(occ) + len(exc)), pres(occ) + pres(exc), absn(occ) + absn(exc)],
    ["Excluded: outside national boundary", len(n_out), pres(n_out), absn(n_out)],
    ["Excluded: unverified coordinates", len(n_unv), pres(n_unv), absn(n_unv)],
    ["Modeled", fmt(L["n_stations"]), L["n_presences"], L["n_absences"]],
    ["Modeled: National Tiger Survey 2021–2022", len(tn), pres(tn), absn(tn)],
    ["Modeled: National Elephant Survey 2016", len(en), pres(en), absn(en)],
    ["Of which dung records", L["n_dung_presences"], L["n_dung_presences"], 0]],
    "Table S1. Occurrence records of Asian elephants in Bhutan from 2 national surveys and exclusions before modeling."))

t = rows(tab_dir / "table_02_predictor_selection.csv")
grp = {"Climate (BIO)": "Climate", "Vegetation/LULC": "Vegetation and land cover", "Anthropogenic": "Human pressure"}
S.append(table(["Predictor", "Group", "Status", "Reason for removal"],
               [[plain(r["Predictor"]), grp.get(r["Category"], r["Category"]), r["Status"],
                 plain_text(r["Removal_reason"] or "–") if r["Status"] != "Selected" else "–"] for r in t],
               "Table S2. Candidate predictors of Asian elephant occurrence in Bhutan and collinearity screening (|*r*| ≥ 0.85 or variance inflation factor [VIF] ≥10)."))

t = rows(tab_dir / "table_20_algorithm_hyperparams.csv")
aname = {"GLM": "Generalized linear model", "Random Forest": "Random forest", "BRT": "Boosted regression trees"}
S.append(table(["Algorithm", "Package", "Settings", "Role"],
               [[aname.get(r["Algorithm"], r["Algorithm"]), r["Package"], r["Key_parameters"],
                 "Sensitivity model only" if r["Algorithm"] == "MaxEnt" else "Ensemble member"] for r in t],
               "Table S3. Algorithm settings for species distribution models of Asian elephants in Bhutan. OOB = out-of-bag."))

t = rows(run / "01_processed_data" / "spatial_block_design_sensitivity.csv")
S.append(table(["Block size (km)", "Presences per fold", "Absences per fold", "Primary design"],
               [[r["block_size_km"], f"{r['min_presences']}–{r['max_presences']}", f"{r['min_reference']}–{r['max_reference']}",
                 "Yes" if r["selected_primary"] == "TRUE" else "No"] for r in t],
               "Table S4. Fold composition for alternative spatial block sizes (5 folds, whole-block assignment balancing presences and absences), Asian elephant stations in Bhutan."))

t = rows(tab_dir / "table_16_threshold_sensitivity.csv")
S.append(table(["Threshold", "Suitable area (km²)", "% of domain"],
               [[r["Threshold"] + (" (primary, rounded)" if r["Is_current"] == "TRUE" else ""), fmt(r["Area_km2"]), r["Pct_suitable"]] for r in t],
               "Table S5. Sensitivity of present Asian elephant habitat area in Bhutan to the classification threshold.",
               ["Areas are calculated at the threshold rounded to 3 decimals and may differ slightly from the main text."]))

t = rows(run / "05_change_metrics" / "habitat_area_change_summary.csv")
body = []
for r in t:
    m = re.match(r"ensemble_(ssp\d+)_(\d{4}_\d{4})", r["scenario"])
    body.append([SSP[m.group(1)], PER[m.group(2)], fmt(round(float(r["gain_km2"]))), fmt(round(float(r["loss_km2"]))), fmt(round(float(r["pers_km2"])))])
S.append(table(["Pathway", "Period", "Gain (km²)", "Loss (km²)", "Persistence (km²)"], body,
               "Table S6. Gain, loss, and persistence of Asian elephant habitat in Bhutan relative to the present, from the general circulation model-mean ensemble map, 2021–2100."))

vi = rows(tab_dir / "table_S1_variable_importance.csv")
preds = []
for r in vi:
    if r["Algorithm"] in ("GLM", "RF", "BRT") and r["Predictor"] not in preds:
        preds.append(r["Predictor"])
get = {(r["Algorithm"], r["Predictor"]): r["Relative_importance_pct"] for r in vi}
preds.sort(key=lambda p: -sum(float(get.get((a, p), 0)) for a in ("GLM", "RF", "BRT")))
S.append(table(["Predictor", "Generalized linear model", "Random forest", "Boosted regression trees"],
               [[plain(p)] + [c(get.get((a, p), "nan"), 1) for a in ("GLM", "RF", "BRT")] for p in preds],
               "Table S7. Relative importance of predictors of Asian elephant occurrence in Bhutan within each algorithm (maximum = 100)."))

t = rows(tab_dir / "table_19_predictor_statistics.csv")
S.append(table(["Predictor", "Presences: median", "Presences: mean ± SD", "Absences: median", "Absences: mean ± SD"],
               [[plain(r["Predictor"]), r["Pres_median"], f"{r['Pres_mean']} ± {r['Pres_SD']}", r["Bg_median"], f"{r['Bg_mean']} ± {r['Bg_SD']}"] for r in t],
               f"Table S8. Predictor values at Asian elephant presence and absence stations in Bhutan ({L['n_presences']} presences, {L['n_absences']} absences).",
               ["Distances are in meters, temperature in °C, and precipitation of the warmest quarter and driest month in mm; precipitation seasonality is a coefficient of variation (%)."]))

t = rows(tab_dir / "table_24_dzongkhag_suitability.csv")
S.append(table(["District (Dzongkhag)", "Modeled area (km²)", "Suitable (km²)", "% suitable", "Mean suitability"],
               [[r["Dzongkhag"], r["Total_area_km2"], r["Suitable_area_km2"], r["Pct_suitable"], r["Mean_suitability"]] for r in t if float(r["Suitable_area_km2"]) > 0],
               "Table S9. Present suitable Asian elephant habitat by district in Bhutan (districts with any suitable habitat)."))

t = rows(ms_dir / "manuscript_claim_ledger_pa_units.csv")
S.append(table(["Protected area or corridor", "Modeled area (km²)", "Suitable habitat (km²)", "% suitable"],
               [[r["unit"], fmt(round(float(r["unit_km2"]))), fmt(round(float(r["suitable_km2"]))),
                 f"{100 * float(r['suitable_km2']) / float(r['unit_km2']):.1f}" if float(r["unit_km2"]) > 0 else "–"] for r in t],
               "Table S10. Present suitable Asian elephant habitat by protected area and biological corridor in Bhutan."))

t = rows(tab_dir / "table_interaction_exposure_summary.csv")
S.append(table(["Pathway", "Period", "Mean exposure index"],
               [[SSP.get(r["SSP"].lower(), r["SSP"]), r["Period"].replace("-", "–"), r["Mean_interaction_exposure"]] for r in t],
               "Table S13. National mean potential human–elephant interaction-exposure index in Bhutan, present and future (human pressure held at present values)."))

t = rows(ms_dir / "manuscript_claim_ledger_gcm_trajectories.csv")
body = []
for s in SSP:
    for p in PER:
        vals = {r["gcm"]: r for r in t if r["ssp"] == s and r["period"] == p}
        body.append([SSP[s], PER[p]] + [f"{fmt(round(float(vals[g]['area_km2'])))} ({signed(vals[g]['change_pct'])}%)" for g in GCM])
S.append(table(["Pathway", "Period"] + list(GCM.values()), body,
               "Table S12. Suitable Asian elephant habitat in Bhutan (km²) and change from present (%) for each general circulation model, 2021–2100."))

S13 = [("Ensemble AUC (spatial held-out predictions)", "ensemble_auc_mean"), ("Ensemble TSS (spatial held-out predictions)", "ensemble_tss_mean"),
       ("Ensemble Boyce index", "ensemble_boyce"), ("Classification threshold", "threshold"),
       ("Present habitat (km²)", "present_suitable_km2"), ("Present habitat inside network (%)", "present_suitable_in_pa_pct"),
       ("Present habitat outside network (%)", "present_suitable_outside_pa_pct"),
       ("Core refugia, suitable in 60 of 60 projections (km²)", "refugia_core_km2"),
       ("Core refugia outside network (%)", "refugia_core_outside_pa_pct"),
       ("High-stability habitat, ≥45 of 60 (km²)", "refugia_high_km2"),
       ("Projections with net loss (of 60)", "per_gcm_n_projections_loss"),
       ("Range of change across projections (%)", None),
       ("Median across models, SSP5-8.5, 2071–2100 (%)", "future_gcm_median_change_pct_ssp585_2071_2100")]
body = []
for label, k in S13:
    if k is None:
        body.append([label, f"{fmt(L['per_gcm_change_pct_min'])} to +{fmt(L['per_gcm_change_pct_max'])}",
                     f"{fmt(L['sens_per_gcm_change_pct_min'])} to +{fmt(L['sens_per_gcm_change_pct_max'])}"])
    elif k in ("ensemble_boyce", "threshold"):
        body.append([label, c(L[k]), c(L["sens_" + k])])
    else:
        body.append([label, fmt(L[k]), fmt(L["sens_" + k])])
S.append(table(["Quantity", "Primary model", "Without distance to protected areas"], body,
               f"Table S11. Sensitivity of the main results for Asian elephants in Bhutan to removing distance to protected areas as a predictor. {AUC_DEF}.",
               [f"The sensitivity run (`{L['sens_run_id']}`) was identical to the accepted run (`{run.name}`) except for this predictor, "
                f"and repeated all 60 projections. Agreement of present-day suitability maps: Pearson *r* = {L['map_pearson_r']}, "
                f"Schoener's *D* = {L['map_schoener_d']}; Jaccard overlap of habitat maps = {L['map_jaccard']}."]))

i11, i13 = [i for i, s in enumerate(S) if "Table S11. Sensitivity" in s or "Table S13. National" in s]
S[i11], S[i13] = S[i13], S[i11]

S.append(PAGE + f"![]({fig_s1.as_posix()}){{width=6.5in}}\n\nFigure S1. Climatic novelty for Asian elephant models in Bhutan relative to the calibration "
         "stations: multivariate environmental similarity surface (MESS), mean of 5 general circulation models (GCM), shared socioeconomic pathway 5-8.5, 2071–2100. "
         "Negative values indicate ≥1 climate predictor outside the range sampled by the surveys, in calibration standard deviations.\n")

odmap = [
    ["Overview", "Taxon; location; scale", "*Elephas maximus*; Bhutan (national); 250-m grid, EPSG:32645"],
    ["Overview", "Objective", "Map present habitat; project to 2021–2100 under CMIP6; identify climate refugia and their protection"],
    ["Overview", "Model algorithms", "Generalized linear model, random forest, boosted regression trees (equal-weight ensemble); MaxEnt sensitivity only"],
    ["Data", "Response data", f"Detection/non-detection at {fmt(L['n_stations'])} stations ({L['n_presences']} presences including {L['n_dung_presences']} dung records, {L['n_absences']} absences); 26 records excluded (Table S1)"],
    ["Data", "Predictors", "18 retained of 35 candidates after |r| ≥ 0.85 and VIF ≥10 screening (Table S2); 1986–2015 climate baseline"],
    ["Data", "Future climate", "5 general circulation models (ACCESS-CM2, CNRM-CM6-1, INM-CM5-0, MIROC6, MPI-ESM1-2-LR) × 4 pathways × 3 periods = 60 projections"],
    ["Model", "Assumptions", "Equilibrium with climate; non-climatic predictors static; national boundary as accessible area"],
    ["Model", "Settings", "Table S3; equal weights fixed a priori; threshold = TSS optimum of pooled held-out ensemble predictions"],
    ["Assessment", "Validation", f"5-fold spatial block cross-validation (15-km blocks; {L['fold_presence_min']}–{L['fold_presence_max']} presences per fold); AUC, TSS, Boyce index, Brier score, calibration slope, Moran's *I*; evaluation by survey and within elephant range"],
    ["Assessment", "Uncertainty", "Per-model areas and median; inter-model standard deviation maps; climatic novelty relative to calibration stations for every projection"],
    ["Prediction", "Outputs", "Present suitability; 60 per-model and 12 model-mean projections; refugia stability count (denominator 60)"],
    ["Prediction", "Extrapolation", f"Reported, not masked: {L['present_suitable_extrap_per_projection_median_pct']}% of present habitat outside the sampled climate range in the median projection"],
    ["Reproducibility", "Code and run", f"Single scripted pipeline; accepted run `{run.name}` with input hashes, package versions, and seeds in its manifest"],
]
S.append(table(["ODMAP section", "Element", "Description"], odmap,
               "Appendix S1. Overview, Data, Model, Assessment, and Prediction (ODMAP) protocol summary for species distribution models of Asian elephants in Bhutan (Zurell et al. 2020)."))
zurell = re.search(r"^Zurell, D\..*$", refs, re.M).group(0)
S.append('::: {custom-style="SIHeading"}\nReferences\n:::\n\n::: {custom-style="Reference"}\n' + zurell + "\n:::")
(src / "supplement_jwm_filled.md").write_text("\n\n".join(S).replace(PAGE, ""), encoding="utf-8")


# ── Word formatting ──────────────────────────────────────────────────────
def field(par, code):
    for kind, text in (("begin", None), (None, code), ("end", None)):
        r = par.add_run()
        if kind:
            e = OxmlElement("w:fldChar")
            e.set(qn("w:fldCharType"), kind)
        else:
            e = OxmlElement("w:instrText")
            e.text = text
        r._r.append(e)


def set_font(style, size=12, bold=None, italic=False):
    f = style.font
    f.name, f.size, f.color.rgb, f.italic = "Times New Roman", Pt(size), RGBColor(0, 0, 0), italic
    if bold is not None:
        f.bold = bold
    rpr = style.element.get_or_add_rPr()
    rf = rpr.find(qn("w:rFonts"))
    if rf is None:
        rf = OxmlElement("w:rFonts")
        rpr.append(rf)
    for a in ("w:ascii", "w:hAnsi", "w:eastAsia", "w:cs"):
        rf.set(qn(a), "Times New Roman")
    for a in ("w:asciiTheme", "w:hAnsiTheme", "w:eastAsiaTheme", "w:cstheme"):
        rf.attrib.pop(qn(a), None)


def finish(path, spacing=2.0, line_numbers=True, header=None, table_pt=10):
    doc = Document(path)
    names = {s.name: s for s in doc.styles}
    for s in doc.styles:
        if s.type == 1:
            set_font(s)
            pf = s.paragraph_format
            pf.line_spacing, pf.space_before, pf.space_after, pf.first_line_indent = spacing, Pt(0), Pt(0), None
    if "AuthorCheck" in names:
        names["AuthorCheck"].font.highlight_color = WD_COLOR_INDEX.YELLOW
    if "Body Text" in names:
        names["Body Text"].paragraph_format.first_line_indent = Inches(0.5) if line_numbers else Inches(0)
    for n in ("Front", "TableTitle", "TableNote", "SIHeading", "Heading 1", "Heading 2", "First Paragraph", "Compact"):
        if n in names:
            names[n].paragraph_format.first_line_indent = Inches(0)
    if not line_numbers:
        for n in ("Body Text", "First Paragraph"):
            if n in names:
                names[n].paragraph_format.space_after = Pt(6)
    for n in ("Heading 1", "Heading 2", "SIHeading"):
        if n in names:
            set_font(names[n], bold=True)
            names[n].paragraph_format.keep_with_next = True
    if "Reference" in names:
        names["Reference"].paragraph_format.left_indent = Inches(0.5)
        names["Reference"].paragraph_format.first_line_indent = Inches(-0.5)
    if "TableTitle" in names:
        names["TableTitle"].paragraph_format.keep_with_next = True
    if "TableNote" in names:
        set_font(names["TableNote"], size=table_pt)
        names["TableNote"].paragraph_format.line_spacing = 1.0
        names["TableNote"].paragraph_format.space_before = Pt(4)
    if "Compact" in names:
        set_font(names["Compact"], size=table_pt)
        names["Compact"].paragraph_format.line_spacing = 1.0
    if "Image Caption" in names:
        names["Image Caption"].paragraph_format.keep_together = True
    for t in doc.tables:
        pr = t._tbl.tblPr
        for tag in ("w:tblStyle", "w:tblBorders", "w:tblW", "w:tblLook"):
            e = pr.find(qn(tag))
            if e is not None:
                pr.remove(e)
        pr.append(parse_xml(f'<w:tblW {nsdecls("w")} w:w="5000" w:type="pct"/>'))
        pr.append(parse_xml(f'<w:tblBorders {nsdecls("w")}><w:top w:val="single" w:sz="8" w:color="000000"/>'
                            '<w:bottom w:val="single" w:sz="8" w:color="000000"/></w:tblBorders>'))
        pr.append(parse_xml(f'<w:tblLayout {nsdecls("w")} w:type="fixed"/>'))
        cols = len(t.columns)
        need = [max(len(r.cells[i].text) for r in t.rows) for i in range(cols)]
        token = [max(len(w) for r in t.rows for w in r.cells[i].text.split()) if need[i] else 1 for i in range(cols)]
        floor = [k * table_pt * 11 + 240 for k in token]
        spare = max(9360 - sum(floor), 0)
        extra = [min(n, 60) - k + 1 for n, k in zip(need, token)]
        widths = [int(f + spare * e / sum(extra)) for f, e in zip(floor, extra)]
        widths = [int(w * 9360 / sum(widths)) for w in widths]
        for g, w in zip(t._tbl.tblGrid.findall(qn("w:gridCol")), widths):
            g.set(qn("w:w"), str(w))
        for row in t.rows:
            for c_, w in zip(row.cells, widths):
                tc = c_._tc.get_or_add_tcPr()
                old = tc.find(qn("w:tcW"))
                if old is not None:
                    tc.remove(old)
                tc.insert(0, parse_xml(f'<w:tcW {nsdecls("w")} w:w="{w}" w:type="dxa"/>'))
        for c_ in t.rows[0].cells:
            tc = c_._tc.get_or_add_tcPr()
            tc.append(parse_xml(f'<w:tcBorders {nsdecls("w")}><w:bottom w:val="single" w:sz="4" w:color="000000"/></w:tcBorders>'))
        t.rows[0]._tr.get_or_add_trPr().append(OxmlElement("w:tblHeader"))
        for row in t.rows:
            row._tr.get_or_add_trPr().append(OxmlElement("w:cantSplit"))
            for c_ in row.cells:
                for p in c_.paragraphs:
                    p.style = doc.styles["Compact"]
    for sec in doc.sections:
        sec.page_width, sec.page_height = Inches(8.5), Inches(11)
        sec.left_margin = sec.right_margin = sec.top_margin = sec.bottom_margin = Inches(1)
        if line_numbers:
            ln = OxmlElement("w:lnNumType")
            ln.set(qn("w:countBy"), "1")
            ln.set(qn("w:restart"), "continuous")
            sec._sectPr.append(ln)
        if header:
            par = sec.header.paragraphs[0]
            par.paragraph_format.line_spacing = 1.0
            field(par, "PAGE")
            par.add_run(" " + header)
    doc.core_properties.title = TITLE
    doc.core_properties.author = "Wangdi; Laxmi Sagar"
    doc.save(path)


def pandoc(md, out):
    subprocess.run(["pandoc", str(md), "-f", "markdown-smart", "-o", str(out), "--resource-path=."], check=True)


outs = [(src / "manuscript_jwm_filled.md", out_dir / "Manuscript_JWM.docx", dict(spacing=2.0, line_numbers=True, header="Wangdi and Sagar")),
        (src / "supplement_jwm_filled.md", out_dir / "Supporting_Information_JWM.docx", dict(spacing=1.15, line_numbers=False, header=None, table_pt=9))]
for name, target in [("cover_letter_jwm", "Cover_Letter_JWM.docx"), ("data_availability_jwm", "Data_Availability_Statement_JWM.docx"),
                     ("graphical_abstract_text_jwm", "Graphical_Abstract_Text_JWM.docx"), ("author_review_jwm", "READ_FIRST_Author_Review.docx")]:
    f = src / f"{name}.md"
    if f.exists():
        filled = src / f"{name}_filled.md"
        filled.write_text(fill(f.read_text(encoding="utf-8")), encoding="utf-8")
        outs.append((filled, out_dir / target, dict(spacing=1.15, line_numbers=False, header=None, table_pt=10)))
for md, out, opts in outs:
    pandoc(md, out)
    finish(out, **opts)
    print("Wrote", out)

# ── Guideline checks ─────────────────────────────────────────────────────
text = (src / "manuscript_jwm_filled.md").read_text(encoding="utf-8")
main = text.split("# REFERENCES")[0]
refs_txt = text.split("# REFERENCES")[1].split("Figure Captions")[0]
problems = []
if len(TITLE.split()) > 15:
    problems.append("title > 15 words")
rh = re.search(r"RH: (.*)", text).group(1)
if len(rh) >= 45:
    problems.append("running head >= 45 characters")
for w in re.findall(r"\b(?:modell\w+|generalis\w+|colou?r\w*ise\w*|colour\w*|recognis\w+|behaviour\w*|metres?|artefact\w*|towards|whilst|maximis\w+|minimis\w+|acknowledgement\w*)\b", main, re.I):
    problems.append("British spelling: " + w)
for w in re.findall(r"\b(?:we|our|ours|us)\b", main, re.I):
    problems.append("first person: " + w)
for w in sorted(set(re.findall(r"\bBIO\d\d\b|\bOOF\b|\bdist_to\w+", main))):
    problems.append("raw label in text: " + w)
cited = set(re.findall(r"([A-Z][\w\-]+)(?: et al\.| and [A-Z][\w\-]+)? (\d{4})(?:\*[ab]\*)?", main))
ref_keys = set()
for line in refs_txt.splitlines():
    m = re.match(r"([^,\[]+?)(?:,| \[)", line)
    y = re.search(r"\. (\d{4})\*?[ab]?\*?\. ", line)
    if m and y:
        name = m.group(1).split()[-1]
        ref_keys.add((name, y.group(1)))
        ab = re.search(r"\[(\w+)\]", line)
        if ab:
            ref_keys.add((ab.group(1), y.group(1)))
skip = {"By", "November", "Punakha", "System", "Team", "Thimphu", "World", "National", "Survey", "Table", "Figure", "October", "January", "February", "June", "March", "May", "Version", "SSP", "CMIP"}
for k in sorted(cited):
    if k not in ref_keys and k[0] not in skip:
        problems.append(f"cited, no reference: {k}")
for k in sorted(ref_keys):
    if k not in cited and k[0] not in ("Computing", "Diversity", "Division", "Services"):
        problems.append(f"reference, not cited: {k}")
if "{{" in text:
    problems.append("unfilled key")
for w in re.findall(r"2,0\d\d–2,\d{3}", text):
    problems.append("comma in year range: " + w)
print("Main text words:", len(main.split()))
print("Checks:", "OK" if not problems else "\n  " + "\n  ".join(problems))
