"""Build the revised Journal of Wildlife Management documents.

Every {{key}} in 05_manuscripts/source/*_jwm.md is replaced from the claim ledgers
(17_manuscript_numbers.R, 21_jwm_numbers_figures.R); a missing key is an error.
Adds outputs from REV_OUT (default 04_outputs/revision_jwm/corrected). Writes only to REV_DOCS;
earlier manuscript files are not touched. Supporting Information items are numbered
automatically in order of first citation from [[S:key]], [[F:key]] and [[A:key]] labels.

Usage (repo root): python 03_analysis/29_build_revised.py 04_outputs/runs/RUN_<id>
"""
import csv
import os
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
jwm_dir = ms_dir / "Journal of Wildlife Management"
out_dir = jwm_dir / os.environ.get("REV_DOCS", "Revision_Corrected")
rev = Path(os.environ.get("REV_OUT", "04_outputs/revision_jwm/corrected"))
led_dir = rev / "ledger"
num_dir = rev / "jwm"
rev_t = rev / "tables"
fig_work = num_dir / "figures"
fig_out = out_dir / "Figures_for_upload"
tab_dir = run / "08_figures_tables"
TITLE = "Most modeled Asian elephant habitat in Bhutan lies outside protected areas, with uncertain climate refugia"
DATE = "6 November 2026"


def rows(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


L = {r["key"]: r["value"] for r in rows(led_dir / "manuscript_claim_ledger.csv")}
if {r["run_id"] for r in rows(led_dir / "manuscript_claim_ledger.csv")} != {run.name}:
    sys.exit("Ledger run does not match " + run.name)
L.update({r["key"]: r["value"] for r in rows(num_dir / "jwm_extra_numbers.csv")})
L.update({r["key"]: r["value"] for r in rows(rev / "revision_numbers_stage2.csv")})
L.update({r["key"]: r["value"] for r in rows(rev / "revision_numbers_stage2b.csv")})
L.update({r["key"]: r["value"] for r in rows(rev / "revision_numbers_limit.csv")})


def num(k):
    return float(L[k])


occ = rows(Path("01_data_raw/01_occurrences/elephant_PA_data.csv"))
L["n_stations"] = str(len(occ))
L["n_presences"] = str(sum(r["presence"] == "1" for r in occ))
L["n_absences"] = str(sum(r["presence"] == "0" for r in occ))
L["prevalence"] = f"{int(L['n_presences']) / int(L['n_stations']):.3f}"
L["present_suitable_outside_pa_pct"] = f"{100 - num('present_suitable_in_pa_pct'):.1f}"
L["present_suitable_outside_pa_km2"] = str(round(num("present_suitable_km2") - num("present_suitable_in_pa_km2")))
L["refugia_core_outside_pa_pct"] = f"{100 - num('refugia_core_in_pa_pct'):.1f}"
changes = [num(k) for k in L if k.startswith("future_change_pct_")]
L["future_mean_change_min"] = f"{min(changes):.1f}"
L["future_mean_change_max"] = f"{max(changes):.1f}"
L["run_id"] = run.name
L["DATE"] = DATE
L["TITLE"] = TITLE
L["novelty_ref_n"] = rows(rev_t / "novelty_calibration_ranges.csv")[0]["n"]
for k in [k for k in L if k.startswith("boot_") or k.endswith("_ci")]:
    L[k] = L[k].replace("-", "–")
L["occu_false_nondetect_pct"] = f"{100 * num('occu_false_nondetect_share'):.1f}"
L["landcover_main_n"] = re.search(r"1:(\d+)", L["landcover_class_values"]).group(1)
dist_counts = dict(p.split(":") for p in L["hwc_by_district"].split())
L["hwc_top2_pct"] = f"{100 * (int(dist_counts['Sarpang']) + int(dist_counts['Samdrupjongkhar'])) / int(L['hwc_inside_bhutan']):.1f}"
cv = rows(rev_t / "cv_block_size_sensitivity.csv")
for r in cv:
    for k, v in r.items():
        L[f"cv{r['block_km']}_{k}"] = f"{float(v):.3f}" if k not in ("nearest_train_lt5km_pct", "width_km", "height_km") else f"{float(v):.1f}"
L["cv_auc_min"], L["cv_auc_max"] = f"{min(float(r['auc']) for r in cv):.3f}", f"{max(float(r['auc']) for r in cv):.3f}"
L["cv_nested_min"], L["cv_nested_max"] = f"{min(float(r['tss_nested']) for r in cv):.3f}", f"{max(float(r['tss_nested']) for r in cv):.3f}"
buf = rows(rev_t / "cv_buffered.csv")
for r in buf:
    for k, v in r.items():
        L[f"buf{int(float(r['buffer_km']))}_{k}"] = f"{float(v):.3f}"
mb = {r["band_km"]: r["moran"] for r in rows(rev_t / "residual_moran_by_distance.csv")}
L["moran_0_5"], L["moran_5_10"] = mb["0-5"], mb["5-10"]
thr_t = rows(rev_t / "threshold_refugia_sensitivity.csv")
tags = {"0.1": "thr010", "0.15": "thr015", "0.2": "thr020"}
for r in thr_t:
    tag = tags.get(r["threshold"], "thr0")
    for k, v in r.items():
        L[f"{tag}_{k}"] = f"{float(v):.1f}" if ("pct" in k or "jaccard" not in k and "threshold" not in k and "km2" not in k) else (f"{float(v):.3f}" if "jaccard" in k else str(round(float(v))))
VARIANTS = [("no_dung", "Without dung records"), ("tiger_only", "National Tiger Survey only"), ("time_aligned", "Without time-mismatched layers"),
            ("no_human", "Without human-related predictors"), ("no_dist_pa", "Without distance to protected areas")]
var = {n: rows(rev_t / f"variant_{n}.csv")[0] for n, _ in VARIANTS}
for n, r in var.items():
    for k, v in r.items():
        if v in ("", "NA") or k == "variant":
            continue
        x = float(v)
        L[f"var_{n}_{k}"] = (str(round(x)) if ("km2" in k or k in ("stations", "presences", "predictors", "n_loss")) else
                             f"{x:.1f}" if ("pct" in k or "change" in k) else f"{x:.3f}")
L["var_no_human_overlap_pct"] = f"{100 * float(var['no_human']['exposure_top15_overlap']):.1f}"
vo = [float(r["habitat_outside_pa_pct"]) for r in var.values()]
vc = [float(r["core_outside_pa_pct"]) for r in var.values()]
vk = [float(r["core_km2"]) for r in var.values()]
vn = [float(r["n_loss"]) for r in var.values()]
vr = [float(r["primary_core_retained_pct"]) for r in var.values()]
to = [float(r["habitat_outside_pa_pct"]) for r in thr_t]; tc = [float(r["core_outside_pa_pct"]) for r in thr_t]
lo, lc = float(L["limit_detect_habitat_outside_pct"]), float(L["limit_detect_core_outside_pct"])
L["thr_outside_min"], L["thr_outside_max"] = f"{min(to):.1f}", f"{max(to):.1f}"
L["var_outside_min"], L["var_outside_max"] = f"{min(vo):.1f}", f"{max(vo):.1f}"
L["habitat_outside_min"], L["habitat_outside_max"] = f"{min(to + vo + [lo]):.1f}", f"{max(to + vo + [lo]):.1f}"
L["core_outside_min"], L["core_outside_max"] = f"{min(tc + vc + [lc]):.1f}", f"{max(tc + vc + [lc]):.1f}"
L["var_core_min"], L["var_core_max"] = str(round(min(vk))), str(round(max(vk)))
L["var_core_out_min"], L["var_core_out_max"] = f"{min(vc):.1f}", f"{max(vc):.1f}"
L["var_retained_min"], L["var_retained_max"] = f"{min(vr):.1f}", f"{max(vr):.1f}"
L["var_nloss_min"], L["var_nloss_max"] = str(round(min(vn))), str(round(max(vn)))
lu = {r["class"]: r for r in rows(rev_t / "land_use_in_habitat_2020.csv")}
L["lu_forest_habitat"] = f"{float(lu['Forests']['habitat_pct']):.1f}"; L["lu_agri_habitat"] = f"{float(lu['Agriculture Land']['habitat_pct']):.1f}"
L["lu_forest_outside"] = f"{float(lu['Forests']['outside_pa_habitat_pct']):.1f}"; L["lu_agri_outside"] = f"{float(lu['Agriculture Land']['outside_pa_habitat_pct']):.1f}"
cr = rows(rev_t / "conflict_reference_comparison_corrected.csv")
def crv(ref, score):
    return next(r["auc"] for r in cr if r["reference"].startswith(ref) and r["score"] == score)
L["cr_all_index"], L["cr_all_suit"] = crv("All modeled", "Exposure index"), crv("All modeled", "Suitability")
L["cr_foothill_index"] = crv("Within 1 km of a building, at or below", "Exposure index"); L["cr_foothill_suit"] = crv("Within 1 km of a building, at or below", "Suitability")
L["cr_agri_index"], L["cr_agri_suit"] = crv("Agriculture", "Exposure index"), crv("Agriculture", "Suitability")


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


ONE_DEC = re.compile(r"pct|^(habitat|core|thr|var|var_core)_(outside|out)_(min|max)$|^var_core_out_|^var_retained_|^lu_")
THREE_DEC = re.compile(r"^(cr|pd|boot|occu)_")


def show(k):
    """Keep the decimals the source reports, which R drops from round numbers."""
    v = L[k]
    if ONE_DEC.search(k):
        try:
            return f"{float(v):.1f}".replace("-", "−", 1)
        except ValueError:
            pass
    if THREE_DEC.search(k) and "." in v and "share" not in k:
        return fmt(re.sub(r"\b\d\.\d+", lambda n: f"{float(n.group(0)):.3f}", v))
    return fmt(v)


def fill(text):
    missing = sorted(set(k for k in re.findall(r"\{\{(\w+)\}\}", text) if k not in L))
    if missing:
        sys.exit("Missing ledger keys: " + ", ".join(missing))
    text = re.sub(r"\{\{(\w+)\}\}", lambda m: L[m.group(1)] if ("year" in m.group(1) or m.group(1) == "threshold_exact") else show(m.group(1)), text)
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
           f"Table 1. Spatially blocked cross-validation performance of models predicting recorded presence of {PLACE}, "
           f"from {fmt(L['n_stations'])} stations of the {SURVEYS}. Values for AUC, TSS, and Brier score of individual algorithms "
           f"are means ± SD across 5 folds (15-km blocks), with TSS at each fold's optimum; other values use pooled held-out predictions. {AUC_DEF}.",
           [f"^a^Equal-weight mean of the 3 algorithms, calculated on pooled held-out predictions, with TSS at the threshold tuned on those predictions ({L['threshold']}). Nested TSS = {L['cv15_tss_nested']}."])

T2rows = [("All stations", "all"), ("All stations, dung stations dropped from scoring", "no_dung"),
          ("National Tiger Survey stations", "tiger"), ("National Elephant Survey stations", "elephant"),
          (f"Stations at or below highest detection ({fmt(L['station_elev_max_presence_m'])} m)", "below_max_presence"),
          ("Stations ≤1,000 m", "below_1000m")]
BK = {"all": "all", "tiger": "tiger", "elephant": "elephant", "below_max_presence": "within", "below_1000m": "below1000"}
T2 = table(["Stations scored", "Stations", "Recorded presences", "Presence frequency", "AUC", "95% CI^c^", "TSS at threshold^a^", "Best TSS^b^"],
           [[n, fmt(L[f"eval_{k}_n"]), L[f"eval_{k}_presences"], L[f"eval_{k}_prevalence"], L[f"eval_{k}_auc"], L.get("boot_auc_" + BK.get(k, ""), "–"),
             L[f"eval_{k}_tss_at_threshold"], L[f"eval_{k}_tss_best"]] for n, k in T2rows],
           f"Table 2. Discrimination of the equal-weight ensemble for {PLACE} on subsets of stations from the {SURVEYS}, "
           f"using the same pooled, spatially blocked held-out predictions in every row. {AUC_DEF}.",
           [f"^a^Threshold = {L['threshold']}, set on all stations.", "^b^TSS at the threshold that is optimal for that subset; this is optimistic.", "^c^From 2,000 bootstrap resamples of 15-km blocks."])

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
           f"Table 4. Climate-driven projections of classified suitable area for {PLACE} under 4 shared socioeconomic pathways (SSP) and 3 periods, 2021–2100, with non-climatic predictors held at present values. "
           f"GCM = general circulation model.",
           [f"Present suitable habitat = {fmt(L['present_suitable_km2'])} km². The GCM-mean map averages the 5 per-GCM ensemble "
            f"projections before thresholding; the median and range are taken over the 5 per-GCM habitat areas."])

T4 = table(["Category", "Definition", "Area (km²)", "% of modeled habitat", "% inside network", "Outside network (km²)",
            "% in top quarter of human pressure"],
           [["Modeled habitat", f"Ensemble score ≥{L['threshold']}", fmt(L["present_suitable_km2"]), "100",
             show("present_suitable_in_pa_pct"), fmt(L["present_suitable_outside_pa_km2"]), show("hp_habitat_top25_pct")],
            ["High-stability area", "Suitable in ≥45 of 60 projections", fmt(L["refugia_high_km2"]), show("refugia_high_pct_present"),
             show("refugia_high_in_pa_pct"), fmt(L["refugia_high_outside_pa_km2"]), "–"],
            ["Candidate core refugia", "Suitable in all 60 projections", fmt(L["refugia_core_km2"]), show("refugia_core_pct_present"),
             show("refugia_core_in_pa_pct"), fmt(L["refugia_core_outside_pa_km2"]), show("hp_core_top25_pct")],
            ["Core refugia within sampled climate", "Also never extrapolated in the 60 projections", fmt(L["core_never_extrap_km2"]),
             f"{100 * num('core_never_extrap_km2') / num('present_suitable_km2'):.1f}", f"{100 - num('core_never_extrap_outside_pa_pct'):.1f}", fmt(L["core_never_extrap_outside_pa_km2"]), "–"]],
           f"Table 5. Modeled habitat and candidate climate refugia for {PLACE}, with coverage by the protected-area network and overlap "
           f"with human pressure. Projections are 5 general circulation models × 4 pathways × 3 periods, 2021–2100.",
           ["The network comprises national parks, wildlife sanctuaries, the strict nature reserve, and biological corridors."])

r5 = [["Primary model", L["threshold"], fmt(L["present_suitable_km2"]), show("present_suitable_outside_pa_pct"), fmt(L["refugia_core_km2"]), show("refugia_core_outside_pa_pct"), "100", L["per_gcm_n_projections_loss"]]]
for tag, lab in [("thr010", "0.10"), ("thr015", "0.15"), ("thr020", "0.20")]:
    r5.append([f"Threshold {lab}", lab, fmt(L[f"{tag}_habitat_km2"]), L[f"{tag}_habitat_outside_pa_pct"], fmt(L[f"{tag}_core_km2"]), L[f"{tag}_core_outside_pa_pct"], L[f"{tag}_primary_core_retained_pct"], "–"])
for n, lab in VARIANTS:
    r5.append([lab, L[f"var_{n}_threshold"], fmt(L[f"var_{n}_habitat_km2"]), L[f"var_{n}_habitat_outside_pa_pct"], fmt(L[f"var_{n}_core_km2"]), L[f"var_{n}_core_outside_pa_pct"], L[f"var_{n}_primary_core_retained_pct"], L[f"var_{n}_n_loss"]])
r5.append([f"Maps limited to highest detection ({fmt(L['presence_elevation_max_m'])} m)", L["threshold"], fmt(L["limit_detect_habitat_km2"]), show("limit_detect_habitat_outside_pct"), fmt(L["limit_detect_core_km2"]), show("limit_detect_core_outside_pct"), "–", "–"])
T5 = table(["Analysis", "Threshold", "Modeled habitat (km²)", "% outside network", "Core refugia (km²)", "% of core outside network", "% of primary core retained", "Projections with net loss (of 60)"], r5,
           f"Table 3. Robustness of modeled habitat and candidate core refugia for {PLACE} to the classification threshold and to data and predictor choices. Each refit used its own threshold and all 60 projections.")

SPAN = f"{SURVEYS}"
FIGS = [
    ("main_figure_01_spatial_cv", "Figure_1_study_area_cross_validation",
     f"Study area and spatial cross-validation design for Asian elephants in Bhutan. Points are stations of the {SPAN} (filled circles, recorded presences; open gray circles, non-detections). Shaded squares are the 15-km blocks used for 5-fold spatial cross-validation, colored by fold."),
    ("Figure_2_predictor_importance", "Figure_2_predictor_importance",
     f"Leading predictors of recorded Asian elephant presence in Bhutan for each algorithm in the ensemble, fitted to {fmt(L['n_stations'])} stations surveyed in 2016 and 2021–2022. Importance is rescaled within each algorithm to a maximum of 100 (generalized linear model, absolute coefficient × predictor standard deviation; random forest, permutation importance; boosted regression trees, relative influence) and is not comparable across algorithms."),
    ("Figure_3_response_curves", "Figure_3_response_curves",
     f"Partial dependence of the predicted probability of recorded Asian elephant presence on 6 leading predictors in Bhutan, from {fmt(L['n_stations'])} stations surveyed in 2016 and 2021–2022. Lines show the mean prediction across all stations when the predictor is set to each value, for each algorithm and their equal-weight ensemble. Axes span the 2nd–98th percentiles of station values. Precipitation seasonality is the coefficient of variation of monthly precipitation."),
    ("main_figure_03_present_suitability", "Figure_4_present_suitability",
     f"Present suitability score for Asian elephants in Bhutan (equal-weight ensemble of generalized linear model, random forest, and boosted regression trees; 1986–2015 climate). Yellow lines are protected-area and biological-corridor boundaries; blank cells lie outside the modeled domain, which has complete predictor data at or below 3,000 m. The classification threshold is {L['threshold']}. Scores represent recorded presence under the survey designs, not occupancy. Projection: World Geodetic System 1984, Universal Transverse Mercator zone 45N."),
    ("main_figure_04_future_habitat_area", "Figure_5_future_habitat_area",
     f"Climate-driven projections of classified suitable area for Asian elephants in Bhutan by shared socioeconomic pathway (SSP) and period, 2021–2100, with non-climatic predictors held at present values. Colored lines are the 5 general circulation models (GCM); the black line is the median across GCMs; the dotted line is the present area ({fmt(L['present_suitable_km2'])} km²)."),
    ("main_figure_05_refugia_stability", "Figure_6_refugia_stability",
     "Stability of present modeled Asian elephant habitat in Bhutan across 60 future projections (5 general circulation models × 4 shared socioeconomic pathways × 3 periods, 2021–2100): the fraction of projections in which each currently suitable cell remains suitable. Cells with value 1 are candidate core refugia. Green lines are protected-area and biological-corridor boundaries."),
    ("main_figure_06_interaction_exposure", "Figure_7_interaction_exposure",
     f"Potential human–elephant interaction exposure in Bhutan at present: the geometric mean of the rescaled suitability score and human pressure. The index describes co-occurrence and is not a fitted or validated conflict-risk model. White points are {L['hwc_inside_bhutan']} geolocated elephant-conflict incidents recorded opportunistically in the Spatial Monitoring and Reporting Tool by the Department of Forests and Park Services in {L['hwc_year_min_domain']}–{L['hwc_year_max_domain']}, shown for context only. Gray lines are district (Dzongkhag) boundaries; green lines are protected areas."),
]

ms = fill((src / "manuscript_jwm_corrected.md").read_text(encoding="utf-8"))
old_ms = (src / "manuscript_jwm.md").read_text(encoding="utf-8")
refs_block = old_ms.split("# REFERENCES", 1)[1]
refs_block = refs_block.replace("Hao, T., J. Elith,", "Fiske, I., and R. Chandler. 2011. unmarked: an R package for fitting hierarchical models of wildlife occurrence and abundance. Journal of Statistical Software 43(10):1–23.\n\nHao, T., J. Elith,", 1)
refs_block = refs_block.replace("Morelli, T. L., C. W. Barrows,", "MacKenzie, D. I., J. D. Nichols, G. B. Lachman, S. Droege, J. A. Royle, and C. A. Langtimm. 2002. Estimating site occupancy rates when detection probabilities are less than one. Ecology 83:2248–2255.\n\nMorelli, T. L., C. W. Barrows,", 1)
ms = ms + refs_block

labels = re.findall(r"\[\[(S|F|A):(\w+)\]\]", ms)
s_order, f_order, a_order = [], [], []
for kind, key in labels:
    bucket = {"S": s_order, "F": f_order, "A": a_order}[kind]
    if key not in bucket:
        bucket.append(key)
SN = {k: f"Table S{i}" for i, k in enumerate(s_order, 1)}
FN = {k: f"Figure S{i}" for i, k in enumerate(f_order, 1)}
AN = {k: f"Appendix S{i}" for i, k in enumerate(a_order, 1)}
seen = set()
def si_ref(m):
    name = {"S": SN, "F": FN, "A": AN}[m.group(1)][m.group(2)]
    first = name not in seen
    seen.add(name)
    return name + (", available in Supporting Information" if first else "")
ms = re.sub(r"\[\[(S|F|A):(\w+)\]\]", si_ref, ms)
ms = re.sub(r"Table S(\d+), available in Supporting Information; Table S(\d+), available in Supporting Information", r"Tables S\1 and S\2, available in Supporting Information", ms)
head, body_main = ms.split("The Asian elephant (*Elephas maximus*) is listed", 1)
ms = '::: {custom-style="Front"}\n' + head + "\n:::\n\nThe Asian elephant (*Elephas maximus*) is listed" + body_main
caps = "\n\n".join(f"Figure {i}. {f[2]}" for i, f in enumerate(FIGS, 1))
ms += "\n" + PAGE + '\n::: {custom-style="Heading 1"}\nFigure Captions\n:::\n\n::: {custom-style="Front"}\n' + caps + "\n:::\n"
ms += "\n" + T1 + "\n" + T2 + "\n" + T5 + "\n" + T3 + "\n" + T4
(src / "manuscript_jwm_corrected_filled.md").write_text(ms, encoding="utf-8")

# ── Figures for upload ───────────────────────────────────────────────────
fig_out.mkdir(parents=True, exist_ok=True)
Image.MAX_IMAGE_PIXELS = None
for stem, name, _ in FIGS:
    p = next(q for q in (rev / "figures" / f"{stem}.png", rev / "figures" / f"{name}.png", fig_work / f"{stem}.png", tab_dir / f"{stem}.png") if q.exists())
    Image.open(p).convert("RGB").save(fig_out / f"{name}.tif", compression="tiff_lzw", dpi=(600, 600))
im = Image.open(tab_dir / "figure_s10_mess_map.png").convert("RGB")
fig_s1 = rev / "figures" / "Figure_novelty.png"
im.crop((0, 540, im.width, 1750)).save(fig_s1, dpi=(300, 300))

# ── Supporting Information ───────────────────────────────────────────────
S = ['::: {custom-style="SIHeading"}\nSupporting Information\n:::', DATE,
     f"Wangdi, and L. Sagar. 2026. {TITLE}. Journal of Wildlife Management.",
     f"Results derive from primary analysis run `{run.name}` and sensitivity analyses that read its outputs without altering them. Recorded presence means an elephant detection or dung record at a station; non-detection means an active camera with no detection and is not assumed to be a true absence."]

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
    "Table S1. Records of Asian elephants in Bhutan from 2 national surveys and exclusions before modeling."))

t = rows(tab_dir / "table_02_predictor_selection.csv")
grp = {"Climate (BIO)": "Climate", "Vegetation/LULC": "Vegetation and land cover", "Anthropogenic": "Human pressure"}
S.append(table(["Predictor", "Group", "Status", "Reason for removal"],
               [[plain(r["Predictor"]), grp.get(r["Category"], r["Category"]), r["Status"],
                 plain_text(r["Removal_reason"] or "–") if r["Status"] != "Selected" else "–"] for r in t],
               "Table S2. Candidate predictors of recorded Asian elephant presence in Bhutan and automated collinearity screening.",
               ["Step 1 examined pairs with |*r*| > 0.85 iteratively and removed the member with the higher mean absolute correlation with all remaining predictors, so a listed partner may itself have been removed later. Step 2 removed all predictors with a variance inflation factor (VIF) >10 in a single pass. Correlations used station values."]))

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
               [f"Areas use the threshold rounded to 3 decimals. The main text uses the exact threshold ({L['threshold_exact']}), which gives {L['area_at_exact_threshold_km2']} km²; rounding it to {L['threshold']} gives {L['area_at_rounded_threshold_km2']} km²."]))

t = rows(run / "05_change_metrics" / "habitat_area_change_summary.csv")
body = []
for r in t:
    m = re.match(r"ensemble_(ssp\d+)_(\d{4}_\d{4})", r["scenario"])
    body.append([SSP[m.group(1)], PER[m.group(2)], fmt(round(float(r["gain_km2"]))), fmt(round(float(r["loss_km2"]))), fmt(round(float(r["pers_km2"])))])
S.append(table(["Pathway", "Period", "Gain (km²)", "Loss (km²)", "Persistence (km²)"], body,
               "Table S6. Gain, loss, and persistence of classified suitable area for Asian elephants in Bhutan relative to the present, from the general circulation model-mean ensemble map, 2021–2100, with non-climatic predictors held constant."))

vi = rows(tab_dir / "table_S1_variable_importance.csv")
preds = []
for r in vi:
    if r["Algorithm"] in ("GLM", "RF", "BRT") and r["Predictor"] not in preds:
        preds.append(r["Predictor"])
get = {(r["Algorithm"], r["Predictor"]): r["Relative_importance_pct"] for r in vi}
preds.sort(key=lambda p: -sum(float(get.get((a, p), 0)) for a in ("GLM", "RF", "BRT")))
S.append(table(["Predictor", "Generalized linear model", "Random forest", "Boosted regression trees"],
               [[plain(p)] + [c(get.get((a, p), "nan"), 1) for a in ("GLM", "RF", "BRT")] for p in preds],
               "Table S7. Relative importance of predictors of recorded Asian elephant presence in Bhutan within each algorithm (maximum = 100)."))

t = rows(tab_dir / "table_19_predictor_statistics.csv")
S.append(table(["Predictor", "Presences: median", "Presences: mean ± SD", "Absences: median", "Absences: mean ± SD"],
               [[plain(r["Predictor"]), r["Pres_median"], f"{r['Pres_mean']} ± {r['Pres_SD']}", r["Bg_median"], f"{r['Bg_mean']} ± {r['Bg_SD']}"] for r in t],
               f"Table S8. Predictor values at Asian elephant presence and absence stations in Bhutan ({L['n_presences']} presences, {L['n_absences']} absences).",
               ["Distances are in meters, temperature in °C, and precipitation of the warmest quarter and driest month in mm; precipitation seasonality is a coefficient of variation (%)."]))

t = rows(tab_dir / "table_24_dzongkhag_suitability.csv")
S.append(table(["District (Dzongkhag)", "Modeled area (km²)", "Suitable (km²)", "% suitable", "Mean suitability"],
               [[r["Dzongkhag"], r["Total_area_km2"], r["Suitable_area_km2"], r["Pct_suitable"], r["Mean_suitability"]] for r in t if float(r["Suitable_area_km2"]) > 0],
               "Table S9. Present classified suitable area for Asian elephants by district in Bhutan (districts with any suitable area).",
               [f"Rows sum to {L['district_sum_km2']} km². The national total is {L['habitat_total_km2_exact']} km² because {L['habitat_outside_all_districts_km2']} km² of cells fall outside every district polygon along the national boundary; rows are also rounded to 0.1 km²."]))

t = rows(led_dir / "manuscript_claim_ledger_pa_units.csv")
S.append(table(["Protected area or corridor", "Modeled area (km²)", "Suitable habitat (km²)", "% suitable"],
               [[r["unit"], fmt(round(float(r["unit_km2"]))), fmt(round(float(r["suitable_km2"]))),
                 f"{100 * float(r['suitable_km2']) / float(r['unit_km2']):.1f}" if float(r["unit_km2"]) > 0 else "–"] for r in t],
               "Table S10. Present classified suitable area for Asian elephants by protected area and biological corridor in Bhutan.",
               [f"Unrounded rows sum to {L['pa_unit_sum_raw_km2']} km², matching the network union and the {fmt(L['present_suitable_in_pa_km2'])} km² in the main text. Rounded rows sum to {L['pa_unit_sum_rounded_rows_km2']} km²."]))

t = rows(rev_t / "exposure_by_scenario_corrected.csv")
S.append(table(["Pathway", "Period", "Mean exposure index"],
               [[r["pathway"], r["period"].replace("-", "–"), c(r["mean_exposure"])] for r in t],
               "Table S13. National mean potential human–elephant interaction-exposure index in Bhutan, present and future, with human pressure held at present values."))

t = rows(led_dir / "manuscript_claim_ledger_gcm_trajectories.csv")
body = []
for s in SSP:
    for p in PER:
        vals = {r["gcm"]: r for r in t if r["ssp"] == s and r["period"] == p}
        body.append([SSP[s], PER[p]] + [f"{fmt(round(float(vals[g]['area_km2'])))} ({signed(vals[g]['change_pct'])}%)" for g in GCM])
S.append(table(["Pathway", "Period"] + list(GCM.values()), body,
               "Table S12. Classified suitable area for Asian elephants in Bhutan (km²) and change from present (%) for each general circulation model, 2021–2100, with non-climatic predictors held constant."))

dp = "var_no_dist_pa_"
dpj = rows(rev_t / "variant_no_dist_pa_projections.csv")
dpm = sorted(float(r["change_pct"]) for r in dpj if r["ssp"] == "ssp585" and r["period"] == "2071_2100")[2]
body = [["Ensemble AUC (spatial held-out predictions)", L["ensemble_auc_mean"], L[dp + "auc"]],
        ["Ensemble TSS (spatial held-out predictions)", L["ensemble_tss_mean"], L[dp + "tss"]],
        ["Classification threshold", c(L["threshold"]), L[dp + "threshold"]],
        ["Present habitat (km²)", fmt(L["present_suitable_km2"]), fmt(L[dp + "habitat_km2"])],
        ["Present habitat outside network (%)", L["present_suitable_outside_pa_pct"], L[dp + "habitat_outside_pa_pct"]],
        ["Core refugia, suitable in 60 of 60 projections (km²)", fmt(L["refugia_core_km2"]), fmt(L[dp + "core_km2"])],
        ["Core refugia outside network (%)", L["refugia_core_outside_pa_pct"], L[dp + "core_outside_pa_pct"]],
        ["High-stability habitat, ≥45 of 60 (km²)", fmt(L["refugia_high_km2"]), fmt(L[dp + "high_km2"])],
        ["Projections with net loss (of 60)", L["per_gcm_n_projections_loss"], L[dp + "n_loss"]],
        ["Range of change across projections (%)", f"{fmt(L['per_gcm_change_pct_min'])} to +{fmt(L['per_gcm_change_pct_max'])}", f"{fmt(L[dp + 'change_min'])} to +{fmt(L[dp + 'change_max'])}"],
        ["Median across models, SSP5-8.5, 2071–2100 (%)", fmt(L["future_gcm_median_change_pct_ssp585_2071_2100"]), fmt(f"{dpm:.1f}")]]
S.append(table(["Quantity", "Primary model", "Without distance to protected areas"], body,
               f"Table S11. Sensitivity of the main results for Asian elephants in Bhutan to removing distance to protected areas as a predictor. {AUC_DEF}.",
               [f"The refit used the primary folds, settings, and seed, its own threshold, and all 60 projections. Agreement of present-day suitability maps: Pearson *r* = {L[dp + 'map_r']}, "
                f"Schoener's *D* = {L[dp + 'schoener_d']}; Jaccard overlap of habitat maps = {L[dp + 'habitat_jaccard']}."]))

OLD = {"S1": "records", "S2": "candidates", "S3": "settings", "S4": "folds", "S5": "thrarea", "S6": "gainloss", "S7": "importance",
       "S8": "stats", "S9": "districts", "S10": "units", "S11": "distpa", "S12": "gcm", "S13": "exposure"}
SI = {}
for s in S[4:]:
    m = re.search(r"Table (S\d+)\.", s)
    SI[OLD[m.group(1)]] = s

sv = rows(rev_t / "survey_comparison.csv")
for r in sv:
    if r["quantity"] == "Dung records":
        r["elephant"] = L["n_dung_presences"]
ext = [["Sampling year and season", "March–May 2016, some cameras left longer", "11 October 2021–31 January 2022 (north); 21 February–30 June 2022 (south)"],
       ["Purpose and placement", "Elephant survey; cameras in cells with elephant sign", "Tiger survey; cameras on trails and roads chosen for tiger captures"],
       ["Camera height (cm)", "About 100", "About 45"], ["Extent", "Southern districts", "National"]]
SI["survey"] = table(["Quantity", "National Elephant Survey", "National Tiger Survey"], ext + [[r["quantity"].replace(" (C)", " (°C)"), r["elephant"].replace("-", "–") if "range" in r["quantity"] else r["elephant"], r["tiger"].replace("-", "–") if "range" in r["quantity"] else r["tiger"]] for r in sv],
    "Table S0. Differences between the 2 national surveys of Asian elephants in Bhutan, for stations used in modeling.",
    [f"Elephant-survey effort is active occasions at camera stations in the supplied detection matrix. Tiger-survey effort uses trap-night values of 1–150; {L['tiger_effort_invalid_rows']} of {L['tiger_rows']} rows had missing or implausible values and were not used. Effort columns include stations outside the modeled set."])
SI["timing"] = table(["Data layer", "Period", "Relation to surveys", "Treatment"],
    [["Climate", "1986–2015", "Before both surveys", "Retained; a long-term baseline"], ["National Elephant Survey", "2016", "–", "Response"], ["National Tiger Survey", "2021–2022", "–", "Response"],
     ["Enhanced vegetation index", "2000–2024 mean", "Spans both surveys; includes later years", "Tested by removal"], ["Human Impact Index", "2020", "After elephant survey; before tiger survey", "Tested by removal"],
     ["Dynamic World land cover", "2025", "After both surveys", "Tested by removal"], ["Protected-area boundaries", "2023 version", "After both surveys", "Tested by removal of distance predictor"],
     ["Land use within habitat", "2020", "After elephant survey; before tiger survey", "Descriptive only"]],
    "Table S0. Timing of data layers relative to the National Elephant Survey (2016) and National Tiger Survey (2021–2022) of Asian elephants in Bhutan.",
    ["No earlier rasters of land cover or human pressure were available in the project to align predictors with each survey. Annual and monthly covariate files covered only 2022 at a subset of stations."])
pd_ = sorted(rows(rev_t / "retained_predictor_diagnostics.csv"), key=lambda r: -float(r["vif"]))
SI["diagnostics"] = table(["Predictor", "Variance inflation factor", "Largest |*r*|", "Most correlated with"],
    [[plain(r["predictor"]), r["vif"], r["max_abs_r"], plain(r["most_correlated"])] for r in pd_],
    "Table S0. Collinearity among the 18 predictors retained for models of recorded Asian elephant presence in Bhutan, recalculated from station values.")
SI["moranband"] = table(["Distance band (km)", "Station pairs", "Moran's *I*"], [[r["band_km"].replace("-", "–"), fmt(r["pairs"]), r["moran"]] for r in rows(rev_t / "residual_moran_by_distance.csv")],
    "Table S0. Residual spatial autocorrelation by distance band for held-out ensemble predictions of recorded Asian elephant presence in Bhutan (projected distances).")
SI["blocks"] = table(["Block size (km)", "Blocks", "AUC", "TSS, tuned", "TSS, cross-fitted", "TSS, nested", "AUC within range", "AUC, elephant survey", "Test stations with training station <5 km (%)"],
    [[r["block_km"], r["n_blocks"], c(r["auc"]), c(r["tss_pooled_opt"]), c(r["tss_crossfit"]), c(r["tss_nested"]), c(r["auc_within"]), c(r["auc_elephant"]), c(r["nearest_train_lt5km_pct"], 1)] for r in cv],
    f"Table S0. Discrimination of the equal-weight ensemble for recorded Asian elephant presence in Bhutan under alternative spatial block sizes. {AUC_DEF}.",
    ["TSS, tuned uses the threshold that maximizes TSS on the pooled held-out predictions. Cross-fitted thresholds come from the other folds' held-out predictions, and nested thresholds from inner cross-validation of training data only. Within range means at or below the highest detection. The 15-km row is a rerun; stochastic differences from the primary run are small."])
SI["blocks2"] = table(["Block size (km)", "Realized block (km)", "Nested threshold range", "Brier score", "Calibration slope", "Moran's *I*"],
    [[r["block_km"], f"{float(r['width_km']):.1f} × {float(r['height_km']):.1f}", f"{c(r['threshold_nested_min'])}–{c(r['threshold_nested_max'])}", c(r["brier"]), c(r["calibration_slope"], 2), c(r["moran5"])] for r in cv],
    "Table S0. Block dimensions, nested thresholds, calibration, and residual autocorrelation (5 nearest neighbors, projected distance) of the ensemble under alternative spatial block sizes, Asian elephant stations in Bhutan.")
SI["buffer"] = table(["Buffer (km)", "Mean training stations removed per fold", "AUC", "TSS, tuned", "TSS, cross-fitted", "AUC within range", "Brier score"],
    [[str(int(float(r["buffer_km"]))), r["mean_training_removed"], c(r["auc"]), c(r["tss_pooled_opt"]), c(r["tss_crossfit"]), c(r["auc_within"]), c(r["brier"])] for r in buf],
    f"Table S0. Effect of removing training stations near test stations on spatial cross-validation (15-km blocks) of the ensemble for recorded Asian elephant presence in Bhutan. {AUC_DEF}.",
    ["Buffering also reduces training data, so it gives a lower estimate of the skill of the full model."])
top_t = {r["threshold"]: r["core_top25_km2"] for r in rows(rev_t / "threshold_top25_corrected.csv")}
SI["threshold"] = table(["Threshold", "Habitat (km²)", "Habitat outside network (%)", "Core refugia (km²)", "Core outside network (%)", "Core outside network (km²)", "Core in top quarter of human pressure (km²)", "Jaccard overlap with primary core", "Core within sampled climate (km²)"],
    [[f"{float(r['threshold']):.3f}" if r["threshold"] not in tags else r["threshold"], fmt(round(float(r["habitat_km2"]))), c(r["habitat_outside_pa_pct"], 1), fmt(round(float(r["core_km2"]))), c(r["core_outside_pa_pct"], 1), fmt(round(float(r["core_outside_pa_km2"]))), fmt(round(float(top_t[r["threshold"]]))), c(r["core_jaccard_vs_primary"]), fmt(round(float(r["core_never_extrapolated_km2"])))] for r in thr_t],
    "Table S0. Sensitivity of modeled habitat and candidate core refugia for Asian elephants in Bhutan to the classification threshold, across all 60 projections.",
    ["Core within sampled climate means never extrapolated in the 60 projections."])
ex_t = rows(rev_t / "core_refugia_extrapolation_classes.csv")
SI["extrap"] = table(["Class", "Area (km²)", "% of core refugia", "% outside network"], [[r["class"].replace("1-30", "1–30").replace("31-59", "31–59"), fmt(round(float(r["km2"]))), c(r["pct_of_core"], 1), c(r["outside_pa_pct"], 1)] for r in ex_t],
    "Table S0. Candidate core refugia for Asian elephants in Bhutan by the number of the 60 projections in which each cell lay outside the climate sampled by the surveys.",
    ["Being outside sampled climate does not mean habitat is lost; it means the suitability prediction for that projection is an extrapolation."])
lt = sorted(rows(rev_t / "land_use_in_habitat_2020.csv"), key=lambda r: -float(r["habitat_km2"]))
SI["landuse"] = table(["Land-use class, 2020", "Modeled habitat (km²)", "% of modeled habitat", "% of modeled habitat outside network", "% of core refugia outside network"],
    [[r["class"], fmt(round(float(r["habitat_km2"]))), c(r["habitat_pct"], 1), c(r["outside_pa_habitat_pct"], 1), c(r["core_outside_pa_pct"], 1)] for r in lt],
    "Table S0. Land use within modeled Asian elephant habitat in Bhutan from area-weighted overlay of the national 2020 land-use layer.")
SI["conflict"] = table(["Reference cells", "Reference cells (n)", "Score", "AUC", "95% CI"], [[r["reference"], fmt(r["reference_cells"]), r["score"], r["auc"], f"{r['ci_low']}–{r['ci_high']}"] for r in cr],
    f"Table S0. Comparison of {L['hwc_inside_bhutan']} opportunistically recorded elephant-conflict incidents in Bhutan ({L['hwc_year_min_domain']}–{L['hwc_year_max_domain']}) with reference cells. AUC = probability that an incident cell scores higher than a random reference cell.",
    ["Intervals come from 2,000 bootstrap resamples of 5-km cells. Values near 0.5 mean no separation from the reference; values below 0.5 mean incidents score lower than the reference. Incidents are not a random sample of conflict, so these values are not measures of predictive accuracy."])

nc_t = rows(rev_t / "nonclimate_predictor_range.csv")
SI["nonclimate"] = table(["Predictor", "Station minimum", "Station maximum", "Modeled habitat beyond station range (%)", "Domain beyond station range (%)"],
    [[plain(r["predictor"]), fmt(f"{float(r['station_min']):.2f}"), fmt(f"{float(r['station_max']):.2f}"), c(r["habitat_outside_range_pct"], 1), c(r["domain_outside_range_pct"], 1)] for r in nc_t],
    "Table S0. Non-climatic predictors in modeled Asian elephant habitat in Bhutan compared with their range at surveyed stations.",
    ["Land-cover class is omitted because it is categorical. These values are not part of the climatic novelty score. Distances are in meters."])

RET = "; ".join(plain(p).lower() for p in sorted(r["predictor"] for r in pd_))
ref = lambda k: f"[[{k}]]"
odmap = [
    ["Overview", "Taxon; location; scale", "*Elephas maximus*; Bhutan (national); 250-m grid, World Geodetic System 1984, Universal Transverse Mercator zone 45N (EPSG:32645)"],
    ["Overview", "Objective", "Classify broad-scale environmental suitability from recorded presence; project climate-driven change to 2100 under CMIP6; identify candidate refugia and their protection"],
    ["Overview", "Model algorithms", "Binomial generalized linear model, random forest, boosted regression trees (equal-weight ensemble); MaxEnt sensitivity only"],
    ["Overview", "Conceptual model", "Scores estimate the probability of recorded presence under the survey designs; detection is not modeled, and scores are not occupancy, habitat quality, or conflict risk"],
    ["Data", "Response data", f"Recorded presence or non-detection at {fmt(L['n_stations'])} stations ({L['n_presences']} recorded presences including {L['n_dung_presences']} dung records; {L['n_absences']} non-detections), National Elephant Survey 2016 and National Tiger Survey 2021–2022; 26 records excluded"],
    ["Data", "Predictors", f"18 of 35 candidates retained after automated screening (pairwise |*r*| > 0.85, then variance inflation factor >10): {RET}"],
    ["Data", "Predictor timing", "Climate 1986–2015; enhanced vegetation index 2000–2024; Human Impact Index 2020; Dynamic World 2025; see survey timing table"],
    ["Data", "Known data issues", "Land-cover class coded numerically; aspect in degrees; polygon layers rasterized to every touched cell"],
    ["Data", "Future climate", "5 general circulation models (ACCESS-CM2, CNRM-CM6-1, INM-CM5-0, MIROC6, MPI-ESM1-2-LR) × 4 pathways × 3 periods = 60 projections"],
    ["Model", "Assumptions", "Equilibrium with climate; non-climatic predictors fixed at present values; national boundary as accessible area; non-detection treated as the 0 class but not as true absence"],
    ["Model", "Settings", "Equal weights fixed a priori; threshold = TSS-maximizing cutoff of pooled held-out ensemble predictions; algorithm settings tabulated; all stations used for fitting, with mapped outputs limited to ≤3,000 m"],
    ["Assessment", "Validation", f"5-fold spatial block cross-validation (15-km blocks; {L['fold_presence_min']}–{L['fold_presence_max']} presences per fold); block sizes 10–30 km; buffers 5–20 km; nested and cross-fitted thresholds; AUC, TSS, Boyce index, Brier score, calibration slope, Moran's *I*; evaluation by survey and within elephant range; block-bootstrap intervals"],
    ["Assessment", "Sensitivity", "Thresholds 0.10, 0.15, 0.20; maps limited to the highest detection; refits without dung records, tiger survey only, without time-mismatched layers, without human-related predictors, and without distance to protected areas"],
    ["Assessment", "Uncertainty", "Per-model areas and median; inter-model standard deviation maps; climatic novelty for every projection, with score ≤0 and 0 inside the sampled range"],
    ["Prediction", "Outputs", "Present suitability score; 60 per-model and 12 model-mean projections; refugia stability count (denominator 60); refugia classes by extrapolation"],
    ["Prediction", "Extrapolation", f"Climatic novelty reported, not masked: {L['present_suitable_extrap_per_projection_median_pct']}% of present modeled habitat outside the sampled climate range in the median projection; {L['core_never_extrap_pct']}% of core refugia within sampled climate in all 60. Non-climatic predictor ranges reported separately; {L['habitat_outside_nonclimate_range_pct']}% of modeled habitat beyond them"],
    ["Reproducibility", "Code and run", f"Scripted pipeline; primary run `{run.name}` with input hashes, package versions, and seeds in its manifest; sensitivity analyses in 03_analysis scripts 25–32"],
]
SI["odmap"] = table(["ODMAP section", "Element", "Description"], odmap,
               "Appendix S1. Overview, Data, Model, Assessment, and Prediction (ODMAP) protocol summary for species distribution models of Asian elephants in Bhutan (Zurell et al. 2020).")
refs = refs_block
zurell = re.search(r"^Zurell, D\..*$", refs, re.M).group(0)
mack = re.search(r"^MacKenzie, D\. I\..*$", refs, re.M).group(0)
missing = (set(SI) - {"odmap"}) ^ set(s_order)
if missing:
    sys.exit("Supporting tables not matched to citations: " + ", ".join(sorted(missing)))
out_s = S[:4]
for i, k in enumerate(s_order, 1):
    out_s.append(re.sub(r"Table S\d+\.", f"Table S{i}.", SI[k], count=1))
for k, ttl, file, cap in [("novelty", "", rev / "figures" / "Figure_novelty.png", "Climatic novelty for Asian elephant models in Bhutan relative to the calibration stations: mean of the score across 5 general circulation models, shared socioeconomic pathway 5-8.5, 2071–2100. The score is 0 where all 4 climate predictors lie within the sampled range; negative values give the distance outside that range in calibration standard deviations. The panel label MESS refers to this score. Bold lines are the national boundary; dashed lines are district boundaries."),
                          ("uncertainty", "", rev / "figures" / "Figure_S2_refugia_uncertainty.png", f"Uncertainty in candidate core refugia for Asian elephants in Bhutan across classification thresholds of 0.10, {L['threshold']}, 0.15, and 0.20 and 60 projections. Colors separate cells that are core at some thresholds only, core at all thresholds but outside sampled climate in ≥1 projection, and core at all thresholds and within sampled climate in all 60. Gray lines are protected-area and biological-corridor boundaries. Projection: World Geodetic System 1984, Universal Transverse Mercator zone 45N.")]:
    out_s.append(f"![]({file.as_posix()}){{width=6.5in}}\n\n{FN[k]}. {cap}\n")
out_s.append(SI["odmap"])
out_s.append('::: {custom-style="SIHeading"}\nReferences\n:::\n\n::: {custom-style="Reference"}\n' + zurell + "\n:::")
txt = "\n\n".join(out_s).replace(PAGE, "")
for a_, b_ in [("presence and absence stations", "recorded presence and non-detection stations"), ("presences and absences", "recorded presences and non-detections"), ("Absences", "Non-detections"), (" absences", " non-detections")]:
    txt = txt.replace(a_, b_)
txt = txt.replace("| Step | Records | Presences |", "| Step | Records | Recorded presences |")
(src / "supplement_jwm_corrected_filled.md").write_text(txt, encoding="utf-8")


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


outs = [(src / "manuscript_jwm_corrected_filled.md", out_dir / "Manuscript_JWM_Revised.docx", dict(spacing=2.0, line_numbers=True, header="Wangdi and Sagar")),
        (src / "supplement_jwm_corrected_filled.md", out_dir / "Supporting_Information_JWM_Revised.docx", dict(spacing=1.15, line_numbers=False, header=None, table_pt=8))]
L["n_si_tables"] = str(len(s_order))
L["ga_outside_share"] = f"{float(L['present_suitable_outside_pa_pct']):.0f}"
for name, target in [("cover_letter_jwm_corrected", "Cover_Letter_JWM.docx"), ("data_availability_jwm_corrected", "Data_Availability_Statement_JWM.docx"),
                     ("graphical_abstract_text_jwm_corrected", "Graphical_Abstract_Text_JWM.docx"), ("author_review_jwm_corrected", "READ_FIRST_Author_Review.docx")]:
    f = src / f"{name}.md"
    filled = src / f"{name}_filled.md"
    filled.write_text(fill(f.read_text(encoding="utf-8")), encoding="utf-8")
    outs.append((filled, out_dir / target, dict(spacing=1.15, line_numbers=False, header=None, table_pt=10)))
for stem in ("Graphical_abstract",):
    Image.open(fig_work / f"{stem}.png").convert("RGB").save(fig_out / f"{stem}.tif", compression="tiff_lzw", dpi=(600, 600))
    shutil.copy(fig_work / f"{stem}.pdf", fig_out / f"{stem}.pdf")
for md, out, opts in outs:
    pandoc(md, out)
    finish(out, **opts)
    print("Wrote", out)

# ── Guideline checks ─────────────────────────────────────────────────────
text = (src / "manuscript_jwm_corrected_filled.md").read_text(encoding="utf-8")
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
for w in re.findall(r"probability of detection|probability of occurrence|habitat quality(?! ?\\.)|conflict risk(?! model)|\\babsences?\\b", main):
    pass
bad = [w for w in re.findall(r"[^.]*\\babsences?\\b[^.]*\\.", main) if not re.search(r"pseudo-absences|true absences|proof of absence", w)]
problems += ["absence wording: " + b[:80] for b in bad]
if "[[" in text:
    problems.append("unresolved SI label")
if "{{" in text:
    problems.append("unfilled key")
for w in re.findall(r"2,0\d\d–2,\d{3}", text):
    problems.append("comma in year range: " + w)
print("Main text words:", len(main.split()))
print("Checks:", "OK" if not problems else "\n  " + "\n  ".join(problems))
