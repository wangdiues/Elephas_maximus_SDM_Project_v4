"""Build the manuscript and supplement (.docx) from the accepted run.

Every {{key}} in 05_manuscripts/source/manuscript.md is replaced from the
claim ledger written by 17_manuscript_numbers.R; a missing key is an error.
Tables are built from run CSVs; figures come from 05_manuscripts/figures.

Usage (repo root): python 03_analysis/19_build_manuscript.py 04_outputs/runs/RUN_<id>
"""
import csv
import re
import subprocess
import sys
from pathlib import Path

run = Path(sys.argv[1])
ms_dir = Path("05_manuscripts")
src = ms_dir / "source"
fig_dir = ms_dir / "figures"
tab_dir = run / "08_figures_tables"


def rows(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


L = {r["key"]: r["value"] for r in rows(ms_dir / "manuscript_claim_ledger.csv")}
run_ids = {r["run_id"] for r in rows(ms_dir / "manuscript_claim_ledger.csv")}
if run_ids != {run.name}:
    sys.exit(f"Ledger run {run_ids} does not match {run.name}")


def num(k):
    return float(L[k])


occ = rows(Path("01_data_raw/01_occurrences/elephant_PA_data.csv"))
L["n_stations"] = str(len(occ))
L["n_presences"] = str(sum(r["presence"] == "1" for r in occ))
L["n_absences"] = str(sum(r["presence"] == "0" for r in occ))
L["prevalence"] = f"{int(L['n_presences']) / int(L['n_stations']):.3f}"
L["run_id"] = run.name
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
mean_changes = [num(k) for k in L if k.startswith("future_change_pct_")]
L["future_mean_change_min"] = f"{min(mean_changes):.1f}"
L["future_mean_change_max"] = f"{max(mean_changes):.1f}"


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
    text = re.sub(r"\{\{(\w+)\}\}", lambda m: fmt(L[m.group(1)]), text)
    return text.replace("+-", "−").replace("%–-", "% to −")


PAGE = '\n```{=openxml}\n<w:p><w:r><w:br w:type="page"/></w:r></w:p>\n```\n'


def table(header, body, caption, note=None):
    out = [PAGE, f"**{caption}**", "", "| " + " | ".join(header) + " |", "|" + "|".join(["---"] * len(header)) + "|"]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in body]
    if note:
        out += ["", f"*Note:* {note}"]
    return "\n".join(out) + "\n"


SSP = {"ssp126": "SSP1-2.6", "ssp245": "SSP2-4.5", "ssp370": "SSP3-7.0", "ssp585": "SSP5-8.5"}
PER = {"2021_2050": "2021–2050", "2051_2080": "2051–2080", "2071_2100": "2071–2100"}
GCM = {"acces_cm2": "ACCESS-CM2", "cnrm_cm6_1": "CNRM-CM6-1", "inm_cm5_0": "INM-CM5-0",
       "miroc6": "MIROC6", "mpi_esm1_2_lr": "MPI-ESM1-2-LR"}


def c(v, d=3):
    return f"{float(v):.{d}f}"


# ── Main tables ────────────────────────────────────────────────────────────
t1 = {r["Algorithm"]: r for r in rows(tab_dir / "table_01_model_performance.csv")}
body = []
for a, name in [("GLM", "GLM"), ("RF", "Random forest"), ("BRT", "Boosted regression trees")]:
    r = t1[a]
    body.append([name, f"{c(r['AUC_fold_mean'])} ± {c(r['AUC_fold_SD'])}", f"{c(r['TSS_fold_mean'])} ± {c(r['TSS_fold_SD'])}",
                 c(r["Boyce"]), c(r["Brier_fold_mean"]), c(r["Calibration_slope"], 2), c(r["Morans_I"])])
body.append(["Ensemble (pooled OOF)", L["ensemble_auc_mean"], L["ensemble_tss_mean"], c(L["ensemble_boyce"]),
             c(L["ensemble_brier"]), c(L["ensemble_calibration_slope"], 2), c(L["ensemble_moran_i"])])
T1 = table(["Model", "AUC", "TSS", "Boyce", "Brier", "Calibration slope", "Moran's *I*"], body,
           "TABLE 1 Spatially blocked out-of-fold performance of the ensemble members and the equal-weight ensemble.",
           f"Algorithm AUC, TSS and Brier are means ± SD across five spatial folds (15 km blocks); Boyce, calibration slope "
           f"and Moran's *I* use pooled OOF predictions. Ensemble values are computed on the pooled OOF predictions. "
           f"Classification threshold = {L['threshold']} (TSS optimum of the ensemble). n = {L['n_stations']} stations.")

body = []
for s in SSP:
    for p in PER:
        k = f"{s}_{p}"
        body.append([SSP[s], PER[p], fmt(L[f"future_area_km2_{k}"]), f"{float(L[f'future_change_pct_{k}']):+.1f}".replace("-", "−"),
                     f"{float(L[f'future_gcm_median_change_pct_{k}']):+.1f}".replace("-", "−"),
                     f"{fmt(L[f'future_gcm_area_min_km2_{k}'])}–{fmt(L[f'future_gcm_area_max_km2_{k}'])}",
                     L[f"future_gcm_n_loss_{k}"]])
T2 = table(["Pathway", "Period", "GCM-mean map (km²)", "Change, GCM-mean (%)", "Change, GCM median (%)",
            "Range across GCMs (km²)", "GCMs with net loss (of 5)"], body,
           "TABLE 2 Projected suitable habitat under four emission pathways and three periods.",
           f"Present suitable habitat = {fmt(L['present_suitable_km2'])} km². The GCM-mean map averages the five per-GCM "
           f"ensemble projections before thresholding; the median and range are taken over the five per-GCM habitat areas.")

T3 = table(["Category", "Definition", "Area (km²)", "% of present habitat", "% inside PA network", "Outside PA network (km²)",
            "% in top quarter of human pressure"],
           [["Present habitat", f"Ensemble suitability ≥ {L['threshold']}", fmt(L["present_suitable_km2"]), "100",
             L["present_suitable_in_pa_pct"], fmt(L["present_suitable_outside_pa_km2"]), L["present_suitable_in_top25_human_pressure_pct"]],
            ["High-stability habitat", "Suitable in ≥ 45 of 60 projections", fmt(L["refugia_high_km2"]), L["refugia_high_pct_present"],
             L["refugia_high_in_pa_pct"], fmt(L["refugia_high_outside_pa_km2"]), "–"],
            ["Core refugia", "Suitable in all 60 projections", fmt(L["refugia_core_km2"]), L["refugia_core_pct_present"],
             L["refugia_core_in_pa_pct"], fmt(L["refugia_core_outside_pa_km2"]), L["refugia_core_in_top25_human_pressure_pct"]]],
           "TABLE 3 Present habitat and climate refugia, protected-area coverage and overlap with human pressure.",
           "Projections: 5 GCMs × 4 SSPs × 3 periods. PA network = national parks, wildlife sanctuaries, strict nature "
           "reserve and biological corridors (DoFPS 2023 boundaries).")

FIGS = [
    ("main_figure_01_spatial_cv", "Study area and spatial cross-validation design. Survey stations (filled circles, presences; open grey circles, absences) and the 15 km blocks used for five-fold spatial cross-validation, coloured by fold. Whole blocks were assigned to folds so that each fold held a similar share of presences and of absences."),
    ("main_figure_02_variable_importance", "Leading predictors of elephant detection in each ensemble algorithm. Importance is rescaled within each algorithm to a maximum of 100 (GLM, absolute coefficient × predictor SD; RF, permutation importance; BRT, relative influence) and is not comparable across algorithms."),
    ("main_figure_03_present_suitability", f"Present habitat suitability for Asian elephants in Bhutan (equal-weight ensemble of GLM, RF and BRT; 1986–2015 climate). Yellow lines are protected-area and biological-corridor boundaries; blank cells lie outside the modelled domain. The habitat threshold is {L['threshold']}."),
    ("main_figure_04_future_habitat_area", f"Projected suitable habitat by emission pathway and period. Coloured lines are the five GCMs; the black line is the median across GCMs; the dotted line is present habitat ({fmt(L['present_suitable_km2'])} km²). Per-GCM areas are given in Table S12."),
    ("main_figure_05_refugia_stability", "Stability of present habitat across the 60 future projections (5 GCMs × 4 SSPs × 3 periods): the fraction of projections in which each currently suitable cell remains suitable. Cells with value 1 are core refugia. Green lines are protected-area and biological-corridor boundaries."),
    ("main_figure_06_interaction_exposure", f"Potential human–elephant interaction exposure at present: the geometric mean of rescaled habitat suitability and human pressure. The index describes co-occurrence and is not a fitted or validated conflict-risk model. White points are {L['hwc_incidents_in_domain']} geolocated elephant-conflict incidents ({L['hwc_year_min']}–{L['hwc_year_max']}; DoFPS), shown for context only. Grey lines are Dzongkhag boundaries; green lines are protected areas."),
]

ms = fill((src / "manuscript.md").read_text(encoding="utf-8"))
ms += "\n" + T1 + "\n" + T2 + "\n" + T3
ms += "\n" + PAGE + "\n# FIGURE LEGENDS\n\n" + "\n\n".join(f"**FIGURE {i}** {cap}" for i, (_, cap) in enumerate(FIGS, 1))
ms += "\n" + PAGE + PAGE.join(
    f"![]({(fig_dir / (f + '.png')).as_posix()}){{width=6.5in}}\n\n**FIGURE {i}**" for i, (f, _) in enumerate(FIGS, 1))
(src / "manuscript_filled.md").write_text(ms, encoding="utf-8")

# ── Supplement ─────────────────────────────────────────────────────────────
S = ['---\ntitle: "Supporting Information"\n---',
     "**Most climate-stable habitat of Asian elephants in Bhutan lies outside protected areas**",
     f"Wangdi and Laxmi Sagar. All tables and figures derive from accepted run `{run.name}`."]

exc = rows(Path("01_data_raw/01_occurrences/occurrence_exclusions.csv"))
n_out = [r for r in exc if r["exclusion_reason"] == "outside_bhutan_boundary"]
n_unv = [r for r in exc if r["exclusion_reason"] == "unverified_coordinate"]
S.append(table(["Step", "Records", "Presences", "Absences"], [
    ["Merged and de-duplicated survey stations", len(occ) + len(exc), sum(r["presence"] == "1" for r in occ) + sum(r["presence"] == "1" for r in exc),
     sum(r["presence"] == "0" for r in occ) + sum(r["presence"] == "0" for r in exc)],
    ["Excluded: outside national boundary", len(n_out), sum(r["presence"] == "1" for r in n_out), sum(r["presence"] == "0" for r in n_out)],
    ["Excluded: unverified coordinates", len(n_unv), sum(r["presence"] == "1" for r in n_unv), sum(r["presence"] == "0" for r in n_unv)],
    ["Modelled", L["n_stations"], L["n_presences"], L["n_absences"]]],
    "Table S1. Occurrence records from the national camera-trap surveys and exclusions before modelling."))

t2 = rows(tab_dir / "table_02_predictor_selection.csv")
S.append(table(["Predictor", "Group", "Status", "Reason for removal"],
               [[r["Predictor"], r["Category"], r["Status"], (r["Removal_reason"] or "–") if r["Status"] != "Selected" else "–"] for r in t2],
               "Table S2. Candidate predictors and collinearity screening (|r| ≥ 0.85 or VIF ≥ 10)."))

t20 = rows(tab_dir / "table_20_algorithm_hyperparams.csv")
S.append(table(["Algorithm", "Package", "Settings", "Role"],
               [[r["Algorithm"], r["Package"], r["Key_parameters"], "Sensitivity model only" if r["Algorithm"] == "MaxEnt" else "Ensemble member"] for r in t20],
               "Table S3. Algorithm settings."))

blk = rows(run / "01_processed_data" / "spatial_block_design_sensitivity.csv")
S.append(table(["Block size (km)", "Presences per fold", "Absences per fold", "Primary design"],
               [[r["block_size_km"], f"{r['min_presences']}–{r['max_presences']}", f"{r['min_reference']}–{r['max_reference']}",
                 "Yes" if r["selected_primary"] == "TRUE" else "No"] for r in blk],
               "Table S4. Fold composition for alternative block sizes (five folds, whole-block assignment balancing presences and absences)."))

t16 = rows(tab_dir / "table_16_threshold_sensitivity.csv")
S.append(table(["Threshold", "Suitable area (km²)", "% of domain"],
               [[r["Threshold"] + (" (primary, rounded)" if r["Is_current"] == "TRUE" else ""), fmt(r["Area_km2"]), r["Pct_suitable"]] for r in t16],
               "Table S5. Sensitivity of present habitat area to the classification threshold.",
               "Areas are computed at the threshold rounded to three decimals and may differ slightly from the main text."))

gl = rows(run / "05_change_metrics" / "habitat_area_change_summary.csv")
body = []
for r in gl:
    m = re.match(r"ensemble_(ssp\d+)_(\d{4}_\d{4})", r["scenario"])
    body.append([SSP[m.group(1)], PER[m.group(2)], fmt(round(float(r["gain_km2"]))), fmt(round(float(r["loss_km2"]))), fmt(round(float(r["pers_km2"])))])
S.append(table(["Pathway", "Period", "Gain (km²)", "Loss (km²)", "Persistence (km²)"], body,
               "Table S6. Gain, loss and persistence of habitat relative to the present, from the GCM-mean ensemble map."))

vi = rows(tab_dir / "table_S1_variable_importance.csv")
preds = []
for r in vi:
    if r["Algorithm"] in ("GLM", "RF", "BRT") and r["Predictor"] not in preds:
        preds.append(r["Predictor"])
get = {(r["Algorithm"], r["Predictor"]): r["Relative_importance_pct"] for r in vi}
preds.sort(key=lambda p: -sum(float(get.get((a, p), 0)) for a in ("GLM", "RF", "BRT")))
S.append(table(["Predictor", "GLM", "RF", "BRT"], [[p] + [c(get.get((a, p), "nan"), 1) for a in ("GLM", "RF", "BRT")] for p in preds],
               "Table S7. Within-algorithm relative importance of predictors (maximum = 100)."))

st = rows(tab_dir / "table_19_predictor_statistics.csv")
S.append(table(["Predictor", "Presences: median", "Presences: mean ± SD", "Absences: median", "Absences: mean ± SD"],
               [[r["Predictor"], r["Pres_median"], f"{r['Pres_mean']} ± {r['Pres_SD']}", r["Bg_median"], f"{r['Bg_mean']} ± {r['Bg_SD']}"] for r in st],
               "Table S8. Predictor values at presence and absence stations.",
               "Distances in metres; BIO05 in °C; BIO14 and BIO18 in mm; BIO15 is a coefficient of variation (%)."))

dz = rows(tab_dir / "table_24_dzongkhag_suitability.csv")
S.append(table(["Dzongkhag", "Modelled area (km²)", "Suitable (km²)", "% suitable", "Mean suitability"],
               [[r["Dzongkhag"], r["Total_area_km2"], r["Suitable_area_km2"], r["Pct_suitable"], r["Mean_suitability"]] for r in dz if float(r["Suitable_area_km2"]) > 0],
               "Table S9. Present suitable habitat by Dzongkhag (districts with any suitable habitat)."))

pu = rows(ms_dir / "manuscript_claim_ledger_pa_units.csv")
S.append(table(["Protected area or corridor", "Modelled area (km²)", "Suitable habitat (km²)", "% suitable"],
               [[r["unit"], fmt(round(float(r["unit_km2"]))), fmt(round(float(r["suitable_km2"]))),
                 f"{100 * float(r['suitable_km2']) / float(r['unit_km2']):.1f}" if float(r["unit_km2"]) > 0 else "–"] for r in pu],
               "Table S10. Present suitable habitat by protected area and biological corridor."))

ex = rows(tab_dir / "table_interaction_exposure_summary.csv")
S.append(table(["Pathway", "Period", "Mean exposure index"],
               [[SSP.get(r["SSP"].lower(), r["SSP"]), r["Period"].replace("-", "–"), r["Mean_interaction_exposure"]] for r in ex],
               "Table S11. National mean potential interaction-exposure index, present and future (human pressure held at present values)."))

tr = rows(ms_dir / "manuscript_claim_ledger_gcm_trajectories.csv")
body = []
for s in SSP:
    for p in PER:
        vals = {r["gcm"]: r for r in tr if r["ssp"] == s and r["period"] == p}
        body.append([SSP[s], PER[p]] + [f"{fmt(round(float(vals[g]['area_km2'])))} ({float(vals[g]['change_pct']):+.1f}%)".replace("-", "−") for g in GCM])
S.append(table(["Pathway", "Period"] + list(GCM.values()), body,
               "Table S12. Suitable habitat (km²) and change from present (%) for each GCM."))

S13 = [("Ensemble AUC (spatial OOF)", "ensemble_auc_mean"), ("Ensemble TSS (spatial OOF)", "ensemble_tss_mean"),
       ("Ensemble Boyce index", "ensemble_boyce"), ("Classification threshold", "threshold"),
       ("Present habitat (km²)", "present_suitable_km2"), ("Present habitat inside PA network (%)", "present_suitable_in_pa_pct"),
       ("Present habitat outside PA network (%)", "present_suitable_outside_pa_pct"),
       ("Core refugia, suitable in 60/60 projections (km²)", "refugia_core_km2"),
       ("Core refugia outside PA network (%)", "refugia_core_outside_pa_pct"),
       ("High-stability habitat, ≥ 45/60 (km²)", "refugia_high_km2"),
       ("Projections with net loss (of 60)", "per_gcm_n_projections_loss"),
       ("Range of change across projections (%)", None),
       ("GCM-median change, SSP5-8.5, 2071–2100 (%)", "future_gcm_median_change_pct_ssp585_2071_2100")]
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
               "Table S13. Sensitivity of the main results to removing distance to protected areas as a predictor.",
               f"The sensitivity run (`{L['sens_run_id']}`) was identical to the accepted run (`{run.name}`) except for this "
               f"predictor, and repeated all 60 projections. Agreement of present-day suitability maps: Pearson *r* = "
               f"{L['map_pearson_r']}, Schoener's *D* = {L['map_schoener_d']}; Jaccard overlap of habitat maps = {L['map_jaccard']}."))

for i, (f, cap) in enumerate([
        ("figure_s10_mess_map", "Climatic novelty relative to the calibration stations, GCM mean, SSP5-8.5, 2071–2100. Negative values indicate at least one climate predictor outside the range sampled by the surveys (in calibration standard deviations)."),
        ("figure_s09_response_curves", "Partial-dependence curves for the leading predictors (GLM and RF), with other predictors held at their medians.")], 1):
    S.append(f"![]({(tab_dir / (f + '.png')).as_posix()}){{width=6.5in}}\n\n**Figure S{i}.** {cap}\n")

odmap = [
    ["Overview", "Taxon; location; scale", "*Elephas maximus*; Bhutan (national); 250 m grid, EPSG:32645"],
    ["Overview", "Objective", "Map present habitat; project to 2021–2100 under CMIP6; identify climate refugia and their protection"],
    ["Overview", "Model algorithms", "GLM, random forest, boosted regression trees (equal-weight ensemble); MaxEnt sensitivity only"],
    ["Data", "Response data", f"Camera-trap detection/non-detection at {L['n_stations']} stations ({L['n_presences']} presences, {L['n_absences']} absences); 26 records excluded (Table S1)"],
    ["Data", "Predictors", "18 retained of 35 candidates after |r| ≥ 0.85 and VIF ≥ 10 screening (Table S2); 1986–2015 climate baseline"],
    ["Data", "Future climate", "5 GCMs (ACCESS-CM2, CNRM-CM6-1, INM-CM5-0, MIROC6, MPI-ESM1-2-LR) × 4 SSPs × 3 periods = 60 projections"],
    ["Model", "Assumptions", "Equilibrium with climate; non-climatic predictors static; national boundary as accessible area"],
    ["Model", "Settings", "Table S3; equal weights fixed a priori; threshold = TSS optimum of pooled OOF ensemble"],
    ["Assessment", "Validation", f"Five-fold spatial block CV (15 km blocks; {L['fold_presence_min']}–{L['fold_presence_max']} presences per fold); AUC, TSS, Boyce, Brier, calibration slope, Moran's I (Table 1)"],
    ["Assessment", "Uncertainty", "Per-GCM areas and median; inter-GCM SD maps; climatic novelty relative to calibration stations for every projection"],
    ["Prediction", "Outputs", "Present suitability; 60 per-GCM and 12 GCM-mean projections; refugia stability count (denominator 60)"],
    ["Prediction", "Extrapolation", f"Reported, not masked: {L['present_suitable_extrap_per_projection_median_pct']}% of present habitat outside the sampled climate range in the median projection"],
    ["Reproducibility", "Code and run", f"Single scripted pipeline; accepted run `{run.name}` with input hashes, package versions and seeds in its manifest"],
]
S.append(table(["ODMAP section", "Element", "Description"], odmap,
               "Appendix S1. ODMAP protocol summary (Zurell et al., 2020)."))
(src / "supplement_filled.md").write_text("\n\n".join(S), encoding="utf-8")

ref = src / "reference.docx"
for md, out in [(src / "manuscript_filled.md", ms_dir / "Manuscript_Ecology_and_Evolution.docx"),
                (src / "supplement_filled.md", ms_dir / "Supporting_Information.docx")]:
    subprocess.run(["pandoc", str(md), "-o", str(out), f"--reference-doc={ref}", "--resource-path=."], check=True)
    print("Wrote", out)
