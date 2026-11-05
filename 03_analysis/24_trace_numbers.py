"""Trace every number in the built JWM manuscript to a source.

Each numeric token in the main text, captions and tables must match a claim ledger
value, a run table, the independent validation report, or a documented entry below.
Writes Number_Trace_JWM.csv and exits non-zero if any number has no source.

Usage (repo root): python 03_analysis/24_trace_numbers.py 04_outputs/runs/RUN_<id>
"""
import csv
import re
import sys
from pathlib import Path

run = Path(sys.argv[1])
ms = Path("05_manuscripts")
out = ms / "Journal of Wildlife Management"
text = (ms / "source" / "manuscript_jwm_filled.md").read_text(encoding="utf-8")


def rows(p):
    with open(p, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def forms(v):
    v = str(v).strip().lstrip("+").replace(",", "").replace("−", "-").lstrip("-")
    try:
        x = float(v)
    except ValueError:
        return set()
    s = {v}
    for d in (0, 1, 2, 3):
        s.add(f"{x:.{d}f}")
    s.add(str(int(x)) if x.is_integer() else str(x))
    return s


known = {}


def add_source(values, label):
    for v in values:
        for f in forms(v):
            known.setdefault(f, label)


add_source([r["value"] for r in rows(ms / "manuscript_claim_ledger.csv")], "claim ledger")
add_source([r["value"] for r in rows(out / "jwm_extra_numbers.csv")], "JWM ledger")
add_source([r["value"] for r in rows(ms / "sensitivity_no_dist_pa" / "claim_ledger_no_dist_pa.csv")], "sensitivity ledger")
add_source([r["value"] for r in rows(ms / "sensitivity_no_dist_pa" / "map_agreement.csv")], "sensitivity map agreement")
add_source([r["reported"] for r in rows(out / "Validation_Report_JWM.csv")], "independent validation")
tab = run / "08_figures_tables"
for name in ("table_01_model_performance.csv",):
    for r in rows(tab / name):
        add_source(r.values(), name)
for r in rows(ms / "manuscript_claim_ledger_gcm_trajectories.csv"):
    add_source(r.values(), "GCM trajectories")

L = {r["key"]: float(r["value"]) for r in rows(ms / "manuscript_claim_ledger.csv") if re.fullmatch(r"-?[\d.]+", r["value"])}
S = {r["key"]: float(r["value"]) for r in rows(ms / "sensitivity_no_dist_pa" / "claim_ledger_no_dist_pa.csv") if re.fullmatch(r"-?[\d.]+", r["value"])}
add_source([f"{100 - L['present_suitable_in_pa_pct']:.1f}"], "100 minus habitat inside network")
add_source([f"{100 - L['refugia_core_in_pa_pct']:.1f}"], "100 minus core refugia inside network")
add_source([f"{100 - S['present_suitable_in_pa_pct']:.1f}", f"{100 - S['refugia_core_in_pa_pct']:.1f}"], "100 minus sensitivity inside-network share")
add_source([round(L["bhutan_km2_on_grid"] - L["domain_km2"])], "Bhutan on grid minus domain")
add_source([round(L["present_suitable_km2"] - L["present_suitable_in_pa_km2"])], "habitat minus habitat inside network")
add_source([r["Mean_interaction_exposure"] for r in rows(tab / "table_interaction_exposure_summary.csv")], "exposure summary table")

DOCUMENTED = {
    "3000": "elevation cutoff used in the claim ledger",
    # Survey and country facts from cited reports
    "38394": "DoFPS 2023: national area", "678": "NCD 2018: estimate", "605": "NCD 2018: range", "761": "NCD 2018: range",
    "7500": "DoFPS 2023: elevation", "100": "NCD 2018 elevation; also 100% in tables", "51": "DoFPS 2023: protected share",
    "2016": "NCD 2018: survey year", "11": "DoFPS 2023: survey dates", "31": "DoFPS 2023: survey dates", "21": "DoFPS 2023: survey dates",
    "30": "DoFPS 2023 survey dates; 10-30 km block range", "2021": "survey and period dates", "2022": "survey dates",
    "45": "DoFPS 2023 camera height cm; 45 of 60 definition", "90": "NCD 2018 deployment days; 90% seasonality anchor",
    "26.7": "study area latitude", "28.3": "study area latitude", "88.8": "study area longitude", "92.1": "study area longitude",
    # Published climate sensitivities (Zelinka table v2.2, verified 2026-11-02)
    "1.9": "ECS INM-CM5-0", "2.6": "ECS MIROC6", "3.0": "ECS MPI-ESM1-2-LR", "4.7": "ECS ACCESS-CM2", "4.9": "ECS CNRM-CM6-1",
    # Study design and methods
    "250": "grid resolution", "12.5": "DEM resolution", "1986": "climate baseline", "2015": "climate baseline", "2000": "MODIS start; 2,000 m and 2,000 mm anchors",
    "2024": "MODIS end", "2025": "Dynamic World year", "2020": "Human Impact Index year", "19": "bioclimatic variables", "18": "retained predictors",
    "7": "distance layers", "0.85": "collinearity rule", "10": "VIF rule; block sizes", "0.65": "AUC gate", "0.30": "TSS gate", "15": "block size; deciles; 15 km",
    "5": "folds, GCMs, neighbors", "4": "pathways, objectives", "3": "periods, algorithms", "2": "surveys, runs", "1": "unit values and counts",
    "60": "projections", "12": "summaries per GCM", "1000": "trees; elevation and rainfall anchors", "0.01": "learning rate", "0.75": "bag fraction",
    "1.5": "MaxEnt multiplier; exclusion distance km", "2.2": "share of stations with filled values", "23": "stations with filled values",
    "2051": "period", "2071": "period", "2050": "period", "2080": "period", "2100": "period", "98": "percentile rule", "75": "45 of 60",
    "0.5": "not used", "1.0": "calibration ideal", "0": "bounds", "6": "predictors in Figure 3; 6-18 km exclusion", "66": "exclusion distance",
    "59": "exclusion distance", "24": "exclusions; latitude", "26": "exclusions", "8": "projections with loss; Royal Manas stations", "9": "exclusions",
    "13": "exclusions", "22": "temperature anchor; latitude", "110": "seasonality anchor", "1200": "response step", "1400": "response step",
    "4.0": "not used", "4.4.0": "R version", "1984": "WGS 1984", "45N": "UTM zone", "29.6": "validated median", "883": "not used",
}
for k, v in DOCUMENTED.items():
    if v != "not used":
        known.setdefault(k, v)

t = text.split("# REFERENCES")
refs_tail = t[1].split("Figure Captions", 1)[1]
scan = t[0] + "\n" + refs_tail
scan = scan[scan.index("**ABSTRACT**"):]
scan = re.sub(r"\(?\b[A-Z][\w\-]+(?: et al\.| and [A-Z][\w\-]+)? (?:19|20)\d\d(?:\*[ab]\*)?(?:, \*[ab]\*)?", " ", scan)
scan = re.sub(r"(?:19|20)\d\d\*[ab]\*(?:, \*[ab]\*)?", " ", scan)
scan = re.sub(r"\b(?:Table|Figure|Appendix)s? S?\d+", " ", scan)
scan = re.sub(r"Biological Corridors? \d+(?: and \d+)?", " ", scan)
scan = re.sub(r"\b[A-Z]{3,}[\w\-]*\d[\w\-.]*", " ", scan)
scan = re.sub(r"SSP\d-\d\.\d|CMIP6|MOD13Q1|ACCESS-CM2|CNRM-CM6-1|INM-CM5-0|MIROC6|MPI-ESM1-2-LR|HC500|R 4\.4\.0|<https?://\S+>|S\d+|version 2\.2", " ", scan)
scan = re.sub(r"\^[a-z]\^|\b1\)|\b2\)|\b3\)|\b4\)", " ", scan)
found, missing = [], []
for m in re.finditer(r"\d[\d,]*(?:\.\d+)?", scan):
    tok = m.group(0).rstrip(",")
    norm = tok.replace(",", "")
    src = known.get(norm)
    ctx = scan[max(0, m.start() - 35):m.end() + 25].replace("\n", " ")
    found.append({"number": tok, "source": src or "NO SOURCE", "context": ctx})
    if not src:
        missing.append((tok, ctx))
with open(out / "Number_Trace_JWM.csv", "w", newline="", encoding="utf-8-sig") as f:
    w = csv.DictWriter(f, fieldnames=["number", "source", "context"])
    w.writeheader()
    w.writerows(found)
print(f"Numbers traced: {len(found)}; no source: {len(missing)}")
for tok, ctx in missing:
    print("  ", tok, "|", ctx)
sys.exit(1 if missing else 0)
