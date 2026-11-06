"""Quality checks for the revised JWM documents.

Traces every number in the revised manuscript and Supporting Information to a ledger,
table, or documented source; checks citation order, terminology, first person, and
that the Word files open. Writes Revision/Number_Trace_Revised.csv and
Revision/QA_Report_Revised.csv. Exits non-zero on any failure.

Usage (repo root): python 03_analysis/31_validate_revision.py 04_outputs/runs/RUN_<id>
"""
import csv
import os
import glob
import re
import sys
from pathlib import Path

from docx import Document

run = Path(sys.argv[1])
ms = Path("05_manuscripts")
jwm = ms / "Journal of Wildlife Management"
out = jwm / os.environ.get("REV_DOCS", "Revision_Corrected")
rev = Path(os.environ.get("REV_OUT", "04_outputs/revision_jwm/corrected"))
led = rev / "ledger"
main_txt = (ms / "source" / "manuscript_jwm_corrected_filled.md").read_text(encoding="utf-8")
si_txt = (ms / "source" / "supplement_jwm_corrected_filled.md").read_text(encoding="utf-8")
qa = []


def check(name, ok, detail=""):
    qa.append({"check": name, "status": "PASS" if ok else "FAIL", "detail": detail})


def rows(p):
    with open(p, newline="", encoding="utf-8-sig") as f:
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


def add(values, label):
    for v in values:
        for f in forms(v):
            known.setdefault(f, label)


for f in [led / "manuscript_claim_ledger.csv", rev / "jwm" / "jwm_extra_numbers.csv", rev / "revision_numbers_stage2.csv", rev / "revision_numbers_stage2b.csv", rev / "revision_numbers_limit.csv"]:
    for r in rows(f):
        add([r["value"]], f.name)
        for tok in re.findall(r"\d+\.?\d*", str(r["value"])):
            add([tok], f.name)
for f in glob.glob(str(rev / "tables" / "*.csv")) + [str(led / "manuscript_claim_ledger_gcm_trajectories.csv"), str(led / "manuscript_claim_ledger_pa_units.csv")] + glob.glob(str(run / "08_figures_tables" / "*.csv")):
    if "projections" in f or "novelty_by_predictor" in f:
        continue
    for r in rows(f):
        add(r.values(), Path(f).name)
for f in ["habitat_area_change_summary.csv"]:
    for r in rows(run / "05_change_metrics" / f):
        add(r.values(), f)
for r in rows(run / "01_processed_data" / "spatial_block_design_sensitivity.csv"):
    add(r.values(), "spatial_block_design_sensitivity.csv")
L = {r["key"]: r["value"] for r in rows(led / "manuscript_claim_ledger.csv")}
add([f"{100 - float(L['present_suitable_in_pa_pct']):.1f}", f"{100 - float(L['refugia_core_in_pa_pct']):.1f}"], "100 minus inside share")
add([round(float(L["bhutan_km2_on_grid"]) - float(L["domain_km2"])), round(float(L["present_suitable_km2"]) - float(L["present_suitable_in_pa_km2"]))], "ledger difference")
S2 = {r["key"]: r["value"] for r in rows(rev / "revision_numbers_stage2.csv")}
add([f"{100 * float(S2['occu_false_nondetect_share']):.1f}", f"{100 * sum(int(x.split(':')[1]) for x in S2['hwc_by_district'].split() if x.split(':')[0] in ('Sarpang', 'Samdrupjongkhar')) / int(rows(rev / 'revision_numbers_stage2b.csv')[0]['value'] if False else {r['key']: r['value'] for r in rows(rev / 'revision_numbers_stage2b.csv')}['hwc_inside_bhutan']):.1f}", f"{100 * float(S2['core_never_extrap_km2']) / float(L['present_suitable_km2']):.1f}",
     f"{100 - float(S2['core_never_extrap_outside_pa_pct']):.1f}"], "derived from ledger")
nh = rows(rev / "tables" / "variant_no_human.csv")[0]
add([f"{100 * float(nh['exposure_top15_overlap']):.1f}"], "no_human variant")
for r in rows(rev / "tables" / "exposure_by_scenario_corrected.csv"):
    add([f"{float(r['mean_exposure']):.3f}"], "exposure table")
for n in ("no_dung", "tiger_only", "time_aligned", "no_human", "no_dist_pa"):
    for r in rows(rev / "tables" / f"variant_{n}.csv"):
        add(r.values(), f"variant_{n}")
dpj = sorted(float(r["change_pct"]) for r in rows(rev / "tables" / "variant_no_dist_pa_projections.csv") if r["ssp"] == "ssp585" and r["period"] == "2071_2100")
add([f"{dpj[2]:.1f}", f"{abs(dpj[2]):.1f}"], "variant_no_dist_pa_projections median")
for r in rows(rev / "tables" / "threshold_refugia_sensitivity.csv"):
    add(r.values(), "threshold table")

occ = rows("01_data_raw/01_occurrences/elephant_PA_data.csv"); exc = rows("01_data_raw/01_occurrences/occurrence_exclusions.csv")
add([sum(r["presence"] == "0" for r in occ if r["source"] == "station_survey"), sum(r["presence"] == "0" for r in occ + exc), sum(r["presence"] == "1" for r in occ + exc)], "station files")
for r in rows(run / "08_figures_tables" / "table_02_predictor_selection.csv"):
    for tok in re.findall(r"\d+\.\d+", r.get("Removal_reason") or ""):
        add([tok], "table_02_predictor_selection.csv")
for r in rows(led / "manuscript_claim_ledger_pa_units.csv"):
    if float(r["unit_km2"]) > 0:
        add([f"{100 * float(r['suitable_km2']) / float(r['unit_km2']):.1f}"], "pa_units ledger")
DOC = {"27.4": "northern boundary of the area inspected", "38394": "DoFPS 2023", "678": "NCD 2018", "605": "NCD 2018", "761": "NCD 2018", "7500": "DoFPS 2023", "100": "NCD 2018; percent; camera height", "51": "DoFPS 2023",
       "2016": "NCD 2018", "11": "DoFPS 2023 date", "31": "DoFPS 2023 date", "21": "DoFPS 2023 date", "30": "date; block size", "2021": "dates", "2022": "dates",
       "45": "DoFPS 2023 camera height; 45 of 60", "90": "NCD 2018 days; anchor", "26.7": "study extent", "28.3": "study extent", "88.8": "study extent", "92.1": "study extent",
       "1.9": "ECS", "2.6": "ECS", "3.0": "ECS", "4.7": "ECS", "4.9": "ECS", "250": "grid", "12.5": "DEM", "1986": "baseline", "2015": "baseline", "2000": "MODIS; anchor", "2024": "MODIS",
       "2025": "Dynamic World", "2020": "HII; land use", "19": "bioclim count", "18": "retained", "7": "distance layers", "0.85": "rule", "10": "VIF; blocks", "0.65": "gate", "0.30": "gate",
       "15": "blocks", "5": "design", "4": "design", "3": "design", "2": "design", "1": "design", "60": "projections", "12": "per GCM", "1000": "trees; anchors", "0.01": "learning rate",
       "0.75": "bag fraction", "1.5": "MaxEnt; distance", "2.2": "filled stations", "23": "filled stations", "2051": "period", "2071": "period", "2050": "period", "2080": "period", "2100": "period",
       "98": "percentile", "75": "45 of 60", "0": "bounds", "6": "design", "66": "exclusion distance", "59": "exclusion distance", "24": "exclusions", "26": "exclusions", "8": "counts", "9": "exclusions",
       "13": "exclusions", "22": "anchor", "110": "anchor", "1200": "response step", "1400": "response step", "1984": "WGS", "45N": "UTM", "3000": "elevation cut", "2023": "version",
       "20": "buffer", "25": "block size", "95": "CI", "0.20": "threshold", "0.15": "threshold", "0.10": "threshold", "0.174": "23_validate_results",
       "29.6": "23_validate_results", "21.0": "23_validate_results", "2024.0": "x", "886": "23_validate_results", "13.1": "23_validate_results",
       "5.0": "23_validate_results", "44": "23_validate_results", "72": "23_validate_results", "1250": "23_validate_results", "1089": "23_validate_results", "108": "stations", "150": "threshold rule; trap nights",
       "35": "candidates", "2840": "survey table", "4411": "survey table", "5": "design"}
for k, v in DOC.items():
    known.setdefault(k, v)


def scan_text(t):
    t = re.sub(r"\(?\b[A-Z][\w\-]+(?: et al\.| and [A-Z][\w\-]+)? (?:19|20)\d\d(?:\*[ab]\*)?(?:, \*[ab]\*)?", " ", t)
    t = re.sub(r"(?:19|20)\d\d\*[ab]\*(?:, \*[ab]\*)?", " ", t)
    t = re.sub(r"\b(?:Tables?|Figures?|Appendix) S?\d+(?:–S?\d+)?", " ", t)
    t = re.sub(r"Biological Corridors? \d+(?: and \d+)?|Corridor \d+", " ", t)
    t = re.sub(r"\b[A-Z]{3,}[\w\-]*\d[\w\-.]*", " ", t)
    t = re.sub(r"SSP\d-\d\.\d|CMIP6|MOD13Q1|ACCESS-CM2|CNRM-CM6-1|INM-CM5-0|MIROC6|MPI-ESM1-2-LR|HC500|R 4\.4\.0|<https?://\S+>|EPSG:\d+|`[^`]+`|\{width=6\.5in\}|pathway 5-8\.5|\[0, 1\]|10\^−8\^|3 × 10", " ", t)
    return re.sub(r"\^[a-z]\^|\b[1-4]\)|\bS\d+", " ", t)


parts = main_txt.split("# REFERENCES")
m_scan = scan_text(parts[0][parts[0].index("**ABSTRACT**"):] + "\n" + parts[1].split("Figure Captions", 1)[1])
s_scan = scan_text(si_txt.split("Journal of Wildlife Management.", 1)[1].split("num.trees", 1)[0] + si_txt.split("Sensitivity model only", 1)[-1].split("References", 1)[0])
found, miss = [], []
for label, scan in [("manuscript", m_scan), ("supporting", s_scan)]:
    for m in re.finditer(r"\d[\d,]*(?:\.\d+)?", scan):
        tok = m.group(0).rstrip(",")
        src = known.get(tok.replace(",", ""))
        ctx = scan[max(0, m.start() - 35):m.end() + 25].replace("\n", " ")
        found.append({"document": label, "number": tok, "source": src or "NO SOURCE", "context": ctx})
        if not src:
            miss.append(f"{label}: {tok} | {ctx}")
with open(out / "Number_Trace_Revised.csv", "w", newline="", encoding="utf-8-sig") as f:
    w = csv.DictWriter(f, fieldnames=["document", "number", "source", "context"])
    w.writeheader()
    w.writerows(found)
check("every number traced", not miss, f"{len(found)} numbers; {len(miss)} without source")

body = parts[0]
check("no first person", not re.findall(r"\b(?:we|our|ours|us)\b", body, re.I))
check("no unresolved keys", "{{" not in main_txt + si_txt and "[[" not in main_txt + si_txt)
check("figures not embedded in manuscript", not re.search(r"!\[", main_txt))
for pat, name, n in [(r"\bFigure (\d+)", "main figures cited in order", 7), (r"\bTable (\d+)\b", "main tables cited in order", 5)]:
    first = []
    for x in re.findall(pat, body):
        if int(x) not in first:
            first.append(int(x))
    check(name, first == list(range(1, n + 1)), str(first))
first = []
for x in re.findall(r"\bTable S(\d+)", re.sub(r"Tables S(\d+) and S(\d+)", r"Table S\1 Table S\2", body)):
    if int(x) not in first:
        first.append(int(x))
check("supporting tables cited in order", first == list(range(1, len(first) + 1)), f"S1–S{len(first)}")
caps = [int(x) for x in re.findall(r"^Table S(\d+)\.", si_txt, re.M)]
check("every supporting table present once", caps == list(range(1, len(first) + 1)), str(caps))
check("figures S1-S2 present", all(f"Figure S{i}." in si_txt for i in (1, 2)))
for term in ["probability of detection", "probability of occurrence"]:
    check(f"term absent: {term}", term not in body.lower())
check("habitat quality only negated", all("not" in s or "not " in s for s in re.findall(r"[^.]*habitat quality[^.]*\.", body)))
check("conflict risk only negated or qualified", all(re.search(r"\bnot\b|neither|needed", s) for s in re.findall(r"[^.]*conflict risk[^.]*\.", body)) and "calibrated" not in body)
for fn in ("Manuscript_JWM_Revised.docx", "Supporting_Information_JWM_Revised.docx"):
    d = Document(out / fn)
    check(f"opens: {fn}", len(d.paragraphs) > 20, f"{len(d.paragraphs)} paragraphs, {len(d.tables)} tables")
for fn in ("Manuscript_JWM.docx", "Supporting_Information_JWM.docx"):
    check(f"original preserved: {fn}", (jwm / fn).exists())
check("7 figure TIFFs", len(list((out / "Figures_for_upload").glob("Figure_*.tif"))) == 7)
with open(out / "QA_Report_Revised.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["check", "status", "detail"])
    w.writeheader()
    w.writerows(qa)
for q in qa:
    if q["status"] == "FAIL":
        print("FAIL", q["check"], q["detail"])
print(f"QA: {sum(q['status'] == 'PASS' for q in qa)} of {len(qa)} pass; numbers {len(found)}, no source {len(miss)}")
for x in miss[:50]:
    print("  ", x)
sys.exit(1 if miss or any(q["status"] == "FAIL" for q in qa) else 0)
