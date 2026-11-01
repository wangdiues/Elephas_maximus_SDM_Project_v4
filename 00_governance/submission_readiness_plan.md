# Submission Readiness Plan for the Asian Elephant SDM Paper

Version: 1.1

Updated: 2026-10-18

Status: Code refinement implemented; production analysis and manuscript revisions remain pending

Target journal: Ecology and Evolution

Code status: Core workflow, OOF ensemble, future-scenario, refugia, publication-table, publication-figure, and final submission-validation contracts implemented on 2026-10-18. Analysis execution and result replacement remain intentionally pending.

## Purpose

This plan defines the work required to convert the current Asian elephant species distribution modelling workflow, manuscript, supplement, figures, tables, code archive, and data statements into a reproducible submission package. The present manuscript remains a working draft until the analytical gates in this plan pass.

The work must proceed in the order below. Numerical manuscript revisions must not begin until a clean production run has passed the scientific and provenance gates.

## Current Table and Figure Audit

Audit basis: the current main manuscript DOCX, supplementary DOCX, six main PNG figures, 36 supplementary PNG files, and the available manuscript PDFs were reviewed on 2026-10-18. The PDFs predate the current main DOCX and therefore cannot serve as final render proofs. All visual checks must be repeated on newly generated submission files.

### Audit conclusion

The current tables and figures are **not yet at publication standard**. The source PNG dimensions are generally sufficient, but resolution alone is not the problem. The blocking issues are scientific consistency, misleading or ambiguous visual encodings, inadequate uncertainty display, colour accessibility, excessive panel density, very wide tables, inconsistent terminology and denominators, and captions that do not always match the displayed content.

The following defects are submission blockers:

1. Figure 4 is titled and plotted as change in mean suitability while the text and caption describe habitat-area trajectories, and it does not display the reported GCM uncertainty.
2. Figure 5 states 13 future scenarios even though 4 SSPs by 3 periods equals 12, and its core-refugia definition conflicts with the manuscript.
3. Figure 3 juxtaposes percentages with different denominators without making that distinction sufficiently clear.
4. Figure 6 presents an unvalidated index as conflict risk and mixes a continuous scale with categorical labels.
5. Figure S4 is not a valid null-model comparison unless each permutation refits the full modelling workflow.
6. Figure S9 is labelled as all 17 predictors and four algorithms but the displayed content does not match that claim.
7. Several supplementary multi-panel figures are unreadable at final page or column width, especially Figures S6, S12, and S14.
8. Table 1 has 12 columns and is not legible at ordinary manuscript width; Table 3 is also too wide and is vulnerable to awkward page splitting.
9. Supplementary Table S10 has 14 columns and requires redesign rather than font reduction.
10. Several tables expose analytical or provenance defects, including absolute local file paths, inconsistent predictor category labels, swapped table locations in the ODMAP checklist, and metrics that must be recomputed after the corrected analysis.

Accordingly, no current figure or quantitative table should be carried unchanged into the submission package. All should be regenerated or explicitly revalidated from the single approved production run.

## Submission Standard

The paper is submission ready only when:

1. All scientific results come from one immutable production run.
2. Every algorithm is evaluated with genuine spatial out-of-fold predictions.
3. Present and future ensembles use the same algorithm definition and weights.
4. Future products contain exactly 5 GCMs, 4 SSPs, 3 periods, 60 GCM scenario combinations, and 12 SSP-period summaries (8 → 5 GCMs on 2026-11-01; see memory_log.md).
5. Refugia calculations exclude historical layers from the future scenario count.
6. National area, accessible area, and complete-case modelling area are reported separately.
7. The conflict product is described and validated at the level supported by the data.
8. All reported numbers are generated directly from production artifacts.
9. All automated tests and manuscript consistency checks pass.
10. The final DOCX, PDF, supplement, figures, repository release, and archive agree.

## Decisions to Lock Before Implementation

These decisions control the analysis and must be recorded in the project memory log before the production run.

| Decision | Recommended primary choice | Required sensitivity analysis |
|---|---|---|
| Algorithm ensemble | Equal weights among algorithms passing prespecified quality gates | AUC-weighted ensemble |
| Ensemble threshold | TSS-optimal threshold from spatial OOF ensemble predictions | Fixed thresholds spanning 0.10 to 0.50 |
| MaxEnt reference sample | Surveyed nondetection stations treated explicitly as the reference/background sample | Exclude MaxEnt and compare the three binary classifiers |
| Refugia primary unit | Present-suitable cells retained across individual GCM projections | Stability across the 12 SSP-period GCM means |
| Core-refugia criterion | Present-suitable cells retained in all 96 individual GCM projections | At least 72/96 retained; SSP-period means reported only as a separate sensitivity |
| Conflict product | Potential human-elephant interaction exposure index | Fully fitted conflict model only if adequate effort/background data are available |
| Area denominator | Report both national boundary and complete-case model domain | Quantify results after filling remediable predictor gaps |
| Null-model test | Remove unless full models are refitted under spatially valid permutations | At least 999 refitted permutations with plus-one p-value correction |

Any departure from these defaults requires a written ecological or statistical justification before results are inspected.

## Work Package 1: Preserve the Current Evidence

### Actions

1. Create a read-only pre-correction archive of the current manuscript, supplement, figures, tables, validation report, configuration, logs, and cited run artifacts.
2. Record the Git commit and dirty-worktree state.
3. Generate checksums for every archived file.
4. Create a claim ledger listing every number in the abstract, results, discussion, conclusions, tables, and figure captions.
5. Mark the archived outputs as superseded and prohibit their use in the corrected manuscript.

### Required artifacts

- `pre_correction_manifest.csv`
- `pre_correction_checksums.csv`
- `manuscript_claim_ledger.csv`
- Archived current manuscript and supplement

### Gate 1 acceptance criteria

- Every current headline value has a recorded location and source.
- The archive can be distinguished unambiguously from later runs.
- No archived file will be overwritten by the corrected workflow.

## Work Package 2: Repair Run Isolation and Provenance

### Actions

1. Restore per-run output directories at `04_outputs/runs/<run_id>/`.
2. Require a new, empty run directory for production execution.
3. Disable `skip_existing` for production runs.
4. Make future projection failure fatal whenever future projections are enabled.
5. Prevent a downstream phase from reading an artifact whose run ID, timestamp, or manifest does not match the active run.
6. Make missing models, scenarios, tables, or figures fatal when they are required by the configured analysis.
7. Record the following in the run manifest:
   - Git commit and dirty status
   - Configuration hash
   - Input file hashes
   - R, Java, and package versions
   - Random seeds
   - Fold definition
   - Predictor set
   - Algorithm settings
   - Ensemble members and weights
   - Threshold definition and value
   - Output checksums
8. Register success only after all scientific acceptance tests pass.

### Tests to add

- Phase 9 failure causes the run to fail.
- A stale future raster is rejected.
- A historical raster cannot enter the future scenario manifest.
- Missing GCM, SSP, period, or algorithm products cause failure.
- An output with a mismatched run ID is rejected.
- A nonempty production directory is rejected.

### Gate 2 acceptance criteria

- An intentionally broken Phase 9 produces a failed run with no success registry entry.
- Downstream products cannot be created from stale upstream artifacts.
- Two runs produce separate immutable output trees.

## Work Package 3: Rebuild Spatial Cross-Validation

### Actions

1. Construct the spatial blocks once and save both block polygons and point assignments.
2. Use projected coordinates and document the block origin, dimensions, and allocation rule.
3. Allocate blocks to folds without replacement where possible and balance presence and absence counts.
4. Require adequate presences and absences in every assessment fold.
5. Compare the proposed 15 km blocks with distances indicated by spatial autocorrelation or an explicit block-size sensitivity analysis.
6. Use the identical saved folds for GLM, RF, BRT, MaxEnt, ensemble evaluation, tuning, and null analyses.
7. Perform predictor selection, transformation selection, and hyperparameter tuning using training data only.

### Required artifacts

- Spatial block polygon file
- Point-to-fold assignment table
- Fold composition table
- Fold map
- Block-size sensitivity report

### Gate 3 acceptance criteria

- Every observation has one and only one fold assignment.
- Every algorithm uses the same fold identifier.
- No assessment-fold observation contributes to fitting, tuning, feature selection, calibration, or threshold selection for its prediction.

## Work Package 4: Correct Algorithm Definitions

### GLM

- Use a prespecified ecologically defensible formula or conduct selection entirely within each training fold.
- Apply transformations consistently within folds and the final model.
- Remove the forward stepwise AIC claim unless the code actually performs that procedure.

### Random Forest

- Fit an explicit classification or probability forest.
- Set and report `num.trees`, `mtry`, `min.node.size`, class handling, and seed.
- Tune only within training data.

### BRT

- Select the number of trees within training data.
- Set and report interaction depth, learning rate, bag fraction, minimum observations, and stopping rule.

### MaxEnt

- Replace internal random cross-validation with the saved spatial folds.
- Generate held-out predictions for both presences and the reference/background sample from the fold-specific model.
- Never substitute full-model predictions into the OOF prediction file.
- State precisely how verified nondetections are used by MaxEnt.
- Save fold-specific settings, lambdas, predictions, and convergence information.

### Gate 4 acceptance criteria

- Each row in the OOF prediction table was generated by a model that did not train on that row.
- Code, configuration, supplementary settings, and manuscript Methods describe the same algorithms.
- Excluding any one algorithm can be evaluated without rebuilding unrelated data stages.

## Work Package 5: Recalculate Evaluation and Thresholds

### Required metrics

- ROC AUC
- Precision-recall AUC
- TSS
- Sensitivity and specificity
- PPV and NPV
- Brier score
- Calibration intercept and slope
- Calibration curve
- Fold-level estimates and dispersion
- Spatial residual diagnostics

### Actions

1. Calculate all metrics from spatial OOF predictions.
2. Report pooled and fold-level estimates.
3. Build the OOF ensemble using the locked primary weighting rule.
4. Select the primary binary threshold from OOF ensemble predictions only.
5. Apply the fixed threshold unchanged to present and future ensemble maps.
6. Reassess performance gates after calibration and spatial diagnostics.
7. Remove the current permutation claim unless full models are refitted for each permutation.

### Gate 5 acceptance criteria

- The complete performance table can be regenerated from one OOF prediction file and one evaluation script.
- No metric or threshold uses a full-model prediction at a training observation.
- The precision shown in the manuscript is supported by the evaluation design.

## Work Package 6: Standardise Present and Future Projections

### Ensemble hierarchy

1. Predict each retained algorithm for each GCM, SSP, and period.
2. Combine algorithms for each GCM using the locked primary weights.
3. Average the eight GCM-specific algorithm ensembles for each SSP-period summary.
4. Preserve all individual GCM products for uncertainty and refugia analyses.

### Additional corrections

- Apply the same model members, weights, response scales, masks, and threshold to present and future maps.
- Build an explicit scenario manifest rather than discovering production inputs with broad filename patterns.
- Evaluate extrapolation using all selected time-varying climate predictors.
- Derive the reference environment from the fitted calibration domain.
- Report how extrapolated cells are handled rather than silently converting them to missing values.
- State that vegetation, land cover, and human-pressure predictors remain static unless future versions are supplied.

### Required completeness checks

| Level | Expected count |
|---|---:|
| GCMs | 8 |
| SSPs | 4 |
| Periods | 3 |
| GCM-SSP-period combinations | 96 |
| SSP-period GCM summaries | 12 |

### Gate 6 acceptance criteria

- Present and future ensembles differ only because predictor values differ.
- The scenario manifest has the expected counts and no duplicates.
- A historical layer is not classified as a future scenario.

## Work Package 7: Rebuild Area, Change, and Refugia Products

### Area accounting

Calculate and report separately:

- National boundary area
- Accessible area
- Complete-case modelling area
- Area excluded by missing predictors
- Present suitable area
- Suitable area as a percentage of the model domain
- Suitable area as a percentage of the national boundary, when valid

Create a map showing excluded model-domain cells. Do not describe the complete-case raster area as the national area.

### Change analysis

- Recompute gain, loss, persistence, and net change using the fixed OOF ensemble threshold.
- Reconcile `future area = persistence + gain` and `present area = persistence + loss` within raster tolerance.
- Report uncertainty across GCMs rather than only change in the GCM mean.

### Refugia analysis

- Use an explicit list of future projections.
- Exclude historical layers from all future counts.
- Require present suitability as a condition for every refugia class.
- Export valid-scenario counts per cell.
- Calculate the primary stability metric across individual GCM projections.
- Reproduce the 12-mean definition as a sensitivity analysis.
- Test alternative thresholds and algorithm ensemble weights.

### Gate 7 acceptance criteria

- Refugia counts match the documented denominator.
- Every refugia pixel is presently suitable.
- Area identities reconcile within one raster cell or a documented tolerance.
- National and model-domain percentages are not conflated.

## Work Package 8: Reframe and Validate the Conflict Product

### Primary recommended analysis

1. Rename the current product as a potential human-elephant interaction exposure index.
2. Define human pressure transparently from its component layers.
3. Use a fixed transformation and scaling across all present and future maps.
4. Do not min-max normalise each scenario independently.
5. Test whether recorded incidents are enriched in high-exposure quantiles using spatially constrained availability or background samples.
6. Report discrimination, calibration where applicable, and uncertainty.
7. Treat missing district and damage classifications explicitly.

### Optional predictive risk model

Develop a separate incident model only if survey effort, temporal alignment, and defensible absence or availability data can be assembled. Keep that model separate from the elephant habitat SDM.

### Gate 8 acceptance criteria

- The name of the product matches what was estimated.
- Present and future index values share one scale.
- A claimed percentage increase has a mathematically comparable denominator.
- Incident data contribute to a stated validation analysis or are described only as an overlay.

## Work Package 9: Verify Predictor and Data Provenance

### Actions

1. Confirm the exact DEM product, provider, resolution, date, and citation.
2. Confirm the Human Influence Index or human-footprint product and version.
3. Reconcile the road-layer file, availability, extraction, removal reason, and manuscript statement.
4. Verify NDVI, EVI, Dynamic World, climate, PA, private-land, settlement, river, and water-source dates.
5. Document resampling methods for continuous and categorical variables.
6. Document temporal mismatches among occurrence data and predictors.
7. Regenerate the predictor selection table from the predictor manifest and removal log.

### Gate 9 acceptance criteria

- Every predictor has one authoritative source record.
- No manuscript statement conflicts with the manifest or removal log.
- All citations, versions, units, resolutions, and date ranges are verified.

## Work Package 10: Execute and Freeze the Production Run

### Execution requirements

- Start from an empty run directory.
- Use a clean tagged Git commit.
- Use the locked configuration and recorded seeds.
- Run all phases without manual replacement of intermediate artifacts.
- Run all automated tests before and after execution.
- Generate the full figure and table set from the production run.

### Independent production audit

Use a separate audit script to verify:

- Artifact timestamps and run IDs
- Scenario completeness
- Output checksums
- Ensemble weights
- Threshold provenance
- Raster geometry and CRS
- Area identities
- PA-union calculations
- Refugia denominator
- Conflict-index scaling
- Agreement between tables and rasters

### Gate 10 acceptance criteria

- The pipeline exits successfully with no scientific phase skipped.
- All tests pass.
- A second run reproduces tabular results within declared tolerances.
- The production directory is made read-only after verification.
- The Git commit and run ID are recorded in the manuscript claim ledger.

## Work Package 11: Rewrite the Manuscript

The current manuscript is a source draft, not a numerical authority. Revise it only from the frozen production claim ledger.

### Title and abstract

- Use a factual title that reflects the corrected primary result.
- State the model domain accurately.
- Report only verified headline estimates.
- Remove unsupported significance and causal language.
- Describe the conflict product as exposure unless a validated predictive model is completed.

### Introduction

- Narrow the research gap and objectives.
- Verify current conservation status and population statements.
- Avoid presenting expected conservation implications as established results.

### Methods

Document:

- Survey design and interpretation of nondetections
- Duplicate handling and absence of additional thinning
- Model domain and missing-data exclusions
- Predictor provenance and temporal alignment
- Spatial block construction
- Within-fold selection and tuning
- Algorithm definitions
- MaxEnt reference/background treatment
- OOF evaluation
- Ensemble formation
- Threshold derivation
- Future projection hierarchy
- Extrapolation treatment
- Change and refugia definitions
- PA rasterisation and overlap accounting
- Conflict exposure construction and validation
- Reproducibility identifiers

### Results

- Separate model performance from ecological interpretation.
- Report estimates with fold or GCM variability.
- Distinguish national percentages from model-domain percentages.
- Pair future mean changes with GCM agreement and spread.
- Avoid treating thresholded projections as certain habitat outcomes.

### Discussion

- Replace causal terms such as `determinant` with association language unless causality is supported.
- Treat a projected contraction as a possible conservation concern rather than a demonstrated demographic bottleneck.
- Limit climate-conflict conclusions to the validated exposure analysis.
- Discuss sampling effort, nondetection, missing predictor coverage, static nonclimatic predictors, extrapolation, threshold sensitivity, and transferability.
- Separate study findings from management recommendations.

### Conclusions

- Retain only conclusions that survive spatial validation, ensemble sensitivity, GCM uncertainty, and corrected domain accounting.
- Avoid repeating precise values that add no decision value.

### Gate 11 acceptance criteria

- Every quantitative claim has a claim-ledger source.
- Methods match executed code.
- Results distinguish evidence, interpretation, uncertainty, and recommendation.
- Abstract, main text, tables, figures, and supplement agree.

## Work Package 12: Rebuild the Supplement

Include:

- Completed ODMAP checklist
- Data and predictor provenance
- Model-domain coverage and missing-data map
- Predictor selection and removal record
- Spatial blocks and fold composition
- Algorithm settings and tuning
- Fold-level OOF performance
- Calibration and residual diagnostics
- Ensemble membership and weight sensitivity
- Threshold sensitivity
- Individual GCM projections and trajectories
- GCM agreement and spread
- Extrapolation maps and fractions
- Refugia sensitivity across definitions
- PA-union and per-unit overlap reconciliation
- Conflict exposure validation
- Machine-generated tables linked to production artifacts

### Gate 12 acceptance criteria

- No result table is manually transcribed when it can be generated.
- Supplementary labels and cross-references match the main manuscript.
- All sensitivity analyses required by this plan are reported.

## Work Package 13: Editorial, Reference, and Visual Quality Assurance

### Scientific editing

- Verify all literature-dependent claims against primary or authoritative sources.
- Check every DOI, author list, year, journal, volume, and page range.
- Standardise species names, abbreviations, units, SSP notation, periods, and significant figures.
- Remove audit language from the submitted scientific narrative.
- Remove unsupported superlatives and causal claims.

### Document quality

- Generate the final DOCX from the approved content.
- Produce the submission PDF from that DOCX.
- Render and inspect every manuscript and supplement page.
- Put main-text tables on separate pages after the references, as requested by the current Ecology and Evolution author instructions, while retaining readable in-text callouts.
- Upload figures as separate files and retain editable/vector sources; prefer PDF or EPS with embedded fonts for graphs and maps, with publication-resolution TIFF or PNG derivatives where required.
- Ensure every raster figure is at least 300 dpi at its intended reproduction size; use vector output for line art whenever possible.
- Verify map legends, labels, colour accessibility, scale bars, north arrows where useful, coordinate-reference-system statements, jurisdictional neutrality, data-source licences, and panel lettering.
- Check every figure at intended single-column and double-column reproduction widths, not only when zoomed on screen.
- Verify captions without relying on the surrounding text.
- Check headings, page breaks, table continuations, references, and embedded fonts.

### Main-figure remediation

| Figure | Current verdict | Required revision |
|---|---|---|
| Figure 1, study area and spatial CV | Major revision | Regenerate after the corrected fold design. Reduce block saturation; distinguish presence and absence with shape, fill, and outline rather than colour alone; use a colour-vision-safe fold palette; clip all layers to the documented domain; clarify the legend; and verify point locations, scale, and map annotations. |
| Figure 2, variable importance | Major revision | Avoid implying that unlike algorithm-specific importance measures are directly comparable. Use stable predictor ordering, readable display names, a colour-vision-safe category palette, and a reduced main-text view; move the complete predictor-by-algorithm display to the supplement. Export as vector line art. |
| Figure 3, present suitability and protected areas | Major revision | Remove or explicitly relabel percentages that use different denominators. Use a perceptually uniform suitability palette and high-contrast protected-area boundaries; distinguish excluded/model-domain cells from water or zero suitability; reduce unexplained abbreviations; and regenerate from the corrected model-domain and PA-union outputs. |
| Figure 4, future trajectories | Complete redesign | Plot the quantity claimed in the text: suitable habitat area in km2 and/or percentage of the stated domain. Show the 8-GCM distribution or uncertainty interval for every SSP-period combination, include the present baseline, label period ranges rather than ambiguous endpoints, and use redundant colour, linetype, and symbol encodings. |
| Figure 5, refugia | Complete regeneration | Build only from the validated scenario manifest. State the exact denominator (96 individual GCM projections or 12 SSP-period means), use definitions that match the Methods and tables, remove the erroneous n=13 claim, mask to present-suitable habitat, and simplify labels so the stability classes remain visible. |
| Figure 6, conflict product | Complete redesign or removal | Rename as potential human-elephant interaction exposure unless a validated risk model is fitted. Use a fixed scale across present and future panels, choose either a continuous or categorical legend, not both, improve incident visibility, report validation evidence, and remove the figure if the evidentiary gate in Work Package 8 fails. |

### Supplementary-figure remediation

1. Regenerate Figures S1-S5 from corrected spatial OOF predictions. Add linetypes or symbols to ROC and calibration graphics, show calibration uncertainty and bin sample sizes, and remove Figure S4 unless full spatially valid model refitting is performed for every permutation.
2. Redesign Figure S6 as a clustered, landscape, vector correlation display or replace it with a concise table of threshold-exceeding pairs; 35 by 35 labels must remain readable at 100% page view.
3. Recompute Figure S7 from the approved OOF ensemble threshold and show both area and denominator clearly.
4. Retain Figures S8a-S8b only after predictor units, climatic-envelope construction, loadings, and sample groups are explained in standalone captions.
5. Rebuild Figure S9 so its content matches its title. If all 17 predictors by four algorithms cannot be read on one page, split it into logically grouped figures or provide algorithm-specific pages.
6. Reduce overplotting in Figures S10a-S10b; state kernel bandwidth and units and avoid treating presence density as survey effort unless that interpretation is supported.
7. Recompute Figure S11 with the approved threshold and make its agreement denominator explicit.
8. Split Figure S12 into readable period- or scenario-specific pages with a common scale and shared legend.
9. Treat Figure S13 as a diagnostic only unless the reliability score is validated and used in the analysis; avoid red-green good/bad encoding.
10. Replace the Figure S14 spaghetti plot with SSP small multiples and GCM median/range summaries or a compact table; individual GCM traces may remain only if directly labelled and readable.
11. Apply identical scales within each family of Figures S15-S18, enlarge text, use accessible class colours for gain/loss/persistence, and state whether uncertainty is GCM SD, algorithm SD, or total uncertainty.
12. Remove or regenerate Figures S19a-S19f according to the conflict-exposure decision and use one fixed scale across scenarios.
13. Rebuild Figure S20 from genuine OOF residuals, with the sampling design, distance bins, pair counts, and uncertainty envelope reported.
14. Recompute Figure S21 from the final dynamic predictor set; use a shared interpretable scale and include a distributional summary if the mapped extrapolation area is visually too small to assess.

### Table redesign and quality control

1. Generate every numeric table directly from production CSV or database artifacts. The build must fail if a table value cannot be traced to the active run manifest.
2. Redesign main Table 1 into a compact primary discrimination/calibration table and move secondary classification metrics to the supplement. Report fold-level uncertainty or an appropriate summary, not only pooled point estimates.
3. Simplify main Table 2 and make every percentage denominator explicit in the column header or note. Avoid placing national-area and within-category percentages side by side without clear spanners.
4. Redesign main Table 3 around the principal claim: scenario-period area and change with GCM uncertainty. Move gain, loss, persistence, and detailed diagnostics to a supplementary table if needed.
5. Reshape supplementary Table S10 from a 14-column matrix to a long-format or scenario-grouped table; never solve width problems by shrinking text below a readable size.
6. Remove absolute workstation paths from Tables S4 and S14 and replace them with repository-relative paths, versioned scripts, package versions, and artifact identifiers.
7. Correct predictor naming and category inconsistencies in Tables S1-S4, including case, units, and the current misclassification of several MaxEnt distance variables as topography.
8. Rebuild Tables S5-S6 after the corrected folds and threshold are locked; include fold sample sizes and explain the aggregation method.
9. Recompute or remove GCM reliability Tables S8-S10 unless the score has a defensible role in inference; clearly separate descriptive spread from weighting.
10. Rename conflict-risk Tables S11-S12 as exposure-index tables unless Work Package 8 supports a risk model, and state how class cut points were selected.
11. Correct Table S16 cross-references and replace any 'No/not computable' analytical deficiency with the corrected output or an explicit limitation that has passed the relevant scientific gate.
12. Recompute Tables S17-S18 after future scenarios and refugia definitions are corrected.
13. Use concise standalone captions; put units in headers; use consistent significant figures; right-align numbers and decimal places; left-align text; use an em dash only for a defined not-applicable or unavailable value; and explain all abbreviations and symbols in table notes.
14. Use minimal horizontal rules and no heavy grid. Do not repeat units in every cell. Prevent rows from splitting across pages, repeat headers on continued pages, and use landscape orientation only where restructuring cannot provide a clearer result.
15. Check that every table is cited in numerical order and that no table duplicates a figure without a stated reason.

### Objective visual acceptance tests

- All text remains legible at 100% PDF view and at intended journal reproduction width; target at least 8-point equivalent lettering unless the journal specifies otherwise.
- Colour-vision-deficiency simulation preserves every scientific distinction; colour is never the sole encoding for algorithms or scenarios.
- Greyscale printing preserves the hierarchy and essential comparisons.
- All multi-panel figures have consistent panel labels, typography, margins, map extents, and legend placement.
- Axis labels include quantities and units; scales are comparable whenever panels invite comparison; truncated axes or transformed scales are disclosed.
- Captions define samples, periods, scenarios, thresholds, uncertainty summaries, abbreviations, and denominators sufficiently for independent interpretation.
- A scripted cross-check confirms that figure annotations, table cells, captions, Results, and Abstract values agree with their production artifacts.
- A current DOCX-to-PDF render is inspected page by page; superseded April 2026 PDFs are not accepted as evidence of final layout quality.

### Gate 13 acceptance criteria

- DOCX and PDF contain identical content and values.
- No clipping, overlap, missing glyphs, broken cross-reference, or unreadable figure remains.
- Reference and terminology audits pass.
- Every main and supplementary visual has a signed QA record covering scientific accuracy, source artifact, dimensions, effective dpi, font embedding, colour accessibility, caption completeness, and reproduction-width legibility.
- Every table has a signed QA record covering provenance, denominator, units, precision, alignment, continuation behaviour, and agreement with the manuscript.
- No visual contains a scenario count, threshold, metric, class definition, or denominator that differs from the Methods or source artifacts.

## Work Package 14: Build the Submission Package

Prepare:

- Clean main manuscript DOCX
- Clean main manuscript PDF
- Supplementary DOCX and PDF
- Separate publication-resolution figures
- Cover letter
- Author contribution statement
- Funding and competing-interest declarations
- Data and code availability statement
- Reporting checklist
- Repository release tied to the production commit
- Versioned Zenodo archive
- Final claim ledger and validation report retained in the repository

### Gate 14 acceptance criteria

- The repository and archive links resolve.
- The archived code corresponds to the manuscript's production run.
- Sensitive occurrence and conflict coordinates are excluded from the public release.
- Journal filenames, formats, word limits, figure requirements, and declarations are satisfied.

## Implementation Order

| Sequence | Work package | Depends on | Manuscript numbers may change? |
|---:|---|---|---|
| 1 | Preserve current evidence | None | No |
| 2 | Repair run isolation | 1 | No |
| 3 | Rebuild spatial CV | 2 | Yes |
| 4 | Correct algorithms | 3 | Yes |
| 5 | Recalculate evaluation | 4 | Yes |
| 6 | Standardise projections | 4 and 5 | Yes |
| 7 | Rebuild change and refugia | 6 | Yes |
| 8 | Reframe conflict product | 6 | Yes |
| 9 | Verify data provenance | 1 | Possibly |
| 10 | Production run and audit | 2 through 9 | Yes |
| 11 | Rewrite manuscript | 10 | Yes |
| 12 | Rebuild supplement | 10 | Yes |
| 13 | Editorial and visual QA | 11 and 12 | No scientific changes expected |
| 14 | Submission package | 13 | No |

## Progress Tracker

| Work package | Status | Evidence of completion | Approved by |
|---|---|---|---|
| 1. Preserve current evidence | Pending | Archive and pre-correction checksums not created |  |
| 2. Repair run isolation | Code implemented; not executed | Immutable run IDs, reuse refusal, manifest and production clean-worktree checks |  |
| 3. Rebuild spatial CV | Code implemented; not executed | Whole-block assignment, fold composition gates, block-size sensitivity report |  |
| 4. Correct algorithms | Code implemented; not executed | OOF completeness gates; MaxEnt leakage excluded; equal-weight contract |  |
| 5. Recalculate evaluation | Code implemented; not executed | Ensemble evaluated only from spatial OOF predictions; fold-level metrics |  |
| 6. Standardise projections | Code implemented; not executed | Strict 96-scenario and 12-summary contracts; same ensemble definition |  |
| 7. Rebuild change and refugia | Code implemented; not executed | Single OOF threshold; refugia based on 96 individual GCM rasters; 96/96 and 72/96 cutoffs |  |
| 8. Reframe conflict product | Code implemented; not executed | Potential interaction-exposure terminology and shared normalized index |  |
| 9. Verify data provenance | Pending | Input coverage, licences, sensitive-coordinate policy, and archive permissions still require review |  |
| 10. Production run and audit | Pending; explicitly not run | Automated contract tests pass; full scientific outputs not regenerated |  |
| 11. Rewrite manuscript | Pending production results | Requires production artifacts and a claim-to-source ledger |  |
| 12. Rebuild supplement | Partial code; production rebuild pending | Corrected S18 refugia contract; all supplementary outputs require final regeneration/review |  |
| 13. Editorial and visual QA | Pending production outputs | Figure/table generation and QA contracts are implemented; rendered outputs remain unreviewed |  |
| 14. Build submission package | Pending | Requires validated manuscript, final outputs, declarations, and journal checks |  |

## Final Go or No-Go Checklist

Submission is authorised only if every item below is checked.

- [ ] One immutable production run supplies all scientific results.
- [ ] The production run has a clean Git commit and complete manifest.
- [ ] No required phase failed or was skipped.
- [ ] All automated tests pass.
- [ ] All algorithms have genuine spatial OOF predictions.
- [ ] Present and future ensembles use one documented weighting rule.
- [ ] The ensemble threshold comes from OOF ensemble predictions.
- [ ] Ninety-six GCM-SSP-period combinations and twelve summaries are complete.
- [ ] Historical layers are excluded from future scenario counts.
- [ ] Refugia definitions and denominators match the code.
- [ ] National and model-domain areas are reported separately.
- [ ] Future uncertainty and extrapolation are reported.
- [ ] The conflict product has common scaling and an appropriate validation claim.
- [ ] Predictor provenance and removal reasons are verified.
- [ ] Every manuscript value maps to a production artifact.
- [ ] Main and supplementary tables are generated rather than transcribed.
- [ ] Manuscript, supplement, figures, and captions agree.
- [ ] Figure 4 displays habitat-area trajectories with GCM uncertainty, not an unmatched mean-suitability quantity.
- [ ] Refugia figures and tables use the same validated scenario denominator and class definitions.
- [ ] All figures pass reproduction-width, colour-accessibility, and caption-independence checks.
- [ ] All tables are readable without reduced microtype, state units and denominators, and contain no local absolute paths.
- [ ] Separate figure files meet Wiley format, font-embedding, and minimum 300-dpi requirements at final size.
- [ ] References and DOIs have been checked.
- [ ] Final DOCX and PDF have passed page-by-page visual inspection.
- [ ] Repository and Zenodo releases match the production commit.
- [ ] Public archives exclude sensitive coordinates.
- [ ] Ecology and Evolution submission requirements are satisfied.

## Immediate Next Action

Begin Work Package 1. Do not alter the current manuscript numbers or regenerate figures until the pre-correction archive and claim ledger are complete.
