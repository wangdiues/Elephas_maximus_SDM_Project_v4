# Complete all 8 GCMs and generate publication figures
# Run this from PowerShell after the background phase9_remaining job finishes.
# It will:
#   1. Complete CNRM-ESM2-1 missing scenarios (skip existing)
#   2. Run all publication figures

$R   = "C:\Program Files\R\R-4.4.0\bin\Rscript.exe"
$DIR = "E:\Elephas_maximus_SDM_Project_v4"
$LOG = "$DIR\06_logs"

Write-Host "=== Step 1: Complete CNRM-ESM2-1 missing scenarios ==="
& $R "$DIR\03_analysis\run_phase9_cnrm_esm2_complete.R" 2>"$LOG\cnrm_esm2_complete_err.log" | Tee-Object "$LOG\cnrm_esm2_complete.log"

Write-Host ""
Write-Host "=== Step 2: Generate all publication figures ==="
& $R "$DIR\03_analysis\pub_figures.R" 2>"$LOG\pub_figures_err.log" | Tee-Object "$LOG\pub_figures.log"

Write-Host ""
Write-Host "=== All done. Figures saved to:"
Write-Host "  $DIR\04_outputs\08_figures_tables\"
