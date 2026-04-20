# Run publication figures generation
# Usage: .\run_pub_figures.ps1
# Must be run AFTER Phase 9 completes for all 8 GCMs

$RScript = "C:\Program Files\R\R-4.4.0\bin\Rscript.exe"
$Script  = "E:\Elephas_maximus_SDM_Project_v4\03_analysis\pub_figures.R"
$Log     = "E:\Elephas_maximus_SDM_Project_v4\06_logs\pub_figures.log"
$ErrLog  = "E:\Elephas_maximus_SDM_Project_v4\06_logs\pub_figures_err.log"

Write-Host "Starting publication figures generation..."
Write-Host "Log: $Log"

& $RScript $Script 2>$ErrLog | Tee-Object -FilePath $Log

Write-Host "Done. Check $Log for output."
