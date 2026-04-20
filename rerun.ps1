param(
  [string]$Config = "00_governance/config.yaml"
)

$ErrorActionPreference = "Stop"
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $scriptRoot

function Resolve-Rscript {
  $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }

  $candidates = @(
    "C:\Program Files\R\R-4.4.0\bin\Rscript.exe",
    "C:\Program Files\R\R-4.4.0\bin\x64\Rscript.exe"
  )

  foreach ($path in $candidates) {
    if (Test-Path $path) { return $path }
  }

  $latest = Get-ChildItem "C:\Program Files\R" -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -like "R-*" } |
    Sort-Object Name -Descending |
    Select-Object -First 1

  if ($latest) {
    $dynamic = Join-Path $latest.FullName "bin\Rscript.exe"
    if (Test-Path $dynamic) { return $dynamic }
  }

  throw "Rscript not found. Install R 4.3+ or add Rscript.exe to PATH."
}

$rscript = Resolve-Rscript
Write-Host "Using Rscript: $rscript"
Write-Host "Working directory: $(Get-Location)"

& $rscript "03_analysis/run_pipeline.R" $Config
exit $LASTEXITCODE
