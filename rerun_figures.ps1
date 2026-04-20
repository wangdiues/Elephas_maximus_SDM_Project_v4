param(
  [string]$RunDir = "",
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
  throw "Rscript not found."
}

if (-not $RunDir) {
  $runs = Get-ChildItem "04_outputs\runs" -Directory | Sort-Object LastWriteTime -Descending
  if ($runs.Count -eq 0) { throw "No runs found in 04_outputs/runs" }
  $RunDir = $runs[0].FullName
}

$rscript = Resolve-Rscript
Write-Host "Using Rscript: $rscript"
Write-Host "Run directory: $RunDir"

& $rscript "rerun_figures.R" $RunDir $Config
exit $LASTEXITCODE
