param(
    [Parameter(Mandatory = $true)]
    [string]$RunDir,
    [string]$Config = "00_governance/config.yaml"
)

$ErrorActionPreference = "Stop"
$RepoRoot = (Resolve-Path -LiteralPath $PSScriptRoot).Path
$ResolvedRun = (Resolve-Path -LiteralPath $RunDir).Path
if (-not $ResolvedRun.StartsWith((Join-Path $RepoRoot "04_outputs\runs"), [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "RunDir must be an immutable directory below 04_outputs/runs"
}
$RScript = (Get-Command Rscript -ErrorAction Stop).Source
$Script = Join-Path $RepoRoot "03_analysis\16_submission_figures.R"
$ConfigPath = Join-Path $RepoRoot $Config

Push-Location $RepoRoot
try {
    & $RScript $Script $ResolvedRun $ConfigPath
    if ($LASTEXITCODE -ne 0) { throw "Submission figure generation failed with exit code $LASTEXITCODE" }
}
finally {
    Pop-Location
}
