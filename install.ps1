[CmdletBinding()]
param(
    [string] $PersonalCodeRoot = (Join-Path $HOME "code\sammthomson"),
    [string] $ProfilePath = $PROFILE.CurrentUserAllHosts
)

$ErrorActionPreference = "Stop"
$repoRoot = $PSScriptRoot
$managedStart = "# >>> sammthomson dotfiles >>>"
$managedEnd = "# <<< sammthomson dotfiles <<<"
$profileTarget = $ProfilePath
$trackedProfile = Join-Path $repoRoot "powershell\profile.ps1"
$emacsConfigSource = Join-Path $repoRoot "home\emacs.d"
$emacsConfigTarget = Join-Path $env:APPDATA ".emacs.d"

function ConvertTo-GitPath {
    param([Parameter(Mandatory = $true)][string] $Path)

    return [System.IO.Path]::GetFullPath($Path).Replace("\", "/")
}

function Add-GitConfigValue {
    param(
        [Parameter(Mandatory = $true)][string] $Key,
        [Parameter(Mandatory = $true)][string] $Value
    )

    $existing = @(git config --global --get-all $Key 2>$null)
    if ($Value -notin $existing) {
        git config --global --add $Key $Value
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to add Git configuration: $Key"
        }
    }
}

if (-not (Test-Path -LiteralPath $trackedProfile -PathType Leaf)) {
    throw "Tracked PowerShell profile not found: $trackedProfile"
}
if (-not (Test-Path -LiteralPath $emacsConfigSource -PathType Container)) {
    throw "Tracked Emacs configuration not found: $emacsConfigSource"
}

$profileDirectory = Split-Path $profileTarget -Parent
New-Item -ItemType Directory -Force -Path $profileDirectory | Out-Null

$existingProfile = if (Test-Path -LiteralPath $profileTarget) {
    [System.IO.File]::ReadAllText($profileTarget)
} else {
    ""
}

$escapedProfile = $trackedProfile.Replace("'", "''")
$managedBlock = @"
$managedStart
`$dotfilesProfile = '$escapedProfile'
if (Test-Path -LiteralPath `$dotfilesProfile) {
    . `$dotfilesProfile
}
$managedEnd
"@

$escapedStart = [regex]::Escape($managedStart)
$escapedEnd = [regex]::Escape($managedEnd)
$managedPattern = "(?ms)^$escapedStart.*?^$escapedEnd\s*"
if ($existingProfile -match $managedPattern) {
    $newProfile = [regex]::Replace($existingProfile, $managedPattern, "$managedBlock`r`n")
} else {
    $separator = if ($existingProfile.Length -gt 0 -and -not $existingProfile.EndsWith("`n")) {
        "`r`n`r`n"
    } elseif ($existingProfile.Length -gt 0) {
        "`r`n"
    } else {
        ""
    }
    $newProfile = "$existingProfile$separator$managedBlock`r`n"
}

[System.IO.File]::WriteAllText(
    $profileTarget,
    $newProfile,
    [System.Text.UTF8Encoding]::new($false)
)

New-Item -ItemType Directory -Force -Path (Join-Path $HOME ".local\bin") | Out-Null

if (-not (Test-Path -LiteralPath $emacsConfigTarget)) {
    New-Item -ItemType Junction -Path $emacsConfigTarget -Target $emacsConfigSource |
        Out-Null
} else {
    $emacsConfigItem = Get-Item -LiteralPath $emacsConfigTarget
    $resolvedSource = [System.IO.Path]::GetFullPath($emacsConfigSource)
    $resolvedTargets = @($emacsConfigItem.Target) |
        ForEach-Object { [System.IO.Path]::GetFullPath($_) }
    if ($resolvedSource -notin $resolvedTargets) {
        Write-Warning "Existing Emacs configuration was not replaced: $emacsConfigTarget"
    }
}

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    throw "Git is required to install the Git configuration."
}

$commonGitConfig = ConvertTo-GitPath (Join-Path $repoRoot "git\common.gitconfig")
$personalGitConfig = ConvertTo-GitPath (Join-Path $repoRoot "git\personal.gitconfig")
$personalRoot = (ConvertTo-GitPath $PersonalCodeRoot).TrimEnd("/") + "/"

Add-GitConfigValue "include.path" $commonGitConfig
Add-GitConfigValue "includeIf.gitdir/i:$personalRoot.path" $personalGitConfig

Write-Host "Installed PowerShell profile stub: $profileTarget"
Write-Host "Installed Emacs configuration: $emacsConfigTarget"
Write-Host "Installed common Git include: $commonGitConfig"
Write-Host "Installed personal Git identity for: $personalRoot"
Write-Host "Restart PowerShell or run: . `$PROFILE.CurrentUserAllHosts"
