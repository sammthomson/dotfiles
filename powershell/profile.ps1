$dotfilesRoot = Split-Path $PSScriptRoot -Parent

function Add-DotfilesPath {
    param([Parameter(Mandatory = $true)][string] $Path)

    $separator = [System.IO.Path]::PathSeparator
    $comparison = if ($IsWindows) {
        [System.StringComparison]::OrdinalIgnoreCase
    } else {
        [System.StringComparison]::Ordinal
    }
    $entries = @($env:PATH -split [regex]::Escape([string] $separator))
    if (-not ($entries | Where-Object { $_.Equals($Path, $comparison) })) {
        $env:PATH = "$Path$separator$env:PATH"
    }
}

Add-DotfilesPath (Join-Path $HOME ".local\bin")
Add-DotfilesPath (Join-Path $dotfilesRoot "bin")

$env:EDITOR = "emacs -nw"
$env:VISUAL = $env:EDITOR
$env:GIT_EDITOR = $env:EDITOR

function global:.. {
    Set-Location ..
}

function global:e {
    emacs @args
}

function global:et {
    emacs -nw @args
}

function global:gti {
    git @args
}

function global:gs {
    git status @args
}

function global:reload {
    . $PROFILE.CurrentUserAllHosts
}

function global:bak {
    [CmdletBinding(SupportsShouldProcess)]
    param([Parameter(Mandatory = $true, Position = 0)][string] $Path)

    $destination = "$Path.bak"
    if (Test-Path -LiteralPath $destination) {
        throw "Backup already exists: $destination"
    }
    if ($PSCmdlet.ShouldProcess($Path, "Move to $destination")) {
        Move-Item -LiteralPath $Path -Destination $destination
    }
}

function global:tgz {
    param(
        [Parameter(Mandatory = $true, Position = 0)][string] $Archive,
        [Parameter(Mandatory = $true, Position = 1, ValueFromRemainingArguments)]
        [string[]] $Path
    )

    tar -czf $Archive @Path
}

function Get-LocalDotfilesRoot {
    if ($env:DOTFILES_LOCAL_ROOT) {
        return $env:DOTFILES_LOCAL_ROOT
    }

    $runningOnWindows = $PSVersionTable.PSEdition -eq "Desktop" -or $IsWindows
    if ($runningOnWindows) {
        foreach ($oneDriveRoot in @($env:OneDriveCommercial, $env:OneDrive)) {
            if ($oneDriveRoot) {
                $candidate = Join-Path $oneDriveRoot "Documents\dotfiles"
                if (Test-Path -LiteralPath $candidate -PathType Container) {
                    return $candidate
                }
            }
        }
    }

    return $null
}

$localDotfilesRoot = Get-LocalDotfilesRoot
Remove-Item Env:COPILOT_WORKBENCH_WORK_JUDGE -ErrorAction SilentlyContinue
if ($localDotfilesRoot) {
    $localProfile = Join-Path $localDotfilesRoot "powershell\profile.ps1"
    if (Test-Path -LiteralPath $localProfile -PathType Leaf) {
        . $localProfile
        $env:COPILOT_WORKBENCH_WORK_JUDGE = "1"
    }
}
