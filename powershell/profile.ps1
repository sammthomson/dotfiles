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

function global:.. {
    Set-Location ..
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

$localProfile = Join-Path (Split-Path $PROFILE.CurrentUserAllHosts -Parent) "profile.local.ps1"
if (Test-Path -LiteralPath $localProfile -PathType Leaf) {
    . $localProfile
}
