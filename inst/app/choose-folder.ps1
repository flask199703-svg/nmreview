param(
    [Parameter(Mandatory = $true)][string]$ConfigPath,
    [Parameter(Mandatory = $true)][string]$ResultPath
)
$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Windows.Forms
[System.Windows.Forms.Application]::EnableVisualStyles()
$config = Get-Content -LiteralPath $ConfigPath -Encoding UTF8 -Raw | ConvertFrom-Json
$dialog = New-Object System.Windows.Forms.FolderBrowserDialog
$owner = New-Object System.Windows.Forms.Form
try {
    $owner.ShowInTaskbar = $false
    $owner.TopMost = $true
    $owner.Opacity = 0
    $owner.Width = 1
    $owner.Height = 1
    $owner.StartPosition = 'CenterScreen'
    $owner.Show()
    $dialog.Description = [string]$config.caption
    $dialog.ShowNewFolderButton = $false
    if (![string]::IsNullOrWhiteSpace([string]$config.default) -and
        (Test-Path -LiteralPath ([string]$config.default) -PathType Container)) {
        $dialog.SelectedPath = [string]$config.default
    }
    if ($dialog.PSObject.Properties.Name -contains 'UseDescriptionForTitle') {
        $dialog.UseDescriptionForTitle = $true
    }
    $selected = $null
    if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {
        $selected = $dialog.SelectedPath
    }
    $json = ConvertTo-Json -InputObject $selected -Compress
    [IO.File]::WriteAllText($ResultPath, $json, [Text.UTF8Encoding]::new($false))
}
finally {
    $dialog.Dispose()
    $owner.Dispose()
}
