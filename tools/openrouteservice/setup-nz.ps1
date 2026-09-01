$ErrorActionPreference = "Stop"

$orsDirectory = Join-Path $PSScriptRoot "ors-docker"
$filesDirectory = Join-Path $orsDirectory "files"
$pbfPath = Join-Path $filesDirectory "new-zealand-latest.osm.pbf"
$pbfUrl = "https://download.geofabrik.de/australia-oceania/new-zealand-latest.osm.pbf"

@("config", "elevation_cache", "files", "graphs", "logs") | ForEach-Object {
    New-Item -ItemType Directory -Force -Path (Join-Path $orsDirectory $_) | Out-Null
}

if (-not (Test-Path $pbfPath)) {
    Write-Host "Downloading the New Zealand OpenStreetMap extract..."
    Invoke-WebRequest -Uri $pbfUrl -OutFile $pbfPath
} else {
    Write-Host "Using existing extract: $pbfPath"
}

Write-Host "Starting openrouteservice. The first graph build can take some time..."
docker compose --file (Join-Path $PSScriptRoot "docker-compose.yml") up --detach

Write-Host "Follow progress with:"
Write-Host "docker compose --file `"$(Join-Path $PSScriptRoot 'docker-compose.yml')`" logs --follow"
