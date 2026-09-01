# Local openrouteservice for New Zealand

This configuration runs openrouteservice locally with only the New Zealand
OpenStreetMap extract and the `driving-car` profile. It exposes the API at
`http://localhost:8080/ors` and raises the local matrix limit to 10,000 routes.

## Requirements

- Docker Desktop for Windows, using the WSL2 backend;
- at least 8 GB system RAM (16 GB recommended);
- at least 10 GB free SSD space.

Docker Desktop should be allowed to use at least 6 GB RAM. The container uses a
maximum Java heap of 6 GB.

## First run

Open PowerShell in the Altitude repository and run:

```powershell
powershell -ExecutionPolicy Bypass -File tools/openrouteservice/setup-nz.ps1
```

The script creates the local folders, downloads Geofabrik's latest New Zealand
PBF if it is not already present, and starts the container. On the first run,
ORS builds a persistent road graph. Follow the build with:

```powershell
docker compose --file tools/openrouteservice/docker-compose.yml logs --follow
```

Wait until the service reports that it is ready, then check:

```powershell
Invoke-RestMethod http://localhost:8080/ors/v2/health
```

The returned status should be `ready`.

## Use from Altitude

Pass the local URL to the updater. No API key or public quota is used:

```r
update_nz_catchment(
  # existing SA2 and population arguments,
  route_airports = "AKL",
  ors_url = "http://localhost:8080/ors"
)
```

Run the major airports one at a time: `AKL`, `CHC`, `WLG`, `ZQN`, and `DUD`.
Then add any regional airports required by the app.

## Normal commands

```powershell
# Start without rebuilding the graph
docker compose --file tools/openrouteservice/docker-compose.yml up --detach

# Stop while retaining the graph and PBF
docker compose --file tools/openrouteservice/docker-compose.yml stop

# View logs
docker compose --file tools/openrouteservice/docker-compose.yml logs --follow
```

To refresh the OSM data later, stop the container, replace
`ors-docker/files/new-zealand-latest.osm.pbf`, remove the existing graph folder,
and rebuild. Do not do this unless a road-network refresh is required.
