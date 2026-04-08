# ecowittR

R package for downloading and visualizing weather data from **Ecowitt API v3**.

**This packages:** uses `ecowitt_auth()` to store credentials in the session,

---

## Instalation

```r
# install.packages("devtools")
devtools::install_github("Ikhwanramadhan/ecowittR")
```

---

## Usage Flow

```r
library(ecowittR)

# 1. Set your credentials
ecowitt_auth("YOUR_API_KEY", "YOUR_APPLICATION_KEY")

# 2. discover all the stations
ecowitt_stations()

# Example output:
# ── Registered stations (3 stasiun) ─────────────────────────────
#   [1] HidroGFM-CC [GW2000]
#         📍 -6.54321, 106.81234
#   [2] HidroGFM-D3 [GW2000]
#         📍 -6.54400, 106.81300
#   [3] HidroGFM-CS [GW2000]
#         📍 -6.54500, 106.81400

# 3. Show interactive map
ecowitt_map()          # all stations
ecowitt_map(1)         # only station [1]
ecowitt_map("GFM")     # partial match name

# 4. Download historical data
df <- ecowitt_download(start_date = Sys.Date() - 7)          # all station
df <- ecowitt_download(station = 1, start_date = "2026-03-01",
                       end_date = "2026-03-31", interval = "hourly")

# Variable
df <- ecowitt_download(
  station   = c(1, 2),
  start_date = Sys.Date() - 2,
  interval   = "5min",
  variables  = c("temp", "humidity", "rainfall")
)

# 5. Data real-time
ecowitt_realtime()       # all stations
ecowitt_realtime(1)      # station [1]
ecowitt_realtime("GFM")  # partial match
```

---

## Functions

| Function | Descriptions |
|--------|-----------|
| `ecowitt_auth(api_key, application_key)` | Set credentials (once per session) |
| `ecowitt_stations()` | Auto-discover & load all stations from account |
| `ecowitt_map(station)` | Interactive map of stations |
| `ecowitt_download(station, start_date, ...)` | Download historical data |
| `ecowitt_realtime(station)` | Current sensor readings |

---

## `station` parameters

All functions accept `station` arguments in various formats:
```r
ecowitt_download(station = NULL)          # all stations
ecowitt_download(station = 1)             # serial number
ecowitt_download(station = c(1, 3))       # some numbers
ecowitt_download(station = "GFM-CC")      # partial match
ecowitt_download(station = "F0:F5:BD:...") # MAC address
```

---

## Weather variables

| Argument | Information | Unit |
|---------|------------|--------|
| `temp` | Air temperature | °C |
| `dew_point` | Dew point temperature | °C |
| `humidity` | Relative humidity | % |
| `rainfall` | Rainfall | mm |
| `rain_rate` | Rain intensity | mm/hr |
| `pressure` | Air pressure | hPa |
| `solar` | Solar radiation | W/m² |
| `wind_speed` | Wind speed | m/s |
| `wind_dir` | Wind direction | degrees |
