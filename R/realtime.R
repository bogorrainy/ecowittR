# ============================================================
#  ecowittR v2 — Real-Time Data
# ============================================================

#' Fetch Real-Time Weather Data
#'
#' Retrieves the latest sensor readings from one or more stations.
#'
#' @param station Station(s) to query:
#'   - \code{NULL} (default): all loaded stations
#'   - Integer: station number(s), e.g. \code{1} or \code{c(1, 2)}
#'   - Character: station name (partial match)
#' @param tz Output timezone. Default \code{"Asia/Jakarta"}.
#'
#' @return A tibble with one row per station containing the latest
#'   sensor readings.
#'
#' @examples
#' \dontrun{
#' ecowitt_auth("api_key", "app_key")
#' ecowitt_stations()
#'
#' ecowitt_realtime()         # all stations
#' ecowitt_realtime(1)        # station [1]
#' ecowitt_realtime("GFM")    # partial name match
#' }
#' @export
ecowitt_realtime <- function(station = NULL, tz = "Asia/Jakarta") {
  cred    <- .get_cred()
  mac_vec <- .resolve(station)
  reg     <- .get_registry()

  results <- lapply(mac_vec, function(m) {
    nm <- if (!is.null(reg)) reg$name[reg$mac == m][1] %||% m else m
    .fetch_rt_one(m, nm, tz, cred)
  })

  df <- dplyr::bind_rows(results)
  cli::cli_alert_success(
    "Real-time data for {length(mac_vec)} station(s) @ {format(Sys.time(), '%H:%M:%S')}."
  )
  df
}


# ---- Internal --------------------------------------------------------------

.fetch_rt_one <- function(mac, name, tz, cred) {
  url <- paste0(
    .realtime_url,
    "?application_key=", cred$application_key,
    "&api_key=",         cred$api_key,
    "&mac=",             mac,
    "&call_back=outdoor,rainfall,solar_and_uvi,pressure,wind"
  )

  res <- tryCatch({
    r <- httr::GET(url)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE)
  }, error = function(e) {
    cli::cli_warn("Real-time fetch failed for {name}: {e$message}")
    NULL
  })

  empty <- dplyr::tibble(
    station        = name,
    mac            = mac,
    time           = as.POSIXct(NA),
    temp           = NA_real_,
    dew_point      = NA_real_,
    humidity       = NA_real_,
    rainfall_today = NA_real_,
    rain_rate      = NA_real_,
    pressure       = NA_real_,
    solar          = NA_real_,
    wind_speed     = NA_real_,
    wind_dir       = NA_real_,
    status         = "error"
  )

  if (is.null(res) || is.null(res$data) || !is.list(res$data)) return(empty)
  d <- res$data

  time_val <- if (!is.null(d$time))
    lubridate::with_tz(
      as.POSIXct(as.numeric(d$time), origin = "1970-01-01", tz = "UTC"), tz
    ) else Sys.time()

  sn <- function(x) if (!is.null(x)) as.numeric(x) else NA_real_

  temp_f    <- sn(d$outdoor$temperature$value)
  dp_f      <- sn(d$outdoor$dew_point$value)
  pres_inhg <- sn(d$pressure$relative$value)
  ws_mph    <- sn(d$wind$wind_speed$value)
  rain_in   <- sn(d$rainfall$daily$value)
  rrate_in  <- sn(d$rainfall$rain_rate$value)

  dplyr::tibble(
    station        = name,
    mac            = mac,
    time           = time_val,
    temp           = if (!is.na(temp_f))    .f_to_c(temp_f)         else NA_real_,
    dew_point      = if (!is.na(dp_f))      .f_to_c(dp_f)           else NA_real_,
    humidity       = sn(d$outdoor$humidity$value),
    rainfall_today = if (!is.na(rain_in))   .in_to_mm(rain_in)      else NA_real_,
    rain_rate      = if (!is.na(rrate_in))  .in_to_mm(rrate_in)     else NA_real_,
    pressure       = if (!is.na(pres_inhg)) .inhg_to_hpa(pres_inhg) else NA_real_,
    solar          = sn(d$solar_and_uvi$solar$value),
    wind_speed     = if (!is.na(ws_mph))    .mph_to_ms(ws_mph)      else NA_real_,
    wind_dir       = sn(d$wind$wind_direction$value),
    status         = "ok"
  )
}
