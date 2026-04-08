# ============================================================
#  ecowittR — Historical Data Download
#  Intervals: "5min" and "hourly"
# ============================================================

#' Download Historical Weather Data
#'
#' Fetch historical data from one or more stations. Data is retrieved
#' day-by-day internally to preserve the device's native resolution.
#'
#' **Rainfall note:**
#' This function uses the \code{rainfall$daily} field (daily cumulative)
#' from the API and computes differences between consecutive timestamps
#' (\emph{diff}) to obtain rainfall per interval. This approach is the most
#' accurate as it reflects the physical sensor counter. Midnight resets
#' (negative diffs) are handled automatically.
#'
#' **\code{rain_rate} and \code{rain_rate_peak} note:**
#' At \code{"5min"}: \code{rain_rate} is the instantaneous sensor reading
#' (mm/hr). At \code{"hourly"}: \code{rain_rate} is re-derived from the
#' actual accumulation (\code{rainfall / 1 hour}) so it is fully consistent
#' with the \code{rainfall} column. The sensor's instantaneous peak is
#' available as \code{rain_rate_peak} (only at \code{"hourly"}).
#'
#' @param station  Station(s) to download data for:
#'   \describe{
#'     \item{\code{NULL}}{All loaded stations (default)}
#'     \item{Integer}{Station number(s), e.g. \code{1} or \code{c(1, 3)}}
#'     \item{Character}{Partial name match, e.g. \code{"GFM-CC"}}
#'   }
#' @param start_date Date or \code{"YYYY-MM-DD"}. Start of period.
#' @param end_date   Date or \code{"YYYY-MM-DD"}. End of period. Default: today.
#' @param interval   \code{"5min"} or \code{"hourly"}. Default: \code{"5min"}.
#' @param variables  Variables to include. Default: all.
#'   Options: \code{"temp"}, \code{"dew_point"}, \code{"humidity"},
#'   \code{"rainfall"}, \code{"rain_rate"},
#'   \code{"rain_rate_peak"} (only at \code{"hourly"}),
#'   \code{"pressure"}, \code{"solar"}, \code{"wind_speed"},
#'   \code{"wind_dir"}.
#' @param tz      Output timezone. Default \code{"Asia/Jakarta"}.
#' @param verbose Print progress messages. Default \code{TRUE}.
#'
#' @return A tibble with columns \code{station}, \code{mac}, \code{time},
#'   and the selected weather variables.
#'   \itemize{
#'     \item \code{rainfall}: rainfall per interval (mm). Not cumulative.
#'     \item \code{rain_rate}: rainfall intensity (mm/hr).
#'       At \code{"5min"}: instantaneous sensor value.
#'       At \code{"hourly"}: derived from actual accumulation
#'       (equals \code{rainfall} since window = 1 hour).
#'     \item \code{rain_rate_peak}: instantaneous peak intensity from
#'       sensor (mm/hr). Only available at \code{"hourly"}.
#'   }
#'
#' @examples
#' \dontrun{
#' ecowitt_auth("api_key", "app_key")
#' ecowitt_stations()
#'
#' # All stations, last 7 days, 5-minute resolution
#' df <- ecowitt_download(start_date = Sys.Date() - 7)
#'
#' # Station [1], hourly interval
#' df <- ecowitt_download(station = 1, start_date = "2026-03-01",
#'                        end_date = "2026-03-31", interval = "hourly")
#'
#' # Selected variables
#' df <- ecowitt_download(
#'   station    = c(1, 2),
#'   start_date = Sys.Date() - 2,
#'   variables  = c("temp", "humidity", "rainfall")
#' )
#' }
#' @export
ecowitt_download <- function(
    station    = NULL,
    start_date,
    end_date   = Sys.Date(),
    interval   = c("5min", "hourly"),
    variables  = c("temp", "dew_point", "humidity", "rainfall", "rain_rate",
                   "pressure", "solar", "wind_speed", "wind_dir"),
    tz         = "Asia/Jakarta",
    verbose    = TRUE
) {
  cred       <- .get_cred()
  interval   <- match.arg(interval)
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (start_date > end_date)
    cli::cli_abort("{.arg start_date} must not be later than {.arg end_date}.")

  all_vars <- c("temp", "dew_point", "humidity", "rainfall", "rain_rate",
                "rain_rate_peak", "pressure", "solar", "wind_speed", "wind_dir")
  bad_vars <- setdiff(variables, all_vars)
  if (length(bad_vars) > 0)
    cli::cli_abort("Unknown variable(s): {.val {bad_vars}}")

  mac_vec <- .resolve(station)
  reg     <- .get_registry()
  n_days  <- as.numeric(end_date - start_date) + 1

  if (verbose) {
    cli::cli_h1("ecowittR — Download Data")
    nms <- if (!is.null(reg)) reg$name[match(mac_vec, reg$mac)] else mac_vec
    cli::cli_bullets(c(
      "*" = "Station(s) : {paste(nms, collapse = ', ')}",
      "*" = "Period     : {start_date} to {end_date} ({n_days} day(s))",
      "*" = "Interval   : {interval}",
      "*" = "Variables  : {paste(variables, collapse = ', ')}"
    ))
  }

  all_results <- lapply(seq_along(mac_vec), function(i) {
    m  <- mac_vec[i]
    nm <- if (!is.null(reg)) reg$name[reg$mac == m][1] else m

    if (verbose)
      cli::cli_progress_step("[{i}/{length(mac_vec)}] Fetching: {nm}")

    df_raw <- .fetch_chunked(m, start_date, end_date, tz, cred, verbose)
    if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)

    df_raw <- dplyr::mutate(df_raw,
      station = nm %||% m,
      mac     = m,
      .before = 1
    )

    # Compute per-interval rainfall from cumulative daily diff
    df_raw <- .calc_rainfall_diff(df_raw)

    # Aggregate if needed
    df_out <- switch(interval,
      "5min"  = df_raw,
      "hourly" = .agg_1hour(df_raw)
    )

    keep <- c("station", "mac", "time", intersect(variables, names(df_out)))
    df_out[, keep, drop = FALSE]
  })

  df_final <- dplyr::bind_rows(all_results)

  if (nrow(df_final) == 0) {
    cli::cli_warn("No data could be retrieved.")
    return(dplyr::tibble())
  }

  if (verbose)
    cli::cli_alert_success(
      "Done: {nrow(df_final)} rows | {length(mac_vec)} station(s) | interval {interval}."
    )

  df_final
}


# ---- Internal: fetch one day from API --------------------------------------

.fetch_one_day <- function(mac, date, tz, cred) {
  start_str <- paste(format(date, "%Y-%m-%d"), "00:00:00")
  end_str   <- paste(format(date, "%Y-%m-%d"), "23:59:59")

  url <- paste0(
    .base_url,
    "?application_key=", cred$application_key,
    "&api_key=",         cred$api_key,
    "&mac=",             mac,
    "&start_date=",      utils::URLencode(start_str, reserved = TRUE),
    "&end_date=",        utils::URLencode(end_str,   reserved = TRUE),
    "&call_back=outdoor,rainfall,pressure,solar_and_uvi,wind"
  )

  res <- tryCatch({
    r <- httr::GET(url)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)

  if (is.null(res) || is.null(res$data)) return(NULL)
  d <- res$data
  if (!"outdoor" %in% names(d) || !"rainfall" %in% names(d)) return(NULL)

  wu <- as.numeric(names(d$outdoor$temperature$list))
  if (length(wu) == 0) return(NULL)
  n <- length(wu)

  dplyr::tibble(
    time      = as.POSIXct(wu, origin = "1970-01-01", tz = "UTC") |>
      lubridate::with_tz(tz),
    temp      = .f_to_c(as.numeric(unlist(d$outdoor$temperature$list))),
    dew_point = .f_to_c(as.numeric(unlist(d$outdoor$dew_point$list))),
    humidity  = as.numeric(unlist(d$outdoor$humidity$list)),
    # rainfall_cum: daily cumulative (in -> mm). Converted to per-interval by .calc_rainfall_diff
    rainfall_cum = if (!is.null(d$rainfall$daily$list))
      .in_to_mm(as.numeric(unlist(d$rainfall$daily$list)))
    else rep(NA_real_, n),
    # rain_rate: instantaneous sensor intensity (in/hr -> mm/hr)
    rain_rate = if (!is.null(d$rainfall$rain_rate$list))
      .in_to_mm(as.numeric(unlist(d$rainfall$rain_rate$list)))
    else rep(NA_real_, n),
    pressure  = if (!is.null(d$pressure$relative$list))
      .inhg_to_hpa(as.numeric(unlist(d$pressure$relative$list))) else rep(NA_real_, n),
    solar     = if (!is.null(d$solar_and_uvi$solar$list))
      as.numeric(unlist(d$solar_and_uvi$solar$list)) else rep(NA_real_, n),
    wind_speed = if (!is.null(d$wind$wind_speed$list))
      .mph_to_ms(as.numeric(unlist(d$wind$wind_speed$list))) else rep(NA_real_, n),
    wind_dir   = if (!is.null(d$wind$wind_direction$list))
      as.numeric(unlist(d$wind$wind_direction$list)) else rep(NA_real_, n)
  )
}


# ---- Internal: chunk by day ------------------------------------------------

.fetch_chunked <- function(mac, start_date, end_date, tz, cred, verbose) {
  days    <- seq.Date(start_date, end_date, by = "day")
  results <- vector("list", length(days))

  for (i in seq_along(days)) {
    results[[i]] <- tryCatch(
      .fetch_one_day(mac, days[[i]], tz, cred),
      error = function(e) NULL
    )
    if (i < length(days)) Sys.sleep(0.25)
  }

  df <- dplyr::bind_rows(results)
  if (nrow(df) == 0) return(NULL)
  dplyr::distinct(df, time, .keep_all = TRUE) |> dplyr::arrange(time)
}


# ---- Internal: cumulative diff -> per-interval rainfall --------------------
#
# Principle:
#   rainfall[i] = rainfall_cum[i] - rainfall_cum[i-1]
#
# Edge cases:
#   1. First row or after a NA gap:
#      Set rainfall[i] = rainfall_cum[i] (accumulation since midnight)
#      -> may overestimate if rain had already fallen before the query window
#      -> mitigation: start query from midnight (00:00 value is typically 0)
#   2. diff < 0  -> midnight reset:
#      Set rainfall[i] = rainfall_cum[i] (new accumulation since reset)
#   3. diff > 50 mm in one step (~5 min) -> sensor anomaly:
#      Set rainfall[i] = NA
#   4. rainfall_cum[i] = NA -> rainfall[i] = NA

.calc_rainfall_diff <- function(df) {
  cum      <- df$rainfall_cum
  n        <- length(cum)
  rainfall <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (is.na(cum[i])) {
      rainfall[i] <- NA_real_
      next
    }
    if (i == 1 || is.na(cum[i - 1])) {
      rainfall[i] <- cum[i]
    } else {
      d <- cum[i] - cum[i - 1]
      if (d < 0) {
        # Midnight reset
        rainfall[i] <- cum[i]
      } else if (d > 50) {
        # Sensor anomaly (> 50 mm in one ~5-min step)
        rainfall[i] <- NA_real_
      } else {
        rainfall[i] <- d
      }
    }
  }

  df |>
    dplyr::mutate(rainfall = rainfall) |>
    dplyr::select(-rainfall_cum)
}


# ---- Internal: 1-hour aggregation ------------------------------------------
#
# rainfall       -> SUM of diffs  = total rainfall in the hour (mm)
# rain_rate      -> DERIVED: equal to rainfall since window = 1 hour (mm/hr)
#                   Fully consistent with the rainfall column;
#                   unaffected by instantaneous sensor spikes.
# rain_rate_peak -> MAX of 5-min rain_rate = peak instantaneous intensity (mm/hr)
#                   Useful for erosivity analysis, etc.
# temp, etc.     -> MEAN
# wind_dir       -> MEAN (simple circular approximation)

.agg_1hour <- function(df) {
  df |>
    dplyr::mutate(time = lubridate::floor_date(time, "1 hour")) |>
    dplyr::group_by(station, mac, time) |>
    dplyr::summarise(
      temp           = mean(temp,       na.rm = TRUE),
      dew_point      = mean(dew_point,  na.rm = TRUE),
      humidity       = mean(humidity,   na.rm = TRUE),
      rainfall       = sum(rainfall,    na.rm = TRUE),
      # rain_rate derived: total rainfall (mm) over 1-hour window = mm/hr directly
      rain_rate      = sum(rainfall,    na.rm = TRUE),
      rain_rate_peak = max(rain_rate,   na.rm = TRUE),
      pressure       = mean(pressure,   na.rm = TRUE),
      solar          = mean(solar,      na.rm = TRUE),
      wind_speed     = mean(wind_speed, na.rm = TRUE),
      wind_dir       = mean(wind_dir,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
      )
    )
}
