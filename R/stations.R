# ============================================================
#  ecowittR v2 — Station Listing & Map
#  Auto-discover: no MAC input needed; all stations fetched from API
# ============================================================

#' Load & Display Weather Stations
#'
#' When called \strong{with no arguments}, this function automatically
#' fetches all stations registered in your ecowitt.net account via the
#' API. No need to type MAC addresses one by one.
#'
#' Optionally, you can still filter by \code{mac} or provide custom
#' alias names.
#'
#' @param mac  Character vector (optional). If provided, only stations
#'   with these MAC addresses are loaded. Default \code{NULL} = fetch
#'   all stations from your account automatically.
#' @param name Character vector (optional). Custom alias names for each
#'   station (must be the same length as \code{mac} if \code{mac} is
#'   provided).
#' @param print Logical. Print a summary to the console. Default \code{TRUE}.
#'
#' @return A data frame of stations (invisibly). Also stored in the
#'   session so other functions can reference stations by name or number.
#'
#' @examples
#' \dontrun{
#' ecowitt_auth("api_key", "app_key")
#'
#' # Auto-discover all stations in your account (no MAC needed!)
#' ecowitt_stations()
#'
#' # Or filter to specific MACs
#' ecowitt_stations(mac = c("F0:F5:BD:83:3B:A8", "F0:F5:BD:84:68:60"))
#'
#' # With custom alias names (mac must also be provided)
#' ecowitt_stations(
#'   mac  = c("F0:F5:BD:83:3B:A8", "F0:F5:BD:84:68:60"),
#'   name = c("Building-A", "Building-B")
#' )
#'
#' # Then use in other functions:
#' ecowitt_map()
#' ecowitt_download(station = 1, start_date = Sys.Date() - 7)
#' ecowitt_realtime()
#' }
#' @export
ecowitt_stations <- function(mac = NULL, name = NULL, print = TRUE) {
  cred <- .get_cred()

  # ---- Mode 1: auto-discover from API ----------------------------------------
  if (is.null(mac)) {
    cli::cli_progress_step("Fetching station list from account...")
    mac_list <- .fetch_all_macs(cred)

    if (length(mac_list) == 0)
      cli::cli_abort(c(
        "No stations found in this account.",
        "i" = "Check your api_key and application_key."
      ))

    cli::cli_progress_done()
    cli::cli_inform("Found {length(mac_list)} station(s). Fetching details...")

    mac <- mac_list
  } else {
    # Validate MAC format if provided manually
    valid <- grepl("^([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}$", mac)
    if (any(!valid))
      cli::cli_abort(
        "Invalid MAC format: {.val {mac[!valid]}}. Use XX:XX:XX:XX:XX:XX format."
      )
  }

  # Validate name length if provided
  if (!is.null(name) && length(name) != length(mac))
    cli::cli_abort(
      "Length of {.arg name} ({length(name)}) must match number of stations ({length(mac)})."
    )

  # Fetch detail info for each station
  n_mac <- length(mac)
  pb    <- cli::cli_progress_bar(
    name   = "Fetching station info",
    total  = n_mac,
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total}",
    .envir = environment()
  )

  rows <- vector("list", n_mac)
  for (i in seq_len(n_mac)) {
    info <- .fetch_station_info(mac[i], cred)
    if (!is.null(name)) info$name <- name[i]
    info$no <- i
    rows[[i]] <- info
    cli::cli_progress_update(id = pb, .envir = environment())
  }

  cli::cli_progress_done(id = pb, .envir = environment())

  df <- dplyr::bind_rows(rows) |>
    dplyr::select(no, name, mac, lat, lon, model) |>
    dplyr::mutate(
      name = dplyr::if_else(is.na(name) | name == "", mac, name)
    )

  .set_registry(df)

  if (print) .print_station_table(df)

  invisible(df)
}


#' Display Stations on an Interactive Map
#'
#' Renders a leaflet map with markers for each loaded station.
#' Click a marker to see its name, coordinates, and model.
#'
#' @param station Stations to display:
#'   - \code{NULL} (default): all loaded stations
#'   - Integer: station number(s), e.g. \code{c(1, 3)}
#'   - Character: station name (partial match), e.g. \code{"GFM"}
#' @param tile Character. Map tile provider. Default \code{"CartoDB.Positron"}.
#'
#' @return A leaflet map widget.
#' @export
ecowitt_map <- function(station = NULL, tile = "CartoDB.Positron") {
  reg <- .get_registry()
  if (is.null(reg) || nrow(reg) == 0)
    cli::cli_abort(c(
      "Station list not loaded.",
      "i" = "Run {.fn ecowitt_stations} first."
    ))

  if (!is.null(station)) {
    mac_sel <- .resolve(station)
    df      <- reg[reg$mac %in% mac_sel, ]
  } else {
    df <- reg
  }

  df <- dplyr::filter(df, !is.na(lat), !is.na(lon))
  if (nrow(df) == 0)
    cli::cli_abort("No stations with valid coordinates to display.")

  colors <- c("cadetblue", "red", "orange", "green", "purple",
              "darkblue", "darkred", "darkgreen", "darkpurple", "beige")

  icons <- leaflet::awesomeIcons(
    icon        = "cloud",
    library     = "fa",
    iconColor   = "white",
    markerColor = colors[(df$no - 1) %% length(colors) + 1]
  )

  popups <- mapply(function(no, name, mac, lat, lon, model) {
    coord     <- paste0(round(lat, 5), ", ", round(lon, 5))
    model_str <- if (!is.na(model)) paste0(
      "<br><span style='font-size:11px;color:#94a3b8;'>&#128225; ", model, "</span>"
    ) else ""
    paste0(
      "<div style='font-family:sans-serif;min-width:200px;line-height:1.6;'>",
      "<span style='font-size:11px;color:#94a3b8;font-weight:600;'>STATION #", no, "</span><br>",
      "<b style='font-size:15px;color:#0f172a;'>", name, "</b>",
      model_str,
      "<hr style='margin:7px 0;border-color:#f1f5f9;'>",
      "<span style='font-size:12px;'>&#128205; ", coord, "</span><br>",
      "<span style='font-size:10px;color:#cbd5e1;'>", mac, "</span>",
      "</div>"
    )
  }, df$no, df$name, df$mac, df$lat, df$lon, df$model,
  SIMPLIFY = TRUE)

  m <- leaflet::leaflet(df) |>
    leaflet::addProviderTiles(tile) |>
    leaflet::setView(
      lng  = mean(df$lon, na.rm = TRUE),
      lat  = mean(df$lat, na.rm = TRUE),
      zoom = 12
    ) |>
    leaflet::addAwesomeMarkers(
      lng    = ~lon,
      lat    = ~lat,
      icon   = icons,
      label  = ~paste0("[", no, "] ", name),
      popup  = popups
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = colors[(df$no - 1) %% length(colors) + 1],
      labels   = paste0("[", df$no, "] ", df$name),
      title    = "Weather Stations",
      opacity  = 0.9
    )

  cli::cli_alert_success("Map showing {nrow(df)} station(s). Click markers for details.")
  m
}


# ---- Internal: fetch all MACs from device/list API -------------------------

.fetch_all_macs <- function(cred) {
  url <- paste0(
    .list_url,
    "?application_key=", cred$application_key,
    "&api_key=",         cred$api_key
  )

  res <- tryCatch({
    r <- httr::GET(url)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE)
  }, error = function(e) {
    cli::cli_abort("Failed to contact Ecowitt API: {e$message}")
  })

  if (is.null(res) || (!is.null(res$code) && res$code != 0)) {
    msg <- res$msg %||% "Invalid API response"
    cli::cli_abort("API error: {msg}")
  }

  # Try common paths in the device/list response structure
  devices <- NULL
  if (!is.null(res$data$list))        devices <- res$data$list
  else if (!is.null(res$data))        devices <- res$data
  else if (!is.null(res$devices))     devices <- res$devices

  if (is.null(devices) || length(devices) == 0) return(character(0))

  # MAC field may be "mac" or "device_mac"
  macs <- if (!is.null(devices$mac))             devices$mac
          else if (!is.null(devices$device_mac)) devices$device_mac
          else NULL

  if (is.null(macs)) return(character(0))

  valid <- grepl("^([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}$", macs)
  macs[valid]
}


# ---- Internal: fetch info for one station from API -------------------------

.fetch_station_info <- function(mac, cred) {
  url <- paste0(
    .info_url,
    "?application_key=", cred$application_key,
    "&api_key=",         cred$api_key,
    "&mac=",             mac
  )
  res <- tryCatch({
    r <- httr::GET(url)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE)
  }, error = function(e) NULL)

  if (is.null(res) || is.null(res$data)) {
    cli::cli_warn("Failed to fetch info for {.val {mac}}.")
    return(dplyr::tibble(name  = NA_character_, mac   = mac,
                         lat   = NA_real_,      lon   = NA_real_,
                         model = NA_character_))
  }
  d <- res$data
  dplyr::tibble(
    name  = (d$name        %||% NA_character_),
    mac   = mac,
    lat   = as.numeric(d$latitude  %||% NA),
    lon   = as.numeric(d$longitude %||% NA),
    model = (d$stationtype %||% NA_character_)
  )
}


# ---- Internal: print station table -----------------------------------------

.print_station_table <- function(df) {
  n <- nrow(df)
  cli::cli_h2("Registered stations ({n})")

  for (i in seq_len(n)) {
    r     <- df[i, ]
    coord <- if (!is.na(r$lat) && !is.na(r$lon))
      paste0(round(r$lat, 5), ", ", round(r$lon, 5))
    else "coordinates not available"
    model <- if (!is.na(r$model)) cli::col_grey(paste0(" [", r$model, "]")) else ""

    cli::cli_text(paste0(
      "  ", cli::style_bold(cli::col_cyan(paste0("[", r$no, "]"))),
      " ", cli::style_bold(r$name), model
    ))
    cli::cli_text(paste0("      ", cli::col_grey(paste0("\U1F4CD ", coord))))
  }

  cli::cli_text("")
  cli::cli_inform(c(
    "v" = "Stations saved for this session.",
    "i" = "Use by name or number in other functions:",
    " " = "{.code ecowitt_map()}",
    " " = "{.code ecowitt_download(station = 1, start_date = Sys.Date() - 7)}",
    " " = "{.code ecowitt_realtime()}"
  ))
}
