# ============================================================
#  ecowittR — Authentication & Config
#  ecowitt_auth() required; stations are auto-discovered
#  (no need to register MACs manually)
# ============================================================

.ecowitt_env <- new.env(parent = emptyenv())

# ---- Exported: ecowitt_auth ------------------------------------------------

#' Set Ecowitt API Credentials
#'
#' Store the api_key and application_key for the current session.
#' Call this once at the start of your session. Afterwards, use
#' \code{\link{ecowitt_stations}} (with no arguments) to auto-discover
#' all stations registered in your account.
#'
#' @param api_key         Character. Your API key from ecowitt.net.
#' @param application_key Character. Your Application key from ecowitt.net.
#' @return Invisibly \code{TRUE}.
#' @export
ecowitt_auth <- function(api_key, application_key) {
  if (missing(api_key) || !nzchar(trimws(api_key)))
    cli::cli_abort("{.arg api_key} must not be empty.")
  if (missing(application_key) || !nzchar(trimws(application_key)))
    cli::cli_abort("{.arg application_key} must not be empty.")

  .ecowitt_env$api_key         <- trimws(api_key)
  .ecowitt_env$application_key <- trimws(application_key)
  .ecowitt_env$stations        <- NULL

  cli::cli_alert_success("Credentials saved successfully.")
  cli::cli_inform(c(
    "i" = "Next step: load all stations automatically with:",
    " " = "{.code ecowitt_stations()}  # no MAC argument needed"
  ))
  invisible(TRUE)
}

# ---- Internal: credentials -------------------------------------------------

.get_cred <- function() {
  ak  <- .ecowitt_env$api_key
  apk <- .ecowitt_env$application_key
  if (is.null(ak) || is.null(apk))
    cli::cli_abort(c(
      "Credentials not set.",
      "i" = "Run {.fn ecowitt_auth} first."
    ))
  list(api_key = ak, application_key = apk)
}

# ---- Internal: station registry --------------------------------------------

.set_registry <- function(df) .ecowitt_env$stations <- df
.get_registry <- function()    .ecowitt_env$stations

# ---- Internal: resolve station argument -> MAC vector ----------------------

.resolve <- function(station = NULL) {
  reg <- .get_registry()

  if (is.null(reg) || nrow(reg) == 0)
    cli::cli_abort(c(
      "Station list not loaded.",
      "i" = "Run {.fn ecowitt_stations} first."
    ))

  if (is.null(station)) return(reg$mac)

  if (is.numeric(station) || is.integer(station)) {
    idx <- as.integer(station)
    bad <- idx[idx < 1 | idx > nrow(reg)]
    if (length(bad) > 0)
      cli::cli_abort("Invalid station number(s): {bad}. Available: 1-{nrow(reg)}.")
    sel <- reg[idx, ]
    cli::cli_inform("Selected station(s): {.val {paste(sel$name, collapse = ', ')}}")
    return(sel$mac)
  }

  is_mac <- grepl("^([0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}$", station)
  if (all(is_mac)) return(station)

  macs <- vapply(station, function(s) {
    hits <- reg[grepl(s, reg$name, ignore.case = TRUE), ]
    if (nrow(hits) == 0)
      cli::cli_abort(c(
        "Name {.val {s}} not found.",
        "i" = "Available: {.val {paste(reg$name, collapse = ', ')}}"
      ))
    if (nrow(hits) > 1)
      cli::cli_warn("{.val {s}} matched {nrow(hits)} stations; using {.val {hits$name[1]}}.")
    hits$mac[1]
  }, character(1))
  unname(macs)
}

# ---- URL constants ---------------------------------------------------------

.base_url     <- "https://api.ecowitt.net/api/v3/device/history"
.info_url     <- "https://api.ecowitt.net/api/v3/device/info"
.realtime_url <- "https://api.ecowitt.net/api/v3/device/real_time"
.list_url     <- "https://api.ecowitt.net/api/v3/device/list"

# ---- Unit converters -------------------------------------------------------

.f_to_c      <- function(x) (x - 32) * 5 / 9
.inhg_to_hpa <- function(x) x * 33.8639
.mph_to_ms   <- function(x) x / 2.237
.in_to_mm    <- function(x) x * 25.4

`%||%` <- function(a, b) if (!is.null(a)) a else b
