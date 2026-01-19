# logging.R
# Logging utilities for the shiny-childreninpoverty project
# Uses the 'logger' package with dated log files in logs/ directory

#' Initialize logging for a script
#'
#' Sets up the logger package to write to both console and a dated log file.
#' Call this at the start of any script that needs logging.
#'
#' @param script_name Name of the script (used in log file name)
#' @param log_level Minimum log level (default: INFO)
#' @return Invisible path to the log file
#' @examples
#' init_logging("download_census_data")
#' log_info("Starting data download...")
init_logging <- function(script_name, log_level = logger::INFO) {
  # Ensure logger package is available
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("Package 'logger' is required. Install with: install.packages('logger')")
  }

  library(logger)

  # Create logs directory if it doesn't exist
  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
  }

 # Create dated log file name
  log_date <- format(Sys.time(), "%Y-%m-%d")
  log_file <- file.path(log_dir, paste0(log_date, "_", script_name, ".log"))

  # Set up console logging (with colors if interactive)
  log_threshold(log_level)

  # Set up file logging
  log_appender(appender_tee(file = log_file))

  # Log session start with system info
  log_info("
================================================================================
 SESSION START: {script_name}
 Time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
 R Version: {R.version.string}
 Platform: {R.version$platform}
 Working Directory: {getwd()}
================================================================================")

  invisible(log_file)
}

#' Log a summary of data download results
#'
#' @param data_type Description of data (e.g., "tracts", "places")
#' @param row_count Number of rows downloaded
#' @param states_processed Number of states processed
log_download_summary <- function(data_type, row_count, states_processed) {
  log_info("Download complete: {data_type}")
  log_info("  Rows: {format(row_count, big.mark = ',')}")
  log_info("  States processed: {states_processed}")
}

#' Log the end of a session with summary stats
#'
#' @param start_time POSIXct time when script started
#' @param success Logical indicating if script completed successfully
log_session_end <- function(start_time, success = TRUE) {
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  status <- if (success) "SUCCESS" else "FAILED"

  log_info("
================================================================================
 SESSION END: {status}
 Elapsed time: {round(as.numeric(elapsed), 1)} minutes
 Time: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
================================================================================")
}
