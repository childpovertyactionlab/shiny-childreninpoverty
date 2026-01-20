# =============================================================================
# Databricks REST API Functions
# =============================================================================
# Uses the SQL Statement Execution API (no ODBC driver needed)
# Docs: https://docs.databricks.com/api/workspace/statementexecution
# =============================================================================

#' Execute a SQL query against Databricks
#' @param sql SQL query string
#' @param host Databricks host URL
#' @param token Databricks access token
#' @param warehouse_id SQL warehouse ID
#' @return Data frame with query results
databricks_query <- function(sql, host, token, warehouse_id) {

  # Ensure host has https:// prefix
  if (!grepl("^https://", host)) {
    host <- paste0("https://", host)
  }

  base_url <- paste0(host, "/api/2.0/sql/statements")

  body <- list(
    warehouse_id = warehouse_id,
    statement = sql,
    wait_timeout = "50s",
    disposition = "INLINE",
    format = "JSON_ARRAY",
    row_limit = 100000  # Request up to 100k rows
  )

  resp <- httr2::request(base_url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Check HTTP status
  status_code <- httr2::resp_status(resp)
  if (status_code != 200) {
    error_body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list(message = httr2::resp_body_string(resp))
    )
    stop(paste("Databricks query failed:", error_body$message %||% paste("HTTP", status_code)))
  }

  result <- httr2::resp_body_json(resp)

  # Check for query failure
  if (result$status$state == "FAILED") {
    stop(paste("Query failed:", result$status$error$message %||% "Unknown error"))
  }

  # Handle PENDING/RUNNING state - poll for completion
  statement_id <- result$statement_id

  while (result$status$state %in% c("PENDING", "RUNNING")) {
    Sys.sleep(2)

    status_resp <- httr2::request(paste0(base_url, "/", statement_id)) |>
      httr2::req_headers(Authorization = paste("Bearer", token)) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    result <- httr2::resp_body_json(status_resp)

    if (result$status$state == "FAILED") {
      stop(paste("Query failed:", result$status$error$message %||% "Unknown error"))
    }
  }

  # Parse successful results
  if (result$status$state == "SUCCEEDED") {
    return(parse_databricks_response(result, base_url, statement_id, token))
  }

  stop(paste("Unexpected query state:", result$status$state))
}


#' Parse Databricks API response into a data frame, handling chunked results
#' @param result Parsed JSON response from API
#' @param base_url Base URL for chunk requests
#' @param statement_id Statement ID for chunk requests
#' @param token Auth token
#' @return Data frame
parse_databricks_response <- function(result, base_url = NULL, statement_id = NULL, token = NULL) {

  # Get column info
  columns <- result$manifest$schema$columns
  if (is.null(columns) || length(columns) == 0) {
    return(data.frame())
  }

  col_names <- sapply(columns, function(c) c$name)
  col_types <- sapply(columns, function(c) c$type_name)

  # Collect all data chunks
  all_data <- list()

  # First chunk from initial response
  if (!is.null(result$result) && !is.null(result$result$data_array)) {
    all_data <- result$result$data_array
  }

  # Fetch additional chunks if available
  if (!is.null(result$result$next_chunk_index) && !is.null(base_url)) {
    chunk_index <- result$result$next_chunk_index

    while (!is.null(chunk_index)) {
      chunk_url <- paste0(base_url, "/", statement_id, "/result/chunks/", chunk_index)

      chunk_resp <- httr2::request(chunk_url) |>
        httr2::req_headers(Authorization = paste("Bearer", token)) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      if (httr2::resp_status(chunk_resp) != 200) {
        warning("Failed to fetch chunk ", chunk_index)
        break
      }

      chunk_result <- httr2::resp_body_json(chunk_resp)

      if (!is.null(chunk_result$data_array)) {
        all_data <- c(all_data, chunk_result$data_array)
      }

      chunk_index <- chunk_result$next_chunk_index
    }
  }

  # Handle empty results
  if (length(all_data) == 0) {
    df <- as.data.frame(matrix(nrow = 0, ncol = length(col_names)))
    names(df) <- col_names
    return(df)
  }

  # Convert data array to data frame
  df <- as.data.frame(do.call(rbind, all_data), stringsAsFactors = FALSE)
  names(df) <- col_names

  # Convert types
  for (i in seq_along(col_names)) {
    df[[i]] <- convert_column_type(df[[i]], col_types[i])
  }

  return(df)
}


#' Convert column to appropriate R type based on Databricks type
#' @param col Column vector
#' @param type_name Databricks type name
#' @return Converted column
convert_column_type <- function(col, type_name) {

  # Handle NULLs
  col <- sapply(col, function(x) if (is.null(x)) NA else x)

  switch(tolower(type_name),
    "int" = ,
    "integer" = ,
    "bigint" = ,
    "long" = as.integer(col),
    "float" = ,
    "double" = ,
    "decimal" = as.numeric(col),
    "boolean" = as.logical(col),
    "date" = as.Date(col),
    "timestamp" = as.POSIXct(col),
    as.character(col)  # Default: keep as character (includes STRING)
  )
}
