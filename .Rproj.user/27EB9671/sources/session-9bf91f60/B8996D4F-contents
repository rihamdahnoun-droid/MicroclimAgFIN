import_weather_data <- function(file_path,
                                date_col      = "date",
                                temp_col      = "temperature",
                                humidity_col  = "humidity",
                                wind_col      = "wind_speed",
                                radiation_col = "radiation",
                                station_col   = "station_id",
                                date_format   = "%Y-%m-%d") {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))

  # --- READ FILE ---
  if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE)

  } else if (ext %in% c("xlsx", "xls")) {

    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel files.")
    }

    df <- readxl::read_excel(file_path)
    df <- as.data.frame(df)

  } else {
    stop("Unsupported format: use CSV or Excel")
  }

  # --- CHECK REQUIRED ---
  required_cols <- c(date_col, temp_col)

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- SAFE DATE PARSING ---
  date_raw <- df[[date_col]]

  result_date <- if (inherits(date_raw, "Date")) {
    date_raw
  } else {
    as.Date(date_raw, format = date_format)
  }

  # --- BUILD RESULT ---
  result <- data.frame(
    date        = result_date,
    temperature = as.numeric(df[[temp_col]])
  )

  # --- OPTIONAL VARIABLES ---
  result$humidity <- if (humidity_col %in% names(df)) {
    as.numeric(df[[humidity_col]])
  } else {
    NA_real_
  }

  result$wind_speed <- if (wind_col %in% names(df)) {
    as.numeric(df[[wind_col]])
  } else {
    NA_real_
  }

  result$radiation <- if (radiation_col %in% names(df)) {
    as.numeric(df[[radiation_col]])
  } else {
    NA_real_
  }

  # --- STATION HANDLING (IMPORTANT FIX) ---
  result$station_id <- if (station_col %in% names(df)) {
    df[[station_col]]
  } else {
    rep("station_1", nrow(df))
  }

  # --- CLEAN ---
  result <- result[!is.na(result$date), ]
  result <- result[order(result$date), ]
  rownames(result) <- NULL

  return(result)
}
