download_lst_data <- function(country,
                              region,
                              year,
                              month,
                              output_dir = "outputs/maps",
                              overwrite  = FALSE,
                              interactive = FALSE) {

  if (interactive) {
    stop("Interactive mode should be handled outside package functions.")
  }

  if (is.null(country) || is.null(year) || is.null(month)) {
    stop("country, year and month must be provided (no interactive mode in package).")
  }

  country <- toupper(country)

  if (!is.numeric(year) || year < 1950) {
    stop("Invalid year.")
  }

  # -----------------------------
  # Boundaries
  # -----------------------------
  if (!is.null(region)) {
    boundaries_all <- geodata::gadm(country, level = 1, path = tempdir())
    boundaries <- boundaries_all[boundaries_all$NAME_1 == region, ]

    if (nrow(boundaries) == 0) {
      stop("Region not found.")
    }
  } else {
    boundaries <- geodata::gadm(country, level = 0, path = tempdir())
  }

  bbox <- terra::ext(boundaries)
  center_lon <- (bbox$xmin + bbox$xmax) / 2
  center_lat <- (bbox$ymin + bbox$ymax) / 2

  # -----------------------------
  # WorldClim
  # -----------------------------
  lst_worldclim <- geodata::worldclim_country(
    country = country,
    var     = "tmax",
    path    = tempdir()
  )

  lst_base <- lst_worldclim[[month]]
  lst_base <- terra::crop(lst_base, boundaries)
  lst_base <- terra::mask(lst_base, boundaries)

  names(lst_base) <- paste0("m", month)

  # -----------------------------
  # Open-Meteo (safe)
  # -----------------------------
  monthly_temps <- rep(NA_real_, length(month))

  for (i in seq_along(month)) {

    m <- month[i]

    date_start <- sprintf("%d-%02d-01", year, m)
    date_end   <- as.character(
      as.Date(sprintf("%d-%02d-01", year + (m == 12), (m %% 12) + 1)) - 1
    )

    url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", center_lat,
      "&longitude=", center_lon,
      "&start_date=", date_start,
      "&end_date=", date_end,
      "&daily=temperature_2m_max",
      "&timezone=auto"
    )

    resp <- tryCatch(
      httr::GET(url, httr::timeout(20)),
      error = function(e) NULL
    )

    if (!is.null(resp) && httr::status_code(resp) == 200) {

      data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

      monthly_temps[i] <- mean(data$daily$temperature_2m_max, na.rm = TRUE)
    }
  }

  # -----------------------------
  # Adjustment
  # -----------------------------
  worldclim_means <- terra::global(lst_base, mean, na.rm = TRUE)[,1]

  lst_adj <- lst_base

  for (i in seq_along(month)) {

    if (!is.na(monthly_temps[i])) {
      anomaly <- monthly_temps[i] - worldclim_means[i]
      lst_adj[[i]] <- lst_base[[i]] + anomaly
    }
  }

  names(lst_adj) <- paste0("LST_", month, "_", year)

  # -----------------------------
  # Output
  # -----------------------------
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  out_file <- file.path(
    output_dir,
    paste0("LST_", country, "_", year, ".tif")
  )

  if (!file.exists(out_file) || overwrite) {
    terra::writeRaster(lst_adj, out_file, overwrite = TRUE)
  }

  return(lst_adj)
}
