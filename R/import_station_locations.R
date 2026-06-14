import_station_locations <- function(file_path,
                                     lon_col  = "longitude",
                                     lat_col  = "latitude",
                                     id_col   = "station_id",
                                     name_col = "name",
                                     crs      = 4326,
                                     plot_map = FALSE) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))

  # --- READ DATA ---
  if (ext == "csv") {

    df <- read.csv(file_path, stringsAsFactors = FALSE)

    missing_cols <- c(lon_col, lat_col)[!c(lon_col, lat_col) %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }

    stations_sf <- sf::st_as_sf(
      df,
      coords = c(lon_col, lat_col),
      crs    = crs
    )

    stations_sf$longitude <- df[[lon_col]]
    stations_sf$latitude  <- df[[lat_col]]

  } else if (ext == "shp") {

    stations_sf <- sf::read_sf(file_path)

    if (is.na(sf::st_crs(stations_sf))) {
      sf::st_crs(stations_sf) <- crs
    }

    current_epsg <- sf::st_crs(stations_sf)$epsg

    if (!is.null(current_epsg) && current_epsg != crs) {
      stations_sf <- sf::st_transform(stations_sf, crs = crs)
    }

  } else {
    stop("Unsupported format. Use CSV or .shp")
  }

  # --- STANDARDISE ---
  stations_sf$station_id <- if (id_col %in% names(stations_sf)) {
    stations_sf[[id_col]]
  } else {
    paste0("S", seq_len(nrow(stations_sf)))
  }

  stations_sf$name <- if (name_col %in% names(stations_sf)) {
    stations_sf[[name_col]]
  } else {
    stations_sf$station_id
  }

  # --- OPTIONAL PLOT (SAFE) ---
  if (isTRUE(plot_map)) {

    p <- ggplot2::ggplot(stations_sf) +
      ggplot2::geom_sf(color = "red", size = 2, shape = 17) +
      ggplot2::geom_sf_text(
        ggplot2::aes(label = name),
        size = 3
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Weather Station Locations",
        subtitle = paste(nrow(stations_sf), "stations")
      )

    print(p)
  }

  return(stations_sf)
}
