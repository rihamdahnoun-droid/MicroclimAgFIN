#' Interpolate Temperature from Weather Stations
#'
#' @description
#' Spatially interpolates air temperature measurements from weather stations
#' to produce a continuous temperature surface. Supports IDW (Inverse Distance
#' Weighting) and simple Kriging methods.
#'
#' @param weather_data Data frame. Weather data from import_weather_data().
#' @param stations_sf SF object. Station locations from import_station_locations().
#' @param lst SpatRaster. LST raster used as spatial template.
#' @param method Character. Interpolation method: "idw" or "kriging". Default is "idw".
#' @param date Character. Date to interpolate (YYYY-MM-DD). If NULL, uses mean temperature.
#' @param output_dir Character. Directory to save results. Default is "outputs/maps".
#' @param save Logical. Whether to save the raster. Default is TRUE.
#'
#' @return A SpatRaster with interpolated temperature surface.
#'
#' @examples
#' \dontrun{
#' # IDW interpolation
#' temp_surface <- interpolate_temperature(
#'   weather_data = weather,
#'   stations_sf  = stations,
#'   lst          = lst_clean,
#'   method       = "idw"
#' )
#'
#' # Kriging interpolation
#' temp_surface <- interpolate_temperature(
#'   weather_data = weather,
#'   stations_sf  = stations,
#'   lst          = lst_clean,
#'   method       = "kriging"
#' )
#' }
#'
#' @importFrom terra rast ext res crs rasterize writeRaster values
#' @importFrom sf st_as_sf st_coordinates st_crs
#' @importFrom gstat idw variogram fit.variogram vgm krige
#' @importFrom ggplot2 ggplot aes geom_raster geom_sf scale_fill_gradientn theme_minimal labs
#' @export
interpolate_temperature <- function(weather_data,
                                    stations_sf,
                                    lst,
                                    method     = "idw",
                                    date       = NULL,
                                    output_dir = "outputs/maps",
                                    save       = TRUE) {

  # ============================================================
  # BLOC 1 — Vérifications
  # ============================================================

  if (!inherits(weather_data, "data.frame")) {
    stop("weather_data must be a data frame.")
  }
  if (!inherits(stations_sf, "sf")) {
    stop("stations_sf must be an sf object.")
  }
  if (!inherits(lst, "SpatRaster")) {
    stop("lst must be a SpatRaster object.")
  }
  if (!method %in% c("idw", "kriging")) {
    stop("Method must be 'idw' or 'kriging'.")
  }

  message("========================================")
  message("  microclimAg — Temperature Interpolation")
  message("========================================\n")
  message("Method   : ", toupper(method))
  message("Stations : ", nrow(stations_sf))

  # ============================================================
  # BLOC 2 — Préparer les données de température
  # ============================================================

  message("\nPreparing temperature data...")

  if (!is.null(date)) {
    # Filtrer par date
    target_date <- as.Date(date)
    temp_df     <- weather_data[weather_data$date == target_date, ]

    if (nrow(temp_df) == 0) {
      warning("No data for date ", date, ". Using mean temperature instead.")
      temp_df <- aggregate(temperature ~ station_id,
                           data = weather_data, FUN = mean, na.rm = TRUE)
    }
    message("  Date     : ", date)
  } else {
    # Utiliser la moyenne par station
    temp_df <- aggregate(temperature ~ station_id,
                         data = weather_data, FUN = mean, na.rm = TRUE)
    message("  Date     : Mean temperature (all dates)")
  }

  # ============================================================
  # BLOC 3 — Joindre températures et localisations
  # ============================================================

  message("Joining temperature data with station locations...")

  stations_temp <- merge(
    as.data.frame(stations_sf),
    temp_df,
    by  = "station_id",
    all = FALSE
  )

  if (nrow(stations_temp) == 0) {
    stop("No matching stations found. Check that station_id matches between datasets.")
  }

  # Recréer objet sf
  stations_sp <- sf::st_as_sf(
    stations_temp,
    coords = c("longitude", "latitude"),
    crs    = 4326
  )

  message("  Matched stations: ", nrow(stations_sp))
  message("  Temperature range: ",
          round(min(stations_sp$temperature, na.rm = TRUE), 1), "C to ",
          round(max(stations_sp$temperature, na.rm = TRUE), 1), "C")

  # ============================================================
  # BLOC 4 — Créer la grille d'interpolation
  # ============================================================

  message("\nCreating interpolation grid...")

  # Utiliser l'étendue du raster LST
  ext_lst  <- terra::ext(lst)
  res_lst  <- terra::res(lst)

  # Créer grille régulière
  grid_x <- seq(ext_lst$xmin, ext_lst$xmax, by = res_lst[1])
  grid_y <- seq(ext_lst$ymin, ext_lst$ymax, by = res_lst[2])

  grid_df <- expand.grid(x = grid_x, y = grid_y)

  grid_sf <- sf::st_as_sf(
    grid_df,
    coords = c("x", "y"),
    crs    = sf::st_crs(stations_sp)
  )

  message("  Grid points: ", nrow(grid_sf))

  # ============================================================
  # BLOC 5 — Interpolation
  # ============================================================

  if (method == "idw") {

    message("\nRunning IDW interpolation...")

    idw_result <- gstat::idw(
      formula  = temperature ~ 1,
      locations = stations_sp,
      newdata  = grid_sf,
      idp      = 2
    )

    temp_interp        <- grid_df
    temp_interp$temp   <- idw_result$var1.pred
    method_label       <- "IDW (p=2)"

  } else if (method == "kriging") {

    message("\nRunning Kriging interpolation...")

    # Calculer le variogramme expérimental
    vgm_exp <- gstat::variogram(
      temperature ~ 1,
      data = stations_sp
    )

    # Ajuster un modèle de variogramme
    vgm_model <- tryCatch({
      gstat::fit.variogram(
        vgm_exp,
        model = gstat::vgm(
          psill  = var(stations_sp$temperature, na.rm = TRUE),
          model  = "Sph",
          range  = max(vgm_exp$dist) / 2,
          nugget = 0
        )
      )
    }, error = function(e) {
      message("  Variogram fitting failed. Using default model.")
      gstat::vgm(
        psill  = var(stations_sp$temperature, na.rm = TRUE),
        model  = "Sph",
        range  = 1,
        nugget = 0
      )
    })

    message("  Variogram model: ", vgm_model$model[2])

    # Krigeage
    krig_result <- gstat::krige(
      formula   = temperature ~ 1,
      locations = stations_sp,
      newdata   = grid_sf,
      model     = vgm_model
    )

    temp_interp        <- grid_df
    temp_interp$temp   <- krig_result$var1.pred
    method_label       <- "Ordinary Kriging"
  }

  # ============================================================
  # BLOC 6 — Convertir en raster
  # ============================================================

  message("\nConverting to raster...")

  # Créer raster vide basé sur LST
  temp_raster <- terra::rast(lst[[1]])
  temp_raster[] <- NA

  # Remplir le raster avec les valeurs interpolées
  cells <- terra::cellFromXY(temp_raster,
                             as.matrix(temp_interp[, c("x", "y")]))
  valid <- !is.na(cells)
  temp_raster[cells[valid]] <- temp_interp$temp[valid]

  names(temp_raster) <- paste0("temp_interp_", method)

  # ============================================================
  # BLOC 7 — Validation simple
  # ============================================================

  message("\nValidation (leave-one-out)...")

  if (nrow(stations_sp) >= 3) {
    errors <- sapply(seq_len(nrow(stations_sp)), function(i) {
      train <- stations_sp[-i, ]
      test  <- stations_sp[i, ]

      pred <- tryCatch({
        if (method == "idw") {
          res <- gstat::idw(temperature ~ 1,
                            locations = train,
                            newdata   = test,
                            idp       = 2)
          res$var1.pred
        } else {
          res <- gstat::krige(temperature ~ 1,
                              locations = train,
                              newdata   = test,
                              model     = vgm_model)
          res$var1.pred
        }
      }, error = function(e) NA)

      pred - test$temperature
    })

    rmse <- round(sqrt(mean(errors^2, na.rm = TRUE)), 3)
    mae  <- round(mean(abs(errors),   na.rm = TRUE), 3)

    message("  RMSE : ", rmse, " C")
    message("  MAE  : ", mae,  " C")

  } else {
    message("  Not enough stations for validation (need >= 3).")
    rmse <- NA
    mae  <- NA
  }

  # ============================================================
  # BLOC 8 — Sauvegarde
  # ============================================================

  if (save) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    out_file <- file.path(output_dir,
                          paste0("temp_interpolated_", method, ".tif"))
    terra::writeRaster(temp_raster, out_file, overwrite = TRUE)
    message("\nInterpolated surface saved to: ", out_file)
  }

  # ============================================================
  # BLOC 9 — Visualisation
  # ============================================================

  message("\nPlotting interpolated surface...")

  temp_df_plot        <- as.data.frame(temp_interp)
  names(temp_df_plot) <- c("x", "y", "temperature")

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = temp_df_plot,
      ggplot2::aes(x = x, y = y, fill = temperature)
    ) +
    ggplot2::geom_sf(
      data  = stations_sp,
      color = "black",
      size  = 3,
      shape = 17
    ) +
    ggplot2::scale_fill_gradientn(
      colors = rev(heat.colors(100)),
      name   = "Temp (C)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = paste("Interpolated Temperature Surface -", method_label),
      subtitle = paste("RMSE:", ifelse(is.na(rmse), "N/A",
                                       paste(rmse, "C")),
                       "| MAE:", ifelse(is.na(mae), "N/A",
                                        paste(mae, "C"))),
      x = "Longitude",
      y = "Latitude"
    )

  print(p)

  # ============================================================
  # BLOC 10 — Résumé final
  # ============================================================

  message("\n========================================")
  message("     Interpolation Complete!")
  message("========================================")
  message("  Method   : ", method_label)
  message("  Stations : ", nrow(stations_sp))
  message("  RMSE     : ", ifelse(is.na(rmse), "N/A", paste(rmse, "C")))
  message("  MAE      : ", ifelse(is.na(mae),  "N/A", paste(mae,  "C")))
  message("  Temp range: ",
          round(min(terra::values(temp_raster), na.rm = TRUE), 1), "C to ",
          round(max(terra::values(temp_raster), na.rm = TRUE), 1), "C")
  message("========================================")

  return(list(
    raster     = temp_raster,
    data       = temp_interp,
    method     = method_label,
    validation = list(rmse = rmse, mae = mae)
  ))
}
