calculate_lst_anomaly <- function(lst,
                                  method     = "mean_diff",
                                  output_dir = "outputs/maps",
                                  save       = TRUE,
                                  plot       = FALSE) {

  # ============================================================
  # BLOC 1 — Vérifications
  # ============================================================

  if (!inherits(lst, "SpatRaster")) {
    stop("Input must be a SpatRaster object.")
  }

  if (!method %in% c("mean_diff", "zscore")) {
    stop("Method must be 'mean_diff' or 'zscore'.")
  }

  message("========================================")
  message("   microclimAg — LST Anomaly           ")
  message("========================================")
  message("Method  : ", method)
  message("Layers  : ", terra::nlyr(lst))

  # ============================================================
  # BLOC 2 — Statistiques pixel-wise correctes
  # ============================================================

  lst_mean <- terra::app(lst, mean, na.rm = TRUE)
  lst_sd   <- terra::app(lst, sd,   na.rm = TRUE)

  # ============================================================
  # BLOC 3 — Anomalies
  # ============================================================

  if (method == "mean_diff") {

    anomaly <- lst - lst_mean
    message("  Method: mean difference")

  } else {

    anomaly <- (lst - lst_mean) / (lst_sd + 1e-10)
    message("  Method: z-score")
  }

  names(anomaly) <- paste0("anomaly_", names(lst))

  # ============================================================
  # BLOC 4 — Threshold robuste
  # ============================================================

  threshold <- terra::global(lst_sd, "mean", na.rm = TRUE)[1,1]

  hot_zones  <- terra::ifel(anomaly >  threshold, 1, NA)
  cold_zones <- terra::ifel(anomaly < -threshold, 1, NA)

  names(hot_zones)  <- paste0("hot_", names(lst))
  names(cold_zones) <- paste0("cold_", names(lst))

  # ============================================================
  # BLOC 5 — Statistiques robustes (sans values())
  # ============================================================

  stats_df <- do.call(rbind, lapply(seq_len(terra::nlyr(anomaly)), function(i) {

    g <- terra::global(anomaly[[i]],
                       fun = c("mean", "sd", "min", "max"),
                       na.rm = TRUE)

    data.frame(
      layer     = names(lst)[i],
      mean_anom = g$mean,
      sd_anom   = g$sd,
      min_anom  = g$min,
      max_anom  = g$max,
      hot_pct   = 100 * terra::global(anomaly[[i]] > threshold, "mean", na.rm = TRUE)[1,1],
      cold_pct  = 100 * terra::global(anomaly[[i]] < -threshold, "mean", na.rm = TRUE)[1,1]
    )
  }))

  message("\nAnomaly statistics:")
  print(stats_df)

  # ============================================================
  # BLOC 6 — Sauvegarde sécurisée
  # ============================================================

  if (save) {

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

    terra::writeRaster(anomaly,
                       file.path(output_dir, "LST_anomaly.tif"),
                       overwrite = TRUE)

    terra::writeRaster(hot_zones,
                       file.path(output_dir, "LST_hot_zones.tif"),
                       overwrite = TRUE)

    terra::writeRaster(cold_zones,
                       file.path(output_dir, "LST_cold_zones.tif"),
                       overwrite = TRUE)

    write.csv(stats_df,
              file.path("outputs/tables", "LST_anomaly_stats.csv"),
              row.names = FALSE)
  }

  # ============================================================
  # BLOC 7 — Plot sécurisé (optionnel)
  # ============================================================

  if (plot) {

    old_par <- par(mfrow = c(1, min(3, terra::nlyr(anomaly))))
    on.exit(par(old_par), add = TRUE)

    cols <- colorRampPalette(c("#2166AC","#F7F7F7","#B2182B"))(100)

    for (i in seq_len(min(3, terra::nlyr(anomaly)))) {
      terra::plot(anomaly[[i]],
                  col = cols,
                  main = names(lst)[i])
    }
  }

  # ============================================================
  # RETURN
  # ============================================================

  message("\n========================================")
  message("        Anomaly Calculation Done        ")
  message("========================================")

  return(list(
    anomaly    = anomaly,
    hot_zones  = hot_zones,
    cold_zones = cold_zones,
    stats      = stats_df
  ))
}
