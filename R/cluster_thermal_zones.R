#' Cluster Thermal Zones
#'
#' @description
#' Classifies the study area into thermal zones (cool, moderate, warm, hot)
#' using K-means clustering on LST data. Helps identify spatial patterns
#' of temperature distribution in agricultural landscapes.
#'
#' @param lst SpatRaster. Cleaned LST raster from clean_lst_data().
#' @param features Data frame. Microclimate features from extract_microclimate_features().
#' @param n_clusters Numeric. Number of thermal clusters. Default is 4.
#' @param vars Character vector. Variables to use for clustering.
#'             Default is c("lst_mean", "ndvi_mean", "elevation").
#' @param output_dir Character. Directory to save results. Default is "outputs/maps".
#' @param save Logical. Whether to save results. Default is TRUE.
#'
#' @return A list containing:
#' \describe{
#'   \item{raster}{SpatRaster with thermal zone classification}
#'   \item{clusters}{Data frame with cluster assignments}
#'   \item{centers}{Data frame with cluster centers}
#'   \item{stats}{Summary statistics per cluster}
#' }
#'
#' @examples
#' \dontrun{
#' thermal_zones <- cluster_thermal_zones(
#'   lst      = lst_clean,
#'   features = features,
#'   n_clusters = 4
#' )
#' }
#'
#' @importFrom terra rast values xyFromCell ncell writeRaster
#' @importFrom ggplot2 ggplot aes geom_point geom_bar scale_color_manual scale_fill_manual theme_minimal labs facet_wrap
#' @export
cluster_thermal_zones <- function(lst,
                                  features   = NULL,
                                  n_clusters = 4,
                                  vars       = c("lst_mean",
                                                 "ndvi_mean",
                                                 "elevation"),
                                  output_dir = "outputs/maps",
                                  save       = TRUE) {

  # ============================================================
  # BLOC 1 — Vérifications
  # ============================================================

  if (!inherits(lst, "SpatRaster")) {
    stop("lst must be a SpatRaster object.")
  }

  message("========================================")
  message("  microclimAg — Thermal Zone Clustering")
  message("========================================\n")
  message("Clusters    : ", n_clusters)
  message("Variables   : ", paste(vars, collapse = ", "))

  # ============================================================
  # BLOC 2 — Préparer les données
  # ============================================================

  message("\nPreparing data for clustering...")

  if (!is.null(features)) {
    # Utiliser le data frame features si fourni
    clust_data <- features

    # Vérifier que les variables existent
    missing_vars <- vars[!vars %in% names(clust_data)]
    if (length(missing_vars) > 0) {
      message("  Variables not found: ", paste(missing_vars, collapse = ", "))
      vars <- vars[vars %in% names(clust_data)]
      message("  Using available vars: ", paste(vars, collapse = ", "))
    }

  } else {
    # Construire depuis le raster LST
    message("  No features provided. Using LST raster only.")

    vals   <- terra::values(lst)
    coords <- terra::xyFromCell(lst[[1]], 1:terra::ncell(lst[[1]]))

    clust_data <- data.frame(
      x        = coords[, 1],
      y        = coords[, 2],
      lst_mean = rowMeans(vals, na.rm = TRUE)
    )

    vars <- "lst_mean"
  }

  # Supprimer les NA
  clust_data_clean <- clust_data[complete.cases(clust_data[, vars]), ]
  message("  Pixels for clustering: ", nrow(clust_data_clean))

  # ============================================================
  # BLOC 3 — Normalisation des variables
  # ============================================================

  message("\nNormalizing variables...")

  clust_matrix <- scale(clust_data_clean[, vars])
  message("  Variables normalized (z-score).")

  # ============================================================
  # BLOC 4 — K-means clustering
  # ============================================================

  message("\nRunning K-means clustering (k = ", n_clusters, ")...")

  set.seed(42)
  kmeans_result <- kmeans(
    clust_matrix,
    centers  = n_clusters,
    nstart   = 25,
    iter.max = 100
  )

  message("  Iterations      : ", kmeans_result$iter)
  message("  Within-SS ratio : ",
          round(kmeans_result$tot.withinss /
                  kmeans_result$totss * 100, 1), "%")

  # ============================================================
  # BLOC 5 — Ordonner les clusters par température
  # ============================================================

  message("\nOrdering clusters by temperature...")

  # Calculer la température moyenne par cluster
  clust_data_clean$cluster_raw <- kmeans_result$cluster

  cluster_temps <- tapply(
    clust_data_clean$lst_mean,
    clust_data_clean$cluster_raw,
    mean, na.rm = TRUE
  )

  # Ordre croissant de température
  temp_order  <- order(cluster_temps)
  cluster_map <- setNames(seq_along(temp_order), temp_order)
  clust_data_clean$cluster <- cluster_map[as.character(clust_data_clean$cluster_raw)]

  # Labels selon nombre de clusters
  if (n_clusters == 4) {
    cluster_labels <- c("1" = "Cool",
                        "2" = "Moderate",
                        "3" = "Warm",
                        "4" = "Hot")
  } else if (n_clusters == 3) {
    cluster_labels <- c("1" = "Cool",
                        "2" = "Moderate",
                        "3" = "Hot")
  } else if (n_clusters == 5) {
    cluster_labels <- c("1" = "Very Cool",
                        "2" = "Cool",
                        "3" = "Moderate",
                        "4" = "Warm",
                        "5" = "Very Hot")
  } else {
    cluster_labels <- setNames(
      paste("Zone", 1:n_clusters),
      as.character(1:n_clusters)
    )
  }

  clust_data_clean$cluster_label <- cluster_labels[
    as.character(clust_data_clean$cluster)
  ]

  # ============================================================
  # BLOC 6 — Statistiques par cluster
  # ============================================================

  message("\nComputing cluster statistics...")

  stats_list <- lapply(1:n_clusters, function(k) {
    sub <- clust_data_clean[clust_data_clean$cluster == k, ]
    row <- data.frame(
      cluster       = k,
      label         = cluster_labels[as.character(k)],
      n_pixels      = nrow(sub),
      pct_area      = round(100 * nrow(sub) / nrow(clust_data_clean), 1),
      mean_lst      = round(mean(sub$lst_mean,  na.rm = TRUE), 2)
    )
    if ("ndvi_mean" %in% names(sub)) {
      row$mean_ndvi <- round(mean(sub$ndvi_mean, na.rm = TRUE), 3)
    }
    if ("elevation" %in% names(sub)) {
      row$mean_elev <- round(mean(sub$elevation, na.rm = TRUE), 0)
    }
    row
  })

  stats_df <- do.call(rbind, stats_list)

  message("\nCluster Statistics:")
  print(stats_df)

  # ============================================================
  # BLOC 7 — Convertir en raster
  # ============================================================

  message("\nConverting clusters to raster...")

  cluster_raster    <- terra::rast(lst[[1]])
  cluster_raster[]  <- NA

  cells <- terra::cellFromXY(
    cluster_raster,
    as.matrix(clust_data_clean[, c("x", "y")])
  )
  valid <- !is.na(cells)
  cluster_raster[cells[valid]] <- clust_data_clean$cluster[valid]

  names(cluster_raster) <- "thermal_zones"

  # ============================================================
  # BLOC 8 — Sauvegarde
  # ============================================================

  if (save) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    terra::writeRaster(
      cluster_raster,
      file.path(output_dir, "thermal_zones.tif"),
      overwrite = TRUE
    )

    write.csv(
      stats_df,
      file.path("outputs/tables", "thermal_zones_stats.csv"),
      row.names = FALSE
    )

    message("\nFiles saved to: ", output_dir)
  }

  # ============================================================
  # BLOC 9 — Visualisation
  # ============================================================

  message("\nPlotting thermal zones...")

  zone_colors <- c(
    "1" = "#4575B4",  # Cool       - bleu
    "2" = "#91BFDB",  # Moderate   - bleu clair
    "3" = "#FC8D59",  # Warm       - orange
    "4" = "#D73027"   # Hot        - rouge
  )

  if (n_clusters == 3) {
    zone_colors <- c(
      "1" = "#4575B4",
      "2" = "#91BFDB",
      "3" = "#D73027"
    )
  } else if (n_clusters == 5) {
    zone_colors <- c(
      "1" = "#313695",
      "2" = "#4575B4",
      "3" = "#91BFDB",
      "4" = "#FC8D59",
      "5" = "#D73027"
    )
  }

  # Carte des zones thermiques
  p1 <- ggplot2::ggplot(
    clust_data_clean,
    ggplot2::aes(x = x, y = y, color = as.factor(cluster))
  ) +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_color_manual(
      values = zone_colors,
      labels = cluster_labels,
      name   = "Thermal Zone"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title    = "Thermal Zone Classification",
      subtitle = paste(n_clusters, "zones | K-means clustering"),
      x = "Longitude",
      y = "Latitude"
    )

  print(p1)

  # Distribution des températures par zone
  p2 <- ggplot2::ggplot(
    clust_data_clean,
    ggplot2::aes(x = cluster_label, y = lst_mean, fill = as.factor(cluster))
  ) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::scale_fill_manual(values = zone_colors, guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "LST Distribution by Thermal Zone",
      x     = "Thermal Zone",
      y     = "Mean LST (C)"
    )

  print(p2)

  # ============================================================
  # BLOC 10 — Résumé final
  # ============================================================

  message("\n========================================")
  message("     Thermal Clustering Complete!")
  message("========================================")
  for (i in 1:n_clusters) {
    message("  Zone ", i, " (", cluster_labels[as.character(i)], "): ",
            stats_df$pct_area[i], "% of area | ",
            "Mean LST: ", stats_df$mean_lst[i], " C")
  }
  message("========================================")

  return(list(
    raster   = cluster_raster,
    clusters = clust_data_clean,
    centers  = as.data.frame(kmeans_result$centers),
    stats    = stats_df
  ))
}
