#' Import Land Cover Data
#'
#' @description
#' Downloads and imports land cover data for a given country and region
#' using ESA Land Cover via the geodata package. Reclassifies land cover
#' into simplified agricultural categories and clips to the study area.
#'
#' @param country Character. ISO3 country code. Default is "MAR".
#' @param region Character. Region name to clip data. Default is NULL.
#' @param year Numeric. Year of land cover data (2015-2020). Default is 2020.
#' @param output_dir Character. Directory to save results. Default is "outputs/maps".
#' @param save Logical. Whether to save the raster. Default is TRUE.
#'
#' @return A SpatRaster with reclassified land cover classes:
#' \describe{
#'   \item{1}{Cropland / Agriculture}
#'   \item{2}{Forest / Tree cover}
#'   \item{3}{Shrubland / Grassland}
#'   \item{4}{Bare soil / Sparse vegetation}
#'   \item{5}{Urban / Built-up}
#'   \item{6}{Water bodies}
#'   \item{7}{Other}
#' }
#'
#' @examples
#' \dontrun{
#' # Interactive mode
#' lc <- import_landcover()
#'
#' # Direct mode
#' lc <- import_landcover(
#'   country = "MAR",
#'   region  = "Souss-Massa",
#'   year    = 2020
#' )
#' }
#'
#' @importFrom terra rast crop mask classify writeRaster freq values
#' @importFrom geodata landcover gadm
#' @export
import_landcover <- function(country    = NULL,
                             region     = NULL,
                             year       = NULL,
                             output_dir = "outputs/maps",
                             save       = TRUE) {

  # ============================================================
  # BLOC 1 — Mode interactif
  # ============================================================

  message("========================================")
  message("   microclimAg — Land Cover Import     ")
  message("========================================\n")

  # -- Pays --
  if (is.null(country)) {
    message("Common ISO3 codes:")
    message("  MAR=Morocco | FRA=France | ESP=Spain | DZA=Algeria\n")
    country <- toupper(trimws(readline(prompt = "Enter country ISO3 code (e.g. MAR): ")))
    if (nchar(country) != 3) stop("Invalid ISO3 code.")
  }

  # -- Région --
  if (is.null(region)) {
    message("\nFetching regions for: ", country, "...")
    boundaries_all    <- geodata::gadm(country = country, level = 1, path = tempdir())
    available_regions <- sort(boundaries_all$NAME_1)

    message("\nAvailable regions:")
    for (i in seq_along(available_regions)) {
      message("  ", i, ". ", available_regions[i])
    }
    message("  0. Entire country\n")

    region_input <- trimws(readline(prompt = "Enter region number or name (0 = entire country): "))

    if (grepl("^[0-9]+$", region_input)) {
      num <- as.integer(region_input)
      if (num == 0) {
        region <- NULL
        message("Selected: Entire country")
      } else if (num >= 1 && num <= length(available_regions)) {
        region <- available_regions[num]
        message("Selected: ", region)
      } else {
        stop("Invalid number.")
      }
    } else {
      region <- region_input
      message("Selected: ", region)
    }
  }

  # -- Année --
  if (is.null(year)) {
    message("\nAvailable years: 2015, 2016, 2017, 2018, 2019, 2020")
    year_input <- trimws(readline(prompt = "Enter year (e.g. 2020): "))
    year       <- as.integer(year_input)
    if (!year %in% 2015:2020) {
      warning("Year outside 2015-2020 range. Using 2020.")
      year <- 2020
    }
    message("Selected year: ", year)
  }

  # ============================================================
  # BLOC 2 — Téléchargement des frontières
  # ============================================================

  message("\nDownloading boundaries...")

  if (!is.null(region)) {
    boundaries_all <- geodata::gadm(country = country, level = 1, path = tempdir())
    region_match   <- boundaries_all$NAME_1 == region
    if (!any(region_match)) stop("Region '", region, "' not found.")
    boundaries <- boundaries_all[region_match, ]
  } else {
    boundaries <- geodata::gadm(country = country, level = 0, path = tempdir())
  }

  # ============================================================
  # BLOC 3 — Téléchargement Land Cover ESA
  # ============================================================

  message("\nDownloading ESA Land Cover ", year, "...")
  message("This may take a few minutes...")

  lc_raw <- geodata::landcover(var = "cropland", path = tempdir())

  # ============================================================
  # BLOC 4 — Découpage sur la zone d'étude
  # ============================================================

  message("Clipping to study area...")
  lc_cropped <- terra::crop(lc_raw, boundaries)
  lc_masked  <- terra::mask(lc_cropped, boundaries)

  # ============================================================
  # BLOC 5 — Reclassification
  # ============================================================

  message("\nReclassifying land cover...")

  # Matrice de reclassification ESA -> classes simplifiées
  # Format: from, to, becomes
  rcl_matrix <- matrix(c(
    0,   10,  7,   # No data -> Other
    10,  20,  1,   # Cropland
    20,  30,  2,   # Forest
    30,  40,  3,   # Shrubland
    40,  60,  3,   # Grassland
    60,  70,  4,   # Bare soil
    70,  80,  5,   # Urban
    80,  100, 6,   # Water
    100, 255, 7    # Other
  ), ncol = 3, byrow = TRUE)

  lc_reclass <- terra::classify(lc_masked, rcl_matrix, include.lowest = TRUE)

  # Ajouter les labels
  levels_df <- data.frame(
    value = 1:7,
    label = c("Cropland", "Forest", "Shrubland/Grassland",
              "Bare Soil", "Urban", "Water", "Other")
  )
  levels(lc_reclass) <- levels_df

  # ============================================================
  # BLOC 6 — Statistiques
  # ============================================================

  message("\nLand Cover Statistics:")

  freq_table <- terra::freq(lc_reclass)
  freq_table$percentage <- round(100 * freq_table$count /
                                   sum(freq_table$count, na.rm = TRUE), 1)

  # Ajouter les labels
  freq_table$class <- levels_df$label[match(freq_table$value,
                                            levels_df$value)]

  freq_table <- freq_table[!is.na(freq_table$value), ]
  print(freq_table[, c("class", "count", "percentage")])

  # ============================================================
  # BLOC 7 — Sauvegarde
  # ============================================================

  if (save) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    region_label <- ifelse(is.null(region), "full", gsub("[ /]", "_", region))
    out_file <- file.path(
      output_dir,
      paste0("landcover_", country, "_", region_label, "_", year, ".tif")
    )

    terra::writeRaster(lc_reclass, out_file, overwrite = TRUE)
    message("\nLand cover saved to: ", out_file)

    # Sauvegarder les stats
    write.csv(freq_table,
              file.path("outputs/tables",
                        paste0("landcover_stats_", country, "_", year, ".csv")),
              row.names = FALSE)
  }

  # ============================================================
  # BLOC 8 — Visualisation
  # ============================================================

  message("\nPlotting land cover map...")

  lc_colors <- c(
    "#FFD700",  # 1 Cropland - jaune
    "#228B22",  # 2 Forest - vert foncé
    "#90EE90",  # 3 Shrubland - vert clair
    "#D2B48C",  # 4 Bare soil - marron
    "#FF4500",  # 5 Urban - rouge
    "#4169E1",  # 6 Water - bleu
    "#808080"   # 7 Other - gris
  )

  plot(lc_reclass,
       col  = lc_colors,
       main = paste("Land Cover -", country,
                    ifelse(is.null(region), "", paste("-", region)),
                    year),
       legend = TRUE)

  # ============================================================
  # BLOC 9 — Résumé final
  # ============================================================

  message("\n========================================")
  message("       Land Cover Import Complete!")
  message("========================================")
  message("  Country : ", country)
  message("  Region  : ", ifelse(is.null(region), "Full country", region))
  message("  Year    : ", year)
  message("  Classes : ", nrow(freq_table))
  message("========================================")

  return(lc_reclass)
}
