test_that("import_station_locations works with CSV", {
  tmp <- tempfile(fileext = ".csv")
  df  <- data.frame(
    station_id = c("S1", "S2"),
    name       = c("Agadir", "Tiznit"),
    longitude  = c(-9.598, -9.732),
    latitude   = c(30.427, 29.697)
  )
  write.csv(df, tmp, row.names = FALSE)
  result <- import_station_locations(tmp, plot_map = FALSE)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  unlink(tmp)
})

test_that("import_station_locations fails on missing file", {
  expect_error(import_station_locations("nonexistent.csv"))
})
