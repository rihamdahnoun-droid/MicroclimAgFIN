test_that("import_weather_data works with CSV", {
  tmp <- tempfile(fileext = ".csv")
  df  <- data.frame(
    date        = as.character(seq(as.Date("2023-01-01"),
                                   as.Date("2023-03-01"), by = "month")),
    temperature = c(12, 15, 18),
    humidity    = c(70, 65, 60),
    wind_speed  = c(3.0, 2.5, 2.0),
    radiation   = c(120, 150, 180),
    station_id  = "S1"
  )
  write.csv(df, tmp, row.names = FALSE)
  result <- import_weather_data(tmp)
  expect_s3_class(result, "data.frame")
  expect_true("temperature" %in% names(result))
  expect_true("date" %in% names(result))
  expect_equal(nrow(result), 3)
  unlink(tmp)
})

test_that("import_weather_data fails on missing file", {
  expect_error(import_weather_data("nonexistent.csv"))
})

test_that("import_weather_data handles missing columns gracefully", {
  tmp <- tempfile(fileext = ".csv")
  df  <- data.frame(
    date        = as.character(seq(as.Date("2023-01-01"),
                                   as.Date("2023-03-01"), by = "month")),
    temperature = c(12, 15, 18),
    station_id  = "S1"
  )
  write.csv(df, tmp, row.names = FALSE)
  result <- import_weather_data(tmp)
  expect_true(all(is.na(result$humidity)))
  unlink(tmp)
})
