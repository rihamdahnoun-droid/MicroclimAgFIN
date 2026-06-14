test_that("clean_lst_data works on SpatRaster", {
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = -10, xmax = -8,
                   ymin = 29,  ymax = 31)
  terra::values(r) <- runif(100, 15, 45)
  result <- clean_lst_data(r, save = FALSE)
  expect_s4_class(result, "SpatRaster")
})

test_that("clean_lst_data removes outliers", {
  r <- terra::rast(nrows = 10, ncols = 10)
  v <- runif(100, 15, 45)
  v[1] <- 999
  v[2] <- -999
  terra::values(r) <- v
  result <- clean_lst_data(r, save = FALSE)
  vals   <- terra::values(result)
  expect_true(all(is.na(vals) | (vals >= -20 & vals <= 60)))
})
