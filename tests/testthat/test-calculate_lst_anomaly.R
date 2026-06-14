test_that("calculate_lst_anomaly works with mean_diff method", {
  r1 <- terra::rast(nrows = 10, ncols = 10)
  r2 <- terra::rast(nrows = 10, ncols = 10)
  terra::values(r1) <- runif(100, 20, 35)
  terra::values(r2) <- runif(100, 25, 40)
  lst <- c(r1, r2)
  names(lst) <- c("Jun", "Jul")

  result <- calculate_lst_anomaly(lst, method = "mean_diff", save = FALSE)
  expect_type(result, "list")
  expect_true("anomaly"    %in% names(result))
  expect_true("hot_zones"  %in% names(result))
  expect_true("cold_zones" %in% names(result))
  expect_true("stats"      %in% names(result))
})

test_that("calculate_lst_anomaly works with zscore method", {
  r <- terra::rast(nrows = 10, ncols = 10)
  terra::values(r) <- runif(100, 20, 40)
  result <- calculate_lst_anomaly(r, method = "zscore", save = FALSE)
  expect_s4_class(result$anomaly, "SpatRaster")
})
