test_that("extract_fire_vars_by_region groups variables by suffix correctly", {
  
  # Test case 1: Simple fire_frp without suffix
  var_names1 <- c("fire_frp", "fire_frp_lag1", "ws", "temp")
  result1 <- creadeweather:::fire.extract_vars_by_region(var_names1)
  expect_s3_class(result1, "tbl_df")
  expect_equal(nrow(result1), 1)
  expect_equal(result1$suffix, "")
  expect_equal(sort(result1$vars[[1]]), c("fire_frp", "fire_frp_lag1"))
  
  # Test case 2: fire_frp with region suffix
  var_names2 <- c("fire_frp_MYS", "fire_frp_MYS_lag1", "fire_frp_MYS_lag2", "ws", "temp")
  result2 <- creadeweather:::fire.extract_vars_by_region(var_names2)
  expect_equal(nrow(result2), 1)
  expect_equal(result2$suffix, "_MYS")
  expect_equal(sort(result2$vars[[1]]), c("fire_frp_MYS", "fire_frp_MYS_lag1", "fire_frp_MYS_lag2"))
  
  # Test case 3: Multiple regions
  var_names3 <- c("fire_frp_MYS", "fire_frp_MYS_lag1", "fire_frp_IDN", "pm25_emission_XYZ", "ws")
  result3 <- creadeweather:::fire.extract_vars_by_region(var_names3)
  expect_equal(nrow(result3), 3)
  expect_true(all(c("_MYS", "_IDN", "_XYZ") %in% result3$suffix))
  mys_row <- result3[result3$suffix == "_MYS", ]
  expect_equal(sort(mys_row$vars[[1]]), c("fire_frp_MYS", "fire_frp_MYS_lag1"))
  idn_row <- result3[result3$suffix == "_IDN", ]
  expect_equal(idn_row$vars[[1]], "fire_frp_IDN")
  xyz_row <- result3[result3$suffix == "_XYZ", ]
  expect_equal(xyz_row$vars[[1]], "pm25_emission_XYZ")
  
  # Test case 4: Mix of with and without suffix
  var_names4 <- c("fire_frp", "fire_frp_lag1", "fire_frp_MYS", "fire_frp_MYS_lag1", "ws")
  result4 <- creadeweather:::fire.extract_vars_by_region(var_names4)
  expect_equal(nrow(result4), 2)
  expect_true(all(c("", "_MYS") %in% result4$suffix))
  empty_row <- result4[result4$suffix == "", ]
  expect_equal(sort(empty_row$vars[[1]]), c("fire_frp", "fire_frp_lag1"))
  mys_row <- result4[result4$suffix == "_MYS", ]
  expect_equal(sort(mys_row$vars[[1]]), c("fire_frp_MYS", "fire_frp_MYS_lag1"))
  
  # Test case 5: No fire variables
  var_names5 <- c("ws", "temp", "precip")
  result5 <- creadeweather:::fire.extract_vars_by_region(var_names5)
  expect_equal(nrow(result5), 0)
  expect_s3_class(result5, "tbl_df")
  expect_true(all(c("suffix", "vars") %in% names(result5)))
  
  # Test case 6: Custom prefixes
  var_names6 <- c("fire_count", "fire_count_lag1", "fire_count_REGION", "ws")
  result6 <- creadeweather:::fire.extract_vars_by_region(var_names6, prefixes = "fire_count")
  expect_equal(nrow(result6), 2)
  expect_true(all(c("", "_REGION") %in% result6$suffix))
  empty_row <- result6[result6$suffix == "", ]
  expect_equal(sort(empty_row$vars[[1]]), c("fire_count", "fire_count_lag1"))
  region_row <- result6[result6$suffix == "_REGION", ]
  expect_equal(region_row$vars[[1]], "fire_count_REGION")
  
  # Test case 7: Multiple prefixes with same suffix
  var_names7 <- c("fire_frp_MYS", "fire_count_MYS", "pm25_emission_MYS", "ws")
  result7 <- creadeweather:::fire.extract_vars_by_region(var_names7)
  expect_equal(nrow(result7), 1)
  expect_equal(result7$suffix, "_MYS")
  expect_equal(sort(result7$vars[[1]]), c("fire_count_MYS", "fire_frp_MYS", "pm25_emission_MYS"))
  
})