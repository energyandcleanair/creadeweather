test_that("utils.attach_city_by_name matches normalized names within countries", {
  cities <- tibble::tibble(
    name = c("São Paulo", "Springfield", "Nowhere"),
    iso2 = c("BR", "GB", "US"),
    pop = c(12e6, 1e5, 50),
    capital = c(FALSE, FALSE, FALSE)
  )
  locations <- tibble::tibble(
    id = c("sao-paulo", "springfield-gb", "springfield-us"),
    name = c("SP monitor", "GB monitor", "US monitor"),
    city = c("Sao Paulu", "Springfield", "Springfield"),
    name_2 = c(NA_character_, NA_character_, NA_character_),
    country = c("BR", "GB", "US"),
    geometry = c("point-sp", "point-gb", "point-us")
  )

  matched <- utils.attach_city_by_name(locations, cities)

  expect_named(
    matched,
    c("id", "name", "city", "country", "geometry", "pop", "capital")
  )
  expect_identical(
    matched$id,
    c("sao-paulo", "springfield-gb", NA_character_)
  )
  expect_identical(matched$name, c("São Paulo", "Springfield", "Nowhere"))
  expect_identical(
    matched$city,
    c("Sao Paulu", "Springfield", NA_character_)
  )
  expect_identical(matched$country, c("BR", "GB", NA_character_))
  expect_identical(matched$geometry, c("point-sp", "point-gb", NA_character_))
  expect_identical(matched$pop, c(12e6, 1e5, 50))
  expect_identical(matched$capital, c(FALSE, FALSE, FALSE))
  expect_false("springfield-us" %in% matched$id)
})


test_that("utils.attach_city_by_name falls back to the GADM2 name", {
  cities <- tibble::tibble(
    name = "Dublin",
    iso2 = "IE",
    pop = 592713,
    capital = TRUE
  )
  locations <- tibble::tibble(
    id = "dublin-district",
    name = "Dublin monitor",
    city = "Station District",
    name_2 = "Dublin",
    country = "IE",
    geometry = "point-dublin"
  )

  matched <- utils.attach_city_by_name(locations, cities)
  fallback <- matched[matched$id == "dublin-district" & !is.na(matched$id), ]

  expect_equal(nrow(fallback), 1)
  expect_identical(fallback$id, "dublin-district")
  expect_identical(fallback$name, "Dublin")
  expect_identical(fallback$city, "Station District")
  expect_identical(fallback$country, "IE")
  expect_identical(fallback$geometry, "point-dublin")
  expect_identical(fallback$pop, 592713)
  expect_identical(fallback$capital, TRUE)
})
