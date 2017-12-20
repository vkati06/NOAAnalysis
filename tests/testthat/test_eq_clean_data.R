context("Check the class of DATE, LATITUDE, LONGITUDE")

fpath <- system.file("extdata", package = "NOAAnalysis")
setwd(fpath)
noaa <- readr::read_delim("signif.txt", delim = "\t") %>%
        eq_clean_data()

test_that("For DATE", {
  expect_is(noaa$DATE, "Date")
})

test_that("For LATITUDE", {
  expect_is(noaa$LATITUDE, "numeric")
})

test_that("For LONGITUDE", {
  expect_is(noaa$LONGITUDE, "numeric")
})
