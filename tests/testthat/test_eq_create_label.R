context("eq_create_label()")

fpath <- system.file("extdata", package = "NOAAnalysis")
setwd(fpath)
noaa <- readr::read_delim("signif.txt", delim = "\t") %>%
  eq_clean_data()

test_that("eq_create_label returns character", {
  expect_is(eq_create_label(noaa), "character")
})

