context("eq_map()")

my_map <- readr::read_delim(system.file("extdata", "signif.txt",
                              package = "NOAAnalysis"), delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & YEAR >= 2000) %>%
  eq_map(annot_col = "DATE")


test_that("eq_map() returns leaflet object", {
  expect_is(my_map, "leaflet")
})




