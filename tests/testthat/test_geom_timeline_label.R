context("geom_timeline_label()")

my_plot <- readr::read_delim(system.file("extdata", "signif.txt",
                              package = "NOAAnalysis"), delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "USA" & YEAR >= 2000) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
                               xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
                               size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
  geom_timeline_label(ggplot2::aes(label = LOCATION_NAME_cleaned)) +
  theme_timeline +
  ggplot2::labs(colour = "# deaths", size = "Richter scale value")

test_that("geom_timeline_label() returns ggplot object", {
  expect_is(my_plot, "ggplot")
})

test_that("The classes of the geom_timeline() plot layers", {
  expect_is(my_plot$layers[[1]], "ggproto")
  expect_is(my_plot$layers[[1]]$geom, "GeomTimelineLabel")
  expect_is(my_plot$layers[[1]]$stat, "StatTimeline")
})


