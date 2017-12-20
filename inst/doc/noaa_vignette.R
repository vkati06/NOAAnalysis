## ----setup, echo = FALSE, message = FALSE--------------------------------
library(NOAAnalysis)
library(magrittr)
library(chron)
g <- system.file("extdata", package = "NOAAnalysis")
knitr::opts_knit$set(root.dir = g)

## ----message = FALSE-----------------------------------------------------
noaa <- readr::read_delim(system.file("extdata", "signif.txt",
        package = "NOAAnalysis"), delim = "\t") %>% eq_clean_data()
head(noaa)
summary(noaa)

## ------------------------------------------------------------------------
eq_location_clean("SYRIA: UGARIT")

## ----message = FALSE, fig.height = 4, fig.width = 6, fig.align = "center"----
readr::read_delim(system.file("extdata", "signif.txt",
       package = "NOAAnalysis"), delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "USA" & YEAR >= 2000) %>%
ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
         xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
         size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
geom_timeline() +
theme_timeline +
ggplot2::labs(colour = "# deaths", size = "Richter scale value")

## ----message = FALSE, fig.height = 4, fig.width = 6, fig.align = "center"----
readr::read_delim(system.file("extdata", "signif.txt",
       package = "NOAAnalysis"), delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY %in% c("USA", "CHINA") & YEAR >= 2000) %>%
ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
                xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
                size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
geom_timeline() +
geom_timeline_label(ggplot2::aes(label = LOCATION_NAME_cleaned)) +
theme_timeline +
ggplot2::labs(colour = "# deaths", size = "Richter scale value")

## ----message = FALSE, fig.height = 4, fig.width = 6, fig.align = "center"----
readr::read_delim(system.file("extdata", "signif.txt",
       package = "NOAAnalysis"), delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "MEXICO" & YEAR >= 2000) %>%
eq_map(annot_col = "DATE")

## ----message = FALSE, fig.height = 4, fig.width = 6, fig.align = "center"----
readr::read_delim(system.file("extdata", "signif.txt",
       package = "NOAAnalysis"), delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "MEXICO" & YEAR >= 2000) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>%
eq_map(annot_col = "popup_text")

