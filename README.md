# NOAAnalysis

https://travis-ci.org/vkati06/NOAAnalysis.svg?branch=master

The NOAAnalysis package can be used to work with the NOAA Significant 
Earthquakes dataset from [link](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1)

## Example

Clean the NOAA data with the `eq_clean_data()` function then 
visualize the earthquakes in time with the `geom_timeline()` geom

```
readr::read_delim("signif.txt", delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
         xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
         size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
geom_timeline() +
theme_timeline +
ggplot2::labs(colour = "# deaths", size = "Richter scale value")
```
