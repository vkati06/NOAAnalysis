## R CMD check: no visible binding for global variable
if (getRversion() >= "2.15.1") globalVariables(c("DATE", "LOCATION_NAME_cleaned", "I_D", "FLAG_TSUNAMI", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "FOCAL_DEPTH", "EQ_PRIMARY", "EQ_MAG_MW", "EQ_MAG_MS", "EQ_MAG_MB", "EQ_MAG_ML", "EQ_MAG_MFA", "EQ_MAG_UNK", "INTENSITY", "COUNTRY", "STATE", "LOCATION_NAME", "LATITUDE", "LONGITUDE", "REGION_CODE", "DEATHS", "DEATHS_DESCRIPTION", "MISSING", "MISSING_DESCRIPTION", "INJURIES", "INJURIES_DESCRIPTION", "DAMAGE_MILLIONS_DOLLARS", "DAMAGE_DESCRIPTION", "HOUSES_DESTROYED", "HOUSES_DESTROYED_DESCRIPTION", "HOUSES_DAMAGED", "HOUSES_DAMAGED_DESCRIPTION", "TOTAL_DEATHS", "TOTAL_DEATHS_DESCRIPTION", "TOTAL_MISSING", "TOTAL_MISSING_DESCRIPTION", "TOTAL_INJURIES", "TOTAL_INJURIES_DESCRIPTION", "TOTAL_DAMAGE_MILLIONS_DOLLARS", "TOTAL_DAMAGE_DESCRIPTION", "TOTAL_HOUSES_DESTROYED", "TOTAL_HOUSES_DESTROYED_DESCRIPTION", "TOTAL_HOUSES_DAMAGED", "TOTAL_HOUSES_DAMAGED_DESCRIPTION"))

## Module 1. Obtain and Clean the Data

#' @title Data Cleaning
#'
#' @description This function cleans the NOAA Significant Earthquake Database
#' from \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @param raw_data Raw NOAA data frame
#'
#' @return This function returns a tibble containing the cleaned earthquake data:
#' \itemize{
#'   \item A DATE column created by uniting the year, month, day and converting
#'         it to the Date class
#'   \item LATITUDE and LONGITUDE columns converted to numeric class
#'   \item LOCATION_NAME_cleaned column from LOCATION_NAME by stripping out the
#'         country name and in title case
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr everything
#'
#' @references For more information on NOAA data see
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @examples
#' \dontrun{
#' noaa <- readr::read_delim(system.file("extdata", "signif.txt",
#'         package = "NOAAnalysis"), delim = "\t") %>% eq_clean_data()
#' }
#'
#' @export
eq_clean_data <- function(raw_data){

  ## Replace NA-s with "01" for missing MONTH and/or DAY then
  ## unite the year, month, day and convert it to the Date class
  ## handling the years before 1000 and even the negative (BC) years
  clean_data <- raw_data %>% tidyr::replace_na(list(MONTH = 1, DAY = 1)) %>%
    dplyr::mutate(DATE = as.Date(julian(as.numeric(MONTH), as.numeric(DAY), as.numeric(YEAR),
                                                        origin = c(month = 1, day = 1, year = 1970)),
                                 origin = "1970-01-01"))

  ## LATITUDE and LONGITUDE columns converted to numeric class
  clean_data <- clean_data %>% dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                                             LONGITUDE = as.numeric(LONGITUDE))

  ## Clean the LOCATION_NAME column
  clean_data <- clean_data %>%
    dplyr::mutate(LOCATION_NAME_cleaned = eq_location_clean(LOCATION_NAME))

  ## Reorder the variables
  clean_data <- clean_data %>% dplyr::select(I_D, FLAG_TSUNAMI, DATE, YEAR,
                                             MONTH, DAY, HOUR, MINUTE, SECOND,
                                             COUNTRY, STATE, LOCATION_NAME, LOCATION_NAME_cleaned,
                                             LATITUDE, LONGITUDE, REGION_CODE,
                                             dplyr::everything())

  return(clean_data)

}


#' @title String Cleaning (Helper Function)
#'
#' @description This function cleans the LOCATION_NAME string by stripping out
#' the country name and converts names to title case
#'
#' @param raw_string Raw string of LOCATION_NAME
#'
#' @return This function returns a clean string of LOCATION_NAME
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{eq_location_clean("SYRIA: UGARIT")}
#'
#' @export
eq_location_clean <- function(raw_string){

  ## Delete everything after the first semi-colon (including the semi-colon) then
  ## keep everything after the last colon
  ## (if there is no colon then delete everything because it is a country name)

  clean_string <- raw_string %>% stringr::str_replace_all(";.*", "") %>%
    stringr::str_trim()

  clean_string <- dplyr::if_else(!stringr::str_detect(clean_string, ":"),
                                 clean_string %>% stringr::str_replace(clean_string, NA_character_),
                                 clean_string %>% stringr::str_replace_all(".*:", "") %>%
                                   stringr::str_trim() %>% stringr::str_to_title())

  return(clean_string)

}

# ==============================================================================

## Module 2. Building Geoms

#' @title Date Filtering Stat Construction
#'
#' @description This stat corresponds to the \code{geom_timeline} and
#' \code{geom_timeline_label} geoms and filters the earthquakes ranging
#' from \code{xmin} to \code{xmax} dates
#'
#' @format NULL
#' @usage NULL
#'
#' @note Not exported to the user
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,

required_aes = c("x", "xmin", "xmax"),

compute_group = function(data, scales) {

  data <- data %>% dplyr::filter(xmin <= x & x <= xmax)

}

)


#' @title GeomTimeline Class Construction
#'
#' @description Construct the new class corresponding to the \code{geom_timeline} geom
#'
#' @format NULL
#' @usage NULL
#'
#' @note Not exported to the user
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid pointsGrob
#' @importFrom grid polylineGrob
#' @importFrom grid xaxisGrob
#' @importFrom grid unit
#' @importFrom grid gpar
#' @importFrom grid gList
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,

                                 required_aes = c("x", "xmin", "xmax"),
                                 default_aes = ggplot2::aes(y = 0.5, colour = "grey50",
                                                            fill = "grey50", size = 4.5,
                                                            alpha = 0.5, shape = 21, stroke = 0.5),

                                 draw_key = ggplot2::draw_key_point,

                                 draw_panel = function(data, panel_scales, coord){

                                   coords <- coord$transform(data, panel_scales)

                                   my_xmin <- coords$xmin[1]
                                   my_xmax <- coords$xmax[1]

                                   my_points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     size = grid::unit(coords$size, "mm"),
                                     pch = coords$shape,
                                     gp = grid::gpar(col = coords$colour,
                                                     fill = coords$colour,
                                                     alpha = coords$alpha)
                                   )

                                   unique_y <- unique(coords$y)
                                   n_unique_y <- length(unique_y)

                                   my_lines <- grid::polylineGrob(
                                     x = grid::unit(rep(c(my_xmin, my_xmax), each = n_unique_y), "npc"),
                                     y = grid::unit(c(unique_y, unique_y), "npc"),
                                     id = rep(seq_along(unique_y), 2),
                                     gp = grid::gpar(col = "grey50")
                                   )

                                   my_xaxis <- grid::xaxisGrob()

                                   grid::gList(my_points, my_lines, my_xaxis)

                                 }

)


#' @title Theme for geom_timeline Plot
#'
#' @description Simple theme for \code{geom_timeline} plot
#'
#' @format NULL
#' @usage NULL
#' 
#' @return Theme object
#'
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#'
#' @examples
#' \dontrun{
#' readr::read_delim(system.file("extdata", "signif.txt",
#'        package = "NOAAnalysis"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & YEAR >= 2000) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
#'          xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
#'          size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
#' geom_timeline() +
#' theme_timeline +
#' ggplot2::labs(colour = "# deaths", size = "Richter scale value")
#' }
#'
#' @export
theme_timeline <- ggplot2::theme_classic() +
  ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.position = "bottom")


#' @title Plot the Time Line of Earthquakes
#'
#' @description Visualize the times at which earthquakes occur ranging from
#' \code{xmin} to \code{xmax} dates with a point for each earthquake
#'
#' @details The \code{x} aesthetic is a date and an optional \code{y} aesthetic
#' is a factor indicating some stratification (e.g. country). Additionally,
#' the magnitudes and the number of deaths associated with each earthquake can be shown.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} Date at which earthquakes occur
#'   \item \strong{xmin} Starting date
#'   \item \strong{xmax} Ending date
#'   \item y Factor indicating stratification (e.g. country)
#'   \item colour Colour for lines and borders and filling
#'   \item size Size of points
#'   \item alpha Alpha channel for transparency
#' }
#' 
#' @return This function returns a ggplot object
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' readr::read_delim(system.file("extdata", "signif.txt",
#'        package = "NOAAnalysis"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & YEAR >= 2000) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
#'          xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
#'          size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
#' geom_timeline() +
#' theme_timeline +
#' ggplot2::labs(colour = "# deaths", size = "Richter scale value")
#' }
#'
#'@export
geom_timeline <- function(mapping = NULL, data = NULL, stat = StatTimeline,
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title GeomTimelineLabel Class Construction
#'
#' @description Construct the new class corresponding to the
#' \code{geom_timeline_label} geom
#'
#' @format NULL
#' @usage NULL
#'
#' @note Not exported to the user
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_blank
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr top_n
#' @importFrom grid nullGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom grid unit
#' @importFrom grid gpar
#' @importFrom grid gList
#' @importFrom grid gTree
#'
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,

                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 0.5, n_max = 5),

                                      draw_key = ggplot2::draw_key_blank,

                                      draw_panel = function(data, panel_scales, coord){

                                        if (!("size" %in% names(data))) return(grid::nullGrob())

                                        my_n_max <- data$n_max[1]

                                        data <- data %>% dplyr::group_by(y) %>%
                                          dplyr::top_n(n = my_n_max, wt = size)

                                        coords <- coord$transform(data, panel_scales)

                                        my_vlines <- grid::segmentsGrob(
                                          x0 = grid::unit(coords$x, "native"),
                                          x1 = grid::unit(coords$x, "native"),
                                          y0 = grid::unit(coords$y, "native"),
                                          y1 = grid::unit(coords$y + 0.04, "native"),
                                          gp = grid::gpar(col = "grey50", alpha = 0.5)
                                        )

                                        my_texts <- grid::textGrob(
                                          label = coords$label,
                                          x = grid::unit(coords$x,"native"),
                                          y = grid::unit(coords$y + 0.05,"native"),
                                          rot = 45,
                                          just = "left",
                                          gp = grid::gpar(fontsize = 8)
                                        )

                                        grid::gTree(children = grid::gList(my_vlines, my_texts))

                                      }

)


#' @title Add Annotation to the Time Line Plot of Earthquakes
#'
#' @description This geom adds line and text annotation to the plot
#'
#' @details Aesthetics are \code{x}, which is the date of the earthquake and
#' \code{label} which takes the column name from which annotations will be obtained.
#' There is an option to subset to \code{n_max} number of earthquakes,
#' where we take the \code{n_max} largest (by magnitude) earthquakes.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @section Aesthetics:
#' \code{geom_timeline_label} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x} Date at which earthquakes occur
#'   \item \strong{label} The column name from which annotations will be obtained
#'   \item n_max To subset to the \code{n_max} largest earthquakes
#' }
#' 
#' @return This function returns a ggplot object
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' readr::read_delim(system.file("extdata", "signif.txt",
#'        package = "NOAAnalysis"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY %in% c("USA", "CHINA") & YEAR >= 2000) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
#'                 xmin = as.Date("2000-01-01"), xmax = as.Date("2016-01-01"),
#'                 size = as.numeric(EQ_PRIMARY), color = as.numeric(TOTAL_DEATHS))) +
#' geom_timeline() +
#' geom_timeline_label(ggplot2::aes(label = LOCATION_NAME_cleaned)) +
#' theme_timeline +
#' ggplot2::labs(colour = "# deaths", size = "Richter scale value")
#' }
#'
#'@export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = StatTimeline,
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ==============================================================================

## Module 3. Mapping Tools

#' @title Visualize the Earthquakes in Space
#'
#' @description Maps and annotates the epicenters
#'
#' @details Each earthquake is shown with a circle, and the radius of the circle
#' is proportional to the earthquake's magnitude. Each point is annotated in pop
#' up window containing annotation data stored in the \code{annot_col} column
#' of \code{data}.
#'
#' @param data The filtered data frame with earthquakes to visualize
#' @param annot_col Column containing annotation data
#'
#' @return This function returns a HTML widget object
#'
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @examples
#' \dontrun{
#' readr::read_delim(system.file("extdata", "signif.txt",
#'        package = "NOAAnalysis"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & YEAR >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {

  leaflet::leaflet() %>%
    leaflet::addProviderTiles("OpenStreetMap") %>%
    leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE,
                              radius = ~ EQ_PRIMARY,  weight = 1.5, opacity = 0.3,
                              popup = ~ paste(get(annot_col)))

}


#' @title HTML Labeling
#'
#' @description Creates an HTML label that can be used as the annotation text
#' in the leaflet map
#'
#' @details This function puts together a character string for each earthquake
#' that showes the cleaned location, the magnitude, and the total number of deaths
#' (if not missing).
#'
#' @param data The filtered data frame with earthquakes to visualize
#'
#' @return This function returns a HTML label
#'
#' @examples
#' \dontrun{
#' readr::read_delim(system.file("extdata", "signif.txt",
#'        package = "NOAAnalysis"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & YEAR >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data){

  loc <- ifelse(!is.na(data$LOCATION_NAME_cleaned),
                paste("<b>Location:</b>", data$LOCATION_NAME_cleaned, "<br />"),
                "<br />")

  mag <- ifelse(!is.na(data$EQ_PRIMARY),
                paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />"),
                "<br />")

  death <- ifelse(!is.na(data$TOTAL_DEATHS),
                  paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br />"),
                  "<br />")

  popup_info <- paste(loc, mag, death)

  popup_info

}
