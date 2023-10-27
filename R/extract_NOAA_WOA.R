#' Extracts the NOAA World Ocean data from a given location based on an Okeanos Explorer CTD cast
#'
#' Adapted from the function "plot_NOAA" from the oceanexplorer package by Martin Schobben
#'
#' Calls: url_parser.R, read_NOAA.R
#'
#' Called by: NOAA Explore QAQC_CTD Rosette.Rmd
#'
#' NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
#' Last update: Oct. 2023
#'
#'
#' @param NOAA Dataset of the NOAA World Ocean Atlas
#'  (with [get_NOAA_WOA()]).
#' @param expedition the ASPIRE campaign expedition (must be '"1805"', '"1810"','"1812"', '"1903l1"','"1905l1"', '"1906"','"2101"','"2102"','"2107"')
#' @param cast the CTD cast number for a given expedition. Must be in the format '"ctd001, ctd002"', etc.
#' @param epsg The epsg used to project the data (currently supported `4326`,
#'  `3031`and `3995`).
#'
#' @return a dataframe of the WOA data for the specified metric, temporal resolution (determined from [get_NOAA_WOA()]), and location
#'
#' @export
#'
#' @examples
#'
#' # retrieve NOAA data
#' NOAA <- get_NOAA_WOA_data("oxygen", 1, "annual", T)
#'
#' # extract WOA data for a single location from starts object, NOAA
#' WOA_dat <- extract_NOAA_WOA(NOAA,
#'                         expedition = "EX2107",
#'                         cast = "ctd001",
#'                         epsg = NULL) # Will inherit from NOAA stars object
#'
#' ggplot() +
#'  geom_path(data =ctd_rosette_data, aes(x = temperature, y = depth),color = "black", linewidth = 1.5) +
#'  geom_path(data =WOA_dat, aes(x = t_an, y = depth), color = "red", linewidth = 1.5) +
#'  labs(x = "Temperature (°C)", y = "Depth (m)") +
#'  scale_y_reverse() +
#'  scale_x_continuous(position = "top",
#'                    breaks = c(5, 10, 15, 20, 25, 30)) +
#'  theme_bw()
#'
#'


extract_NOAA_WOA <- function(NOAA, ex, ctd_cast, epsg = NULL) {

  library(ggplot2)

  # Create a dataset of the expedition, cast, variable of interest and values

  # CTD <- NOAA.Explore.QAQC::ASPIRE_SHIP_CTD_data |>
  #   dplyr::select(expedition, cast, deployment_longitude, deployment_latitude, depth, all_of(var)) |> # Later turn temperature to a variable
  #   dplyr::filter(expedition == ex) |>
  #   dplyr::filter(cast == ctd_cast)

  # Create variables to add to the WOA dataset

  # Create a dataframe of just the points of interest for a
  xy <- NOAA.Explore.QAQC::ASPIRE_SHIP_CTD_data |>
    dplyr::select(expedition, cast, deployment_longitude, deployment_latitude) |>
    dplyr::filter(expedition == ex) |>
    dplyr::filter(cast == ctd_cast) |>
    # For now, just take the mean of the two points, update later
    dplyr::summarise(lat_degrees = mean(deployment_latitude),
                     lon_degrees = mean(deployment_longitude), .groups = c("expedition", "cast"))

  # specify coordinates
  sp::coordinates(xy) <- ~lon_degrees+lat_degrees

  # Get data from WOA
  #NOAA <- get_NOAA_WOA_data("temperature", 1, "annual", cache = T)

  # check the projection for the NOAA object
  epsg <- epsg_check(NOAA, epsg)
  if (epsg == "original") epsg <- sf::st_crs(NOAA)
  #print(epsg$input)

  # Give the lat and lon of the xy object the same projection as the NOAA object
  sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

  # Extract all the data available at point xy - it may take a minute

  dat <- as.data.frame(stars::st_extract(NOAA, sf::st_as_sf(xy))) |>
    # Add in meta data
    dplyr::mutate(cast = ctd_cast,
                  expedition = ex,
                  #depth_WOA = depth,
                  WOA_lat = xy@bbox[2],
                  WOA_lon = xy@bbox[1])

  # filter out depths with no values
  WOA_dat <- dat |>
    dplyr::filter(!is.na(dat[[3]])) |>
    # Remove columns
    dplyr::select(-geometry)

  return(WOA_dat)


  # # Plot the CTD and WOA data together
  # ggplot() +
  #   geom_path(data =CTD, aes(x = temperature, y = depth),color = "black", linewidth = 1.5) +
  #   geom_path(data =WOA_dat, aes(x = t_an, y = depth), color = "red", linewidth = 1.5) +
  #   labs(x = "Temperature (°C)", y = "Depth (m)") +
  #   scale_y_reverse() +
  #   scale_x_continuous(position = "top",
  #                      breaks = c(5, 10, 15, 20, 25, 30)) +
  #   theme_bw()
  #
  # Figure out mapping stuff later - write a custom app with a shapefile

  # RegHR <- maps::map("worldHires", namesonly=TRUE, plot=FALSE)
  #
  #
  # basemap <-  maps::map.where('worldHires', x=-77.25374, y= 30.15007)
  #
  # ggmap::ggmap(basemap)
  #
  # map('usa', fill=TRUE, col="#00A600",  bg="#CCEEFF", ylim=c(-35,65), wrap=c(-25,335),
  #     region=RegHR[-grep("Antarctica", RegHR)])
  #
  # lats<-c(25,35)
  # lons<-c(-83,-77)
  # bb<-ggmap::make_bbox(lon=lons,lat=lats,f=0.05)
  # cda<-ggmap::get_map(bb,zoom=3,maptype="terrain")
  # ggmap::ggmap(cda)+xlim(lons)+ylim(lats)+theme_bw()+labs(x="Longitude",y="Latitude")
}

reproject <- function(obj, epsg, ...) {

  # check if epsg is different of original
  epsg <- epsg_check(obj, epsg)
  # epsg NULL, "", or "original" then use crs of supplied object
  if (epsg == "original") {
    return(obj)
  }

  UseMethod("reproject")
}

epsg_check <- function(obj, epsg) {

  # correct format and same as original
  if (is.null(epsg) || epsg == "original" || epsg == character(1)) {
    # return early
    "original"
  } else if (inherits(epsg, "crs")) {
    # return early
    if (epsg == sf::st_crs(obj)) {
      "original"
    } else {
      epsg
    }
  } else if (is.character(epsg) & grepl("^[[:digit:]]+$", epsg)) {
    # recast to numeric
    epsg <- as.numeric(epsg)
    # try if crs exist
    tryCatch(
      sf::st_crs(epsg),
      warning = function(cnd) {
        stop("Unknown format supplied to epsg.", call. = FALSE)
      }
    )

    if (sf::st_crs(epsg)  == sf::st_crs(obj)) {
      "original"
    } else {
      epsg
    }
  } else if (is.numeric(epsg)) {
    # try if crs exist
    tryCatch(
      sf::st_crs(epsg),
      warning = function(cnd) {
        stop("Unknown format supplied to epsg.", call. = FALSE)
      }
    )
    if (sf::st_crs(epsg) == sf::st_crs(obj)) {
      "original"
    } else {
      epsg
    }
  }
}

