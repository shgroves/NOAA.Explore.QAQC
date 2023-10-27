#' Plotting the global NOAA World Ocean Atlas
#'
#' Plots the NOAA World Ocean Atlas on worldmap including optional
#' filtered locations.
#'
#' A worldmap is plotted as an [`ggplot`][ggplot2::ggplot()] object which by
#' default will plot the surface layer of the selected  oceanographic variable.
#' One can plot different depth slices by selecting the appropriate depth in
#' meters (e.g., `depth = 100`). It is, furthermore possible to visualize the
#' locations of data extractions with [filter_NOAA()]. See the examples below
#' for a more detailed overview of this workflow. Different projections of the
#' worldmap can be selected by supplying an `epsg`. Currently only three
#' projections are allowed: 4326, 3031, and 3995, besides the original. It is
#' possible to fix the range of the color scale (for the oceanographic
#' variable) to a custom range. For example, one can fix the color scale
#' to the total range of the ocean (instead of the current depth slice).
#'
#' @param NOAA Dataset of the NOAA World Ocean Atlas
#'  (with [get_NOAA()]).
#' @param points Add locations of extracted point geometry ([`sf`][sf::st_sf()]
#'  object).
#' @param epsg The epsg used to project the data (currently supported `4326`,
#'  `3031`and `3995`).
#' @param expedition the ASPIRE campaign expedition (must be '"1805"', '"1810"','"1812"', '"1903l1"','"1905l1"', '"1906"','"2101"','"2102"','"2107"')
#'
#' @return [ggplot2::ggplot()]
#'
#' @export
#'
#' @examples
#' if (curl::has_internet() && interactive()) {
#'
#' # data
#' NOAA <- get_NOAA("oxygen", 1, "annual")
#'
#' # plot
#' plot_NOAA(NOAA)
#'
#' # coordinates
#' pts <- filter_NOAA(NOAA, 1, list(lon = c(-160, -120), lat =  c(11,12)))
#'
#' # plot
#' plot_NOAA(NOAA, points = pts)
#'
#' }

plot_NOAA_WOA <- function(NOAA, expedition, cast, epsg = NULL) {

  library(ggplot2)

  # Create a dataset of the expedition, cast, variable of interest and values

  CTD <- NOAA.Explore.QAQC::ASPIRE_SHIP_CTD_data |>
    dplyr::select(expedition, cast, deployment_longitude, deployment_latitude, depth, temperature) |> # Later turn temperature to a variable
    dplyr::filter(expedition == i) |>
    dplyr::filter(cast == j)

  # Create variables to add to the WOA dataset

  cast <- unique(CTD$cast)
  expedition <- unique(CTD$expedition)

  # Create a dataframe of just the points of interest for a
  xy <- NOAA.Explore.QAQC::ASPIRE_SHIP_CTD_data |>
    dplyr::select(expedition, cast, deployment_longitude, deployment_latitude) |>
    dplyr::filter(expedition == i) |>
    dplyr::filter(cast == j) |>
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

  WOA_dat <- as.data.frame(stars::st_extract(NOAA, sf::st_as_sf(xy))) |>
    # Add in meta data
    dplyr::mutate(cast = cast,
                  expedition = expedition,
                  #depth_WOA = depth,
                  WOA_lat = xy@bbox[2],
                  WOA_lon = xy@bbox[1]) |>
    # filter out depths with no values
    dplyr::filter(!is.na(t_an)) |>
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

