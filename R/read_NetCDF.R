#' Function to read netcdf file given a URL and attribute
#'
#' Adapted from the function "read_NOAA" from the oceanexplorer package by Martin Schobben
#'
#'
#' Called by: get_NOAA_WOA.R
#'
#' NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
#' Last update: Oct. 2023
#'
#'
#' Functions to retrieve data from a given url for a netCDF file.
#' Data are a 3D array (longitude, latitude, and depth) and is loaded as a
#' [`stars`][stars::st_as_stars()] object. Check [`https://data.nodc.noaa.gov/woa/WOA18/DOC/woa18documentation.pdf`] for available
#' variables, respective units and their citations. See url_parser function in get_NOAA_WOA_data.R for help with creating a URL from variables.
#'
#' @seealso [Introduction to the stars package](https://r-spatial.github.io/stars/articles/stars1.html), code{\link{https://github.com/shgroves/NOAA.ASPIRE}}
#'
#' @param conn a URL leading to the netCDF file
#' @param var a variable representing the attributes in the NOAA WOA netCDF files
#'
#' @return [`stars`][stars::st_as_stars()] object
#' @export
#'
#' @examples
#'
#'
#' if (curl::has_internet() && interactive()) {
#'
#' # retrieve netCDF and convert to stars object
#' read_netCDF("https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA/temperature/netcdf/decav91C0/1.00/woa23_decav91C0_t08_01.nc",
#'             "t_an")
#'
#' }


# read the NOAA netcdf
read_netCDF <- function(conn, var) {

  # make connection
  nc <- RNetCDF::open.nc(conn)
  #nc <- RNetCDF::open.nc("https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA/temperature/netcdf/decav91C0/1.00/woa23_decav91C0_t08_01.nc")
  # variable
  lat <- RNetCDF::var.get.nc(nc, "lat")
  lon <- RNetCDF::var.get.nc(nc, "lon")
  depth <- RNetCDF::var.get.nc(nc, "depth")
  lat_bnds <- RNetCDF::var.get.nc(nc, "lat_bnds")
  lon_bnds <- RNetCDF::var.get.nc(nc, "lon_bnds")
  depth_bnds <- RNetCDF::var.get.nc(nc, "depth_bnds")
  attr <- RNetCDF::var.get.nc(nc, var)


  # close connection
  RNetCDF::close.nc(nc)


  st <- stars::st_as_stars(attr) |>
    stars::st_set_dimensions(
      which = 1,
      offset = min(lon_bnds),
      delta = unique(diff(lon)),
      refsys = sf::st_crs(4326),
      names = "lon"
    ) |>
    stars::st_set_dimensions(
      which = 2,
      offset = min(lat_bnds),
      delta = unique(diff(lat)),
      refsys = sf::st_crs(4326),
      names = "lat"
    ) |>
    stars::st_set_dimensions(
      which = 3,
      values = depth_bnds[1, ],
      names = "depth"
    )

  # variable name
  names(st) <- var

  st


}
