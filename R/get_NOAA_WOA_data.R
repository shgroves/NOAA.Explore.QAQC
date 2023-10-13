#' Function to obtain NOAA World Ocean Atlas dataset
#'
#' Retrieves data from the NOAA World Ocean Atlas. Adapted from the function "get_NOAA" from the oceanexplorer package by Martin Schobben
#'
#' Functions to retrieve data from the
#' [NOAA World Ocean Atlas](https://www.ncei.noaa.gov/products/world-ocean-atlas)
#' . Data is an 3D array (longitude, latitude, and depth) and is loaded as a
#' [`stars`][stars::st_as_stars()] object. Check [`NOAA_data`] for available
#' variables, respective units and their citations. The function can automatically
#' cache the extracted files (default: `cache = FALSE`). The cached file will
#' then reside in the package's `extdata` directory.
#'
#' @seealso [Introduction to the stars package](https://r-spatial.github.io/stars/articles/stars1.html)
#'
#' @param var The chemical or physical variable of interest (possible choices:
#'  `"temperature"`, `"conductivity"`, `"oxygen"`, `"mixed layer depth"`,
#'  `"salinity"`, `"density"`).
#' @param spat_res Spatial resolution, for temperature and salinity, either 1 or .25 degree grid-cells (numeric)
#'  .
#' @param av_period Temporal resolution, either `"annual"`, specific seasons
#'  (e.g. `"winter"`), or month (e.g. `"August"`).
#' @param cache Caching the extracted NOAA file in the package's `extdata`
#'  directory (default = `FALSE`). Size of individual files is around 12 Mb. Use
#'  [list_NOAA()] to list cached data resources.
#'
#' @return [`stars`][stars::st_as_stars()] object or path.
#' @export
#'
#' @examples
#'
#' # path to NOAA server or local data source
#' url_parser("density", 1, "annual")
#'
#' if (curl::has_internet() && interactive()) {
#'
#' # retrieve NOAA data
#' get_NOAA_WOA_data("oxygen", 1, "annual")
#'
#' }

get_NOAA_WOA_data <- function(var, spat_res, av_period, cache = FALSE) {

  # Set categories for data type based on temporal availability at https://www.ncei.noaa.gov/products/world-ocean-atlas
  # For temp and salinity use WOA 2023 data; everything else uses WOA 2018 data

  #WOA23 <- c("salinity")
  WOA23 <- c("salinity", "temperature")
  #WOA18 <- c("conductivity", "oxygen", "mixed layer depth", "density", "temperature")
  WOA18 <- c("conductivity", "oxygen", "mixed layer depth", "density")

  # abbreviate variable - take the first letter for temp, salinity
  if (var == "density") {
    v <- "I"
  } else if  (var == "mixed layer depth") {
    v <- "M0"
  } else {
    v <- strsplit(var, "")[[1]][1]
  }

  # stat to extract
  if (spat_res < 5) {
    # standard set to objectively analyzed climatology for variable of interest
    stat <- paste(v, "an", sep = "_")
  } else {
    # standard set to Statistical mean for variable of interest
    stat <- paste(v, "mn", sep = "_")
  }

  # where is package
  pkg_path1 <- fs::path_package("NOAA.Explore.QAQC")
  pkg_path2 <- "C:/Users/sarah.groves/Documents/R_packages/NOAA.Explore.QAQC"
  # path
  NOAA_path <- url_parser(var, spat_res, av_period, cache = cache)

  if (!"external" %in% names(NOAA_path)) {
    # get data and make stars
    NOAA <- readRDS(fs::path(pkg_path1, NOAA_path$local)) |> stars::st_as_stars()
  } else {
    # get netcdf
    NOAA <- read_NOAA(NOAA_path$external, stat)

    if (isTRUE(cache)) {

      # write stars object if extracted from NOAA server
      # create dir
      fs::dir_create(pkg_path1, fs::path_dir(NOAA_path$local))
      # create file
      saveRDS(NOAA, fs::path(pkg_path1, NOAA_path$local))

      # create dir
      fs::dir_create(pkg_path2, fs::path_dir(NOAA_path$local))
      # create file
      saveRDS(NOAA, fs::path(pkg_path2, NOAA_path$local))
    }
  }

  # return object
  return(NOAA)
}
#' @rdname get_NOAA_WOA_data
#'
#' @export

url_parser <- function(var, spat_res, av_period, cache = FALSE) {

  #WOA23 <- c("salinity")
  WOA23 <- c("salinity", "temperature")
  #WOA18 <- c("conductivity", "oxygen", "mixed layer depth", "density", "temperature")
  WOA18 <- c("conductivity", "oxygen", "mixed layer depth", "density")

  # temporal resolution
  averaging_periods <- c("annual", month.name, "winter", "spring", "summer",
                         "autumn")

  stopifnot(av_period %in% averaging_periods)

  if (var %in% WOA23) {
    # base path to NCEI server
    base_path <- "https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA"
  } else{

    base_path <- "https://www.ncei.noaa.gov/thredds-ocean/dodsC/ncei/woa"
  }

  # grouped variables
  #grp1 <- c("oxygen", "density")
  grp1 <- c("oxygen", "density", "temperature")
  #grp2 <- c("salinity")
  grp2 <- c("salinity", "temperature")
  grp3 <- c("conductivity", "mixed layer depth")
  # see https://www.ncei.noaa.gov/data/oceans/woa/WOA18/DOC/woa18documentation.pdf
  # for metadata names

  # recording range
  if (var %in% grp1) {
    deca <- "decav"
  }
  if (var %in% grp2) {
    deca <- "decav91C0"
  }
  if (var %in% grp3) {
    deca <- "A5B7"
  }

  # abbreviate variable - take the first letter for temp, salinity
  if (var == "density") {
    v <- "I"
  } else if (var == "mixed layer depth") {
    v <- "M0"
  } else {
    v <- strsplit(var, "")[[1]][1]
  }

  # averaging period
  tp <- grep(av_period, averaging_periods, ignore.case = TRUE) - 1
  tp <- sprintf(fmt = "%02.0f", tp)
  # grid-cell size

  #gr <- if(spat_res > 1) "5d" else "01"

  if (var %in% WOA23) {
    gr <- if(spat_res == 1) "01" else "04"

    # complete file name
    file <- paste0(paste("woa23", deca, paste0(v, tp), gr, sep = "_"), ".nc")
    # complete file path
    file_path <- paste(var, "netcdf", deca, if(spat_res == 1) "1.00" else "0.25", file,
                       sep = "/")

  }

  if (var %in% WOA18) {
    gr <- if(spat_res == 1) "01" else "04"
    # complete file name
    file <- paste0(paste("woa18", deca, paste0(v, tp), gr, sep = "_"), ".nc")
    # complete file path
    file_path <- paste(var, deca, if(spat_res == 1) "1.00" else "0.25", file,
                       sep = "/")

  }

  if (isTRUE(cache)) {
    # where is package
    pkg_path1 <- fs::path_package("NOAA.Explore.QAQC") # C:/Users/sarah.groves/AppData/Local/Programs/R/R-4.2.2/library/NOAA.Explore.QAQC
    pkg_path2 <- "C:/Users/sarah.groves/Documents/R_packages/NOAA.Explore.QAQC"
    # create extdata if not already existing
    if (!fs::dir_exists(fs::path(pkg_path1, "extdata"))) {
      fs::dir_create(fs::path(pkg_path1, "extdata"))
    }

    if (!fs::dir_exists(fs::path(pkg_path2, "extdata"))) {
      fs::dir_create(fs::path(pkg_path2, "extdata"))
    }
  }

  # check whether exist locally (respecting conventions for paths of the OS)
  local_path <- fs::path("extdata", fs::path_ext_remove(file_path), ext = "rds")

  # if not exist make external path to server
  noaa_path <- try(fs::path_package("NOAA.Explore.QAQC", local_path), silent = TRUE)

  if (!inherits(noaa_path, "fs_path")) {
    external_path <- paste(base_path, file_path, sep = "/")
    pt <- list(external = external_path)
    # caching also add local_path
    if (isTRUE(cache)) {
      pt <- append(pt, list(local = local_path))
    }
    pt
  } else {
    list(local = local_path)
  }
} # END URL PARSER FUNCTION

# read the NOAA netcdf note conn = NOAA_path$external
read_NOAA <- function(conn, var) {

  # make connection
  nc <- RNetCDF::open.nc(external_path)

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





