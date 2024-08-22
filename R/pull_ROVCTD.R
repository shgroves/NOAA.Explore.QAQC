# Function to pull an ROV CTD file from the data landing page and clip the dive based on provied expedition, dive, and start and end times.

# Called by: NOAA Explore QAQC_Annotations.Rmd

# NOAA Exploration Science and Technology Data Analysis team: Groves
# Last update: June 2024


#' Pull ROV CTD data from data landing page based on specified parameters
#' @export
#' @description
#' pull an ROV CTD file from the data landing page and clip the dive based on provided expedition, dive, and start and end times.
#' @param expedition A list indicating the expeditions of interest (must be from the ASPIRE campaign where an ROV dive was conducted, do not include "EX" before the number; acceptable values are: "1806" "1811" "1903" "1904" "1905" "1907" "2103" "2104" "2107" "2201" "2205" "2206")
#' @param dive The dive of interest, must be in a two-digit format in " ", for example, "01", "02", "03", etc.
#' @param start_time The start time of the cast of interest, must be in the XXX format. Default value is the beginning of the descent. These can be pulled from the ROV Text file in the ancillary data download.
#' @param end_time The end time of the cast of interest, must be in the XXX format. Default is when the ROV hits bottom. These can be pulled from the ROV Text file in the ancillary data download.
#' @param path A file path to where you would like your file to be exported to.
#' @returns a data frame of an ROV cast
#' @details
#' Cool blurb about NOAA Exploration
#' @seealso \code{\link{https://github.com/shgroves/NOAA.ASPIRE}}
#' @examples
#'
#' @importFrom oce read.ctd.sbe


pull_ROVCTD <- function(expedition, dive, start_time, end_time, path) {

  #https://oer.hpc.msstate.edu/okeanos/ex1903l2/ex1903l2-DIVE02-CP-CTD.zip # use this to create the 1903 condition statements

  # Create some tests to make sure input info is correct

  expeditions<- c("1806", "1811", "1903", "1904", "1905", "1907", "2103", "2104", "2107", "2201", "2205", "2206")

  if (!expedition %in% expeditions) {
    stop(paste(expedition, "is not an acceptable input value for 'expedition', don't forget to remove the 'EX'"))
  }

  # URL of the file to be downloaded
  url <- paste0("https://oer.hpc.msstate.edu/okeanos/ex", expedition, "/ex", expedition, "-DIVE", dive, "-ROV-CTD.zip")

  # Destination directory to save the downloaded file
  dest_dir <- path

  # Download the file using download.file()
  download.file(url, destfile = paste0(dest_dir, "ex", expedition, "-profile-data.zip"), cacheOK = F, quiet = FALSE)

  #Create a new folder in the path provided (dest_dir) based on the expedition name

  dir.create(paste0(dest_dir, "EX", expedition, "-profile-data"))

  # Unzip the downloaded file
  unzip(paste0(dest_dir, "ex", expedition, "-profile-data.zip"), exdir = paste0(dest_dir, "EX", expedition, "-profile-data"))

  # Delete the downloaded .zip file
  file.remove(paste0(dest_dir, "ex", expedition, "-profile-data.zip"))

  # These data are provided in .hex format and need to be manually converted in the SeaBird software (LAME!!).

  # Maybe could be done in R, but not confident on output https://dewey.dunnington.ca/post/2020/hacking-seabird-hex-files-using-r-and-the-tidyverse/
  # See also: https://semba-blog.netlify.app/10/04/2018/processing-ctd-data-with-oce-package-in-r/

  # Now that you have converted your .hex files to .cnv files, you can create a dataframe from the .cnv files using the oce package.

    # Load the ROV CTD .cnv files

    ## Set a "wd" based on the path provided
    wd <- paste0(path, "EX", expedition, "-profile-data/")

    ## Get the name of the .cnv files based on a pattern bc the actual file name has a date in it which varies from file to file
    temp = list.files(path=wd, pattern = "*cnv")

    ## Import the cnv files as CTD data objects using the oce package
    file_data <- oce::read.ctd.sbe(paste0(wd, temp))

    # Extract the actual CTD data

        tmp <- file_data
        tmp2 <- tmp@data |>
          as.data.frame() |>
          dplyr::mutate(filename = tmp@metadata$hexfilename,
                        expedition_num = expedition,
                        expedition = paste0("EX", expedition),
                        platform = "Deep Discoverer",
                        station = tmp@metadata$station,
                        dive = dive,
                        deployment_longitude = tmp@metadata$longitude,
                        deployment_latitude = tmp@metadata$latitude,
                        max_depth = max(depth),
                        start_time = tmp@metadata$startTime,
                        date_time = lubridate::as_datetime(tmp@data$timeQ, origin='2000-01-01 UTC'),
                        year =  format(date_time, format = "%Y"),
                        month = format(date_time, format = "%m")) |>

          #Pull out all the units for each sensor to check for consistency across casts
          dplyr::mutate(depth_unit = ifelse(length(as.character(tmp@metadata$units$depth$unit)) > 0, as.character(tmp@metadata$units$depth$unit),
                                            "Sensor not used"),
                        temp_unit = ifelse(length(as.character(tmp@metadata$units$temperature$unit)) > 0, as.character(tmp@metadata$units$temperature$unit),
                                           "Sensor not used"),
                        cond_unit = ifelse(length(as.character(tmp@metadata$units$conductivity$unit)) > 0, as.character(tmp@metadata$units$conductivity$unit),
                                           "Sensor not used"),
                        sal_unit = ifelse(length(as.character(tmp@metadata$units$salinity$scale)) > 0, as.character(tmp@metadata$units$salinity$scale),
                                          "Sensor not used"),
                        den_unit = ifelse(length(as.character(tmp@metadata$units$density$unit)) > 0, as.character(tmp@metadata$units$density$unit),
                                          "Sensor not used"),
                        oxy_unit = ifelse(length(as.character(tmp@metadata$units$oxygen$unit)) > 0, as.character(tmp@metadata$units$oxygen$unit),
                                          "Sensor not used"),
                        oxy2_unit = ifelse(length(as.character(tmp@metadata$units$oxygen2$unit)) > 0, as.character(tmp@metadata$units$oxygen2$unit),
                                           "Sensor not used"),
                        sS_unit = ifelse(length(as.character(tmp@metadata$units$soundSpeed$unit)) > 0, as.character(tmp@metadata$units$soundSpeed$unit),
                                         "Sensor not used"),
                        #turb_unit = ifelse(length(as.character(tmp@metadata$units$turbidity$unit)) > 0, as.character(tmp@metadata$units$turbidity$unit),
                        #                   "Sensor not used"),
                        #fluor_unit = ifelse(length(as.character(tmp@metadata$units$fluorescence$unit)) > 0, as.character(tmp@metadata$units$fluorescence$unit),
                        #                    NA_character_),
                        upoly_unit = ifelse(is.na(upoly), "Sensor not used", ifelse(length(as.character(tmp@metadata$units$upoly$unit)) == 0,
                                                                                    "Units not found", as.character(tmp@metadata$units$upoly$unit))),
                        nit_unit = ifelse(length(as.character(tmp@metadata$units$nitrogenSaturation)) > 0, as.character(tmp@metadata$units$nitrogenSaturation$unit),
                                          "Sensor not used")) |>
          #Pull out all the sensors used to check for consistency across casts
          dplyr::mutate(depth_sensor = tmp@metadata$dataNamesOriginal$depth,
                        temp_sensor = tmp@metadata$dataNamesOriginal$temperature,
                        cond_sensor = tmp@metadata$dataNamesOriginal$conductivity,
                        sal_sensor = tmp@metadata$dataNamesOriginal$salinity,
                        den_sensor = tmp@metadata$dataNamesOriginal$density,
                        oxy_sensor = tmp@metadata$dataNamesOriginal$oxygen,
                        oxy2_sensor = tmp@metadata$dataNamesOriginal$oxygen2,
                        sS_sensor = tmp@metadata$dataNamesOriginal$soundSpeed,
                        #turb_sensor = "Sensor not used",
                        #fluor_sensor = ifelse(is.na(fluor_unit), "Sensor not used", tmp@metadata$dataNamesOriginal$fluorescence),
                        upoly_sensor = tmp@metadata$dataNamesOriginal$upoly,
                        nit_sensor = tmp@metadata$dataNamesOriginal$nitrogenSaturation) |>

          dplyr::select(-flag, -filename, -expedition_num)


    # Reorganize the data so it is pretty and in logical order.

    ROV_CTD_data <- tmp2 |>
      dplyr::select(expedition, dive, platform, start_time, date_time, month, year, deployment_latitude, deployment_longitude, max_depth, depth, depth_sensor, depth_unit,
                    temperature, temp_sensor,  temp_unit, conductivity, cond_sensor, cond_unit,  salinity, sal_sensor, sal_unit, density, den_sensor, den_unit,
                    oxygen, oxy_sensor, oxy_unit, oxygen2, oxy2_sensor, oxy2_unit, soundSpeed, sS_sensor, sS_unit,
                    upoly, upoly_sensor, upoly_unit, nitrogenSaturation, nit_sensor, nit_unit,  pressure)

    # Now we need to choppy chop the data into the time series of interest

    ROV_CTD_data_clipped <- tibbletime::as_tbl_time(ROV_CTD_data, index = date_time) |>
      tibbletime::filter_time(start_time ~ end_time)

    write.csv(ROV_CTD_data_clipped, paste0(path, "EX", expedition,"_", dive, "_ROVCTD_clipped.csv"), row.names = F)

  return(ROV_CTD_data_clipped)

}
