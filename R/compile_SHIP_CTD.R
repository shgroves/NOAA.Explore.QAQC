# Function to compile all Okeanos Explorer CTD rosette data collected during a campaign.

# NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
# Last update: Apr 2023


#' Compile CTD Rosette
#' @export
#' @description
#' Compiles all CTD rosette data from cnv files into a single dataframe
#' @param path A string indicating the filepath to the .hex files
#' @param return_dataframe A logical value (TRUE or FALSE) indicating whether a dataframe should be returned.
#' @returns This function returns a dataframe of CTD casts
#' @details
#' NOAA Ocean Exploration collects oceanographic data and water samples by request during mapping and ROV
#' expeditions on *Okeanos Explorer* with a CTD rosette system. The CTD rosette contains conductivity,
#' temperature, depth, oxygen, salinity, oxidation reduction potential, turbidity, and fluorometer sensors.
#' CTD rosette data are archived as .hex and .cnv files, which are produced using Seabird software.
#' @seealso \code{\link{NOAA.ASPIRE::getROVCTD, NOAA.ASPIRE::getCTDRosette}}
#' @examples
#' compile_SHIP_CTD(path = "C:/Users/sarah.groves/Documents/Data/CTD/", campaign = "ASPIRE", return_dataframe = T)
#' @importFrom magrittr %>%
#' @importFrom oce read.ctd.sbe



compile_SHIP_CTD <- function(expeditions, path, return_dataframe = "NULL") {

  library(oce)
  library(magrittr)

  # Create an empty dataframe to populate with all CTD data

  # depth <- ""
  # temperature <- ""
  # conductivity <- ""
  # salinity <- ""
  # density <- ""
  # oxygen <- ""
  # oxygen2 <- ""
  # oxygen3 <- ""
  # upoly <- ""
  # soundSpeed <- ""
  # turbidity <- ""
  # fluorescence <- ""
  # nitrogen <- ""
  # pressure <- ""
  # flag <- ""
  # expedition <- ""
  # platform <- ""
  # station <- ""
  # cast <- ""
  # deployment_longitude <- ""
  # deployment_latitude <- ""
  # max_depth <- ""
  # year <- ""

  SHIP_CTD_data <- data.frame(depth = numeric(),
                              temperature = numeric(),
                              conductivity = numeric(),
                              salinity = numeric(),
                              density = numeric(),
                              oxygen = numeric(),
                              oxygen2 = numeric(),
                              oxygen3 = numeric(),
                              upoly = numeric(),
                              soundSpeed = numeric(),
                              turbidity = numeric(),
                              fluorescence = numeric(),
                              nitrogen = numeric(),
                              pressure = numeric(),
                              flag = numeric(),
                              expedition = character(),
                              platform = character(),
                              station = character(),
                              cast = character(),
                              deployment_longitude = numeric(),
                              deployment_latitude = numeric(),
                              max_depth = numeric(),
                              year = factor())

  for (i in expeditions) {

    # Load all the CTD .cnv files

    ## Set a "wd" based on the path provided
    wd <- paste0(path, "EX", i, "-profile-data/Profile_Data/SHIPCTD/")

    ## Make a list of all the names of the .cnv files in the wd
    temp = list.files(path=wd, pattern = "*cnv")

    ## Create an empty list
    file_data <- list()

    ## Populate that list with the cnv files as CTD data objects using the oce package
    for (j in 1:length(temp)) {
      file_data[[j]] <- oce::read.ctd.sbe(paste0(wd, temp[j]))
    }

    # Extract the actual CTD data

    ## Create an empty dataframe with the data file headings

    dat1 <- file_data[[1]]@data %>%
      as.data.frame() %>%
      dplyr::mutate(expedition = "",
                    station = "",
                    cast = "")

    dat2 <- dat1[0,]

    ## Loop through the list of CTD objects for a single expedition, extracting the data and key metadata, and compiling it all onto a single dataframe

    ### Pull out EX1805 separately bc it is missing so much metadata


    if(i == "1805") {

      for (h in 1:length(file_data)){
        tmp <- file_data[[h]]
        tmp2 <- tmp@data %>%
          as.data.frame() %>%
          dplyr::mutate(filename = temp[[1]],
                        expedition_num = i,
                        platform = "EX",
                        expedition = paste0(platform, expedition_num),
                        station = paste0("ctd00", h),
                        cast = station,
                        deployment_longitude = tmp@metadata$longitude,
                        deployment_latitude = tmp@metadata$latitude,
                        max_depth = max(depth),
                        date = tmp@metadata$date,
                        year =  format(date, format = "%Y"),
                        month = format(date, format = "%m")) %>%

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
                        turb_unit = ifelse(length(as.character(tmp@metadata$units$turbidity$unit)) > 0, as.character(tmp@metadata$units$turbidity$unit),
                                           "Sensor not used"),
                        fluor_unit = ifelse(length(as.character(tmp@metadata$units$fluorescence$unit)) > 0, as.character(tmp@metadata$units$fluorescence$unit),
                                            NA_character_),
                        upoly_unit = ifelse(is.na(upoly), "Sensor not used", "Units not found"),
                        nit_unit = ifelse(length(as.character(tmp@metadata$units$nitrogenSaturation)) > 0, as.character(tmp@metadata$units$nitrogenSaturation$unit),
                                            "Sensor not used")) %>%

          #Pull out all the sensors used to check for consistency across casts
          dplyr::mutate(depth_sensor = tmp@metadata$dataNamesOriginal$depth,
                        temp_sensor = tmp@metadata$dataNamesOriginal$temperature,
                        cond_sensor = tmp@metadata$dataNamesOriginal$conductivity,
                        sal_sensor = tmp@metadata$dataNamesOriginal$salinity,
                        den_sensor = tmp@metadata$dataNamesOriginal$density,
                        oxy_sensor = tmp@metadata$dataNamesOriginal$oxygen,
                        oxy2_sensor = tmp@metadata$dataNamesOriginal$oxygen2,
                        sS_sensor = tmp@metadata$dataNamesOriginal$soundSpeed,
                        turb_sensor = tmp@metadata$dataNamesOriginal$turbidity,
                        fluor_sensor = ifelse(is.na(fluor_unit), "Sensor not used", tmp@metadata$dataNamesOriginal$fluorescence),
                        upoly_sensor = tmp@metadata$dataNamesOriginal$upoly,
                        nit_sensor = tmp@metadata$dataNamesOriginal$nitrogenSaturation) %>%
          dplyr::select(-flag, -filename, -expedition_num)

        dat2 <- dplyr::bind_rows(dat2, tmp2)

      }

    }else{
      for (h in 1:length(file_data)){
        tmp <- file_data[[h]]
        tmp2 <- tmp@data %>%
          as.data.frame() %>%
          dplyr::mutate(filename = tmp@metadata$hexfilename,
                        expedition_num = i,
                        #expedition_num = substr(filename, 13, 16),
                        platform = tmp@metadata$ship,
                        expedition = paste0(platform, expedition_num),
                        station = tmp@metadata$station,
                        cast = stringr::str_extract(string =filename, pattern = "ctd[0-9]+"),
                        deployment_longitude = tmp@metadata$longitude,
                        deployment_latitude = tmp@metadata$latitude,
                        max_depth = max(depth),
                        date = tmp@metadata$date,
                        year =  format(date, format = "%Y"),
                        month = format(date, format = "%m")) %>%

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
                        turb_unit = ifelse(length(as.character(tmp@metadata$units$turbidity$unit)) > 0, as.character(tmp@metadata$units$turbidity$unit),
                                           "Sensor not used"),
                        fluor_unit = ifelse(length(as.character(tmp@metadata$units$fluorescence$unit)) > 0, as.character(tmp@metadata$units$fluorescence$unit),
                                            NA_character_),
                        upoly_unit = ifelse(is.na(upoly), "Sensor not used", ifelse(length(as.character(tmp@metadata$units$upoly$unit)) == 0,
                                                                                    "Units not found", as.character(tmp@metadata$units$upoly$unit))),
                        nit_unit = ifelse(length(as.character(tmp@metadata$units$nitrogenSaturation)) > 0, as.character(tmp@metadata$units$nitrogenSaturation$unit),
                                          "Sensor not used")) %>%
          #Pull out all the sensors used to check for consistency across casts
          dplyr::mutate(depth_sensor = tmp@metadata$dataNamesOriginal$depth,
                        temp_sensor = tmp@metadata$dataNamesOriginal$temperature,
                        cond_sensor = tmp@metadata$dataNamesOriginal$conductivity,
                        sal_sensor = tmp@metadata$dataNamesOriginal$salinity,
                        den_sensor = tmp@metadata$dataNamesOriginal$density,
                        oxy_sensor = tmp@metadata$dataNamesOriginal$oxygen,
                        oxy2_sensor = tmp@metadata$dataNamesOriginal$oxygen2,
                        sS_sensor = tmp@metadata$dataNamesOriginal$soundSpeed,
                        turb_sensor = tmp@metadata$dataNamesOriginal$turbidity,
                        fluor_sensor = ifelse(is.na(fluor_unit), "Sensor not used", tmp@metadata$dataNamesOriginal$fluorescence),
                        upoly_sensor = tmp@metadata$dataNamesOriginal$upoly,
                        nit_sensor = tmp@metadata$dataNamesOriginal$nitrogenSaturation) %>%

          dplyr::select(-flag, -filename, -expedition_num)

        dat2 <- dplyr::bind_rows(dat2, tmp2)

      }

    }

    # Compile the CTD data from each expedition into a single dataframe

    SHIP_CTD_data <- dplyr::bind_rows(SHIP_CTD_data, dat2) %>%
      dplyr::select(platform, year, date, month, expedition, station, cast, deployment_latitude, deployment_longitude, max_depth, depth, depth_sensor, depth_unit,
                    temperature, temp_sensor,  temp_unit, conductivity, cond_sensor, cond_unit,  salinity, sal_sensor, sal_unit, density, den_sensor, den_unit,
                    oxygen, oxy_sensor, oxy_unit, oxygen2, oxy2_sensor, oxy2_unit, soundSpeed, sS_sensor, sS_unit,
                    turbidity, turb_sensor, turb_unit, fluorescence, fluor_sensor, fluor_unit, upoly, upoly_sensor, upoly_unit, nitrogenSaturation, nit_sensor, nit_unit,  pressure)

  }
  return(SHIP_CTD_data)

}

