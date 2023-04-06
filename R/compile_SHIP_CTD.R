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

  depth <- ""
  temperature <- ""
  conductivity <- ""
  salinity <- ""
  density <- ""
  oxygen <- ""
  oxygen2 <- ""
  oxygen3 <- ""
  upoly <- ""
  soundSpeed <- ""
  pressure <- ""
  flag <- ""
  #campaign <- ""
  expedition <- ""
  platform <- ""
  station <- ""
  cast <- ""
  deployment_longitude <- ""
  deployment_latitude <- ""
  max_depth <- ""

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
                              pressure = numeric(),
                              flag = numeric(),
                              #campaign = character(),
                              expedition = character(),
                              platform = character(),
                              station = character(),
                              cast = character(),
                              deployment_longitude = numeric(),
                              deployment_latitude = numeric(),
                              max_depth = numeric()  )


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
                      max_depth = max(depth)) %>%
        dplyr::select(-flag, -filename, -expedition_num)

      dat2 <- dplyr::bind_rows(dat2, tmp2)

    }

  # Compile the CTD data from each expedition into a single dataframe

    SHIP_CTD_data <- dplyr::bind_rows(SHIP_CTD_data, dat2) %>%
      dplyr::select(platform, expedition, station, cast, deployment_latitude, deployment_longitude, max_depth, depth,
                    temperature, conductivity, salinity, density, oxygen, oxygen2, oxygen3, upoly, soundSpeed,
                    pressure)

  }
 return(SHIP_CTD_data)

}

