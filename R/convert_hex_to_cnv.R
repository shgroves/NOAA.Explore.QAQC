# Function to read .hex CTD rosette files, convert them to .cnv files and export .cnv file with the same name as the .hex file

# NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
# Last update: Mar 2023


#' Convert .hex to .cnv
#' @export
#' @description
#' Converts .hex files to .cnv files
#' @param path A string indicating the filepath to the .hex files
#' @returns This function returns a .cnv file to the specified path
#' @details
#' NOAA Ocean Exploration collects oceanographic data and water samples by request during mapping and ROV
#' expeditions on *Okeanos Explorer* with a CTD rosette system. The CTD rosette contains conductivity,
#' temperature, depth, oxygen, salinity, oxidation reduction potential, turbidity, and fluorometer sensors.
#' CTD rosette data are archived as .hex and .cnv files, which are produced using Seabird software.
#' @seealso \code{\link{NOAA.ASPIRE::getROVCTD, NOAA.ASPIRE::getCTDRosette}}
#' @examples
#' # You must specify at least one path
#' path1 <- "C:/Users/sarah.groves/Documents/Data/CTD/ASPIRE_EX1905L1/SHIPCTD"
#' path2 <- "C:/Users/sarah.groves/Documents/Data/CTD/ASPIRE_EX1906/SHIPCTD"


convert_hex_to_cnv <- function(path1, path2 = "NULL", path3 = "NULL", path4 = "NULL") {

  library(data.table)

  # Set path to folders containing .hex files
  hex_folder_path <- c(path1, path2, path3, path4)

  # Loop through folders and convert .hex files to .cnv files
  for (folder in hex_folder_path) {

    # Get list of .hex files in current folder
    hex_files <- list.files(path = folder, pattern = "\\.hex$", full.names = TRUE)

    # Loop through .hex files in current folder
    for (hex_file in hex_files) {

      # Read in data from .hex file
      data_hex <- fread(hex_file)

      # Set path and file name for output .cnv file
      cnv_file <- paste0(tools::file_path_sans_ext(hex_file), ".cnv")

      # Write data to .cnv file with same file name
      fwrite(data_hex, cnv_file)

    }
  }
}
