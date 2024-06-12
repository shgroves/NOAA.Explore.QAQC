

download_ROV_tracklines <- function(expeditions, dive, path, data_inventory, data_downloaded = "NULL") {



  # Section I - download profile data file for each cruise

  if(data_downloaded == FALSE ||
     data_downloaded == F ||
     data_downloaded == "NULL"){

    for(i in expeditions) {

      dat <- data_inventory |> dplyr::filter(cruise == i)

      dives <- unique(dat$dive)

      for(j in expeditions) {

      # URL of the file to be downloaded

      url <- paste0("https://oer.hpc.msstate.edu/okeanos/ex", i, "/ex", i, "-DIVE", j, "-ancillary-data.zip")

      # Destination directory to save the downloaded file
      dest_dir <- "C:/Users/sarah.groves/Documents/Data/ROV_tracks/"

      # Download the file using download.file()
      download.file(url, destfile = paste0(dest_dir, "ex", i, "-DIVE", j, "-ancillary-data.zip"), cacheOK = F, quiet = FALSE)

      #Create a new folder in the path provided (dest_dir) based on the expedition name

      dir.create(paste0(dest_dir, "EX", i, "-profile-data"))

      # Unzip the downloaded file
      unzip(paste0(dest_dir, "ex", i, "-DIVE", j, "-ancillary-data.zip"), exdir = paste0(dest_dir, "EX", i, "DIVE", j, "-ancillary-data.zip"))

      # Delete the downloaded .zip file
      file.remove(paste0(dest_dir, "ex", i, "-profile-data.zip"))

      }

    }

  }


}










