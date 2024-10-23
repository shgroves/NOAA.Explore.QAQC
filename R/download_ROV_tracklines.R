

download_ROV_tracklines <- function(expeditions, data, data_location, username = "NULL", password = "NULL", data_downloaded = "NULL") {



  # Section I - download profile data file for each cruise

  if(data_downloaded == FALSE ||
     data_downloaded == F ||
     data_downloaded == "NULL"){

    if(data_location == "DLP") {

      for(i in expeditions) {

        dat <- data |> dplyr::filter(expedition == i)

        dives <- unique(dat$Dive)

        for(j in dives) {

          # URL of the file to be downloaded

          url <- paste0("https://oer.hpc.msstate.edu/okeanos/", i, "/", i, "-DIVE0", j, "-ancillary-data.zip")

          # Destination directory to save the downloaded file
          dest_dir <- "C:/Users/sarah.groves/Documents/Data/ROV_tracks/"

          # Download the file using download.file()
          download.file(url, destfile = paste0(dest_dir, i, "-DIVE", j, "-ancillary-data.zip"), cacheOK = F, quiet = FALSE)

          #Create a new folder in the path provided (dest_dir) based on the expedition name

          dir.create(paste0(dest_dir, "EX", i, "-profile-data"))

          # Unzip the downloaded file
          unzip(paste0(dest_dir, "ex", i, "-DIVE", j, "-ancillary-data.zip"), exdir = paste0(dest_dir, "EX", i, "DIVE", j, "-ancillary-data.zip"))

          # Delete the downloaded .zip file
          file.remove(paste0(dest_dir, "ex", i, "-profile-data.zip"))

        }

      }

    }



    if(data_location == "GFOE") {

      for(i in expeditions) {

        dat <- data |> dplyr::filter(expedition == i)

        dives <- unique(dat$Dive)

        for(j in dives) {

          dat2 <- data |> dplyr::filter(Dive == j)

          # Pull out the year month and day bc GFOE put the date in the file path (yay!)
          year <- unique(lubridate::year(lubridate::mdy(dat2$ObservationDate)))
          month <- paste0("0", unique(lubridate::month(lubridate::mdy(dat2$ObservationDate))))
          day <- first(unique(lubridate::day(lubridate::mdy(dat2$ObservationDate))))

          if(day >= 10){
            day <- day
          }else{
            day <- paste0("0", day)
          }

          # URL of the file to be downloaded
          ## ROV Text file
          url <- paste0("https://", username,":", password, "@exdata.tgfoe.org/OkeanosCruises/", i, "/Products/ROV/", toupper(j), "_", year, month, day, "/", toupper(j), ".txt")
          ## Rov Track .csv
          url2 <- paste0("https://", username,":", password, "@exdata.tgfoe.org/OkeanosCruises/", i, "/Products/ROV/", toupper(j), "_", year, month, day, "/", toupper(j), "_RovTrack.csv")

          # Destination directory to save the downloaded file
          dest_dir <- "C:/Users/sarah.groves/Documents/Data/"

          #Create a new folder in the path provided (dest_dir) for ROV data
          dir.create(paste0(dest_dir, i, "/ROV_data"))

          # Download the file using download.file()
          download.file(url, destfile = paste0(dest_dir, i, "/ROV_data/", j, ".txt"), cacheOK = F, quiet = FALSE)
          download.file(url2, destfile = paste0(dest_dir, i, "/ROV_data/", j, "_RovTrack.csv"), cacheOK = F, quiet = FALSE)

        }

      }

    }

    if(is.null(data_location)) {print("Please specify a data location, either `DLP` for the Data Landing Pages or `GFOE` for the GFOE server.
                                      Note GFOE server requires collaboration tools account and username and password. For security reasons,
                                      delete your username and password when sharing this function online")}



  } # End data_download condition
} # End function











