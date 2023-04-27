

download_SHIP_CTD_data <- function(expeditions, path, data_inventory, data_downloaded = "NULL") {

  x <- data_inventory


  # Section I - download profile data file for each cruise

  if(data_downloaded == FALSE ||
     data_downloaded == F ||
     data_downloaded == "NULL"){

    for(i in expeditions) {

      # URL of the file to be downloaded

      url <- paste0("https://oer.hpc.msstate.edu/okeanos/ex", i, "/ex", i, "-profile-data.zip")

      # Destination directory to save the downloaded file
      dest_dir <- "C:/Users/sarah.groves/Documents/Data/CTD_test/"

      # Download the file using download.file()
      download.file(url, destfile = paste0(dest_dir, "ex", i, "-profile-data.zip"), cacheOK = F, quiet = FALSE)

      #Create a new folder in the path provided (dest_dir) based on the expedition name

      dir.create(paste0(dest_dir, "EX", i, "-profile-data"))

      # Unzip the downloaded file
      unzip(paste0(dest_dir, "ex", i, "-profile-data.zip"), exdir = paste0(dest_dir, "EX", i, "-profile-data"))

      # Delete the downloaded .zip file
      file.remove(paste0(dest_dir, "ex", i, "-profile-data.zip"))

    }

  }
    # Section II - check file paths for each expedition file

    dest_dir <- path


    ## Create an empty dataframe
    Expedition <- ""
    Profile_Data <- ""
    SHIPCTD <- ""
    dat_check <- data.frame(Expedition=character(), Profile_Data = logical(), CTD= logical(), SHIPCTD= logical())


    # Populate the data frame
    for(i in expeditions) {
      p <- data.frame(Expedition)
      p$Expedition <- i

      # QAQC the folder structure for consistency - is the first sub-folder called "Profile_Data"?
      #print(paste("Is the first folder in the EX", i,  "directory? called 'Profile_Data'?"))
      tmp<- dir.exists(paste0(dest_dir, "EX", i, "-profile-data/Profile_Data"))
      #print(tmp)


      p$Profile_Data <- tmp

      if(p$Profile_Data == TRUE){

        # QAQC the folder structure for consistency - is there a subfolder called "SHIPCTD"?
        #print(paste("Is there a subfolder called 'SHIPCTD'in the EX", i, "'Profile_Data' folder?"))
        tmp<-dir.exists(paste0(dest_dir, "EX", i, "-profile-data/Profile_Data/SHIPCTD"))
        #print(tmp)

        p$SHIPCTD <- tmp

        if(p$SHIPCTD == TRUE){

          p$file_path <- paste0(dest_dir, "EX", i, "-profile-data/Profile_Data/SHIPCTD")

        }else{

          p$file_path <- "File path missing 'SHIPCTD' folder"

        }

        dat_check <- dplyr::bind_rows(dat_check, p)

      }

      if(p$Profile_Data == FALSE) {

        # QAQC the folder structure for consistency - is the first sub-folder called "CTD"?
        #print(paste("Is the first folder in the EX", i,  "directory? called 'CTD'?"))
        tmp<- dir.exists(paste0(dest_dir, "EX", i, "-profile-data/CTD"))
        #print(tmp)


        p$CTD <- tmp


        if(p$CTD == TRUE){

          # QAQC the folder structure for consistency - is there a subfolder called "SHIPCTD"?
          #print(paste("Is there a subfolder called 'SHIPCTD'in the EX", i, "CTD' folder?"))
          tmp<-dir.exists(paste0(dest_dir, "EX", i, "-profile-data/CTD/SHIPCTD"))
          #print(tmp)

          p$SHIPCTD <- tmp

          if(p$SHIPCTD == TRUE){

            p$file_path <- paste0(dest_dir, "EX", i, "-profile-data/CTD/SHIPCTD")

          }else{

            p$file_path <- "File path missing 'SHIPCTD' folder"

          }

        }

        dat_check <- dplyr::bind_rows(dat_check, p)

      }

    }





  # Reorder the columns in the dat_check df
  dat_check <- dat_check %>% dplyr::select(Expedition, Profile_Data, CTD, SHIPCTD, file_path)

  return(dat_check)

}










