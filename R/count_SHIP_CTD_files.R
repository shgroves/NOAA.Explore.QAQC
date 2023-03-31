

count_SHIP_CTD_files <- function(expeditions, data_inventory, path){



  dat_check <- download_SHIP_CTD_data(expeditions = expeditions,
                                path = "C:/Users/sarah.groves/Documents/Data/CTD/",
                                data_inventory = data_inventory,
                                data_downloaded = T)


# Filter dat_check to just the expeditions with a findable SHIPCTD folder

dat <- dat_check %>% dplyr::filter(SHIPCTD == TRUE)

## Create an empty dataframe from dat_check

dat_check2 <- dat_check[, FALSE]

# Create a list of the cruises in dat

cruise <- dat$Expedition

# Create a loop to count .hex and .cnv files for the selected cruises

for(i in cruise){

  # Filter to single cruise

  tmp <- dat %>% dplyr::filter(cruise == i)

  # set the directory path
  dir_path <- tmp$file_path

  # set the file extension to count
  file_ext1 <- ".hex"

  file_ext2 <- ".cnv"

  # list all files in the directory
  files <- list.files(dir_path)

  # count the number of files with the specified extension
  tmp$n_files_hex <- length(grep(file_ext1, files, fixed = TRUE))
  num_files1 <- length(grep(file_ext1, files, fixed = TRUE))

  tmp$n_files_cnv <- length(grep(file_ext2, files, fixed = TRUE))
  num_files2 <- length(grep(file_ext2, files, fixed = TRUE))

  # print the number of files
  #cat("Number of", file_ext1, "files in", dir_path, "is", num_files1, "\n")
  #cat("Number of", file_ext2, "files in", dir_path, "is", num_files2, "\n")

  dat_check2 <- dplyr::bind_rows(dat_check2, tmp)

}

dat_check3 <- dplyr::left_join(dat_check, dat_check2) %>%
  dplyr::mutate(Expedition = paste0("EX", Expedition)) %>%
  dplyr::mutate(Expedition = dplyr::recode(Expedition,
                                           "EX1903l1" = "EX1903L1",
                                           "EX1905l1" = "EX1905L1"))

dat_check4 <- dplyr::left_join(dat_check3, data_inventory %>% dplyr::select(Cruise.ID, CTD.Casts), by=c("Expedition" = "Cruise.ID")) %>%
  dplyr::mutate(Pass_QC = dplyr::case_when(n_files_cnv == CTD.Casts ~ "Yes", TRUE ~"No"))

return(dat_check4)

}


