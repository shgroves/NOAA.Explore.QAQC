


library(magrittr)

# Set wd
#setwd("C:/Users/sarah.groves/Documents/Data/Seatube/search32510306/")

# temp = list.files(pattern="*.csv")
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))



# Read in and combine all annotation csv files
dat <-
  list.files(
    #path = path
    # ,
    pattern = "*.csv"
  ) %>%
  purrr::map_df(~read.csv(.))


# Create a df of all of the dives
dive_sites <- dat %>%
  dplyr::select(Dive.Name,
                # Multiple column headings are used for Lat and Lon, convert the NAs to zeros and then sum the 2 lat columns and the 2 lon columns
                DEEPDISCOVERERNAV01_23975_Latitude..deg.,
                DEEPDISCOVERERNAV01_23975_Longitude..deg.,
                DEEPDISCOVERERNAV01_23975_Latitude,
                DEEPDISCOVERERNAV01_23975_Longitude) %>%
  tidyr::replace_na(list(DEEPDISCOVERERNAV01_23975_Latitude..deg.= 0,
                         DEEPDISCOVERERNAV01_23975_Longitude..deg.= 0,
                         DEEPDISCOVERERNAV01_23975_Latitude = 0,
                         DEEPDISCOVERERNAV01_23975_Longitude = 0)) %>%
dplyr::mutate(Latitude = DEEPDISCOVERERNAV01_23975_Latitude..deg.+ DEEPDISCOVERERNAV01_23975_Latitude,
              Longitude = DEEPDISCOVERERNAV01_23975_Longitude..deg. + DEEPDISCOVERERNAV01_23975_Longitude) %>%
  # Remove the old lat and lon columns
  dplyr::select(-2, -3, -4, -5) %>%
  # Get rid of rows where lat and lon == 0
  dplyr::filter(Latitude != 0,
                Longitude != 0) %>%
  dplyr::group_by(Dive.Name) %>%
  dplyr::summarise(lat = mean(Latitude),
                   lon = mean(Longitude))


# Create a df of all of the dives where Chrysogorgiidae were observed

presence <- dat %>%
  dplyr::select(Dive.Name, Family) %>%
  dplyr::filter(Family == "Chrysogorgiidae") %>%
  unique() %>%
  #Create a presence column
  dplyr::mutate(presence = 1)

#Combine sites with presence and change NAs to 0
final_df <- presence %>%
  dplyr::full_join(dive_sites) %>%
  dplyr::mutate(Family = "Chrysogorgiidae",
                presence = tidyr::replace_na(presence, 0))
