library(robis)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)

setwd("~/NOAA/NOAA.Explore.QAQC/R")
expedition = "EX2306_Survey_Data_20240528.csv"
survey_df = read.csv(paste0("../../data/",expedition)) |>
  dplyr::filter(AphiaID != -999)

get_occurences <- check_aphiaid_near_location <- function(aphiaid) {
  # Fetch occurrence data for the given AphiaID
  occurrences <- occurrence(taxonid = aphiaid)
  return(occurrences = occurrences)
}

check_distances <- function(aphiaid, occurrences, samples_df, radius = 50, print_map = FALSE){
  subset_df = samples_df[samples_df$AphiaID == aphiaid, ]
  lattitudes = subset_df$Latitude
  longitudes = subset_df$Longitude
  subset_df[, "Distance"] = NA
  if (nrow(occurrences) != 0) {
    print(nrow(occurrences))
    for (dist in 1:length(lattitudes)){
      subset_df[dist,]$Distance = min(geosphere::distHaversine(
        matrix(c(longitudes[dist], latitudes[dist]), ncol = 2),
        matrix(c(occurrences$decimalLongitude, occurrences$decimalLatitude), ncol = 2)
      ) / 1000 )
    }
  }
  return(subset_df)
}
check_aphiaid_near_location <- function(occurences, subset_df, print_map = FALSE) {
  num_found = sum(subset_df$Distance <= radius)
  # print(subset_df$Genus[1], subset_df$Species[1])
  print(paste("Found", num_found, "samples within", radius, "km"))
  if (num_found > 0){
    found = TRUE
  } else {
    found = FALSE
  }
  if (print_map) {
    map_leaflet(occurrences)
  }
  return(found)
}

aphiaids <- survey_df$AphiaID
samples_df = survey_df %>% select(AphiaID, Genus, Species, Latitude, Longitude)
output_dir = "aphiaid/"
for (i in 1:length(aphiaids)){
  print(i)
  aphiaid = aphiaids[i]
  print(paste("Aphia ID:", aphiaid))
  occurrences <- get_occurences(aphiaid)
  subset_df <- check_distances(aphiaid, occurrences, samples_df, radius = 50)
  found <- check_aphiaid_near_location(occurrences, subset_df)
  if (found) {
    write.csv(occurrences, file = paste(output_dir, "aphiaid_",aphiaid, "_occurences.csv", sep=""))
    write.csv(subset_df, file = paste(output_dir, "aphiaid_",aphiaid, "_survey.csv", sep=""))
    # if (print_map) {
    #   map_leaflet(occurrences)
    # }
  }
}
