library(robis)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(geosphere)

setwd("~/NOAA/NOAA.Explore.QAQC/R")
expedition = "EX2306_Survey_Data_20240528.csv"
survey_df = read.csv(paste0("../../data/",expedition)) |>
  dplyr::filter(AphiaID != -999)

get_occurences <- check_aphiaid_near_location <- function(aphiaid) {
  # Fetch occurrence data for the given AphiaID
  occurrences <- occurrence(taxonid = aphiaid)
  return(occurrences = occurrences)
}

check_distances <- function(aphiaid, occurrences, samples_df, radius = 0.5, print_map = FALSE){
  subset_df = samples_df[samples_df$AphiaID == aphiaid, ]
  latitudes = subset_df$Latitude
  longitudes = subset_df$Longitude
  subset_df[, "Distance"] = NA
  if (nrow(occurrences) != 0) {
    print(nrow(occurrences))
    for (dist in 1:length(latitudes)){
      subset_df[dist,]$Distance = min(geosphere::distHaversine(
        matrix(c(longitudes[dist], latitudes[dist]), ncol = 2),
        matrix(c(occurrences$decimalLongitude, occurrences$decimalLatitude), ncol = 2)
      ) / 1000 )
    }
  }
  return(subset_df)
}

check_aphiaid_near_location <- function(occurences, subset_df, radius, output_dir, print_map = FALSE) {
  aphiaid = subset_df$AphiaID[1]
  num_found = sum(subset_df$Distance <= radius)
  # print(subset_df$Genus[1], subset_df$Species[1])
  print(paste("Found", num_found, "samples within", radius, "km"))
  if (num_found > 0){
    found = TRUE
    write(paste(cat(aphiaid[1]), cat(num_found[1])), paste0(output_dir, "found.csv"))
  } else {
    found = FALSE
    write(paste0(aphiaid[1], num_found[1]), file = paste0(output_dir, "checked.csv"))  }
  if (print_map) {
    map_leaflet(occurrences)
  }
  return(found)
}

aphiaids <- unique(survey_df$AphiaID)
samples_df = survey_df %>% select(AphiaID, Genus, Species, Latitude, Longitude)
output_dir = "~/NOAA/NOAA.Explore.QAQC/data/aphiaid/"
radius = 0.5

# Check if the output directory exists and create it if it does not
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
checked = c(read.table("~/NOAA/NOAA.Explore.QAQC/data/aphiaid/checked.csv", header = FALSE))$V1
for (i in 1:length(aphiaids)){
  aphiaid = aphiaids[i]
  # Define the file path
  file_path <- paste(output_dir, "aphiaid_", aphiaid, "_occurences.csv", sep="")
  # Check if the file exists
  if (file.exists(file_path) || aphiaid %in% checked) {
    next
  }
  checked = c(checked, aphiaid)
  print(i)
  print(paste("Aphia ID:", aphiaid))
  occurrences <- get_occurences(aphiaid)
  subset_df <- check_distances(aphiaid, occurrences, samples_df, radius = radius)
  found <- check_aphiaid_near_location(occurrences, subset_df, radius, output_dir)

  if (found) {
    write.csv(occurrences, file = paste(output_dir, "aphiaid_", aphiaid, "_occurences.csv", sep=""))
    write.csv(subset_df, file = paste(output_dir, "aphiaid_", aphiaid, "_survey.csv", sep=""))
  }
  else{
    write(paste(aphiaid), file = paste(output_dir, "checked.csv", sep=""), append=TRUE)
  }
}
