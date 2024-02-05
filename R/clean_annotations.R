# Function to clean SeaTube Annotations from the ASPIRE campaign

# Called by: NOAA Explore QAQC_Annotations.Rmd

# NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
# Last update: Dec 2023


#' Clean and compile ROV annotation files
#' @export
#' @description
#' Cleans ROV annotations data from the ASPIRE campaign
#' @returns a dataframe
#' @details
#' Cool blurb about NOAA Exploration
#' @seealso \code{\link{https://github.com/shgroves/NOAA.ASPIRE}}
#' @examples
#'
#'ASPIRE_dat <- clean_annotations()
#'

clean_annotations <- function(){
  # clean up the rov main df a bit (select certain columns and rename)
  dat <- read.csv("C:/Users/sarah.groves/Documents/Data/Seatube/SeaTubeAnnotations_02_05_2024.csv")

  all_rov <- dat |>
    dplyr::select(`Cruise.Name`, `Dive.ID`, `Dive.Name`, `SeaTube.Link`, `Start.Date`, `End.Date`,
                  `Annotation.ID`, `Annotation.Source`, `Modified.Date`, `Creator.First.Name`, `Creator.Last.Name`,
                  `Modifier.First.Name`, `Modifier.Last.Name`, Comment, `To.Be.Reviewed`,
                  `Geoform`, `Geoform.Origin`, `Geoform.Type`,
                  `Taxon.Path`, `Taxonomy`, `Taxon`,`Kingdom`, `Phylum`, `Subphylum`, `Superfamily`, `Family`, `Subfamily`, `Gigaclass`,
                  `Superclass`, `Class`, `Subclass`, `Superorder`, `Order`,`Infraorder`, `Suborder`, `Genus`, `Species`, `Total...Reviews`, `X..Positive.Reviews`,
                  20:23,26:27,34:43) |>

    # Clean out column names in dataframe, conveniently, if you do this for 1 it gets rid of all
    dplyr::filter(Cruise.Name != "Cruise Name") |>
    # Rename columns
    dplyr::mutate(ROV1_tempC = as.numeric(`TEMPPROBEDEEPDISCOVERERROV_23974_Temperature..C.`),
                  ROV1_tempC_time = as.numeric(`TEMPPROBEDEEPDISCOVERERROV_23974_Temperature..C..Time`),
                  ROV1_tempC_SBE = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Temperature..C.`),
                  ROV1_tempC_SBE_time = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Temperature..C..Time`),
                  ROV1_lat_dd = as.numeric(`DEEPDISCOVERERNAV01_23975_Latitude..deg.`),
                  ROV1_lon_dd = as.numeric(`DEEPDISCOVERERNAV01_23975_Longitude..deg.`),
                  ROV1_O2_mgL = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration..mg.l.`),
                  ROV1_O2_mgL_time = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Oxygen.Concentration..mg.l..Time`),
                  ROV1_S_psu = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity..psu.`),
                  ROV1_S_psu_time = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Practical.Salinity..psu..Time`),
                  ROV1_t_FTU = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Turbidity..FTU.`),
                  ROV1_t_FTU_time = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Turbidity..FTU..Time`),
                  ROV1_depth_m = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Depth..m.`),
                  ROV1_depth_m_time = as.numeric(`SBECTD9PLUSDEEPDISCOVERER_23978_Depth..m..Time`),
                  positive_reviews = as.numeric(X..Positive.Reviews),
                  total_reviews = as.numeric(Total...Reviews) )|>

    # Clean up and modify other columns
    # Pull out the cruise #
    dplyr::mutate(cruise_number = stringr::str_extract(Cruise.Name, paste0("(?<=", "EX", ").{", 4, "}")),
                  cruise = paste0("EX", cruise_number))|>

    # Pull out the dive #
    dplyr::mutate(Dive.Name = toupper(Dive.Name),
                  dive_number = stringr::str_extract(Dive.Name, paste0("(?<=", "DIVE", ").{", 2, "}")) )|>

    # Fix missing cruise / dive info
    dplyr::mutate(dive_number = dplyr::case_when(Dive.ID == "1193" ~ "test",
                                                 Dive.ID == "1253" ~ "test",
                                                 Dive.ID == "5750" ~ "test",
                                                 Dive.ID == "6020" ~ "test",
                                                 T ~ as.character(dive_number))) |>

    # Combine creator and modifier into single columns
    dplyr::mutate(creator = tolower(paste(Creator.First.Name, Creator.Last.Name, sep = "_")),
                  modifier = tolower(paste(Modifier.First.Name, Modifier.Last.Name, sep = "_"))) |>

    # Pull out the date and time
    dplyr::mutate(start_date_time_UTC = lubridate::ymd_hms(Start.Date),
                  end_date_time_UTC = lubridate::ymd_hms(End.Date),
                  modified_date_UTC = lubridate::ymd_hms(Modified.Date),
                  ROV1_tempC_time = lubridate::ymd_hms(ROV1_tempC_time),
                  ROV1_tempC_SBE_time = lubridate::ymd_hms(ROV1_tempC_SBE_time),
                  ROV1_O2_mgL_time = lubridate::ymd_hms(ROV1_O2_mgL_time),
                  ROV1_S_psu_time = lubridate::ymd_hms(ROV1_S_psu_time),
                  ROV1_t_FTU_time = lubridate::ymd_hms(ROV1_t_FTU_time),
                  ROV1_depth_m_time = lubridate::ymd_hms(ROV1_depth_m_time)) |>

    # Get rid of junk
    dplyr::select(-c(Creator.First.Name, Creator.Last.Name, Modifier.First.Name, Modifier.Last.Name, cruise_number, Dive.Name, Start.Date, End.Date,
                     Modified.Date))

  # Clean up taxonomy categories - for many of the dives in EX1806, the annotations ended up in the Comments column
  all_rov2 <- all_rov |>
    dplyr::mutate(annotation_type = dplyr::case_when(
      Taxonomy == "WoRDSS" ~ "biological",
      Taxonomy == "WoRMS" ~ "biological",
      Taxonomy == "Simplified CMECS" ~ "substrate",
      Taxonomy == "CMECS" ~ "substrate",
      Taxonomy == "Video Quality" ~ "video_quality",
      Taxonomy == "OER Education " ~ "education",
      Taxonomy == "Education" ~ "education",
      Taxonomy == "Underwater Cultural Heritage" ~ "UHC_restricted",
      Taxonomy == "OER Archaeology" ~ "UHC_restricted",

      # This is a list of the biological terms I found in the 1806 Comments column
      grepl("Cnidaria|Chordata|Sargassum|Actiniaria|Annelida|Argyropelecus|Arthropoda|Bryozoa|Bryozoan|Bryozoans|Chaceon|Cottunculus|Crinoidea|Ctenophora|Hydrozoa|Octocorallia|Tube worms|
            Echinodermata|Ophiuroidea|Filefish|Hagfish|hatchetfish|Lanternfish|Lophelia|MOLLUSCA|Nemertean|Octocorallia|Invertebrate|Vertebrate|Porifera|Sabellid|Corallimorph|
            Chloroscombrus|shrimp|Spoonworm|Stoloniferan|Stomiiform|Tubeworms|Tubularia|invert|Geodia|Tunicata|Bathypathes|Hexactinellida|Dercitus|Actiniaria|Nemertea|Gastropoda|
            Acanella|Chrysogorgia|Paragorgia|pentacrinoid|Foraminifera|jellyfish|whips|medusa|Medusozoa|roughy|lophila|sea star|chaetognaths|Demospongiae|Holothuroidea|Bryozon|
            Pyuridae|Stauropathes|Thouarella|Leiopathes|Eunicella|Duva florida|Pseudoanthomastus|Swiftia|Primnoidae|Chloroscombrus chrysurus|Echinoidea|Hyalinoecia|
            ctenophores", Comment, ignore.case = TRUE) ~ "biological",


      grepl("delete|deleted", Comment, ignore.case = TRUE) ~ "to_be_deleted",

      grepl("Bioturbation|Gouge|feeding|pits|burrow|predation|ink", Comment, ignore.case = TRUE) ~ "biological_behavior",

      grepl("test", Comment, ignore.case = TRUE) ~ "test_annotation",

      grepl("Torpedo|Ballast|Chain plate|Periscope", Comment, ignore.case = TRUE) ~ "UHC_restricted",

      grepl("Ridge|Primary|Unconsolidated|Flat|Slope|Sloping|Ripples|Fine Clay|Overhang|dunes|substrate|Vertical|rock|rocks|wall|sand|Rubble|carbonate", Comment, ignore.case = TRUE) ~ "substrate",

      #grepl("transect|Begin|End|Launch|Surface|Descending|Bottom|m|meters|Start|On left|ROV|seafloor|in water|out of water|stern", Comment, ignore.case = TRUE) ~ "rov_positioning",

      grepl("Bubbles|bubble", Comment, ignore.case = TRUE) ~ "gas_seep",

      TRUE ~ "other"
    ))

  #other <- dplyr::filter(all_rov2, annotation_type == "other")

 # Final name cleaning - remove all the "."
 names(all_rov2) <- gsub("\\.", "_", names(all_rov2))



all_rov_clean <- all_rov2 |>

  dplyr::select(Cruise_Name, cruise, dive_number, Dive_ID, SeaTube_Link, start_date_time_UTC, end_date_time_UTC,
                Annotation_ID, Annotation_Source, creator, modifier, modified_date_UTC,
                Comment, To_Be_Reviewed, positive_reviews, total_reviews,
                Geoform, Geoform_Origin, Geoform_Type, annotation_type,
                Taxon_Path, Taxonomy, Taxon, Kingdom, Phylum, Subphylum, Superfamily, Family, Subfamily, Gigaclass,
                Superclass, Class, Subclass, Superorder, Order, Infraorder, Suborder, Genus, Species,
                ROV1_tempC,
                ROV1_tempC_time,
                ROV1_tempC_SBE,
                ROV1_tempC_SBE_time,
                ROV1_lat_dd,
                ROV1_lon_dd,
                ROV1_O2_mgL,
                ROV1_O2_mgL_time,
                ROV1_S_psu_time,
                ROV1_S_psu,
                ROV1_t_FTU,
                ROV1_t_FTU_time,
                ROV1_depth_m,
                ROV1_depth_m_time)

# Calculate number of biological annotations

N_annotate <- all_rov_clean |>
  dplyr::filter(annotation_type == "biological") |>
  dplyr::group_by(cruise) |>
  dplyr::summarise(n_annotations = length(unique(Annotation_ID)))

# Replace the EX1806 annotations with the corrected ones

EX1806_clean <- clean_EX1806()

remove <- EX1806_clean$Annotation_ID

all_rov_clean <- all_rov_clean |>
  dplyr::filter(!Annotation_ID %in% remove)

all_rov_clean <- dplyr::bind_rows(all_rov_clean, EX1806_clean) |>
# Create depth bins
dplyr::mutate(an_depth_bin = dplyr::case_when(dplyr::between(ROV1_depth_m,
                                               0, 250) ~ "0-250",
                                       dplyr::between(ROV1_depth_m,
                                               250, 500) ~ "250-500",
                                       dplyr::between(ROV1_depth_m,
                                               500, 750) ~ "500-750",
                                       dplyr::between(ROV1_depth_m,
                                               750, 1000) ~ "750-1000",
                                       dplyr::between(ROV1_depth_m,
                                               1000, 1250) ~ "1000-1250",
                                       dplyr::between(ROV1_depth_m,
                                               1250, 1500) ~ "1250-1500",
                                       dplyr::between(ROV1_depth_m,
                                               1500, 1750) ~ "1500-1750",
                                       dplyr::between(ROV1_depth_m,
                                               1750, 2000) ~ "1750-2000",
                                       dplyr::between(ROV1_depth_m,
                                               2000, 2500) ~ "2000-2500",
                                       dplyr::between(ROV1_depth_m,
                                               2500, 3000) ~ "2500-3000",
                                       dplyr::between(ROV1_depth_m,
                                               3000, 4000) ~ "3000-4000",
                                       dplyr::between(ROV1_depth_m,
                                               4000, 5000) ~ "4000-5000",
                                       dplyr::between(ROV1_depth_m,
                                               5000, max(ROV1_depth_m, na.rm = T)) ~ ">5000",
                                       TRUE ~ "No Data"))
# ASPIRE_ROV_BIOPHYS_data <- all_rov_clean
# usethis::use_data(ASPIRE_ROV_BIOPHYS_data, overwrite = T)
return(all_rov_clean)

}
