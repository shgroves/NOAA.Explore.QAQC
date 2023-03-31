# Function to read campaign data inventory Google sheet into R and compute summary metrics

# NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
# Last update: Mar 2023


#' Make campaign metrics
#' @export
#' @description
#' Computes campaign metrics from Google sheet
#' @param link A string indicating the the link to the Google sheet
#' @returns a dataframe with campaign metrics summary
#' @details
#' Cool blurb about NOAA Exploration
#' @seealso \code{\link{https://github.com/shgroves/NOAA.ASPIRE}}
#' @examples
#' # You must specify the link
#' link <- "https://docs.google.com/spreadsheets/d/19jRXe7SeDQRjv-guH2po5xTHqfzfeicDzZ74cb3OnNY/edit?pli=1#gid=0"
#' @importFrom magrittr %>%

make_campaign_metrics <- function(link) {
 library(magrittr)
  #Read google sheets data into R
  x <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/19jRXe7SeDQRjv-guH2po5xTHqfzfeicDzZ74cb3OnNY/edit?pli=1#gid=0',
                                 .name_repair = "universal")
  #x <- googlesheets4::read_sheet(link)



  # What metrics do we want to highlight for the campaign?
  #   *Area mapped (% Inside/Outside EEZ)
  #   *Dives completed (% Inside/Outside EEZ)
  #   *Significant communities?
  #      Look through summary factsheets for each dive (compile #),
  #      Relate our metrics to Vulnerable Marine Ecosystems metrics (VMEs)
  #   *N Cruises
  #   *N Samples (bio, geo, eDNA)
  #   *Hours of video on bottom
  #   *Scientist Participation
  #   *N annotations made pre vs. post dive (count after QAQC?)
  #      Broken up by region/habitat
  #   *Live video views
  #   *Student participants/Explorers-In-Training

  # Filter to just EX data

  datEX <- x %>%
    dplyr::filter(Vessel == "NOAAS Okeanos Explorer") %>%
    dplyr::group_by(Campaign, Vessel, Fiscal.Year, Mission.Type) %>%
    dplyr::summarise(Total_MB_linear_KM = sum(Total.MB.Linear.KM, na.rm = T),
                     Total_MB_sq_KM = sum(Total.MB.Square.KM, na.rm = T),
                     Total_Converted_MB_NM = sum(Converted.MB.Square.NM, na.rm = T),
                     Total_EEZ_MB_KM = sum(Total.MB.KM..USEEZ...200m, na.rm = T),
                     CTD_casts = sum(CTD.Casts, na.rm = T),
                     XBT_casts = sum(XBT.Casts, na.rm = T),
                     Completed_ROV_dives = sum(Dives..Successful..calculation., na.rm = T),
                     Total_EEZ_ROV = sum(Total.ROV.Dives.in.USEEZ, na.rm = T),
                     Total_Bio_samples = sum(Samples..Bio.Total..Calculated., na.rm = T),
                     Total_Geo_samples = sum(Samples..Geo.Total..Calculated., na.rm = T),
                     Total_water_samples = sum(Samples..Water, na.rm = T)) %>%
    dplyr::mutate(MB_completed_in_EEZ = (Total_EEZ_MB_KM/Total_MB_sq_KM)*100)

  datEX2 <- x %>%
    dplyr::filter(Vessel == "NOAAS Okeanos Explorer") %>%
    dplyr::group_by(Campaign, Vessel, Fiscal.Year) %>%
    dplyr::summarise(Total_MB_linear_KM = sum(Total.MB.Linear.KM, na.rm = T),
                     Total_MB_sq_KM = sum(Total.MB.Square.KM, na.rm = T),
                     Total_Converted_MB_NM = sum(Converted.MB.Square.NM, na.rm = T),
                     Total_EEZ_MB_KM = sum(Total.MB.KM..USEEZ...200m, na.rm = T),
                     CTD_casts = sum(CTD.Casts, na.rm = T),
                     XBT_casts = sum(XBT.Casts, na.rm = T),
                     Completed_ROV_dives = sum(Dives..Successful..calculation., na.rm = T),
                     Total_EEZ_ROV = sum(Total.ROV.Dives.in.USEEZ, na.rm = T),
                     Total_Bio_samples = sum(Samples..Bio.Total..Calculated., na.rm = T),
                     Total_Geo_samples = sum(Samples..Geo.Total..Calculated., na.rm = T),
                     Total_water_samples = sum(Samples..Water, na.rm = T)) %>%
    dplyr::mutate(MB_completed_in_EEZ = (Total_EEZ_MB_KM/Total_MB_sq_KM)*100,
                  Dives_in_EEZ = (Total_EEZ_ROV/Completed_ROV_dives)*100)

  datEX3 <- x %>%
    dplyr::filter(Vessel == "NOAAS Okeanos Explorer") %>%
    dplyr::group_by(Campaign, Vessel) %>%
    dplyr::summarise(Total_MB_linear_KM = sum(Total.MB.Linear.KM, na.rm = T),
                     Total_MB_sq_KM = sum(Total.MB.Square.KM, na.rm = T),
                     Total_Converted_MB_NM = sum(Converted.MB.Square.NM, na.rm = T),
                     Total_EEZ_MB_KM = sum(Total.MB.KM..USEEZ...200m, na.rm = T),
                     CTD_casts = sum(CTD.Casts, na.rm = T),
                     XBT_casts = sum(XBT.Casts, na.rm = T),
                     Completed_ROV_dives = sum(Dives..Successful..calculation., na.rm = T),
                     Total_EEZ_ROV = sum(Total.ROV.Dives.in.USEEZ, na.rm = T),
                     Total_Bio_samples = sum(Samples..Bio.Total..Calculated., na.rm = T),
                     Total_Geo_samples = sum(Samples..Geo.Total..Calculated., na.rm = T),
                     Total_water_samples = sum(Samples..Water, na.rm = T)) %>%
   dplyr::mutate(MB_completed_in_EEZ = (Total_EEZ_MB_KM/Total_MB_sq_KM)*100,
                 Dives_in_EEZ = (Total_EEZ_ROV/Completed_ROV_dives)*100)





}
