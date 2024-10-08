% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_NOAA_WOA.R
\name{extract_NOAA_WOA}
\alias{extract_NOAA_WOA}
\title{Extracts the NOAA World Ocean data from a given location based on an Okeanos Explorer CTD cast}
\usage{
extract_NOAA_WOA(NOAA, ex, ctd_cast, epsg = NULL)
}
\arguments{
\item{NOAA}{Dataset of the NOAA World Ocean Atlas
(with \code{\link[=get_NOAA_WOA]{get_NOAA_WOA()}}).}

\item{epsg}{The epsg used to project the data (currently supported \code{4326},
\code{3031}and \code{3995}).}

\item{expedition}{the ASPIRE campaign expedition (must be '"1805"', '"1810"','"1812"', '"1903l1"','"1905l1"', '"1906"','"2101"','"2102"','"2107"')}

\item{cast}{the CTD cast number for a given expedition. Must be in the format '"ctd001, ctd002"', etc.}
}
\value{
a dataframe of the WOA data for the specified metric, temporal resolution (determined from \code{\link[=get_NOAA_WOA]{get_NOAA_WOA()}}), and location
}
\description{
Adapted from the function "plot_NOAA" from the oceanexplorer package by Martin Schobben
}
\details{
Calls: url_parser.R, read_NOAA.R

Called by: NOAA Explore QAQC_CTD Rosette.Rmd

NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
Last update: Oct. 2023
}
\examples{

# retrieve NOAA data
NOAA <- get_NOAA_WOA_data("oxygen", 1, "annual", T)

# extract WOA data for a single location from starts object, NOAA
WOA_dat <- extract_NOAA_WOA(NOAA,
                        expedition = "EX2107",
                        cast = "ctd001",
                        epsg = NULL) # Will inherit from NOAA stars object

ggplot() +
 geom_path(data =ctd_rosette_data, aes(x = temperature, y = depth),color = "black", linewidth = 1.5) +
 geom_path(data =WOA_dat, aes(x = t_an, y = depth), color = "red", linewidth = 1.5) +
 labs(x = "Temperature (°C)", y = "Depth (m)") +
 scale_y_reverse() +
 scale_x_continuous(position = "top",
                   breaks = c(5, 10, 15, 20, 25, 30)) +
 theme_bw()


}
