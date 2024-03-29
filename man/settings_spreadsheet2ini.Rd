% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion_functions.R
\name{settings_spreadsheet2ini}
\alias{settings_spreadsheet2ini}
\title{Converts a settings spreadsheet to a \code{settings.ini} file}
\usage{
settings_spreadsheet2ini(spreadsheet, output_path = "./cwatm_settings.ini")
}
\arguments{
\item{spreadsheet}{character vector,  the path to  an MS Excel default settings spreadsheet}

\item{output_path}{character vector,  the settings file's path and name}
}
\value{
a \code{settings.ini} file (see Details)
}
\description{
This function converts a default settings spreadsheet to a \code{settings.ini} file.
}
\details{
CWatM requires a \code{settings.ini} file to run. This function converts the user-friendly MS Excel   to a \code{settings.ini} file.
}
\examples{
\dontrun{
createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "./cwatm_settings_sorekBasin.ini")
createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "./cwatm_settings_sorekBasin")
}
}
