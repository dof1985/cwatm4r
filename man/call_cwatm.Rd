% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelSetupRun.R
\name{call_cwatm}
\alias{call_cwatm}
\title{Call a CWatM simulation from within R}
\usage{
call_cwatm(modelpath = NULL, settingsfile, overwrite = FALSE, ...)
}
\arguments{
\item{modelpath}{character. defines the path to the folder with CWatM (see details)}

\item{settingsfile}{character. defines the path to  an MS Excel settings spreadsheet or to a \code{settings.ini} file (see details)}

\item{overwrite}{logical. Currently out of use}

\item{...}{additional arguments passed to the \code{system()} function}
}
\value{
CWatM outputs
}
\description{
This function calls a CWatM simulation from within R.
}
\details{
The function uses the  \code{system()} function to call a CWatM simulation.
In case the function receives a settings file as an MS Excel file (.xlsx),
the function calls \code{createSettingsFile()},
and saves the output in the same folder with a combination of file name and a unique date-time identifier (E.g., \code{cwatm_settings_20212904102753.ini}).
The function automatically uses this \code{settings.ini} file for the call.
The \code{modelpath} argument indicates where are the CWatM files located (e.g., run_cwatm.py). If set to \code{NULL} the function assumes that they are located in
the parent directory of the PathRoot folder.
}
\examples{
\dontrun{
call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", intern = TRUE) # for `intern` see `?system`
call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings_sorekBasin.ini")
}
}
