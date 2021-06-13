# FUNCTIONS EXPORTED

# get settings domains  ####

#' Get all domains names from the default settings file
#'
#' @description
#' This function provides a charachter vector with all domains (i.e., sections) of the CWatM settings file.
#'
#' @return A character vector with all domains of the CWatM settings file.
#' @examples
#' getSettingsDomains()
#'
#' @export
getSettingsDomains <- function() {
  return(names(settings))
}



# get settings from a sepcific domain  ####

# get settings table  ####

#' Get all domains names from the default settings file
#'
#' @description
#' This function provides a table with  all variables and values for a given domain of the CWatM settings file.
#'
#' @details
#' The function extracts data from a user-defined settings file. In case a user does not define a settings file (`settingsfile = NULL`),
#' the results show a default settings file.
#'
#' @param settingsfile A character vector with a path to an MS Excel settings file or to a `settings.ini` file. Defaults to `NULL`
#' @param domain  A character vector with a settings file domain name
#' @return A `data.frame`  with all variables, values, and comments for a given domain (see Details).
#' @examples
#' getSettingsTable(settingsfile = NULL, domain = "FILE_PATHS")
#' dontrun {
#' getSettingsTable(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", domain = "OPTIONS")
#' getSettingsTable(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings_sorekBasin.ini", domain = "OPTIONS")
#' }
#'
#' @export
getSettingsTable <- function(settingsfile = NULL, domain) {
  if(is.null(settingsfile)) return(settings[[domain]])

  file_extension <- getSuffix(settingsfile)

  if(file_extension == "ini") {
    # parse ini
    getSettingsTable_ini(inipath = settingsfile, domain = domain)
  } else if (file_extension == "xlsx") {
    # parse xlsx
    openxlsx::read.xlsx(xlsxFile = settingsfile, sheet = domain)
  } else {
    stop(sprintf("Unknown file extension: %s", file_extension))
  }

}

#####




# FUNCTIONS NOT EXPORTED

# get settings from a specific domain of an ini ####
# DO NOT EXPORT
getSettingsTable_ini <- function(inipath, domain) {
  lines <- readLines(inipath)

  start <- grep(sprintf("^\\[%s\\]$", domain), lines)
  if(length(start) == 0) stop("The domain was not found")

  end   <- grep("^######$", lines)
  end   <- min(end[end > start])

  return(do.call("rbind", lapply(lines[(start + 1):(end - 1)], tableFromSettingLine)))
}

# create a line of the settings file ####
# DO NOT EXPORT

buildSettingLine <- function(line) {
  c(sprintf("# %s", line["comment"]),
  sprintf("%s = %s", line["variable"], line["value"]))
}

# create a one-line data.frame from a settings file line ####
# DO NOT EXPORT

tableFromSettingLine <- function(line) {

  variable <- gsub(" $", "", strsplit(line, split = "=")[[1]][1])
  value    <- gsub("^ ", "", strsplit(line, split = "=")[[1]][2])
  comment  <- gsub("^ ", "", strsplit(line, split = "#")[[1]][2])
  return(data.frame("variable" = variable,
                    "value" = value,
                    "comment" = comment,
                    stringsAsFactors = FALSE))

}

# get suffix of a filename/path ####
# DO NOT EXPORT

getSuffix <- function(s) {
  if(length(grep(".", s, fixed = TRUE)) == 0) return("No suffix")

  splt_s <- strsplit(s, split = ".", fixed = TRUE)[[1]]
  return(splt_s[length(splt_s)])
}

# create a unique time identifier ####
# DO NOT EXPORT
timeid <- function() {
 format(Sys.time(), format = "%Y%m%d%H%M%S")
}

# get settings domains from ini ####
# DO NOT EXPORT
getSettingsDomains_ini <- function(inipath, value = TRUE) {
  gsub("\\]", "", gsub("\\[", "",
                       grep("^\\[[A-Za-z_-]+\\]$|^\\[[A-Za-z_-]+ [A-Za-z_-]+\\]$", readLines(inipath), value = value)))
}


# validata raster input, if character load, if rasterlayer do noting ####
# DO NOT EXPORT
validateRaster <- function(r) {
  if(class(r) %in% "RasterLayer") return(r)
  if(class(r) %in% "character") {
    r <- raster::raster(r)
    return(r)
  } else {
    return(stop("fucntion expecting a 'RasterLayer' or a path to a GeoTiff"))
  }


}

# update values in a settings List object for a selected domain and columns. a replacement function ####
# DO NOT EXPORT

`updateSetting<-` <-  function(tbl, setting, col = "value", value) {
  tbl[tbl$variable %in% setting, col] <- value
  return(tbl)
}
# remove multiple patterns (strip patterns) from a character vector ####
strip <- function(strip, x, ...) {
  for(s in strip) {
    x <- gsub(s, "", x, ...)
  }
  return(x)
}

#####
