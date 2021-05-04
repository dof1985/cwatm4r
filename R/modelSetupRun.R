# Function to create an excel settings spreadsheet ####

#' Define CWatM settings & parameters
#'
#' @description
#' This function creates a default settings spreadsheet, granting the user with complete control over all model parameters.
#'
#' @details
#' The `pathRoot` variable indicates the root folder that stores CWatM files.
#' The output settings file path is: `pathRoot/cwatm_settings/cwatm_settings.xlsx`, and the user should make sure that path exists.
#'
#' @param pathRoot a character vector with the path to the CWatM root folder
#' @param overwrite logical. Should the function  overwrite existing files?
#' @return an MS Excel settings spreadsheet (see Details)
#' @examples
#' \dontrun{
#' defineParameters(pathRoot = "C:/IIASA/cwatm", overwrite = TRUE)
#' }
#' @export
defineParameters <- function(pathRoot, overwrite = FALSE) {

  # Error handling - check folder structure
  if(!dir.exists(paste0(gsub("/", "//", pathRoot), "//CWatM_settings"))) {
    stop(sprintf("The path `%s` does not exist. Create these directories to continue or set a different `pathRoot`\n", paste0(gsub("/", "//", pathRoot), "//CWatM_settings")))
  }

  # Error handling - check overwrite opt.
  if(!overwrite & file.exists(paste0(gsub("/", "//", pathRoot), "//CWatM_settings//cwatm_settings.xlsx"))) {
    stop("The file already exists. Use overwrite = TRUE to proceed\n")
  }

  settings[[3]][1, 2] <- pathRoot

  settings_wb <- openxlsx::createWorkbook(title = "cwatm4r-settingFilesSpreadsheet")

  for(n in names(settings)) {
    dim <- c(nrow(settings[[n]]), ncol(settings[[n]]))
    openxlsx::addWorksheet(wb = settings_wb,
                           sheetName = n)
    openxlsx::writeData(wb = settings_wb,
                        sheet = n,
                        x = settings[[n]])

    openxlsx::addStyle(wb = settings_wb, # styling of first column
                           sheet = n,
                           cols = 1,
                           rows = 1:(dim[1] + 1),
                           style = openxlsx::createStyle(fontColour = "white",
                                                          textDecoration = "bold",
                                                          bgFill = "black"))

    openxlsx::addStyle(wb = settings_wb, # styling of first row
                       sheet = n,
                       cols = 1:(dim[2] + 1),
                       rows = 1,
                       style = openxlsx::createStyle(fontColour = "white",
                                                     textDecoration = "bold",
                                                     bgFill = "black"))

    openxlsx::setColWidths(wb = settings_wb,
                           sheet = n,
                           cols = 1:(dim[2] + 1),
                           widths = "auto")
  }

  openxlsx::saveWorkbook(wb = settings_wb,
                         file = paste0(gsub("/", "//", pathRoot), "//CWatM_settings//cwatm_settings.xlsx"),
                         overwrite = overwrite)
}



# Function to create a settings.ini from an ms excel spreadsheet ####

#' Converts a settings spreadsheet to a `settings.ini` file
#'
#' @description
#' This function converts a default settings spreadsheet to a `settings.ini` file.
#'
#' @details
#' CWatM requires a `settings.ini` file to run. This function converts the user-friendly MS Excel   to a `settings.ini` file.
#' The output file is saved to the same folder as the spreadsheet.
#'
#' @param spreadsheet a character vector with the path to  an MS Excel default settings spreadsheet
#' @param output_name a character vector providing the settings file name (see Details)
#' @return a `settings.ini` file (see Details)
#'
#' @examples
#' \dontrun{
#' createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "cwatm_settings_sorekBasin.ini")
#' createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "cwatm_settings_sorekBasin")
#' }
#' @export
createSettingsFile <- function(spreadsheet, output_name = "cwatm_settings.ini") {

  # check and set .ini suffix for the output_(file)name
  if(getSuffix(output_name) != "ini") output_name <- paste0(output_name, ".ini")

  #check if spreadsheet names match the template name list; ISSUE A WARNING IF DOMAINS ARE MISSING
  nameList <- openxlsx::getSheetNames(spreadsheet)
  if(!all(getSettingsDomains() %in% nameList)) {
    warning(sprintf("The '%s' domain was not found in the spreadsheet. This may cause errors in the model run\n" , getSettingsDomains()[!getSettingsDomains() %in% nameList]))
  }

  settings_out <- unlist(lapply(nameList[-1], function(domain) {

    string_out <- sprintf("[%s]", domain)
    string_out <- c(string_out, apply(getSettingsTable(domain = domain), MARGIN = 1, FUN = buildSettingLine), "######")
    return(string_out)
  }))

  # write output to file
  path <- strsplit(spreadsheet, "/")[[1]]
  path[length(path)] <- output_name
  path <- paste0(path, collapse = "/")

  writeLines(settings_out, path)

}



# Function to modify a settings.ini/xlsx from the consule using a table #### TO DEVELOP ####

modifySettingsFile <- function(settings, changeset, output_name) {
  # outputnames should be by default input_name1.***
  # accept either a spreadsheet or a settings.ini # if Crops should be changes must be a spreadsheet
  # if spreadsheet, export via createSettingsFile
  # if .ini - should be modified and rewritten
  # save results
}



# Function to run cwatm from R ####

#' Call a CWatM simulation from within R
#'
#' @description
#' This function calls a CWatM simulation from within R.
#'
#' @details
#' The function uses the  `system()` function to call a CWatM simulation.
#' In case the function receives a settings file as an MS Excel file (.xlsx),
#' the function calls `createSettingsFile()`,
#' and saves the output in the same folder with a combination of file name and a unique date-time identifier (E.g., `cwatm_settings_20212904102753.ini`).
#' The function automatically uses this `settings.ini` file for the call.
#'
#' @param settingsfile a character vector with the path to  an MS Excel settings spreadsheet or to a `settings.ini` file (see Details)
#' @param overwrite logical. Currently out of use
#' @param ... additional arguments passed to the `system()` function
#' @return CWatM outputs
#' @examples
#' \dontrun{
#' call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", intern = TRUE) # for `intern` see `?system`
#' call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings_sorekBasin.ini")
#' }
#' @export
call_cwatm <- function(settingsfile,  overwrite = FALSE, ...) { # overwrite is not usable at the moment

  settingsFileType <- getSuffix(settingsfile)

  # error handling check all domains

  if(settingsFileType == "xlsx") nameList <- openxlsx::getSheetNames(settingsfile)
  if(settingsFileType == "ini")  nameList <- getSettingsDomains_ini(inipath = settingsfile, value = TRUE)

  if(!all(getSettingsDomains()[-1] %in% nameList)) {
    warning(sprintf("The '%s' domain was not found in the settings files. CWATM may not work properly\n" , getSettingsDomains()[!getSettingsDomains() %in% nameList]))
  }

  # extract file_path settings table xlsx/ini
  if(settingsFileType == "xlsx") filepaths <- openxlsx::read.xlsx(xlsxFile = settingsfile, sheet = "FILE_PATHS")
  if(settingsFileType == "ini")  filepaths <- getSettingsTable_ini(inipath = settingsfile, domain = "FILE_PATHS")

  pathout   <- filepaths$value[filepaths$variable %in% "PathOut"]
  modelpath <- filepaths$value[filepaths$variable %in% "PathRoot"]

  # error handling check output directory exists
  if(!dir.exists(pathout)) stop(sprintf("Model outputs' folder does not exist at %s\n", pathout))

  # if settingsFileType == xlsx -> create a settings.ini file
  if(settingsFileType == "xlsx") {
    output_name_unique <- sprintf("cwatm_settings_%s.ini", timeid())
    createSettingsFile(spreadsheet = settingsfile, output_name = output_name_unique)
    settingsfile <- strsplit(settingsfile, split = "/")[[1]]
    settingsfile[length(settingsfile)] <- output_name_unique
    settingsfile <- paste0(settingsfile, collapse = "/")

  }


  call <- sprintf("python %srun_cwatm.py %s -l", modelpath, settingsfile)
  system(call, ...)


}
# settingsfile = "d:/dropbox/iiasa/cwatm4r_test/cwatm_settings/cwatm_settings.xlsx"
# settingsfile = "d:/dropbox/iiasa/cwatm4r_test/cwatm_settings/myfirstini.ini"
