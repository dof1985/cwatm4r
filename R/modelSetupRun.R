# Function to modify a settings.ini/xlsx from the consule using a table #### TO DEVELOP ####

modifySettingsFile <- function(settings, changeset, output_name) {}

  # outputnames should be by default input_name1.***
  # accept either a spreadsheet or a settings.ini # if Crops should be changes must be a spreadsheet
  # if spreadsheet, export via createSettingsFile
  # if .ini - should be modified and rewritten
  # save results


# Function to update settings list ####

#' Function to serach and automatically upadte values in a CWatM named settings list
#'
#' @description
#' This function search user-defined folders for CWatM data inputs and automatically updates a settings named `list`
#'
#' @details
#' This function assumes that the CWatM data folders' structure and file names follows the convention described in the models' documentation:
#' \url{https://cwatm.iiasa.ac.at/}. The user can provoke a call to  [cwatm4r::settings_setFilePaths()], if `character` FILE_PATHS are provided.
#' In order to search for a specific file, the function uses a set of pre-defined variable-specific search expressions.
#' When updated with a new file path; the followin comment ("Updated") is appended to the settings table. If the search results with multiple matchs, the first one is updated
#' and the following comment is appended to the settings table: "Multiple results were found; taken the first one".
#'
#' @param settingsList named `list` with CWatM settings
#' @param ... additional arguments (see details)
#' @return named `list`; a CWatM settings list.
#' @examples
#' \dontrun{
#' my_settings <- settings_ini2list(inipath = "./cwatm/settings_template.ini")
#'
#' # set user-defined paths
#' root = "d:/cwatm/model_data"
#' maps = "d:/cwatm/model_data/maps"
#' meteo = "./climate"
#'
#' my_settings <- settings_searchFiles(settingsList = my_settings,
#'                                      PathRoot = root,
#'                                      PathMaps = maps,
#'                                      PathMeteo = meteo)
#' }
#' @export
settings_searchFiles <- function(settingsList = NULL, ...) { # overwrite is not usable at the moment



  # SEARCH EXPRESSIONS #####

  settingsListExpr <- list("TOPOP" = list("Ldd" = c("[Ll]dd", "LDD", "flowd"),
                                          "ElevationStd" = c("[Ss]td", "[Ee]lv[Ss]td"),
                                                             "CellArea" = c("[Cc]ell[Aa]rea", "[Aa]rea")),
                           "METEO" = list("PrecipitationMaps" = c("pr"),
                                          "TavgMaps" = c("tavg", "temp"),
                                          "E0Maps" = c("EW"),
                                          "ETMaps" = c("ET")),
                           "EVAPORATION" = list("albedoMaps" = c("[Aa]lbedo"),
                                                "TminMaps" = c("tmin"),
                                                "TmaxMaps" = c("tmax"),
                                                "PSurfMaps" = c("ps", "pressure"),
                                                "RhsMaps" = c("hurs"),
                                                "WindMaps" = c("wind"),
                                                "RSDSMaps" = c("rsds", "RSDS"),
                                                "RSDLMaps" = c("rlds", "RLDS")),
                           "VEGETATION" = list("cropgroupnumber" = c("cropgrp", "cropgroup")),
                           "SOIL" = list("tanslope" = c("tanslope", "slope"),
                                         "slopeLength" = NULL, # why do we need it?
                                         "relativeElevation" = c("dz[Rr]el"),
                                         "KSat1" = c("[Kk]sat1", "[Kk]sat_1"),
                                         "KSat2" = c("[Kk]sat2", "[Kk]sat_2"),
                                         "KSat3" = c("[Kk]sat3", "[Kk]sat_3"),
                                         "alpha1" = c("[Aa]lpha1", "[Aa]lpha_1"),
                                         "alpha2" = c("[Aa]lpha2", "[Aa]lpha_2"),
                                         "alpha3" = c("[Aa]lpha3", "[Aa]lpha_3"),
                                         "lambda1" = c("[Ll]amda1", "[Ll]amda_1", "[Ll]ambda1", "[Ll]ambda_1"),
                                         "lambda2" = c("[Ll]amda2", "[Ll]amda_2", "[Ll]ambda2", "[Ll]ambda_2"),
                                         "lambda3" = c("[Ll]amda3", "[Ll]amda_3", "[Ll]ambda3", "[Ll]ambda_3"),
                                         "thetas1" = c("[Tt]hetas1", "[Tt]hetas_1", "[Tt]heta_s1", "[Tt]heta_s_1"),
                                         "thetas2" = c("[Tt]hetas2", "[Tt]hetas_2", "[Tt]heta_s2", "[Tt]heta_s_2"),
                                         "thetas3" = c("[Tt]hetas3", "[Tt]hetas_3", "[Tt]heta_s3", "[Tt]heta_s_3"),
                                         "thetar1" = c("[Tt]hetar1", "[Tt]hetar_1", "[Tt]heta_r1", "[Tt]heta_r_1"),
                                         "thetar2" = c("[Tt]hetar2", "[Tt]hetar_2", "[Tt]heta_r2", "[Tt]heta_r_2"),
                                         "thetar3" = c("[Tt]hetar3", "[Tt]hetar_3", "[Tt]heta_r3", "[Tt]heta_r_3"),
                                         "percolationImp" = c("[Pp]ercolation[Ii]mp", "[Pp]ercolation"),
                                         "StorDepth1" = c("[Ss]torage[Dd]epth1", "[Ss]torage[Dd]epth_1"),
                                         "StorDepth2" = c("[Ss]torage[Dd]epth2", "[Ss]torage[Dd]epth_2")),
                           "LANDCOVER" = list("fractionLandcover" = c("[Ff]raction[Ll]and[Cc]over", "[Ff]rac[Ll]and")),
                           "GROUNDWATER" = list("recessionCoeff" = c("[Rr]ecession[Cc]oeff"),
                                                "specificYield" = c("[Ss]pecific[Yy]ield"),
                                                "kSatAquifer" = c("[Kk][Ss]at[Aa]quifer")),
                           "WATERDEMAND" = list("allocSegments" = c("catchx"),
                                                "domesticWaterDemandFile" = c("[Dd]omestic[Ww]ater[Dd]emand"),
                                                "industryWaterDemandFile" = c("[Ii]ndustry[Ww]ater[Dd]emand",
                                                                              "[Ii]ndustrial[Ww]ater[Dd]emand"),
                                                "irrNonPaddy_efficiency" = c("[Ee]fficiency"),
                                                "irrPaddy_efficiency" = c("[Ee]fficiency"),
                                                "EnvironmentalFlowFile" = NULL,
                                                "averageDischarge" = NULL,
                                                "averageBaseFlow" = NULL),
                           "ROUTING" =  list("chanGrad" = c("slope", "[Cc]han[Gg]rad"),
                                             "chanMan" = c("[Mm]anning", "[Cc]han[Mm]an"),
                                             "chanLength" = c("[Cc]han[Ll]eng"),
                                             "chanWidth" = c("[Cc]han[Bb]w", "[Cc]han[Ww]idth"),
                                             "chanDepth" = c("[Cc]hanbnkf", "[Cc]han[Dd]epth")),
                           "LAKES_RESERVOIRS" = list("waterBodyID" = c("ID", "id"), # ignore small lakes
                                                     "waterBodyTyp" = c("[Tt]ype"),
                                                     "waterBodyDis" = c("[Dd]is"),
                                                     "waterBodyArea" = c("[Aa]rea"),
                                                     "waterBodyVolRes" = c("[Vv]ol"),
                                                     "waterBodyYear" = c("[Yy]ear")),
                           "INFLOW" = list("InflowPoints" = c("[Ii]n")), # ignore environmental flow
                           "__forest" = list( "forest_KSat1" = c("[Kk]sat1", "[Kk]sat_1"),
                                              "forest_KSat2" = c("[Kk]sat2", "[Kk]sat_2"),
                                              "forest_KSat3" = c("[Kk]sat3", "[Kk]sat_3"),
                                              "forest_alpha1" = c("[Aa]lpha1", "[Aa]lpha_1"),
                                              "forest_alpha2" = c("[Aa]lpha2", "[Aa]lpha_2"),
                                              "forest_alpha3" = c("[Aa]lpha3", "[Aa]lpha_3"),
                                              "forest_lambda1" = c("[Ll]amda1", "[Ll]amda_1", "[Ll]ambda1", "[Ll]ambda_1"),
                                              "forest_lambda2" = c("[Ll]amda2", "[Ll]amda_2", "[Ll]ambda2", "[Ll]ambda_2"),
                                              "forest_lambda3" = c("[Ll]amda3", "[Ll]amda_3", "[Ll]ambda3", "[Ll]ambda_3"),
                                              "forest_thetas1" = c("[Tt]hetas1", "[Tt]hetas_1", "[Tt]heta_s1", "[Tt]heta_s_1"),
                                              "forest_thetas2" = c("[Tt]hetas2", "[Tt]hetas_2", "[Tt]heta_s2", "[Tt]heta_s_2"),
                                              "forest_thetas3" = c("[Tt]hetas3", "[Tt]hetas_3", "[Tt]heta_s3", "[Tt]heta_s_3"),
                                              "forest_thetar1" = c("[Tt]hetar1", "[Tt]hetar_1", "[Tt]heta_r1", "[Tt]heta_r_1"),
                                              "forest_thetar2" = c("[Tt]hetar2", "[Tt]hetar_2", "[Tt]heta_r2", "[Tt]heta_r_2"),
                                              "forest_thetar3" = c("[Tt]hetar3", "[Tt]hetar_3", "[Tt]heta_r3", "[Tt]heta_r_3"),
                                              "forest_fracVegCover" = NULL,
                                              "forest_rootFraction1" = c("[Rr]oot[Ff]reaction1", "[Rr]oot[Ff]reaction_1"),
                                              "forest_rootFraction1" = c("[Rr]oot[Ff]reaction2", "[Rr]oot[Ff]reaction_2"),
                                              "forest_maxRootDepth" = c("max[Rr]oot[Dd]epth"),
                                              "forest_minSoilDepthFrac"= c("min[Ss]oil[Dd]epth"),
                                              "forest_cropCoefficientNC" = c("crop[Cc]oeff&_10[Dd]ay"),
                                              "forest_interceptCapNC" = c("intercept[Cc]ap&_10[Dd]ay")),
                           "__grassland" = list("grassland_fracVegCover" = NULL,
                                                "grassland_rootFraction1" = c("[Rr]oot[Ff]reaction1", "[Rr]oot[Ff]reaction_1"),
                                                "grassland_rootFraction1" = c("[Rr]oot[Ff]reaction2", "[Rr]oot[Ff]reaction_2"),
                                                "grassland_maxRootDepth" = c("max[Rr]oot[Dd]epth"),
                                                "grassland_minSoilDepthFrac"= c("min[Ss]oil[Dd]epth"),
                                                "grassland_cropCoefficientNC" = c("crop[Cc]oeff&_10[Dd]ay"),
                                                "grassland_interceptCapNC" = c("intercept[Cc]ap&_10[Dd]ay")),
                           "__irrPaddy" = list("irrPaddy_fracVegCover" = NULL,
                                               "irrPaddy_rootFraction1" = c("[Rr]oot[Ff]reaction1", "[Rr]oot[Ff]reaction_1"),
                                               "irrPaddy_rootFraction1" = c("[Rr]oot[Ff]reaction2", "[Rr]oot[Ff]reaction_2"),
                                               "irrPaddy_maxRootDepth" = c("max[Rr]oot[Dd]epth"),
                                               "irrPaddy_minSoilDepthFrac"= c("min[Ss]oil[Dd]epth"),
                                               "irrPaddy_cropCoefficientNC" = c("crop[Cc]oeff&_10[Dd]ay")),
                           "__irrNonPaddy" = list("irrNonPaddy_fracVegCover" = NULL,
                                                  "irrNonPaddy_rootFraction1" = c("[Rr]oot[Ff]reaction1", "[Rr]oot[Ff]reaction_1"),
                                                  "irrNonPaddy_rootFraction1" = c("[Rr]oot[Ff]reaction2", "[Rr]oot[Ff]reaction_2"),
                                                  "irrNonPaddy_maxRootDepth" = c("max[Rr]oot[Dd]epth"),
                                                  "irrNonPaddy_minSoilDepthFrac"= c("min[Ss]oil[Dd]epth"),
                                                  "irrNonPaddy_cropCoefficientNC" = c("crop[Cc]oeff&_10[Dd]ay")))

  # END SERACH EXPRESSIONS ####
  # load settings List/
  if(is.null(settingsList)) stop("The 'SettingsList' argument expects a named settings list object")
  settings <- settings_setFilePaths(settingsList = settingsList, ...)


  # check from here how the function handles paths - parse paths local vs. file_path
  #if(!is.null(cwatmInput_path)) updateSetting(settings$FILE_PATHS, col = c("value", "comment"), setting = "PathMaps")   <- c(cwatmInput_path, "Updated")
  #if(!is.null(climateInput_path)) updateSetting(settings$FILE_PATHS, col = c("value", "comment"), setting = "PathMeteo")  <- c(climateInput_path, "Updated")

  #settings <- settings_setFilePaths(settingsList = settingsList)

  for(domain in names(settingsListExpr)) {
  #  print(domain)
    for(var in settings[[domain]]$variable) {
   #   print(var)
        expr <- settingsListExpr[[domain]][[var]]
        if(is.null(expr)) next()
        expr <- paste0(expr,  collapse = "|")


        pth <- settings[[domain]][settings[[domain]]$variable %in% var, "value"]
        if(is.null(pth)) next()

        # parse pth

        splt_pth <- strsplit(pth, "/")[[1]]
        ref_pth <- strip(c("$(", " $(", ")", " ") , grep("$", splt_pth, fixed = TRUE, value = TRUE), fixed = TRUE)

        if(grepl(":", ref_pth)) {


          ref_pth <- strsplit(ref_pth, split = ":")[[1]]
          ref_pth <- settings[[ref_pth[1]]][settings[[ref_pth[1]]]$variable %in% ref_pth[2], "value"]
          if(grepl("$(PathRoot)", ref_pth, fixed = TRUE)) {
            # add PathRoot
            ref_pth <- paste0(settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathRoot", "value"], strip("$(PathRoot)", ref_pth, fixed = TRUE))
          }
        } else {
          # with no : E.G., CHECK FOR DOMAIN == SOIL; ALSO CHECK DOMAIN == METEO
          splt_pth[1] <- settings[[domain]][settings[[domain]]$variable %in% ref_pth, "value"]
          splt_pth <- c(strsplit(splt_pth[1], split = "/")[[1]], splt_pth[-1])

          ref_pth <- strip(c("$(", " $(", ")", " ") , grep("$", splt_pth, fixed = TRUE, value = TRUE), fixed = TRUE)
          ref_pth <- strsplit(ref_pth, split = ":")[[1]]
          ref_pth <- settings[[ref_pth[1]]][settings[[ref_pth[1]]]$variable %in% ref_pth[2], "value"]
          if(grepl("$(PathRoot)", ref_pth, fixed = TRUE)) {
            # add PathRoot
            ref_pth <- paste0(settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathRoot", "value"], strip("$(PathRoot)", ref_pth, fixed = TRUE))
          }
        }

        splt_pth[1] <- ref_pth

        #paste0(splt_pth[-length(splt_pth)], collapse = "/")
        file_mtch <- grep(expr, list.files(paste0(splt_pth[-length(splt_pth)], collapse = "/")), value = TRUE)
        if(length(file_mtch) == 0) next()

        #pth <- strsplit(pth, split = "/")[[1]]
        splt_pth[length(splt_pth)] <- file_mtch[1]
        splt_pth <- paste0(splt_pth, collapse =  "/")
        if(length(file_mtch) > 1) {
          # take first and comment
          cmnt <- settings[[domain]][settings[[domain]]$variable %in% var, "comment"]
          if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
          write_cmnt <- "Multiple results were found; taken the first one"
          if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

          updateSetting(settings[[domain]], col = c("value", "comment"), setting = var)   <- c(splt_pth, write_cmnt)
        } else {
          # take the only one and comment
          cmnt <- settings[[domain]][settings[[domain]]$variable %in% var, "comment"]
          if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
          write_cmnt <- "Auto-updated"
          if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

          updateSetting(settings[[domain]], col = c("value", "comment"), setting = var)   <- c(splt_pth, write_cmnt)
        }
      }
  }
  return(settings)
}

# RUN AND CHECK
#sl <- settings_ini2list(inipath = "d:/dropbox/iiasa/cwatm4r_test/settings_rhine30min.ini")
#sl <- settings_autoUpdate(settingsList = sl,
 #                   cwatmInput_path = "d:/dropbox/iiasa/cwatm-datacollection/israel_1km/cwatm_data/cwatm_input30sec",
  #                  climateInput_path = "d:/dropbox/iiasa/cwatm-datacollection/israel_1km/cwatm_data/climate/global")
#settings_list2spreadsheet(settingsList = sl, output_path = "d:/dropbox/iiasa/cwatm4r_test/settingsAutoUpdate.xlsx", overwrite = TRUE)
# Function to update FilePaths in a settingsList object ####

#' Update FILE_PATHS in a CWatM settings `list`
#'
#' @description/
#' This function updates the FILE_PATHS domain of a CWatM settings named `list` based on user-defined values.
#'
#' @details
#' The function updates the settings file FILE_PATHS domain allowing the user to define its own paths to the model data and outputs.
#' By convention, both `PathMaps` and `PathMeteo` are sub-folders of `PathRoot`. Users are encourages to provide each of these argument with the
#' full pat. if a sub-folder of `PathRoot` the function rewrites the path as: "$(PathRoot)/Path_to_data". Alternatively, the user can use the following format to
#' provide a relative path: "./Path_to_data", whereas the "./" stands for the current working directory.
#'
#' @param settingsList named `list` with CWatM settings
#' @param PathRoot `character`; path to folder containing all CWatM datasets
#' @param PathOut `character`; path to folder to save CWatM simulation's outputs
#' @param PathMaps `character`; path to folder containing all input data (excluding meteorological varaibales)
#' @param PathMeteo `character`; path to folder containing all meteorological variables
#' @return named `list`; a CWatM settings list.
#' @examples
#' \dontrun{
#'
#' # set workinf directory
#' setwd("d:/cwatm/model_data")
#'
#' # load a settings file template to a named `list`
#' my_settings <- settings_ini2list(inipath = "./cwatm/settings_template.ini")
#'
#' # set user-defined paths
#' root = "d:/cwatm/model_data"
#' maps = "d:/cwatm/model_data/maps"
#' meteo = "./climate"
#'
#' my_settings <- settings_setFilePaths(settingsList = my_settings,
#'                                      PathRoot = root,
#'                                      PathOut = NULL,
#'                                      PathMaps = maps,
#'                                      PathMeteo = meteo)
#' }/
#' @export

# to main model setuprun
settings_setFilePaths <- function(settingsList, PathRoot = NULL, PathOut = NULL, PathMaps = NULL, PathMeteo = NULL) {
  #tmp <- settingsList$FILE_PATHS
  if(!is.null(PathRoot)) {
    if(grepl("^\\.", PathRoot)) PathRoot <- gsub(".", getwd(), PathRoot, fixed = TRUE)

    cmnt <- settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathRoot", "comment"]
    if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
    write_cmnt <- "Auto-updated"
    if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

    updateSetting(tbl = settingsList$FILE_PATHS, setting = "PathRoot", col = c("value", "comment")) <- c(PathRoot, write_cmnt)
  }

  if(!is.null(PathOut))   {
    if(grepl("^\\.", PathOut)) PathOut <- gsub(".", getwd(), PathOut, fixed = TRUE)

    cmnt <- settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathOut", "comment"]
    if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
    write_cmnt <- "Auto-updated"
    if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

    updateSetting(tbl = settingsList$FILE_PATHS, setting = "PathOut", col = c("value", "comment")) <- c(PathOut, write_cmnt)
  }

  if(!is.null(PathMaps))  {
    if(grepl("^\\.", PathMaps)) PathMaps <- gsub(".", getwd(), PathMaps, fixed = TRUE)
    if(grepl(PathRoot, PathMaps)) PathMaps  <- gsub(PathRoot, "$(PathRoot)", PathMaps)

    cmnt <- settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathMaps", "comment"]
    if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
    write_cmnt <- "Auto-updated"
    if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

    updateSetting(tbl = settingsList$FILE_PATHS, setting = "PathMaps", col = c("value", "comment")) <- c(PathMaps, write_cmnt)
  }

  if(!is.null(PathMeteo)) {
    if(grepl("^\\.", PathMeteo)) PathMeteo <- gsub(".", getwd(), PathMeteo, fixed = TRUE)
    if(grepl(PathRoot, PathMeteo)) PathMeteo <- gsub(PathRoot, "$(PathRoot)", PathMeteo)

    cmnt <- settings$FILE_PATHS[settings$FILE_PATHS$variable %in% "PathMaps", "comment"]
    if(is.na(cmnt) ||  cmnt == "") cmnt = NULL
    write_cmnt <- "Auto-updated"
    if(!is.null(cmnt)) write_cmnt <- paste0(write_cmnt, "; ", cmnt)

    updateSetting(tbl = settingsList$FILE_PATHS, setting = "PathMeteo", col = c("value", "comment")) <- c(PathMeteo, write_cmnt)
  }

  return(settingsList)

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
#' The `modelpath` argument indicates where are the CWatM files located (e.g., run_cwatm.py). If set to `NULL` the function assumes that they are located in
#' the parent directory of the PathRoot folder.
#'
#' @param settingsfile character. defines the path to  an MS Excel settings spreadsheet or to a `settings.ini` file (see details)
#' @param modelpath character. defines the path to the folder with CWatM (see details)
#' @param overwrite logical. Currently out of use
#' @param ... additional arguments passed to the `system()` function
#' @return CWatM outputs
#' @examples
#' \dontrun{
#' call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", intern = TRUE) # for `intern` see `?system`
#' call_cwatm(settingsfile = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings_sorekBasin.ini")
#' }
#' @export
call_cwatm <- function(modelpath = NULL, settingsfile,  overwrite = FALSE, ...) { # overwrite is not usable at the moment

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
  if(is.null(modelpath)) modelpath <- filepaths$value[filepaths$variable %in% "PathRoot"]

  # error handling check output directory exists
  if(!dir.exists(pathout)) stop(sprintf("Model outputs' folder does not exist at %s\n", pathout))

  # if settingsFileType == xlsx -> create a settings.ini file
  if(settingsFileType == "xlsx") {
    output_name_unique <- sprintf("cwatm_settings_%s.ini", timeid())
    settings_spreadsheet2ini(spreadsheet = settingsfile, output_name = output_name_unique)
    settingsfile <- strsplit(settingsfile, split = "/")[[1]]
    settingsfile[length(settingsfile)] <- output_name_unique
    settingsfile <- paste0(settingsfile, collapse = "/")

  }

  if(!grepl("/$", modelpath)) modelpath <- paste0(modelpath, "/")
  call <- sprintf("python %srun_cwatm.py %s -l", modelpath, settingsfile)
  system(call, ...)


}
# settingsfile = "d:/dropbox/iiasa/cwatm4r_test/cwatm_settings/cwatm_settings.xlsx"
# settingsfile = "d:/dropbox/iiasa/cwatm4r_test/cwatm_settings/myfirstini.ini"/
