# conversions

# Function to convert a RasterLayer/RasterStack/list of the latters to a netCDF file (using 'raster' & 'ncdf4' libraries) ####

#' Converts raster to netCDF
#'
#' @description
#' This function converts a `RasterLayer`, `RasterStack` or a `list` of these classes to a 'NetCDF' file.
#'
#' @details
#' This function accepts spatio-temporal data and export it to a NetCDF file.
#' For spatio-temporal data, a list of `RasterLayer`/`RasterStack` objects is expected. Each list item stands for a point in time, and
#' each `RasterLayer` stands for the spatial dimensions and data of a variable. If multiple variable are required, a `RasterStack` can be used, so
#' each layer stands for a variable.
#' In case  temporal dimension is required, the user can use the 'time' argument to specify the time points for the data. This argument
#' accepts a vector of the class `Date` and assumes the origin is 01-01-1901.
#' The 'nc_attrs' argument accepts a named list with the following variable attributes: 'dname', 'dlname', 'units'
#' and 'prec', and some of the global attributes: 'history', 'source_software', 'title', 'keywords', 'source', and 'institution'.
#' The variable name (dname) and long name (dlname) expects a character vector of the same length of the number of variables, or `NULL`.
#' If `NULL` is assigned, the variable name is taken from the `RasterLayer` name.
#'
#'
#' @param raster_in an input `RasterLayer`, `RasterStack` or a `list`
#' @param time a vector of class `Date` for the temporal dimension of the dataset (Default: NULL; see Details)
#' @param nc_attrs a named list with the variable and global attributes to be written to the NetCDF file (see Details)
#' @param pathfile_out a path and a name for the output NetCDF to be written
#' @param loud `logical.` Do you want to print the `ncdf4` object to consule?
#' @param fill.value NoData value (Default: NaN)
#' @return an NetCDF file (see Details)
#'
#' @examples
#' \dontrun{
#' PUT EXAMPLE HERE
#' }
#' @export
raster_as_nc <- function(raster_in, time, nc_attrs, pathfile_out = "../output.nc", loud = FALSE, fill.value = NaN) {
  print("Reading files")


  if(!is.null(time)) { # check raster_in validity - if time NOT NULL
    if(!class(raster_in) %in% c("list")) stop(sprintf("raster_in of class '%s' is not valid", class(raster_in)))
    if(!all(unlist(lapply(raster_in, class)) %in% c("RasterLayer", "RasterStack"))) stop("raster_in of class holds invalid data")
  }

  if(is.null(time)) { # check raster_in validity - if time IS NULL
    if(!class(raster_in) %in% c("list", "RasterStack", "RasterLayer")) stop(sprintf("raster_in of class '%s' is not valid", class(raster_in)))
    if(class(raster_in) %in% "list") {
      if(!all(unlist(lapply(raster_in, class)) %in% c("RasterLayer", "RasterStack"))) stop("raster_in of class holds invalid data")
    } else {
      raster_in <- list(raster_in)
    }
  }


  if(!is.null(time) & class(time) != "Date") stop("time input should be of class 'Date'")
  if(!is.null(time) & length(raster_in) != length(time))  stop("Lengths of 'time' and 'raster_in' do not match")

  # make pathout
  if(regexpr(".nc$", pathfile_out) == -1) pathfile_out <- paste0(pathfile_out, ".nc")

  # coordinates lon
  rx <- raster::xres(raster_in[[1]])
  lon  <- as.array(seq(raster_in[[1]]@extent@xmin + rx * 0.5, raster_in[[1]]@extent@xmax - rx * 0.5, by = rx))
  nlon <- length(lon)

  # coordinates lat
  ry <- raster::yres(raster_in[[1]])
  lat  <- as.array(seq(raster_in[[1]]@extent@ymin + ry * 0.5, raster_in[[1]]@extent@ymax - ry * 0.5, by = ry))
  nlat <- length(lat)

  # var time
  if(!is.null(time)) {
    time <- as.array(as.numeric(time - as.Date("1901-01-01", origin = "1901-01-01")))
    nt <- length(time)
  }


  print("Mapping data to array")

  # map number of variables
  if(class(raster_in[[1]]) %in% "RasterLayer") {
    n <- 1
  } else {
    n <- unique(unlist(lapply(raster_in, function(x) length(x@layers))))
  }

  if(length(n) > 1) stop("all items in 'raster_in' should have the same number of 'RasterLayers'")

  all_Arrays <- lapply(seq_len(n), function(lyr_i) {
    # build a list of 3D arrays (with time var); each list item is a variable; each array is [lon, lat, time]
    fillvalue <- as.double(fill.value)
    dimList <- c(nlon, nlat)
    if(!is.null(time)) dimList <-c(dimList, nt)
    arrList <- array(fillvalue, dim = dimList)
    for(i in seq_len(length(raster_in))) {
      if(length(raster_in) > 1) {
        arrList[ , ,i] <- as.array(matrix(raster::flip(raster::t(raster_in[[i]][[lyr_i]]), direction = "x"),
                                          nrow = nlat, byrow = TRUE))
      } else {
        arrList[ , ] <- as.array(matrix(raster::flip(raster::t(raster_in[[i]][[lyr_i]]), direction = "x"),
                                        nrow = nlat, byrow = TRUE))
      }

    }
    return(arrList)
  })


  # define dimensions
  londim  <- ncdf4::ncdim_def(name = "lon", units = "degrees_east", longname = "longitude_coordinate", vals = as.double(lon))
  latdim  <- ncdf4::ncdim_def(name = "lat", units = "degrees_north", longname = "latitude_coordinate", vals = as.double(lat))
  if(!is.null(time)) {
    timedim <- ncdf4::ncdim_def(name = "time", units = "Days since 1901-01-01", longname = "Days since 1901-01-01", vals = as.double(time))
  }

  # define a functions' list
  f_nc <- list("ncvar_def" = ncdf4::ncvar_def,
               "nc_create" = ncdf4::nc_create)


  # define attributes for each variable/data layer
  arr_def_List <- lapply(seq_len(n), function(lyr_i) {
    fillvalue <- as.double(fill.value)
    if(!is.null(nc_attrs$dname)) { # set dname
      dname <- nc_attrs$dname[[lyr_i]]
    } else {
      dname <- names(raster_in[[1]])[lyr_i]
    }

    if(!is.null(nc_attrs$dlname)) { # set dlname
      dlname <- nc_attrs$dlname[[lyr_i]]
    } else {
      dlname <- names(raster_in[[1]])[lyr_i]
    }

    dimList <- list(londim, latdim)
    if(!is.null(time)) {
      dimList <- c(dimList, list(timedim))
    }

    arr_def <- ncdf4::ncvar_def(name = dname,
                                longname = dlname,
                                units = nc_attrs$units,
                                dim = dimList,
                                missval = fillvalue,
                                prec = nc_attrs$prec)

    return(arr_def)

  })

  print("Creating file")
  # create netCDF file and put arrays]\\
  ncout <- ncdf4::nc_create(filename = pathfile_out,
                            vars = arr_def_List,
                            force_v4 = TRUE)


  # put variables
  for(i in seq_len(length(arr_def_List))) {
    dname <- ncout$var[[i]]$name
    ncdf4::ncvar_put(nc = ncout, varid = arr_def_List[[i]], vals = all_Arrays[[i]])
    ncdf4::ncatt_put(nc = ncout, dname, "standard_name" , dname)
    dlname <- ncout$var[[i]]$longname
    ncdf4::ncatt_put(nc = ncout, dname, "long_name" , dlname)
  }


  # put additional attributes into dimension and data variables
  ncdf4::ncatt_put(ncout, "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
  ncdf4::ncatt_put(ncout, "lat", "axis", "Y")
  if(!is.null(time)) {
    ncdf4::ncatt_put(ncout, "time", "axis", "time")
  }
  #ncatt_put(ncout,"time","axis","T")

  #add global attributes
  if("history" %in% names(nc_attrs)) ncdf4::ncatt_put(ncout, 0, "history", nc_attrs$history)
  if("Source_software" %in% names(nc_attrs)) ncdf4::ncatt_put(ncout, 0, "Source_software", nc_attrs$Source_software)
  if("title" %in% names(nc_attrs)) {
    ncdf4::ncatt_put(ncout, 0, "title", nc_attrs$title)
  } else if("dlname" %in% names(nc_attrs)) {
    ncdf4::ncatt_put(ncout, 0, "title", nc_attrs$dlname)
  }
  if("keywords" %in% names(nc_attrs)) ncdf4::ncatt_put(ncout, 0, "keywords", nc_attrs$keywords)
  if("source" %in% names(nc_attrs)) ncdf4::ncatt_put(ncout, 0, "source", nc_attrs$source)
  if("institution" %in% names(nc_attrs)) ncdf4::ncatt_put(ncout, 0, "institution", nc_attrs$institution)

  # Get a summary of the created file:
  if(loud) print(ncout)

  ncdf4::nc_close(ncout)
}



# Function to create a settings.ini from an ms excel spreadsheet ####

#' Converts a settings spreadsheet to a `settings.ini` file
#'
#' @description
#' This function converts a default settings spreadsheet to a `settings.ini` file.
#'
#' @details
#' CWatM requires a `settings.ini` file to run. This function converts the user-friendly MS Excel   to a `settings.ini` file.
#'
#' @param spreadsheet character vector,  the path to  an MS Excel default settings spreadsheet
#' @param output_path character vector,  the settings file's path and name
#' @return a `settings.ini` file (see Details)
#'
#' @examples
#' \dontrun{
#' createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "./cwatm_settings_sorekBasin.ini")
#' createSettingsFile(pathRoot = "C:/IIASA/cwatm/cwatm_settings/cwatm_settings.xlsx", output_name = "./cwatm_settings_sorekBasin")
#' }
#' @export
settings_spreadsheet2ini <- function(spreadsheet, output_path = "./cwatm_settings.ini") {

  # check and set .ini suffix for the output_(file)name
  if(getSuffix(output_path) != "ini") output_path <- paste0(output_path, ".ini")

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
  # path <- strsplit(spreadsheet, "/")[[1]] # CHANGE TO SAVE TO USER DEFINED PATH&NAME
  # path[length(path)] <- output_name
  # path <- paste0(path, collapse = "/")

  writeLines(settings_out, output_path)

}



# Function to create a settingsList from a cwatm settings file ####

#' Import a cwatm .ini setting files to a settings List
#'
#' @description
#' This function imports a cwatm .ini settings file and save it as a named settings `list`
#'
#' @details
#' This function reads a cwatm .ini file from a path and saves it as a named list. Each list item stands for a settings domain, in which
#' all variable definitions are ketp in a `data.frame` (with 3 columns: variable, value, comment). The funtion does not import comment though.
#'
#'
#' @param inipath a path to the input `.ini` settings file.
#' @return `list`. A names setting `list` )
#'
#' @examples
#' \dontrun{
#' PUT EXAMPLE HERE
#' }
#' @export
settings_ini2list <- function(inipath = "./settings_cwatm.ini") {
  # can't read comments
  lnes <-  readLines(inipath)
  doms  <- getSettingsDomains_ini(inipath = inipath, value = TRUE)
  sline <- as.numeric(getSettingsDomains_ini(inipath = inipath, value = FALSE))
  eline <- c(sline[-1] - 1, length(lnes))

  # pasre domains
  return(setNames(lapply(seq_along(doms), function(i) {
    #dom <- doms[i]
    slnes <- lnes[(sline[i] + 1):eline[i]]
    slnes <- grep("^#", slnes, invert = TRUE, value = TRUE)
    slnes <- grep("=", slnes, value = TRUE)
    return(do.call("rbind", lapply(slnes, tableFromSettingLine)))
  }), nm = doms))
}

#inipath = "D:/Dropbox/IIASA/cwatm4r_test/settings_rhine30min.ini"
####

# Function to create an excel settings spreadsheet ####

#' Define CWatM settings & parameters
#'
#' @description
#' This function creates a default settings spreadsheet, granting the user with complete control over all model parameters.
#'
#' @details
#' The user can choose a settingList object (e.g., as in the output of `settings_ini2list()`). Otherwise (if `NULL`) the function uses a default settingsList
#'
#' @param settingsList named list, a settingsList object (Defaults to `NULL`; see Details)
#' @param output_path character vector, the settings spreadsheet's path and name
#' @param overwrite logical. Should the function  overwrite existing files?
#' @return an MS Excel settings spreadsheet (see Details)
#' @examples
#' \dontrun{
#' settings_list2spreadsheet( output_path = "./cwatm_settings.xlsx", overwrite = TRUE)
#' }
#' @export
settings_list2spreadsheet <- function(settingsList = NULL, output_path = "./cwatm_settings.xlsx", overwrite = FALSE) {

  # # Error handling - check folder structure
  # if(!dir.exists(paste0(gsub("/", "//", pathRoot), "//CWatM_settings"))) {
  #   stop(sprintf("The path `%s` does not exist. Create these directories to continue or set a different `pathRoot`\n", paste0(gsub("/", "//", pathRoot), "//CWatM_settings")))
  # }

  # Error handling - check overwrite opt.
  if(!overwrite & file.exists(output_path)) {
    stop("The file already exists. Use overwrite = TRUE to proceed\n")
  }
  if(!is.null(settingsList)) settings <- settingsList
  # settings[[3]][1, 2] <- pathRoot

  settings_wb <- openxlsx::createWorkbook(title = "cwatm4r-settingFilesSpreadsheet")

  for(n in names(settings)) {
    dim <- c(nrow(settings[[n]]), ncol(settings[[n]]))
    openxlsx::addWorksheet(wb = settings_wb,
                           sheetName = n)

    dt <- settings[[n]]
    dt$value <- strip(c("^ ", " $"), dt$value)
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
                         file = output_path, # change to user defined path + name
                         #file = paste0(gsub("/", "//", pathRoot), "//CWatM_settings//cwatm_settings.xlsx"),
                         overwrite = overwrite)
}


