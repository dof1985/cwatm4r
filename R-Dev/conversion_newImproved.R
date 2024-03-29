

## function that read netcdf and return a raster, rasterStack or list of raster
## options to subset by time/dates/numeric index
## options to get a point(s) table, map, masked map
## options to apply sumamry function (e.g., output table); or to get temporal stat -e.g., sum arrays
library(raster)
pth1 <- "c:/Users/fridman/Dropbox/IIASA/cwatm_Israel/outputs/ayalon_wastewater2_s2/totalET_daily.nc"
pthcellarea <- "c:/Users/fridman/Dropbox/IIASA/cwatm_Israel/outputs/ayalon_wastewater2_s2/cellArea_totalend.nc"
mskpth <- "c:/Users/fridman/Dropbox/IIASA/cwatm-dataCollection/Israel_1km/CWATM_data/cwatm_input30sec/areamaps/maskmap_ayalon1km_CutOutMishmarAyalon.nc"
pth2 <- "c:/Users/fridman/Dropbox/IIASA/cwatm-dataCollection/Israel_1km/CWATM_data/cwatm_input30sec/landsurface/fractionLandcover-0.nc"
ncdfInfo <- function(pth, dim = TRUE, attrs = FALSE) {
  tmp <- ncdf4::nc_open(pth)
  info <- list()
  if(dim) {
    vars <- names(tmp$var)
    dims <- names(tmp$dim)
    info <- c(info, "vars"= list(vars), "dims" = list(dims))
  }
  if(attrs) {
    varattr <- do.call("rbind", lapply(vars, function(v) {
      outdf <- as.data.frame(t(as.data.frame(unlist(ncdf4::ncatt_get(tmp, varid = v)))))
      row.names(outdf) <- v
      return(outdf)
    }))

    globattr <- as.data.frame(t(as.data.frame(unlist(ncdf4::ncatt_get(tmp, varid = 0)))))
    row.names(globattr) <- NULL

    info <- c(info, "varAttributes" = varattr, "globalAttributes" = globattr)
  }
  ncdf4::nc_close(tmp)
  return(info)
}



ncdf2raster <- function(pth, flip = NULL, transpose = FALSE, time = NULL, origin = "1901-01-01", spatial = NULL,
                        varName = NULL, fun = NULL, temporal_fun = NULL, crs = "+init=EPSG:4326", ...) {


  ## input validation
  if(!is.null(time)) {
    stopifnot("'time' argument should be of class 'Date', 'integer' or 'numeric'" = any(c("integer", "Date", "numeric") %in% class(time)))

    stopifnot("'time' argument should be of length 1 or 2" = setNames(length(time) == 1 || length(time) ==2, errmsg))

    if(length(time) == 2) {
      stopifnot("The first member of the 'time' argument should be smaller than its second member" = time[2] > time[1])
    }



  }

  if(!is.null(spatial)) {
    stopifnot("'spatial' argument should be of class 'RasterLayer' or 'data.frame'" = any(c("RasterLayer", "data.frame") %in% class(spatial)))

    if(class(spatial) == "data.frame") {
      cond <- all(c(any(c("lat", "Y", "y") %in% names(spatial)), any(c("lon", "X", "x") %in% names(spatial))))
      stopifnot("Points coordinates columun should recieve one of the following names: c('x', 'y'), c('X', 'Y'), c('lon', 'lat')" = cond)
    }
  }

  if(!is.null(var)) {
    stopifnot("'var' should be of class 'character'" = class(var) %in% "character")
  }

  ## functions
  getAxis <- function(array, idx, axis) {
    ndim <- length(dim(array))
    idx_list <- lapply(seq_len(ndim), function(d) {
      if(d == axis) {
        return(idx)
      } else {
        return(seq_len(dim(array)[d]))
      }
    })
    return(do.call("[", c(list(array), idx_list)))
  }

  sumArray <- function(array, axis, fun) {
    n <- dim(array)[axis]
    as.array(fun(stack(lapply(seq_len(n), function(i) {
      raster(getAxis(array = array, idx = i, axis = axis))
    }))))

  }

  # open file
  tmp <- ncdf4::nc_open(pth)

  # get dim x, dim y
  y <- tmp$dim$lat$vals
  x <- tmp$dim$lon$vals

  resx <- x[2] - x[1]
  resy <- abs(y[2] - y[1])

  # set temporal dim
  timeExists <- "time" %in% names(tmp$dim)
  tempnm <- NULL

  if(timeExists && is.null(time)) {
    temp <- tmp$dim$time$vals
    s_time <- 1
    e_time <- length(temp)
    tempnm <- temp
  }

  if(!is.null(time)) {
    errmsg = "'time' argument included, but input data has no time dimension"
    stopifnot(errmsg = timeExists)

    temp <- tmp$dim$time

    # time inputs asDate
    if(class(time) %in% "Date") {
      time <- which(temp$vals %in% as.numeric(time - as.Date(origin)))

    }


    s_time <- time[1]
    e_time <- s_time
    if(length(time) == 2) e_time <- time[2] - s_time + 1

    tempnm <- temp$vals[s_time:(s_time + e_time - 1)]

  }

  s_x <- 1
  c_x <- -1
  s_y <- 1
  c_y <- -1

  # set spatial mask
  spatExists <- !is.null(spatial)
  isPts <- FALSE
  isMask <- FALSE
  if(spatExists) {
    isPts <- class(spatial) %in% "data.frame"
    isMask <- class(spatial) %in% "RasterLayer"
  }

  # pts
  if(isPts) {
    x_idx <- na.omit(match(c("lon", "X", "x"), names(spatial)))
    y_idx <- na.omit(match(c("lat", "Y", "y"), names(spatial)))

    x_loc <- unlist(lapply(spatial[ , x_idx], function(x1) {
      which.min(abs(x1 - x))
    }))

    y_loc <- unlist(lapply(spatial[ , y_idx], function(y1) {
      which.min(abs(y1 - y))
    }))

    #mask2array <- as.matrix(spatial)
    mask2Extent <- c(min(x[x_loc]), max(x[x_loc]), min(y[y_loc]), max(y[y_loc]))

    s_x <- which.min(abs(mask2Extent[1] - x))
    e_x <- which.min(abs(mask2Extent[2] - x))

    s_y <- which.min(abs(mask2Extent[4] - y))
    e_y <- which.min(abs(mask2Extent[3]- y))

    c_x <- e_x - s_x + 1
    c_y <- e_y - s_y + 1
  }

  # msk
  if(isMask) {
    mask2array <- as.matrix(spatial)
    mask2Extent <- raster::extentFromCells(spatial, raster::Which(!is.na(spatial), cell = TRUE))

    s_x <- which.min(abs(mask2Extent@xmin - x))
    e_x <- which.min(abs(mask2Extent@xmax - x))

    s_y <- which.min(abs(mask2Extent@ymax - y))
    e_y <- which.min(abs(mask2Extent@ymin - y))

    c_x <- e_x - s_x + 1
    c_y <- e_y - s_y + 1
  }

  varid <- names(tmp$var)
  if(!is.null(varName)) varid <- varName

  from <- c(s_x, s_y)
  counts <- c(c_x, c_y)
  if(timeExists) {
    from <- c(from, s_time)
    counts <- c(counts, e_time)
  }

  out_ds <- setNames(lapply(varid, function(varid) {
    arr <- ncdf4::ncvar_get(tmp, varid = varid, start = from, count = counts)


    arrDims <- dim(arr)
    time_arrDim <- NULL
    if(timeExists) {
     if(is.null(time)) {
       time_arrDim <- length(arrDims)
      } else if (length(time) > 1) {
        time_arrDim <- length(arrDims)
      }
   }

    if(!is.null(temporal_fun) && !is.null(time_arrDim) && !isPts) { # ignore points
      arr <- sumArray(array = arr, axis = time_arrDim, fun = temporal_fun)

      tempnm <- NULL

      if(is.null(time)) {
        time_arrDim <- length(arrDims)
      } else if (length(time) > 1) {
        time_arrDim <- length(arrDims)
      }
    }

    if(isMask) {
      xmn = x[s_x] - 0.5 * resx
      xmx = x[e_x] + 0.5 * resx
      ymn = y[e_y] - 0.5 * resy
      ymx = y[s_y]  + 0.5 * resy

      mask2array <- as.matrix(raster::crop(spatial, raster::extent(xmn, xmx, ymn, ymx)))
      if(transpose) mask2array <- raster::t(mask2array)
      if(!is.null(time_arrDim)) mask2array <- array(rep(mask2array, dim(arr)[time_arrDim]), dim = dim(arr))
      arr <- mask2array * arr
    }



    iter <- 1
    if(!is.null(time_arrDim)) iter <- seq_len(dim(arr)[time_arrDim])
    outr <- setNames(lapply(iter, function(l) {
      if(!is.null(time_arrDim)) {
        arr2rast <- as.matrix(getAxis(array = arr, idx = l, axis = time_arrDim))
        #if(time_arrDim == 2) arr2rast <- raster::t(arr2rast)
      } else {
        arr2rast <- as.matrix(arr)
      #if(isPts) arr2rast <- raster::t(arr2rast)
      }

      if(transpose) arr2rast <- raster::t(arr2rast)

      if(isPts) {
        x_ext <- x_loc - min(x_loc) + 1
        y_ext <- y_loc - min(y_loc) + 1
        do.call("rbind", lapply(seq_len(length(x_ext)), function(i) {
          dfpts <- data.frame("x" = x[x_loc[i]],
                     "y" = y[y_loc[i]],
                     "var"= varid, stringsAsFactors = FALSE)
          if(!is.null(time_arrDim)) dfpts$time <- tempnm[l]
          dfpts$value <-  arr2rast[x_ext[i], y_ext[i]]
          return(dfpts)
        }))


      } else {

       # coords
       xmn = min(x) - 0.5 * resx
       xmx = max(x) + 0.5 * resx
       ymn = min(y) - 0.5 * resy
       ymx = max(y) + 0.5 * resy

       if(isMask) {
          xmn = x[s_x] - 0.5 * resx
          xmx = x[e_x] + 0.5 * resx
          ymn = y[e_y] - 0.5 * resy
          ymx = y[s_y]  + 0.5 * resy
        }


        if(!is.null(fun)) {
          dfout <- data.frame("var"= varid, stringsAsFactors = FALSE)
          if(!is.null(time_arrDim) && is.null(temporal_fun)) dfout$time <- tempnm[l]
          dfout$value <- fun(arr2rast, ...)
          return(dfout)
        }
        rast <- raster::raster(arr2rast, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = raster::crs(crs))
        if(!is.null(flip)) rast <- raster::flip(rast, direction = flip)

        return(rast)
      }

    }), nm = tempnm)

    if(length(outr) == 1) outr <- outr[[1]]
    if(isPts && !is.null(time_arrDim)) outr <- do.call("rbind", outr)
    if(!is.null(fun) && is.null(temporal_fun) && !is.null(time_arrDim)) outr <- do.call("rbind", outr)
    return(outr)
  }), nm = varid)

  if((isPts | !is.null(fun)) && length(varid) > 1) out_ds <- do.call("rbind", out_ds)
  if(class(out_ds) %in% "data.frame") row.names(out_ds) <- NULL
  if(class(out_ds) %in% "list") {
    if(length(varid) > 1 && length(tempnm) == 1) {
      out_ds <- stack(out_ds)
    } else if(length(varid) > 1) {
      out_ds <- setNames(lapply(tempnm, function(timename) {
        tmp <- stack(lapply(varid, function(varname) {
          out_ds[[varname]][[as.character(timename)]]
        }))
        names(tmp) <- varid
        return(tmp)
      }), nm = tempnm)
    }
  }
  ncdf4::nc_close(tmp)
  return(out_ds)
}


msk <- ncdf2raster(pth = mskpth, transpose = TRUE, spatial = NULL)
areacell <- ncdf2raster(pth = pthcellarea, transpose = TRUE, spatial = NULL)
tst <- ncdf2raster(pth = pth1, transpose = TRUE, time = 1)
tst2 <- ncdf2raster(pth = pth1, transpose = TRUE, time = c(1), spatial = msk)

tst3 <- ncdf2raster(pth = pth1, transpose = TRUE, spatial = areacell, fun = sum, na.rm = TRUE)


pts <- data.frame("lat" = c(31.92, 32), "lon" = c(34.81, 34.81))
pts2 <- data.frame("ID" =1:2, "lat" = c(31.92, 32), "lon" = c(34.81, 34.81))

tst4 <- ncdf2raster(pth = pth1, transpose = TRUE, time = NULL, spatial = pts)

tst5 <- ncdf2raster(pth = pth1, transpose = TRUE, temporal_fun = sum, spatial = pts,  na.rm = TRUE,
                    time = as.Date(c("2003-01-01", "2003-02-01")))

ncdfInfo(pth = pth2, attrs =  FALSE)

tst6 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = areacell, time = c(1,10))
## add stats, fun, points, origin for date (e.g., time by date), multiple variables
## make sure to terminate asap - make sure to close con before terminating

tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = areacell, time = "a")
tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = areacell, time = 1:4)
tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = areacell, time = as.Date(c("2002-01-01", "2001-01-01")))
tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = data.frame("LATIT" = 21, "Long" =35), time = c(1,4))
tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = NULL, spatial = stack(msk), time = c(1,4))
tst7 <- ncdf2raster(pth = pth2, transpose = TRUE, var = 1, spatial = (msk), time = c(1,4))


