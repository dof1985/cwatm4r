#library(rgdal)
#library(gdalUtils)
#library(sp)
#library(raster)

# GET SOILGRIDS DATA

#e1 <- raster::extent(matrix(c(33.071, 37.071, 29.004, 34.051), byrow = TRUE, nrow = 2))


getSoilgridsLayer <- function(out_path = "./",
                              b_box = NULL, # in wgs84
                              variable = "bdod",
                              depth = "0-5cm",
                              quantile = "mean",
                              at1km = TRUE) {

  # ERROR HANDLING
  #####
  # CHECK ARGS: depth
  if(!depth %in% c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")) {
    stop(sprintf("`%s` is not a found in `%s`", depth,
                 paste0(c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"), collapse = ", ")))
  }

  # CHECK ARGS: variable
  if(!variable %in% c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocd", "phh2o", "sand", "silt", "soc", "wrb")) {
    stop(sprintf("`%s` is not a found in `%s`", variable,
                 paste0(c("bdod", "cec", "cfvo", "clay", "nitrogen", "ocd", "ocd", "phh2o", "sand", "silt", "soc", "wrb"), collapse = ", ")))
  }


  # CHECK ARGS: variable
  if(!quantile %in%  c("mean", "Q0.05", "Q0.5", "Q0.95", "uncertainty")) {
    stop(sprintf("`%s` is not a found in `%s`", quantile,
                 paste0( c("mean", "Q0.05", "Q0.5", "Q0.95", "uncertainty"), collapse = ", ")))
  }

  # ALERT IF BOUNDING BOX IS NOT DEFINED
  if(is.null(b_box)) warning("No bounding box defined. Downloading a global layer may take a lot of time")

  if(!is.null(b_box) && !class(b_box) %in% "Extent") stop("b_box expect and Extent object")
  #####

  # DEFINE THE HOMOLOSINE PROJECTION
  igh    <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

  # DEFINE THE WGS84 COORD. SYSTEM
  wgs84  <- '+proj=longlat +datum=WGS84 +no_defs'

  # DEFINE SOILGRIDS' URL
  sg_url <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"


  # TRANSFORM b_box to homolosine

  e <- as(b_box, "SpatialPolygons")
  sp::proj4string(e) <- wgs84
  e.homolosine <- sp::spTransform(e, igh)


  # GET HOMOLOSINE B_BOX AS bb

  bb     <- c(e.homolosine@bbox["x", "min"], # ulx
              e.homolosine@bbox["y", "max"], # uly
              e.homolosine@bbox["x", "max"], # lrx
              e.homolosine@bbox["y", "min"]) # lry

  # START DOWNLOAD

  # create temp env.
  tmp_dir_path <- tempdir()

  # CREATE FUNCTIONS LIST

  f_list <- list(
    "gdal_translate"  = gdalUtils::gdal_translate,
    "gdal_warp" = gdalUtils::gdalwarp
  )

  # construct source path
  src <- sprintf("%s%s/%s_%s_%s.vrt", sg_url, variable, variable, depth, quantile)

  # download virtual layer in bb
  f1 <- paste0(tempfile(),".vrt")

  args <- list("src_dataset" = src,
               "dst_dataset" = f1,
               "tr" = c(250, 250),
               "projwin" = bb,
               "projwin_srs" = igh,
               "verbose" = TRUE)
  do.call(f_list$gdal_translate, args)

  # transfrom virtual layer from homolosine to wgs84 - separate from aggregation &/or define function to aggrrgation
  f2 <- paste0(tempfile(),".vrt")

  args <- list("src_dataset" = f1,
               "dst_dataset" = f2,
               "s_srs" = igh,
               "t_srs" = wgs84,
               "of" = "VRT")


  if(at1km) {
    args <- c(args, list("tr" = c(1000, 1000),
                         "r" = "average"))
  } else {
    warning("No aggregation. Output has a 250 meter resolution")
  }

  do.call(f_list$gdal_warp, args)


  args <- list("src_dataset" = f2,
             "dst_dataset" = sprintf("%s/%s_%s_%s.tif", out_path, variable, depth, quantile),
             "co" = c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))

  do.call(f_list$gdal_translate, args)

}
