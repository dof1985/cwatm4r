# Process DEM to (a) Aggregate to get average height at 1 km (b) get stdev at 1 km (c) get dzRel at 1 km

#dem <- "d:/dropbox/iiasa/cwatm-datacollection/dem_merit/Israel_DEM.tif"

createElvStd <- function(dem, to_res = 30/3600) {
  f_list <- list("gdalwarp" = gdalUtils::gdalwarp)
  f1 <- tempfile()

  orig_dem <- raster::raster(dem)

  args <- list("srcfile" = dem,
               "dstfile" = f1,
               "tr" = c(to_res, to_res),
               "r" = "average",
               "output_Raster" = TRUE
               )

  mean_dem <- do.call(f_list$gdalwarp, args)

  f2 <- tempfile()
  args <- list("srcfile" = f1,
               "dstfile" = f2,
               "tr" = raster::res(orig_dem),
               "r" = "max",
               "output_Raster" = TRUE,
               "overwrite" = FALSE
  )

  do.call(f_list$gdalwarp, args)

  f3 <- sprintf("%s.tif", tempfile() )
  raster::writeRaster((orig_dem - raster::raster(f2)) ^ 2,
                    filename = f3,
                    format = "GTiff")

  f4 <- tempfile()
  args <- list("srcfile" = f3,
               "dstfile" = f4,
               "tr" = c(to_res, to_res),
               "r" = "average",
               "output_Raster" = TRUE,
               "overwrite" = TRUE
  )

  std_dem <- do.call(f_list$gdalwarp, args)
  std_dem <- std_dem ^ 0.5

  return(setNames(stack(mean_dem, std_dem), nm = c("elev", "elevStd")))
}


flwdir_ldd <- "d:/dropbox/iiasa/cwatm-datacollection/dem_merit/GRASS7_IsraelFlowdir_ldd.tif"
elev_hr <- "d:/dropbox/iiasa/cwatm-datacollection/dem_merit/Israel_DEM.tif"
f1 <- sprintf(tempfile(), ".py")

dependencies <- c("rasterio", "pyflwdir")

pyLines <- c(sprintf("import %s", dependencies),
             sprintf("with rasterio.open('%s', 'r') as src:", flwdir_d8),
             "  flwdir = src.read(1)",
             sprintf("with rasterio.open('%s', 'r') as src:", elev_hr),
             "  elevtn = src.read(1)",
             "  prof = src.profile",
             "flw = pyflwdir.from_array(flwdir, ftype='ldd', latlon=True, cache=True)",
             "print(flw)"

             )
writeLines(pyLines, f1)


system(command = sprintf("py %s", f1), intern = TRUE)
