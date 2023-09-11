

# You need to run data/R/01_basin_boundaries.R and 02_nldas-data-download.R prior to this file

download_dir = '/Volumes/Transcend/NLDAS2/'
dir.create("data/tifs", showWarnings = FALSE)

# NOTE: using `raster` over `terra` here for the excellent `stackApply` and `corLocal` options
# To date, I have not found a seamless alternative for these functions
# 
ppt_stack    <- stack(list.files(glue("{download_dir}ppt_grids/"),  full.names = TRUE))
et_stack     <- stack(list.files(glue("{download_dir}/pet_grids/"), full.names = TRUE))
asnow_stack  <- stack(list.files(glue("{download_dir}/asnow_grids/"), full.names = TRUE))
snowc_stack  <- stack(list.files(glue("{download_dir}/snowc_grids/"),  full.names = TRUE))
ids <- rep(1:(nlayers(pptstack)/12), each = 12)

if(!file.exists("data/tifs/annual_mean_ppt.tif")){
  annual_ppt_sum <- stackApply(ppt_stack, ids, fun = sum)
  annual_mean_ppt <- mean(annual_ppt_sum)
  writeRaster(annual_mean_ppt, "data/tifs/annual_mean_ppt.tif")
}

if(!file.exists("data/tifs/annual_mean_et.tif")){
  annual_et_sum <-  stackApply(et_stack, ids, fun = sum)
  annual_mean_et <- mean(annual_et_sum)
  writeRaster(annual_mean_et, "data/tifs/annual_mean_et.tif")
}

if(!file.exists("data/tifs/annual_mean_asnow.tif")){
  annual_asnow_sum <-  stackApply(asnow_stack, ids, fun = sum)
  annual_mean_asnow <- mean(annual_asnow_sum)
  writeRaster(annual_mean_asnow, "data/tifs/annual_mean_asnow.tif")
}

if(!file.exists("data/tifs/annual_mean_ppt.tif")){
  annual_snowc_sum <-  stackApply(snowc_stack, ids, fun = sum)
  annual_mean_snowc <-  mean(annual_snowc_sum)
  writeRaster(annual_mean_snowc, "data/tifs/annual_mean_snowc.tif")
}

if(!file.exists("data/tifs/annual_mean_ppt.tif")){
  corlayer <- corLocal(ppt_stack, et_stack, method = "spearman")
  writeRaster(corlayer, "data/tifs/phase_cor.tif")
}

if(!file.exists("data/tifs/annual_mean_ppt.tif")){
  ai = annual_mean_et / annual_mean_ppt
  raster::writeRaster(ai, "data/tifs/ai.tif")
}


##### 
# Note: transition back to `terra`
##### 

s = terra::rast(list.files('data/tifs',  full.names = TRUE))

bas = st_transform(read_sf("data/basins.gpkg"), crs(s))

out = zonal::execute_zonal(s, bas, ID = "GAGE_ID")

left_join(bas, out, by, "GAGE_ID") %>% 
  write_sf("data/basins.gpkg")
