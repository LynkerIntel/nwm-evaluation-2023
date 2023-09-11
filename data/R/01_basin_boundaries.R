# downlaod file from here:  https://water.usgs.gov/GIS/dsdl/boundaries_shapefiles_by_aggeco.zip
# Unzip and fill in the filepath here:
library(dplyr); library(sf);library(arrow)

gages2_dir = "private/"

if(!dir.exists(glue("{gages2_di}")))
url <- "https://water.usgs.gov/GIS/dsdl/boundaries_shapefiles_by_aggeco.zip"
out_f <- file.path(gages2_dir, basename(url))
if(!file.exists(file.path(out, basename(url)))) { 
  httr::GET(url, httr::write_disk(out_f, overwrite = TRUE))
}
outdir <- gsub(".zip", "", out_f)
try(zip::unzip(out_f, overwrite = FALSE, exdir = outdir))

if(!file.exists("data/basins.gpkg")){
  
  basins = bind_rows(lapply(
    list.files(
      gages2_dir,
      pattern = ".shp",
      full.names = TRUE,
      recursive = TRUE
    ),
    read_sf
  )) %>% 
    select(-AREA)
  
  bas = inner_join(read_parquet("data/usgs_metadata.parquet"), basins, by = "GAGE_ID") %>% 
    inner_join(read_parquet("data/nhdplus_nwis_mapping.parquet"), by = "GAGE_ID") %>% 
    st_as_sf()
  
  # THIS IS TO BIG TO COMMIT SO PLEASE MAKE IT YOURSELF:
  write_sf(bas, "data/basins.gpkg") 
  
}


