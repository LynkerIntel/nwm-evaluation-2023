library(httr)
library(fs)
library(glue)

download_dir <- '/Volumes/Transcend/NLDAS2/'
dir_create(download_dir)
dates <- seq.Date(as.Date("1993-01-01"), as.Date("2018-12-31"), by = "m")

# Go here: https://disc.gsfc.nasa.gov/datasets/NLDAS_FORA0125_M_002/summary?keywords=NLDAS
# Click "Subset/ Get Data
# Download Method = Get File Subsets using the GES DISC Subsetter
# Refine data range to 1993-01-01 to 2018-12-31
# Select variable: APCP
# Select Output file format = COG
# Hit "Get Data"
# Download Links List = subset_NLDAS_FORA0125_M_002_20230907_224958_.txt

ppt_files <- readLines('data/subset_NLDAS_FORA0125_M_002_20230907_224958_.txt')[-1]

# Use to test...
# ppt_files = ppt_files[1:24]

dir_create(glue("{download_dir}pet_grids/"))
dir_create(glue("{download_dir}ppt_grids/"))
dir_create(glue("{download_dir}asnow_grids/"))
dir_create(glue("{download_dir}snowc_grids/"))

## Get PPT Data
for (i in 1:length(ppt_files)) {
  filename <- glue("{download_dir}ppt_grids/{dates[i]}_ppt.tif")
  if (!file.exists(filename)) {
    response <-
      GET(
        ppt_files[i],
        write_disk(filename, overwrite = TRUE),
        progress(),
        config(netrc = TRUE, netrc_file =  climateR::getNetrcPath()),
        set_cookies("LC" = "cookies")
      )
  }
  
}

## Get PET Data
for (i in 1:length(ppt_files)) {
  pet_file <- gsub("APCP", "PEVAP", ppt_files[i])
  filename <- glue("{download_dir}pet_grids/{dates[i]}_pet.tif")
  if (!file.exists(filename)) {
    response <-
      GET(
        pet_file,
        write_disk(filename, overwrite = TRUE),
        progress(),
        config(netrc = TRUE, netrc_file =  climateR::getNetrcPath()),
        set_cookies("LC" = "cookies")
      )
  }
}

# ASNOW = Snowfall (frozen precipitation) (kg/m^2)
for (i in 1:length(ppt_files)) {
  asnow    <-
    gsub("APCP", "ASNOW",  gsub("FORA0125", "VIC0125", ppt_files[i]))
  filename <- glue("{download_dir}asnow_grids/{dates[i]}_asnow.tif")
  if (!file.exists(filename)) {
    response <-
      GET(
        asnow,
        write_disk(filename, overwrite = TRUE),
        progress(),
        config(netrc = TRUE, netrc_file =  climateR::getNetrcPath()),
        set_cookies("LC" = "cookies")
      )
  }
}

# SNOWC = Snow cover (fraction)
for (i in 1:length(ppt_files)) {
  snowc    <-
    gsub("APCP", "SNOWC", gsub("FORA0125", "VIC0125", ppt_files[i]))
  filename <-
    glue("{download_dir}snowc_grids/{dates[i]}__snowc.tif")
  if (!file.exists(filename)) {
    response <-
      GET(
        snowc,
        write_disk(filename, overwrite = TRUE),
        progress(),
        config(netrc = TRUE, netrc_file =  climateR::getNetrcPath()),
        set_cookies("LC" = "cookies")
      )
  }
}