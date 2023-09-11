library(terra); library(sf); library(exactextractr); library(dplyr)

bas = read_sf("data/basins.gpkg")
#####
# This section requires the NLCD 2016 (used in NWM2.0) landcover and impervious surface data
# Please download from here:
#   Imperv: https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_impervious_l48_20210604.zip
#   Landcover: https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_land_cover_l48_20210604.zip
# Unzip both files and populate the NLCD directory below  
#####
nlcd_dir = '/Volumes/Transcend/NLCD'
# IMPERV

f = list.files(nlcd_dir, pattern = ".img", recursive = TRUE, full.names = TRUE)

imp = rast(grep("impervious",f, value = TRUE))
bas2 = st_transform(bas, st_crs(imp))

imp_per      = exact_extract(imp,
                             bas2,
                             function(value, cov_frac) {
                               mean(cov_frac[value <= 100] * value[value <= 100])
                             })

# LAND
lc           = rast(grep("land_cover",f, value = TRUE))

t = exact_extract(lc, bas2, function(value, cov_frac) {
  data.frame(value = value, cov_frac = cov_frac) %>%
    group_by(CLASS = as.character(substr(value, 1, 1))) %>%
    dplyr::summarize(c = sum(cov_frac * (900 / 1e6))) %>%
    right_join(data.frame(CLASS = as.character(
      c("0", "1", "2", "3", "4", "5", "7", "8", "9")
    ),
    tmp = rep(NA, 9)), by = "CLASS") %>%
    pull(c)
})

out  = round(100*mapply(`/`, data.frame(t), bas2$AREA), 2) %>% 
  t()  %>% 
  data.frame() %>% 
  setNames(paste0("LU_", c(0,1,2,3,4,5,7,8,9))) %>% 
  mutate(GAGE_ID = bas$GAGE_ID,
         imp_per = imp_per)


left_join(bas, out, by = "GAGE_ID") %>% 
  write_sf("data/basins.gpkg")
