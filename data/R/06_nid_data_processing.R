library(arrow); library(sf); library(dplyr)

bas = read_sf("data/basins.gpkg")
bas2 = st_transform(bas, st_crs(imp))

# For github size constraints the 26MB xlsx files is reduced and written as parquet
# readxl::read_excel('data/NID2019.xlsx') %>% 
#   filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>% 
#   write_parquet("data/nid_2019_subset.parquet")

nid =  arrow::read_parquet("data/nid_2019_subset.parquet") %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  st_transform(st_crs(bas2))

dam_summary = st_join(bas2, nid) %>% 
  st_drop_geometry() %>% 
  group_by(GAGE_ID) %>% 
  dplyr::summarise(count_dams = n(), 
                   total_storage = sum(NID_STORAGE, na.rm = TRUE),
                   contributing_storage = sum(!is.na(NID_STORAGE)) / n(),
                   total_surface = sum(SURFACE_AREA, na.rm = TRUE),
                   contributing_area = sum(!is.na(SURFACE_AREA)) / n()) %>% 
  ungroup()


left_join(bas, dam_summary, by = "GAGE_ID") %>% 
  write_sf("data/basins.gpkg")


