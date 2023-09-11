library(sf); library(arrow)
read_sf("data/basins.gpkg") %>% 
  st_drop_geometry() %>% 
  write_parquet("data/v20_summary_stats.parquet")
