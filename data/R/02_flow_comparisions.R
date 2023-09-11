library(RNetCDF); library(sf); library(dplyr); library(hydroGOF)

basins = read_sf('data/basins.gpkg') %>% 
  filter(!is.na(feature_id))  

## `nwmTools::readNWMdata()` and `dataRetrieval::readNWISdata()` was used to extract the complete timseries for gages of interest. 
## These were then save within a merged NetCDF file.
## 
## Regrettably when I first made these I was not as diligent about saving reproducable code and 
## now rely on the data outputs which have been vetted for accuracy. 
## I am happy to rewrite the code to remake these if anyone is interested.
## I can also share them on request (due to size). Please email me at `jjohnson@lynker.com` to set up a transfer!

nwis       = open.nc('private/mergednwis.nc')
nwm20      = open.nc('private/mergednwm20.nc')

evals = list()

for( i in 1:10){
  
  nwm20ID  = which(var.get.nc(nwm20, "feature_id")  == basins$feature_id[i])
  nwisID   = which(var.get.nc(nwis,  "feature_id")  == basins$feature_id[i])
  
  df =  data.frame(
    nwm_q20  = tryCatch({var.get.nc(nwm20, 
                                    "streamflow", 
                                    start = c(1,nwm20ID),  
                                    count = c(NA, 1), 
                                    unpack = TRUE)}, error = function(e){NA}),
    nwis_q   = tryCatch({var.get.nc(nwis,  
                                    "streamflow", 
                                    start = c(1,nwisID), 
                                    count = c(NA, 1), 
                                    unpack = TRUE) * 0.028316846592}, error = function(e){NA}))
  
  v20 = dplyr::select(df, nwis = nwis_q, nwm = nwm_q20) %>%
    filter(nwm > 0, nwis > 0) 
  
  if(nrow(v20) > 10){
    v20 = v20 %>% 
      summarize(NSEA  = cor(nwm, nwis)^2,
                NSEB  = (cor(nwm, nwis) - (sd(nwm) / sd(nwis)))^2,
                NSEC  = ((mean(nwm) - mean(nwis)) / sd(nwis))^2,
                NSE   = NSEA - NSEB - NSEC,
                NSE_HG = hydroGOF::NSE(nwm, nwis),
                pbais  = hydroGOF::pbias(nwm, nwis),
                nrmse  = hydroGOF::nrmse(nwm, nwis),
                kge    = hydroGOF::KGE(nwm, nwis), 
                mae    = hydroGOF::mae(nwm, nwis),
                count = nrow(.), 
                feature_id = basins$feature_id[i],
                model = "v20") 
  } else {
    v20 = NULL
  }
  
  evals[[i]] = v20
  message(i)
}

left_join(basins, 
          bind_rows(evals), 
          by = "feature_id") %>% 
  write_sf("data/basins.gpkg")

