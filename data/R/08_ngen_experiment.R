# NWM / NextGen / NWIS Flows
library(nwmTools); library(arrow); library(dataRetrieval)

ngen_sims = read.csv('data/10244950_PET_2_CFE_X.csv') 

ind = findNLDI(nwis = "10244950")

ngen_sims$NWM20_cms =  readNWMdata(comid = ind$origin$comid, 
                                             startDate = min(ngen_sims$time), 
                                             endDate = max(ngen_sims$time), 
                                             version = 2)$flow_cms_v2

ngen_sims = mutate(ngen_sims,
                   time = as.POSIXct(time),
                   cumNG = cumsum(replace_na(q_cms_sim, 0))/10000,
                   cumNWIS = cumsum(replace_na(q_cms_obs, 0))/10000,
                   cumNWM = cumsum(replace_na(NWM20_cms, 0))/10000)

write_parquet(ngen_sims, "data/obs_sim_retro20_flows_10244950.parquet")
