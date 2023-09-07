
ppt_files = readLines('/Volumes/Transcend/NLDAS2/ppt_download_list.txt')[-1]
length(ppt_files)
length(dates)

ppt_files[1]
fs::dir_create("/Volumes/Transcend/NLDAS2/pet_grids/")
fs::dir_create("/Volumes/Transcend/NLDAS2/ppt_grids/")
fs::dir_create("/Volumes/Transcend/NLDAS2/snow_grids/")

dates = seq.Date(as.Date("1993-01-01"), as.Date("2018-12-31"), by = "m")

library(httr)

for (i in 1:length(ppt_files)) {
  
  hope = gsub("FORA0125", "VIC0125", ppt_files[i])
  hope = gsub("APCP", "ASNOW", hope)
  filename <-   paste0("/Volumes/Transcend/NLDAS2/snow_grids/", dates[i], "_ppt.tif")

  response <- httr::GET(hope, write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = '/Users/mikejohnson/.netrc'), set_cookies("LC" = "cookies"))
}


prcp = list.files("/Volumes/Transcend/NLDAS2/ppt_grids/", full.names = TRUE)
et   = list.files("/Volumes/Transcend/NLDAS2/pet_grids/", full.names = TRUE)

bas = read_sf("/Users/mikejohnson/github/nwm-evaluation/data/shp_basins/basins.shp") %>%
  st_transform(st_crs(ppt))

bas = bas[!st_is_empty(bas),,drop=FALSE]

etstack = raster::stack(et)
pptstack = raster::stack(prcp)


et_ts = list()
ppt_ts = list()

for(i in 1:nlayers(pptstack)){
  #et_ts[[i]]  = exactextractr::exact_extract(etstack[[i]], bas, "mean")
  ppt_ts[[i]] = exactextractr::exact_extract(pptstack[[i]], bas, "mean")
  message(i)
}

info = read.csv('/Users/mikejohnson/github/nwm-evaluation/data/info_stations.txt') %>%
  mutate(siteID = sprintf("%08s", siteID))

csv  = read.csv('/Users/mikejohnson/github/nwm-evaluation/data/Verification_Metrics_overall_v2') %>%
  mutate(siteID = sprintf("%08s", siteID))

dat = merge(info, csv) %>%
  mutate(region = ifelse( HUC2 < 14, "east", "west") ) %>%
  merge(bas) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

saveRDS(dat, '/Users/mikejohnson/github/nwm-evaluation/data/outlets-with-correlation.rds')

library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-1, 1))

ggplot() +
  geom_sf(data = dat, aes(col = cor)) +
  sc

i = 50

bet = do.call(rbind, et_ts)
bppt = do.call(rbind, ppt_ts)
dim(bet)
bas$cor = NA
for(i in 1:nrow(bas)){
  bas$cor[i] = cor.test(bet[,i], bppt[,i], method = 'spearman')$estimate
}

c$estimate
mppt = mean(pptstack)
met  = mean(etstack)

hist(bas$cor, main = "Spearman")
length(et_ts)

ids = rep(1:26, each = 12)

corlayer = raster::corLocal(pptstack, etstack, method = "spearman")


plot(corlayer)
pptsum <- stackApply(pptstack, ids, fun = sum)
etsum <- stackApply(etstack, ids, fun = sum)

ai = mean(etsum) / mean(pptsum)
ai[ai > 3] = 3
plot(ai)



writeRaster(corlayer, '/Users/mikejohnson/github/src-evaluation/data/corlayer.tif')
writeRaster(ai, '/Users/mikejohnson/github/src-evaluation/data/ailayer.tif', overwrite = TRUE)

corlayer = raster::raster('/Users/mikejohnson/github/src-evaluation/data/corlayer.tif')
ailayer = raster::raster('/Users/mikejohnson/github/src-evaluation/data/ailayer.tif')

simp$cor = exactextractr::exact_extract(corlayer, simp, "mean")
simp$ai = exactextractr::exact_extract(ailayer, simp, "mean")

plot(hucs2['cor'])

ai = ailayer

ai = raster::reclassify(ailayer, c(-Inf,1,3, 
                                    1,2,2, 
                                    2,Inf,1), 
                         include.lowest = TRUE)

cor = raster::reclassify(corlayer, c(-Inf,-.2,3, 
                                    -.2,.2,2, 
                                    .2,Inf,1), 
                         include.lowest = TRUE)

ai = raster::mask(ai, st_transform(HUC22, st_crs(ai)))
cor = raster::mask(cor, st_transform(HUC22, st_crs(cor)))
corp = spex::qm_rasterToPolygons(cor)
aip = spex::qm_rasterToPolygons(ai)
corp$bi_scale = paste0(corp$corlayer, "-", aip$ailayer)

corpp = corp %>% group_by(bi_scale) %>% summarise()
c2 = st_transform(corpp, 5070)
map <- ggplot() +
  geom_sf(data = c2, mapping = aes(fill = bi_scale), col = "white", size = 0.01, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(
    title = "NWM Confidence",
    subtitle = "Aridity and Phase Timing"
  ) +
  bi_theme()

map

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "More Out of Phase ",
                    ylab = "More Humid ",
                    size = 15) + 
  geom_rect(aes(xmin=1.5, xmax=3.5, ymin=1.5, ymax=3.5), color = "black", fill = NA, size = 2, inherit.aes = FALSE)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .05, 0.2, 0.2)
finalPlot

fin = ai
fin[!is.na(fin)] = factor(corp$bi_scale)


table(dat$CLASS)
dim(corp)
dat$tmp = levels(factor(corp$bi_scale))[raster::extract(fin, dat)]

table(dat$tmp, dat$NSE_A >.5) %>% 
  data.frame() %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(tot = `TRUE` + `FALSE`, good =100* `TRUE` / tot)



point_in_polygon3 = function(points, polygon, var){
  st_join(polygon, points) %>% 
    st_drop_geometry() %>% 
    count(get(var)) %>% 
    setNames(c(var, "n")) %>%
    left_join(polygon, by = var) %>% 
    st_as_sf() 
}


corp  = corp %>% mutate(id = 1:n())
pip = point_in_polygon3(st_transform(damssf, st_crs(corp)), corp, "id")

d = ai
d[!is.na(d)] = factor(pip$n)

e

raster::plot(d)
library(sf)
library(dplyr)
hucs = st_read( '/Volumes/Transcend/WBD_National_GDB/WBD_National_GDB.gdb', layer = 'WBDHU8')
hucs2 = st_make_valid(hucs)

huc2 = hucs2 %>% 
  select(huc8) %>% 
  mutate(huc2 = as.numeric(substr(huc8,1,2))) %>% 
  filter(huc2 <=18) %>% 
  st_transform(5070)

out = list()

for(i in 1:18){
  tmp = filter(huc2, huc2 == !!i)
  tmp = rmapshaper::ms_simplify(tmp, keep = .01)
  out[[i]] = st_intersection(tmp,usa)
  print(i)
}

simp = do.call(rbind, out)

simp$cor = exactextractr::exact_extract(corlayer, simp, "mean")
simp$ai  = exactextractr::exact_extract(ailayer, simp, "mean")

library(raster)
proj4_string = as.character(crs(ai))
te = as.numeric(st_bbox(ai))
ts = c(ncol(ai), nrow(ai))
gdalUtilities::gdalwarp('/Volumes/Transcend/NLCD_Impervious/NLCD_2016_Impervious_L48_20190405.img',
                        '/Users/mikejohnson/github/src-evaluation/data/imperv.tif',
                        te = te,
                        t_srs = proj4_string,
                        ts = ts,
                        r = 'bilinear')


imp = raster('/Users/mikejohnson/github/src-evaluation/data/imperv.tif') 
#plot(imp)
imp[is.na(imp)] = 0


imp = raster::reclassify(imp, c(-Inf,10,1, 
                                   10,25,2, 
                                   25,Inf,3), 
                        include.lowest = TRUE)
imp = raster::mask(imp, st_transform(usa, st_crs(imp)))
#plot(imp)

d = ai
d[!is.na(d)] = factor(pip$n)
d[is.na(d)] = 0
dams = raster::reclassify(d, c(-Inf,5,1, 
                                5,15,2, 
                                15,Inf,3), 
                         include.lowest = TRUE)
usa = st_transform(usa, st_crs(imp))
dams = raster::mask(dams, usa)
#plot(dams)

ss = stack(imp, dams) %>% 
  mask(st_buffer(usa, -.1))
plot(ss)

p = spex::qm_rasterToPolygons(ss)
p$bi_class = paste0(p$imperv, "-", p$ailayer)

p2 = p %>% group_by(bi_class) %>% summarise()
p3 = st_transform(p2, 5070)
unique(p2$bi_class)

map <- ggplot() +
  geom_sf(data = p3, mapping = aes(fill = bi_class), col = "white", size = 0.01, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  labs(
    title = "NWM Confidence",
    subtitle = "Percent Impervious and NID Storage", x = "", y = "")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "More Impervious",
                    ylab = "More Dams",
                    size = 15) 
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .05, 0.2, 0.2)






plot(nlcd)
simp2 = simp %>% 
  mutate(cor2 = case_when(
    cor < -0.2 ~ '3',
    cor > .2 ~ '1',
    between(cor, -.2, .2) ~'2'
  ),
  ai2 = case_when(
    ai < 1 ~ '3',
    ai > 2 ~ '1',
    between(ai, 1, 2) ~'2'
  ),
  bi_class = paste0(cor2, "-", ai2)) %>% 
  st_make_valid()



usa = AOI::aoi_get(state = "conus", union = TRUE) %>% 
  st_transform(5070) %>% st_cast("POLYGON") %>% st_as_sf() %>%  
  mutate(area = as.numeric(st_area(.))) %>% 
  slice_max(area, n = 1)


map <- ggplot() +
  geom_sf(data = simp2, mapping = aes(fill = bi_class), col = "white", size = 0.01, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(
    title = "NWM Confidence",
    subtitle = "Aridity and Phase Timing"
  ) +
  bi_theme() +
  geom_sf(data = HUC22, col = "black", size = 2, fill = NA) 

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "More Out of Phase ",
                    ylab = "More Humid ",
                    size = 15) + 
  geom_rect(aes(xmin=1.5, xmax=3.5, ymin=1.5, ymax=3.5), color = "black", fill = NA, size = 2, inherit.aes = FALSE)

library(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .05, 0.2, 0.2)
finalPlot


dams = readxl::read_xlsx('/Users/mikejohnson/Dropbox/NID2019.xlsx')

damssf = dams %>% 
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  select(NID_STORAGE) %>% 
  st_transform(5070)

damssf2 = st_join(damssf, simp2, .predicate = st_intersects)

damssf3 = damssf2 %>% 
  st_drop_geometry() %>% 
  group_by(huc8) %>% 
  summarize(storage = sum(NID_STORAGE, na.rm = TRUE), n = n()) %>% 
  mutate(n = log(n))

simp3$n = left_join(simp3, damssf3)

nlcd = raster::raster('/Volumes/Transcend/NLCD_Impervious/NLCD_2016_Impervious_L48_20190405.img')
simp3$imperv_per = exactextractr::exact_extract(nlcd, simp3, "mean")
simp3$areakm2 = as.numeric(st_area(simp3)/1e6)
simp3$impervkm2 = simp3$imperv_per * simp3$areakm2
simp3$imperv_log = log(simp3$imperv_per)
simp3$storage = ifelse(is.na(simp3$storage), 0, simp3$storage)
simp3$n = ifelse(is.na(simp3$n), 0, simp3$n)
simp3$storage_log = sqrt(simp3$storage)
simp3$storage_log = pmax(0,simp3$storage_log)
simp3$impervkm2_log = sqrt(simp3$impervkm2 )

simp3$n_log = log(simp3$n )
simp3$n_log = pmax(0,simp3$n_log)

data <- bi_class(simp3, x = storage_log, y = impervkm2, style = "jenks", dim = 3)

# cities = read.csv('/Users/mikejohnson/github/lab-05-W/data/uscities.csv') %>% 
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
#   filter(population > 2500000) %>% 
#   st_transform(5070) %>% 
#   st_filter(HUC2)
# 

dat2 = st_transform(dat,5070)
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), col = "white", size = 0.01, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
 # geom_sf(data = cities, col = 'black') +
 # ggrepel::geom_label_repel(data = cities, aes(label = city, geometry = geometry),
 #                           stat = "sf_coordinates") +
  bi_theme() +
  labs(
    title = "NWM Confidence",
    subtitle = "Percent Impervious and NID Storage", x = "", y = "") +
  geom_sf(data = HUC22, col = "black", size = .8, fill = NA) +
  geom_sf(data = filter(dat2, NSE_B > .2), size = .15, col = "gray10")


test = st_join(dat2,simp3)

tab = table(test$NSE_A > .50, test$bi_class) %>% 
  data.frame() %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(tot = `TRUE` + `FALSE`, good =100* `TRUE` / tot)

plot(tab$Var2, tab$good)
abline(v = 3.5)
abline(v = 6.5)

test = st_join(dat2,data)

tab = table(test$NSE_B < .10, test$bi_class) %>% 
  data.frame() %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(tot = `TRUE` + `FALSE`, good =100* `TRUE` / tot)

plot(tab$Var2, tab$good)
abline(v = 3.5)
abline(v = 6.5)


tab = table(test$NSE_A > .50, test$bi_class) %>% 
  data.frame() %>% 
  pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(tot = `TRUE` + `FALSE`, good =100* `TRUE` / tot)

ggplot(data = tab, aes(x = Var2, y = Freq, group = Var1, col = Var1)) + geom_line()

plot(tab)

map

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "More Storage ",
                    ylab = "More Impervious ",
                    size = 15) + 
  geom_rect(aes(xmin=0.5, xmax=2.5, ymin=0.5, ymax=2.5), color = "black", fill = NA, size = 2, inherit.aes = FALSE)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .05, 0.2, 0.2)
finalPlot


cities

######
simp2$storageMAF = simp2$storage/1e6
brks = BAMMtools::getJenksBreaks(simp2$impervkm2, 5)
#brks = BAMMtools::getJenksBreaks(simp2$storageMAF, 5)


simp3 = simp2 %>% 
  mutate(store = case_when(
    storageMAF < 1 ~ '1',
    storageMAF > 3 ~ '3',
    between(storageMAF, 1, 3) ~'2'
  ),
  imperv2 = case_when(
    impervkm2 < brks[2] ~ '1',
    impervkm2 > brks[4] ~ '3',
    between(impervkm2, brks[2], brks[4]) ~'2'
  ),
  bi_class = paste0(store, "-", imperv2)) %>% 
  st_make_valid()

simpl


#simp4 = simp3 %>% group_by(bi_class) %>% summarise()
table(simp3$store)
table(simp3$imperv2)

map <- ggplot() +
  geom_sf(data = simp3, mapping = aes(fill = storageMAF), col = "white", size = 0.01, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(
    title = "NWM Confidence",
    subtitle = "Aridity and Phase Timing"
  ) +
  bi_theme()


map <- ggplot() +
  geom_sf(data = simp3, mapping = aes(fill = log(storageMAF)), col = "white", size = 0.01, show.legend = FALSE) +
  #bi_scale_fill(pal = "DkCyan", dim = 3) +
  labs(
    title = "NWM Confidence",
    subtitle = "Aridity and Phase Timing"
  ) +
  geom_sf(data = HUC22, col = "black", size = 3, fill = NA) + 
  bi_theme()

map

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "More Storage",
                    ylab = "More Imperviousness",
                    size = 15)

library(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .05, 0.2, 0.2)
finalPlot

HUC22

dat %>% 
  group_by(CLASS, region) %>% 
  slice_max(NSE_B, n = 3) %>% 
  select(siteID, region, CLASS, NSE_A, NSE_B, NSE_C)


