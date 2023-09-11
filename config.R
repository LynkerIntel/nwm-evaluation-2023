pacman::p_load(
  AOI,
  sf,
  ggplot2,
  patchwork,
  dplyr,
  zoo,
  RColorBrewer,
  grid,
  pals,
  secr,
  ggpubr,
  rstatix,
  plyr,
  BAMMtools,
  tidyr,
  arrow
)

options(warn=-1)

source('R/utils.R')

states  = aoi_get(state = 'conus') %>% st_transform(5070)
conus   = st_union(states)
conusbb = st_bbox(conus)

data = read_parquet("data/v20_summary_stats.parquet") %>%
  mutate(nseb_tmp = ifelse(NSEB > 1 , 1, NSEB),
         nsec_tmp = ifelse(NSEC > 1 , 1, NSEC),
         Water = ifelse(is.na(LU_1),  0, LU_1),
         Urban = ifelse(is.na(LU_2),  0, LU_2),
         Barren = ifelse(is.na(LU_3), 0, LU_3),
         Forest = ifelse(is.na(LU_4), 0, LU_4),
         Shrubland = ifelse(is.na(LU_5), 0, LU_5),
         Herbaceous = ifelse(is.na(LU_7), 0, LU_7),
         Agriculture = ifelse(is.na(LU_8), 0, LU_8),
         Wetland = ifelse(is.na(LU_9), 0, LU_9),
         `Total Storage` = (total_storage / 810700) ,
         `Number of Dams` = count_dams,
         `Mean PPT` = mean.ppt / 10,
         `Mean PET` = mean.et / 10,
         `Mean Aridity` = mean.ai,
         `Mean Phase Correlation` = mean.cor,
         `Snow Mean` = snow.mean / 10,
         `Impervious Percent` = imperv_per,
         `Snow Fraction` = snow.frac * 100)

cats = list(
  AREA    = c(0, 2500, 20000),
  `Number of Dams`  = round_any(getJenksBreaks(data$`Number of Dams`, 4), 50),
  `Total Storage` = round_any(getJenksBreaks(data$`Total Storage`, 4), 5),
  #total_surface = round_any(getJenksBreaks(data$total_surface, 4), 1000),
  #Water = round_any(getJenksBreaks(data$Water, 3), 1),
  Urban = round_any(getJenksBreaks(data$Urban, 3), 5),
  Barren = round_any(getJenksBreaks(data$Barren, 4), 5),
  Forest = round_any(getJenksBreaks(data$Forest, 4), 5),
  Shrubland = round_any(getJenksBreaks(data$Shrubland, 3), 5),
  Herbaceous = round_any(getJenksBreaks(data$Herbaceous, 4), 5),
  Agriculture = round_any(getJenksBreaks(data$Agriculture, 3), 5),
  Wetland = round_any(getJenksBreaks(data$Wetland, 4), 5),
  `Mean PPT` = c(0, 63.5, 120, 400),
  #round_any(getJenksBreaks(data$mean.ppt, 4), 5),
  `Mean PET` = round_any(getJenksBreaks(data$`Mean PET`, 5), 5),
  `Mean Phase Correlation` = c(-1, -.4, 0, .4, 1),
  `Mean Aridity` = c(0, 1, 2, 3, 20),
  `Snow Mean` = round_any(getJenksBreaks(data$`Snow Mean`, 4), 5),
  `Impervious Percent` = round_any(getJenksBreaks(data$`Impervious Percent`, 3), 5),
  `Snow Fraction` = round_any(getJenksBreaks(data$`Snow Fraction`, 4), 5)
)


tmp = data %>% 
  mutate(lng = round(LNG_GAGE)) %>% 
  group_by(class, lng) %>% 
  dplyr::summarize(medianNSEA1 = fivenum(NSEA)[2],
            medianNSEA  = fivenum(NSEA)[3],
            medianNSEA3 = fivenum(NSEA)[4],
            medianNSEB1 = fivenum(nseb_tmp)[2],
            medianNSEB  = fivenum(nseb_tmp)[3],
            medianNSEB3 = fivenum(nseb_tmp)[4],
            medianNSEC1 = fivenum(nsec_tmp)[2],
            medianNSEC  = fivenum(nsec_tmp)[3],
            medianNSEC3 = fivenum(nsec_tmp)[4],
            medianNNSE1 = fivenum(nnse)[2],
            medianNNSE  = fivenum(nnse)[3],
            medianNNSE3 = fivenum(nnse)[4]
  )

my_theme = 
  theme(legend.position = "right",
        strip.text.x = element_blank(),
        plot.margin= unit(c(0,0,0,0), "mm"),
        legend.text = element_text(size =18),
        legend.title  = element_text(size =24),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 36),
        plot.caption = element_text(size = 18))
