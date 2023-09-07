source('config.R')

g0 = ggplot(data = filter(data, GAGE_ID == 10244950)) +
  geom_jitter(aes(x = X, y = Y), color = "red", size = 4, alpha = 1, pch = 16) + 
  geom_sf(data = conus, fill = NA, size = .35, color = "black") +
  geom_sf(data = states, fill = NA, size = .15, color = "black")  + 
  labs(title = "Gage 10244950", 
       subtitle = "Area (sqkm): 28.2") + 
  theme_minimal() + 
  labs(x = "", y = "") +
  theme(axis.text = element_text(size =18))

g1 = data.frame(
  PET_Model     = c("Noah OWP", "Energy Balance",  "Aerodynamic", "Combined", "Priestley-Taylor", "Penman-Monteith"),
  Aridity_ratio = c(.48,        .95,                1.02,          .85,        .82,                1.18)
) %>% 
  ggplot() + 
  geom_col(aes(x = PET_Model, y = Aridity_ratio, fill = PET_Model)) + 
  geom_hline(yintercept = 1,size = 2) +
  scale_fill_manual(values=c("darkgreen",
                             "darkblue",
                             "darkblue",
                             "darkred",
                             "darkblue",
                             "darkblue")) + 
  scale_y_continuous(breaks = seq(0, 1.25, by = .25),limits=c(0, 1.25)) +
  labs(x = "PET Method", y = "NextGen AI / Long Term AI",
       title = "Impact of PET Method on NextGen Aridity") +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 14, face = "bold", angle = 15, vjust = .7, hjust=.5),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(size=16,face="bold"))


ngen_sims = read.csv('/Users/mjohnson/github/nwm-evaluation/fin/10244950_PET_2_CFE_X.csv') 

ngen_sims$NWM20_cms =  nwmTools::readNWMdata(comid = 11339045, 
                                             startDate = min(ngen_sims$time), 
                                             endDate = max(ngen_sims$time), 
                                             version = 2)$flow_cms_v2


ngen_sims = mutate(ngen_sims,
                   time = as.POSIXct(time),
                   cumNG = cumsum(replace_na(q_cms_sim, 0))/10000,
                   cumNWIS = cumsum(replace_na(q_cms_obs, 0))/10000,
                   cumNWM = cumsum(replace_na(NWM20_cms, 0))/10000,
                   )

g2 = ggplot(data = ngen_sims) +
  geom_line(aes(x = time, y = cumNG , color = "NextGen Simulation"), size = 2) +
  geom_line(aes(x = time, y = cumNWIS, color = "Observed NWIS"), size = 2)+
  geom_line(aes(x = time, y = cumNWM, color = "NWM 2.0 Reanalysis"), size = 2) +
  theme_bw() + 
  scale_size(guide = "none") +
  labs(x = "", y = "Cumulative Q (m)", color = "",
       title = "Comparing NextGen, NWM2.0 and NWIS",
       subtitle = "NWIS Site ID: 10244950") + 
  scale_color_manual(values = c("darkgreen", "darkred", "black")) +
  theme(#legend.position = "bottom",
        axis.title  = element_text(size = 18),
        plot.title = element_text(size = 24),
        plot.subtitle  = element_text(size = 16),
        axis.text = element_text(size =12, face = "bold"),
        strip.text.x = element_text(size = 24),
        plot.margin= unit(c(0, 0, 0, 0), "null"),
        legend.text = element_text(size =18),
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.position = c(.01,.95)
  ) 


p1 = (g0 + g1) / g2  + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size  = 36))

ggsave(p1, filename = "images/png/jgr-fig-6.png", dpi = 300, width = 16, height = 12, units ="in")
