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


g1 = readr::read_csv('data/ngen-experiment/Info_AllModels.csv') %>% 
  select(Aridity_ratio, AridityNG, PET_Model,  GAGE_ID = ...1, A_Best1) %>% 
  left_join(data) %>% 
  filter(!is.na(PET_Model)) %>% 
  mutate(#Aridity_ratio = ifelse(Aridity_ratio > 2, 2, Aridity_ratio),
    my_ai_ratio= AridityNG / mean.ai,
    #my_ai_ratio = ifelse(my_ai_ratio > 2, 2, my_ai_ratio)
  ) %>% 
  filter(!is.na(my_ai_ratio)) %>% 
  group_by(GAGE_ID)  %>% 
  mutate(my_best = ifelse(any(my_ai_ratio > 1), PET_Model[which(my_ai_ratio == min(my_ai_ratio[my_ai_ratio > .99]))], PET_Model[which.max(my_ai_ratio)])) %>% 
  mutate(my_best = case_when(
    my_best == 'NOAH' ~ "Noah OWP",
    my_best == 'PET_1' ~ "Energy Balance",
    my_best == 'PET_2' ~ "Aerodynamic",
    my_best == 'PET_3' ~ "Combined",
    my_best == 'PET_4' ~ "Priestley-Taylor",
    my_best == 'PET_5' ~ "Penman-Monteith"
  ),
  PET_Model = case_when(
    PET_Model == 'NOAH' ~ "Noah OWP",
    PET_Model == 'PET_1' ~ "Energy Balance",
    PET_Model == 'PET_2' ~ "Aerodynamic",
    PET_Model == 'PET_3' ~ "Combined",
    PET_Model == 'PET_4' ~ "Priestley-Taylor",
    PET_Model == 'PET_5' ~ "Penman-Monteith"
  ),
  A_Best1 = case_when(
    A_Best1 == 'NOAH' ~ "Noah OWP",
    A_Best1 == 'PET_1' ~ "Energy Balance",
    A_Best1 == 'PET_2' ~ "Aerodynamic",
    A_Best1 == 'PET_3' ~ "Combined",
    A_Best1 == 'PET_4' ~ "Priestley-Taylor",
    A_Best1 == 'PET_5' ~ "Penman-Monteith"
  )) %>% 
  ungroup() %>% 
  mutate(ring = ifelse(PET_Model == my_best, TRUE, FALSE)) %>% 
  filter(GAGE_ID == 10244950)  %>% 
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


ngen_sims = arrow::read_parquet("data/ngen-experiment/obs_sim_retro20_flows_10244950.parquet")

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
