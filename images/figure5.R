source('config.R')

t = filter(data, nsec_tmp < 1)

test = data %>% 
  group_by(class) %>% 
  dplyr::summarize(bad_nseb = sum(NSEB > 1), 
            bad_nsec = sum(NSEC > 1))  %>% 
  mutate(NSEB_label = paste0(class, "\n(", bad_nseb, " basins dropped)"),
         NSEC_label = paste0(class, "\n(", bad_nsec, " basins dropped)"))


t = left_join(t, test, by = "class")

nsec1 = ggplot() + 
  geom_jitter(data = t, aes(x = X, y = Y, color = nsec_tmp), size = 4, alpha = .45, pch = 16) + 
  scale_color_gradient2(low  = warmcool(3)[3], 
                        high = warmcool(3)[1],
                        mid  = warmcool(3)[2], 
                        midpoint = .2) +
  facet_grid(~NSEC_label) +
  geom_sf(data = conus, fill = NA, size = .35, color = "black") +
  geom_sf(data = states, fill = NA, size = .15, color = "black") +
  theme_void() + 
  coord_sf(xlim = c(conusbb[1], conusbb[3]), ylim = c(conusbb[2], conusbb[4]), expand = FALSE) +
  labs(x = NULL, y = NULL,
       color = "NSEC") +
  theme(legend.position = "right",
        strip.text.x = element_text(size = 36, face = "bold"),
        plot.margin= unit(c(0, 0, 0, 0), "null"),
        legend.text = element_text(size =18),
        legend.title  = element_text(size =24)
  )+ 
  guides(color  = guide_colorbar(
    title.position = "top", title.vjust = 1,
    # draw border around the legend
    frame.colour = "black",
    barwidth = 1,
    barheight = 15))

nsec2 = ggplot(tmp) + 
  geom_line(aes(x = lng, y = rollmean(medianNSEC1, k = 5, fill = NA), 
                color = "25th Percentile"),size = 2)+
  geom_line(aes(x = lng, y = rollmean(medianNSEC, k = 5, fill = NA), 
                color = "50th Percentile"), size = 2)+
  geom_line(aes(x = lng, y = rollmean(medianNSEC3, k = 5, fill = NA),  
                color = "75th Percentile"), size = 2) + 
  scale_color_manual(values = brewer.pal(3, "Dark2")) + 
  facet_grid(~class) +
  theme_minimal() + 
  ylim(0,1) +
  geom_hline(yintercept = .5) +
  labs(color   = "", 
       x = 'Longitude', y = "NSEC",
       caption = "5 degree rolling mean") + 
  my_theme
  


nsec3 = plot_anova_charts_bar(data, metric = "NSEC", 1, .3, cats)
nsec3 = ggarrange(plotlist = nsec3, nrow = 4, ncol = 3, common.legend = TRUE, legend = "bottom")

pt = nsec1/nsec2 

p = pt / plot_spacer() / nsec3 + 
  plot_layout(heights = c(1,1,.25, 4)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size  = 36))

ggsave(p, filename = "images/png/jgr-fig-5.png", dpi = 300, width = 24, height = 36, units ="in")