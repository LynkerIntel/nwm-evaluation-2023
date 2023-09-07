source("config.R")

nsea1 = ggplot() + 
  geom_jitter(data = data, aes(x = X, y = Y, color = NSEA), size = 4, alpha = .45, pch = 16) + 
  scale_color_gradient2(low = warmcool(10)[1], high = warmcool(10)[10],
                        mid = warmcool(10)[6], midpoint = .5)+
  facet_grid(~class) +
  geom_sf(data = conus, fill = NA, size = .35, color = "black") +
  geom_sf(data = states, fill = NA, size = .15, color = "black") +
  theme_void() + 
  coord_sf(xlim = c(conusbb[1], conusbb[3]), ylim = c(conusbb[2], conusbb[4]), expand = FALSE) +
  labs(x = NULL, y = NULL,
       color = "NSEA") +
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

nsea2 = ggplot(tmp) + 
  geom_line(aes(x = lng, y = rollmean(medianNSEA1, k = 5, fill = NA), 
                color = "25th Percentile"),size = 2)+
  geom_line(aes(x = lng, y = rollmean(medianNSEA, k = 5, fill = NA), 
                color = "50th Percentile"), size = 2)+
  geom_line(aes(x = lng, y = rollmean(medianNSEA3, k = 5, fill = NA),  
                color = "75th Percentile"), size = 2) + 
  scale_color_manual(values = brewer.pal(3, "Dark2")) + 
  facet_grid(~class) +
  ylim(0,1) +
  theme_minimal() + 
  geom_hline(yintercept = .5) +
  labs(color = "", x = 'Longitude (5 degree rolling mean)', y = "NSE-A"
  ) + 
  theme(legend.position = "right",
        strip.text.x = element_blank(),
        plot.margin= unit(c(0,0,0,0), "mm"),
        legend.text = element_text(size =18),
        legend.title  = element_text(size =24),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 36),
        plot.caption = element_text(size = 18))


nsea3 = plot_anova_charts_bar(data, metric = "NSEA", minMetric = 1, ymax = .7, cats) 
nsea3 = ggarrange(plotlist = nsea3, nrow = 4, ncol = 3, common.legend = TRUE, legend = "bottom")

pt = nsea1/nsea2 

p = pt / plot_spacer() / nsea3 +
  plot_layout(heights = c(1,1,.25, 4)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size  = 36))

ggsave(p, filename = "images/png/jgr-fig-3.png", dpi = 300, width = 24, height = 36, units ="in")
