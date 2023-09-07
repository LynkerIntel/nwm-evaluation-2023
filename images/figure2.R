source('config.R')

nnse1 = ggplot() + 
  geom_jitter(data = data, aes(x = X, y = Y, color = nnse), size = 1.5, alpha = .35, pch = 16) + 
  scale_color_gradient2(low = warmcool(10)[1], high = warmcool(10)[10],
                        mid = warmcool(10)[6], midpoint = .66)+
  facet_grid(~class) +
  geom_sf(data = conus, fill = NA, size = .35, color = "black") +
  geom_sf(data = states, fill = NA, size = .15, color = "black") +
  theme_void() + 
  coord_sf(xlim = c(conusbb[1], conusbb[3]), ylim = c(conusbb[2], conusbb[4]), expand = FALSE) +
  labs(x = NULL, y = NULL,
       color = "NNSE") +
  theme(legend.position = "right",
        strip.text.x = element_text(size = 24),
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

nnse2 = ggplot(tmp) + 
  geom_line(aes(x = lng, y = rollmean(medianNNSE1, k = 5, fill = NA), 
                color = "25th Percentile"),linewidth = 1)+
  geom_line(aes(x = lng, y = rollmean(medianNNSE, k = 5, fill = NA), 
                color = "50th Percentile"), linewidth = 1)+
  geom_line(aes(x = lng, y = rollmean(medianNNSE3, k = 5, fill = NA),  
                color = "75th Percentile"), linewidth = 1) + 
  scale_color_manual(values = brewer.pal(3, "Dark2")) + 
  facet_grid(~class) +
  ylim(0,1) +
  theme_minimal() + 
  geom_hline(yintercept = c(.66, .74, .8 )) +
  labs(color = "", x = 'Longitude', y = "NNSE",
       caption = "5 degree rolling mean") + 
  theme(legend.position = "right",
        strip.text.x = element_blank(),
        plot.margin=  unit(c(0,0,0,0), "mm"),
        legend.text = element_text(size =18),
        legend.title  = element_text(size =24),
        axis.text = element_text(size = 18),
        plot.caption = element_text(size = 18))

str = paste(data$ai, data$class)

nnse3 = ggplot(data = data, aes(x = nnse, color = str)) +
  annotate('text', x = .95, y = .5, label = 'very good', fontface = 'bold', angle = 90, size =12, alpha = .15) + 
  annotate('text', x = .77, y = .5, label = 'good', fontface = 'bold', angle = 90, size = 12, alpha = .15) + 
  annotate('text', x = .7, y = .5, label  = 'satisfactory', fontface = 'bold', angle = 90, size = 12, alpha = .15) + 
  annotate('text', x = .6, y = .5, label  = 'unsatisfactory', fontface = 'bold', angle = 90, size = 12, alpha = .15) +
  stat_ecdf(aes(color = str), geom = "step", linewidth = 1, pad = FALSE) +
  scale_color_manual(breaks = unique(str), values = c("blue", "navy", "red", "darkred"))+
  labs(y = "f(NNSE)", title = "Cumulative Distribution of NWM NNSE",
       color = "",
       subtitle = "Qualitative descriptions according to Moriasi, 2007"
  ) +
  geom_vline(xintercept = 1/(2 - .75)) + 
  geom_vline(xintercept = 1/(2 - .65)) + 
  geom_vline(xintercept = 1/(2-.5)) + 
  theme_bw(12) +
  scale_x_continuous(breaks = c(0, .25, .5, 1/(2-.5), 1/(2 - .65), 1/(2 - .75)), labels = c(0, .25, .5, .66, .74, .8),
                     name = "Normalized NSE",
                     sec.axis=sec_axis(trans=~2 - 1/., name="NSE",
                                       breaks = c(-1000, 0, .5,.65, .75), labels = c("-14,000", 0, .5,.65, .75))) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "right",
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 24),
        plot.subtitle =  element_text(size = 18)) +
  guides(color = guide_legend(override.aes = list(size = 4),
                              #label.position = "bottom",
                              title.position = "top", title.vjust = 1))

p = nnse1/nnse2/nnse3 + 
  plot_layout(heights = c(1,1,2)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size  = 24))

ggsave(p, filename = "images/png/jgr-fig-2.png", height=18, width=16, units="in",  dpi=600)
