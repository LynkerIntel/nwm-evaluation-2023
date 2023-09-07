find_significant = function(input, n, metric, type, cats) {
  all = list()
  
  for (i in 1:length(cats)) {
    tmp = select(input, !!metric, data = !!names(cats)[i]) %>%
      filter(!is.na(data)) %>%
      mutate(cut = cut(data, breaks = cats[[i]], include.lowest = TRUE)) %>%
      filter(!is.na(cut))
    
    suppressMessages({
      xx = tryCatch({
        anova_test(tmp,
                   as.formula(paste0(metric, " ~ cut")),
                   effect.size = "ges",
                   detailed = TRUE) %>%
          data.frame() %>%
          filter(ges >= .01) %>%
          mutate(test = names(cats)[i], type = type)
        
      }, error = function(e) {
        NULL
      })
    })
    
    all[[i]] = xx
  }
  
  good = bind_rows(all) %>%
    mutate(
      effect_size = case_when(
        ges < .01 ~ "None",
        between(ges, .01, .06) ~ "Small",
        between(ges, .06, .14) ~ "Medium",
        ges > .14 ~ "Large"
      )
    ) %>%
    mutate(p_effect = case_when(
      between(p, .05, 1) ~ "*",
      between(p, .01, .05) ~ "**",
      p <= .001 ~ "***",
    )) %>%
    select(test, type, p, ges, effect_size, p_effect) %>%
    arrange(-ges)
  
  good
  
}

plot_anova_charts_bar = function(data, metric, minMetric, ymax, cats){
  
  nat  = find_significant(input= filter(data, class == "Natural", get(metric) < minMetric), cats = cats, metric = metric, type =  "Natural") 
  cont = find_significant(filter(data, class == "Controlled", get(metric) < minMetric), cats = cats, metric = metric, type =  "Controlled") 
  
  testers = bind_rows(nat, cont)
  
  uni = testers %>% 
    group_by(test) %>% 
    slice_max(ges) %>% 
    ungroup() %>% 
    arrange(-ges) %>% 
    mutate(title_color = case_when(
      effect_size == "Large" ~"darkgreen",
      effect_size == "Medium" ~"orange",
      effect_size == "Small" ~"brown"
    ))
  
  message(nrow(uni), " variables!")
  
  plots = list()
  
  for(i in 1:nrow(uni)){
    var = uni$test[i]
    
    curr = filter(testers, test == var)
    
    if(nrow(curr) == 3){
      pos = "bottom"
    } else {
      pos = "none"
    }
    
    full_data = bind_rows(data, mutate(data, class = "All")) %>% 
      filter(class %in% curr$type) %>% 
      filter(get(metric) < minMetric) %>% 
      select(class, metric,data = var) %>% 
      mutate(cut = cut(data, breaks = cats[[var]], include.lowest = TRUE)) %>% 
      filter(!is.na(cut)) %>% 
      mutate(class = factor(class, levels = c("All", "Natural", "Controlled")))
    
    tmp = suppressMessages({
      full_data %>% 
        group_by(class) %>% 
        dplyr::mutate(n_class = n()) %>% 
        ungroup() %>% 
        group_by(class, cut) %>% 
        dplyr::summarise(n = n(), n_class = n_class[1]) %>% 
        ungroup() %>% 
        mutate(per = n/n_class) %>% 
        filter(per < .05)
    })
    
    if(nrow(tmp) > 0){
      warning(var, " yeilds ", nrow(tmp), " groups with less then 5%\n")
    } else {
      res.aov =
        suppressMessages({
          anova_test(filter(full_data, class == uni$type[i]), 
                     as.formula(paste0(metric, ' ~  + cut')))
        })
      
      plots[[length(plots) + 1]] = ggbarplot(
        full_data, 
        x = "cut", 
        y = metric, 
        color = NA, fill = 'class', alpha = .7,
        position = position_dodge(0.7),
        add  = c('mean_ci'), ylim = c(0,ymax)
      ) + 
        scale_fill_manual(values =  brewer.pal(n = 3, name = "Dark2"), 
                          breaks = c("All", "Natural", "Controlled")) +
        geom_hline(yintercept = mean(full_data[[metric]]), lwd = 1) +
        labs(
          title = paste0(letters[i], ": ", gsub("`","",var)),
          subtitle = get_test_label(res.aov, detailed = TRUE),
          color = "Class"
        ) +
        theme(plot.title = element_text(face = "bold", size = 28, color = uni$title_color[i]),
              plot.subtitle = element_text(size = 18),
              axis.text = element_text(size = 24),
              axis.title = element_text(size = 24),
              legend.text = element_text(size = 24),
              legend.title = element_text(size = 24),
              legend.position   = pos,
              legend.key.height  = unit(1, 'cm'),
              legend.key.width  = unit(2, 'cm')) + 
        labs(fill = '', x = "")
      
    }
    }

  plots
  
}
    