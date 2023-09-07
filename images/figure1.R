source('config.R')

n = 30
pts = sample(1:100, n)
rand =sample(seq(-10, 10, by = .1), n)


p1 = ggplot(data = data.frame(x = pts, y = pts), 
              aes(x = x, y = y)) + 
    geom_abline(slope = 1, intercept = 0, col = 'red',lwd = 2, alpha = .2) + 
    geom_abline(slope = 1, intercept = 0, col = 'blue') + 
    geom_point(size = .5) + 
    ylim(c(0,100)) + 
    xlim(c(0,100)) + 
    labs(title = "A. Perfect Simulation",
         subtitle = "A = 1, B = 0, C = 0", x = "", y = "Simulated") + 
    theme_bw()
  
p2 = ggplot(data = data.frame(x = pts + rand, y = pts), 
              aes(x = x, y = y)) + 
    geom_abline(slope = 1, intercept = 0, col = 'red',lwd = 2, alpha = .2) + 
    geom_abline(slope = 1, intercept = 0, col = 'blue') + 
    geom_point(size = .5) + 
    ylim(c(0,100)) + 
    xlim(c(0,100)) + 
    labs(title = "B. Imperfect Correlation",
         subtitle = paste("A <",  round(cor(pts, pts+rand)^2,2), " , B = 0, C = 0"), x = "", y = "") + 
    theme_bw()

temp <- cbind(pts,pts)
temp2 <- data.frame(secr::rotate(cbind(pts,pts), -20, centrexy = c(50,50))) %>% 
    setNames(c('x','y'))
  
p3 = ggplot(data = temp2, 
              aes(x = x, y = y)) + 
    geom_abline(slope = 1, intercept = 0, col = 'red',lwd = 2, alpha = .2) + 
    geom_abline(slope = 2.145, intercept = -57.225 , col = 'blue') + 
    geom_point(size = .5) + 
    ylim(c(0,100)) + 
    xlim(c(0,100)) + 
    labs(title = "C. Conditional Bias Present",
         subtitle = "A = 1, B > 0, C = 0", x = "Observed", y = "Simulated") + 
    theme_bw() +
    geom_curve(
      aes(xend = 62, yend = 75, x = 68, y = 68),
      arrow = arrow(
        length = unit(0.02, "npc"), 
        type="open" # Describes arrow head (open or closed)
      ),
      colour = "black",
      linewidth = 1,
      angle = -90 # Anything other than 90 or 0 can look unusual
    )
  
p4 = ggplot(data = data.frame(x = pts, y = pts + 20), 
              aes(x = x, y = y)) + 
    geom_abline(slope = 1, intercept = 0, col = 'red',lwd = 2, alpha = .2) + 
    geom_abline(slope = 1, intercept = 20, col = 'blue') + 
    geom_point(size = .5) + 
    ylim(c(0,100)) + 
    xlim(c(0,100)) + 
    labs(title = "D. Unconditional Bias Present",
         subtitle = "A = 1, B = 0, C > 0", x = "Observed", y = "") + 
    theme_bw()  + 
    geom_curve(
      aes(xend = 42, yend = 62, x = 50, y = 50),
      arrow = arrow(
        length = unit(0.03, "npc"), 
        type="open" # Describes arrow head (open or closed)
      ),
      colour = "black",
      linewidth = 1,
      angle = 0 # Anything other than 90 or 0 can look unusual
    )
  
p5 = p1 + p2 + p3 + p4

ggsave(p5, filename = "images/png/jgr-fig-1.png", height=8, width=8, units="in", dpi=600)
