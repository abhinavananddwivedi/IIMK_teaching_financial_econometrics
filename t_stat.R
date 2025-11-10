library(ggplot2)

df <- 77
x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df = df)
data_t <- data.frame(x = x, y = y)
critical_val <- 1.96

# Your actual t-statistic (truncated for visualization)
your_t <- 4  # We can't show 70.47 on this scale!

p <- ggplot(data_t, aes(x = x, y = y)) +
  geom_line(linewidth = 1.5, color = "navy") +
  
  # Shade rejection regions
  geom_area(data = subset(data_t, x < -critical_val), 
            aes(x = x, y = y), fill = "red", alpha = 0.4) +
  geom_area(data = subset(data_t, x > critical_val), 
            aes(x = x, y = y), fill = "red", alpha = 0.4) +
  
  # Critical value lines
  geom_vline(xintercept = c(-critical_val, critical_val), 
             linetype = "dashed", color = "red", linewidth = 1.2) +
  
  # Arrow showing your t-statistic is way off the chart
  annotate("segment", x = 3.5, xend = 3.9, y = 0.3, yend = 0.05,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           color = "darkgreen", linewidth = 1.5) +
  annotate("text", x = 3.5, y = 0.32, 
           label = "Your t = 70.47\n(way off chart!)", 
           color = "darkgreen", size = 5, fontface = "bold") +
  
  # Labels
  annotate("text", x = 0, y = 0.35, 
           label = "Fail to Reject H₀\n(Accept Region)", 
           size = 4.5, fontface = "bold") +
  annotate("text", x = -2.7, y = 0.05, label = "Reject", 
           color = "red", size = 4, fontface = "bold") +
  annotate("text", x = 2.7, y = 0.05, label = "Reject", 
           color = "red", size = 4, fontface = "bold") +
  annotate("text", x = -critical_val, y = 0.4, 
           label = "-1.96", size = 4, color = "red") +
  annotate("text", x = critical_val, y = 0.4, 
           label = "+1.96", size = 4, color = "red") +
  
  # Theme
  theme_minimal(base_size = 14) +
  labs(title = "t-Distribution: Testing H₀: β = 0",
       subtitle = "Two-Tailed Test at 5% Significance Level",
       x = "t-statistic", 
       y = "Probability Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(p)
#ggsave("t_distribution.png", plot = p, width = 10, height = 6, dpi = 300)