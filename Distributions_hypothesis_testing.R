# Create visualizations using base R (no external packages needed)

# Set up output directory
dir.create("/mnt/user-data/outputs", showWarnings = FALSE, recursive = TRUE)

# ============================================
# Plot 1: Normal vs t-distributions
# ============================================
png("/mnt/user-data/outputs/plot1_normal_vs_t.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))

x <- seq(-4, 4, length.out = 500)

plot(x, dnorm(x), type = "l", lwd = 3, col = "black", 
     xlab = "Value", ylab = "Density",
     main = "Normal vs t-Distributions",
     sub = "t-distribution has heavier tails; approaches normal as df increases",
     cex.lab = 1.3, cex.main = 1.5, cex.sub = 1.1,
     ylim = c(0, 0.45))

lines(x, dt(x, df = 3), lwd = 3, col = "#E41A1C", lty = 2)
lines(x, dt(x, df = 10), lwd = 3, col = "#377EB8", lty = 3)
lines(x, dt(x, df = 30), lwd = 3, col = "#4DAF4A", lty = 4)

legend("topright", 
       legend = c("Normal", "t (df=3)", "t (df=10)", "t (df=30)"),
       col = c("black", "#E41A1C", "#377EB8", "#4DAF4A"),
       lty = c(1, 2, 3, 4),
       lwd = 3,
       cex = 1.2)

grid(col = "gray90")
dev.off()

# ============================================
# Plot 2: Comparing tail probabilities
# ============================================
png("/mnt/user-data/outputs/plot2_heavy_tails.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))

x <- seq(-4, 4, length.out = 500)

plot(x, dnorm(x), type = "l", lwd = 3, col = "black",
     xlab = "Value", ylab = "Density",
     main = "Heavier Tails in t-Distribution",
     sub = "Shaded regions show P(|X| > 2): larger for t-distribution",
     cex.lab = 1.3, cex.main = 1.5, cex.sub = 1.1,
     ylim = c(0, 0.45))

lines(x, dt(x, df = 3), lwd = 3, col = "#E41A1C", lty = 2)

# Shade the tails for Normal
x_tail <- x[abs(x) > 2]
y_norm_tail <- dnorm(x_tail)
polygon(c(x_tail[x_tail < -2], rev(x_tail[x_tail < -2])), 
        c(rep(0, sum(x_tail < -2)), rev(y_norm_tail[x_tail < -2])),
        col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
polygon(c(x_tail[x_tail > 2], rev(x_tail[x_tail > 2])), 
        c(rep(0, sum(x_tail > 2)), rev(y_norm_tail[x_tail > 2])),
        col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)

# Shade the tails for t
y_t_tail <- dt(x_tail, df = 3)
polygon(c(x_tail[x_tail < -2], rev(x_tail[x_tail < -2])), 
        c(rep(0, sum(x_tail < -2)), rev(y_t_tail[x_tail < -2])),
        col = rgb(0.89, 0.1, 0.11, 0.3), border = NA)
polygon(c(x_tail[x_tail > 2], rev(x_tail[x_tail > 2])), 
        c(rep(0, sum(x_tail > 2)), rev(y_t_tail[x_tail > 2])),
        col = rgb(0.89, 0.1, 0.11, 0.3), border = NA)

abline(v = c(-2, 2), lty = 3, col = "gray40", lwd = 2)

text(2.5, 0.35, "t has MORE\nprobability\nin tails", 
     col = "#E41A1C", font = 2, cex = 1.3)

legend("topright", 
       legend = c("Normal", "t (df=3)"),
       col = c("black", "#E41A1C"),
       lty = c(1, 2),
       lwd = 3,
       cex = 1.2)

grid(col = "gray90")
dev.off()

# ============================================
# Plot 3: F-distributions
# ============================================
png("/mnt/user-data/outputs/plot3_f_distributions.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))

x_f <- seq(0, 5, length.out = 500)

plot(x_f, df(x_f, df1 = 5, df2 = 10), type = "l", lwd = 3, col = "#E41A1C",
     xlab = "Value", ylab = "Density",
     main = "F-Distributions",
     sub = "Right-skewed, always positive; used for testing multiple restrictions",
     cex.lab = 1.3, cex.main = 1.5, cex.sub = 1.1,
     ylim = c(0, 1))

lines(x_f, df(x_f, df1 = 10, df2 = 20), lwd = 3, col = "#377EB8", lty = 2)
lines(x_f, df(x_f, df1 = 30, df2 = 50), lwd = 3, col = "#4DAF4A", lty = 3)

legend("topright", 
       legend = c("F(5, 10)", "F(10, 20)", "F(30, 50)"),
       col = c("#E41A1C", "#377EB8", "#4DAF4A"),
       lty = c(1, 2, 3),
       lwd = 3,
       cex = 1.2)

grid(col = "gray90")
dev.off()

# ============================================
# Plot 4: All three together
# ============================================
png("/mnt/user-data/outputs/plot4_all_three.png", width = 10, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 4, 2))

x_compare <- seq(-4, 6, length.out = 500)

plot(x_compare, dnorm(x_compare), type = "l", lwd = 3, col = "black",
     xlab = "Value", ylab = "Density",
     main = "Comparing All Three Distributions",
     sub = "Normal & t are symmetric; F is right-skewed and non-negative",
     cex.lab = 1.3, cex.main = 1.5, cex.sub = 1.1,
     ylim = c(0, 0.45))

lines(x_compare, dt(x_compare, df = 5), lwd = 3, col = "#E41A1C", lty = 2)

# For F, shift and scale for visibility, and only show positive values
x_f_plot <- x_compare[x_compare >= 0]
lines(x_f_plot, df(x_f_plot, df1 = 10, df2 = 20) * 0.6, 
      lwd = 3, col = "#4DAF4A", lty = 3)

text(-2.5, 0.35, "Symmetric", font = 3, cex = 1.3)
text(2.5, 0.1, "Right-skewed", col = "#4DAF4A", font = 3, cex = 1.3)

legend("topright", 
       legend = c("Normal", "t (df=5)", "F (scaled)"),
       col = c("black", "#E41A1C", "#4DAF4A"),
       lty = c(1, 2, 3),
       lwd = 3,
       cex = 1.2)

grid(col = "gray90")
dev.off()

# ============================================
# Combined plot (2x2 grid)
# ============================================
png("/mnt/user-data/outputs/combined_distributions.png", width = 16, height = 12, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))

# Plot 1
x <- seq(-4, 4, length.out = 500)
plot(x, dnorm(x), type = "l", lwd = 2.5, col = "black", 
     xlab = "Value", ylab = "Density",
     main = "Normal vs t-Distributions",
     cex.lab = 1.2, cex.main = 1.3,
     ylim = c(0, 0.45))
lines(x, dt(x, df = 3), lwd = 2.5, col = "#E41A1C", lty = 2)
lines(x, dt(x, df = 10), lwd = 2.5, col = "#377EB8", lty = 3)
lines(x, dt(x, df = 30), lwd = 2.5, col = "#4DAF4A", lty = 4)
legend("topright", 
       legend = c("Normal", "t(3)", "t(10)", "t(30)"),
       col = c("black", "#E41A1C", "#377EB8", "#4DAF4A"),
       lty = c(1, 2, 3, 4), lwd = 2.5, cex = 0.9)
grid(col = "gray90")

# Plot 2
plot(x, dnorm(x), type = "l", lwd = 2.5, col = "black",
     xlab = "Value", ylab = "Density",
     main = "Heavier Tails in t-Distribution",
     cex.lab = 1.2, cex.main = 1.3,
     ylim = c(0, 0.45))
lines(x, dt(x, df = 3), lwd = 2.5, col = "#E41A1C", lty = 2)
x_tail <- x[abs(x) > 2]
y_t_tail <- dt(x_tail, df = 3)
polygon(c(x_tail[x_tail < -2], rev(x_tail[x_tail < -2])), 
        c(rep(0, sum(x_tail < -2)), rev(y_t_tail[x_tail < -2])),
        col = rgb(0.89, 0.1, 0.11, 0.3), border = NA)
polygon(c(x_tail[x_tail > 2], rev(x_tail[x_tail > 2])), 
        c(rep(0, sum(x_tail > 2)), rev(y_t_tail[x_tail > 2])),
        col = rgb(0.89, 0.1, 0.11, 0.3), border = NA)
abline(v = c(-2, 2), lty = 3, col = "gray40", lwd = 1.5)
legend("topright", legend = c("Normal", "t(3)"),
       col = c("black", "#E41A1C"), lty = c(1, 2), lwd = 2.5, cex = 0.9)
grid(col = "gray90")

# Plot 3
x_f <- seq(0, 5, length.out = 500)
plot(x_f, df(x_f, df1 = 5, df2 = 10), type = "l", lwd = 2.5, col = "#E41A1C",
     xlab = "Value", ylab = "Density",
     main = "F-Distributions",
     cex.lab = 1.2, cex.main = 1.3,
     ylim = c(0, 1))
lines(x_f, df(x_f, df1 = 10, df2 = 20), lwd = 2.5, col = "#377EB8", lty = 2)
lines(x_f, df(x_f, df1 = 30, df2 = 50), lwd = 2.5, col = "#4DAF4A", lty = 3)
legend("topright", legend = c("F(5,10)", "F(10,20)", "F(30,50)"),
       col = c("#E41A1C", "#377EB8", "#4DAF4A"),
       lty = c(1, 2, 3), lwd = 2.5, cex = 0.9)
grid(col = "gray90")

# Plot 4
x_compare <- seq(-4, 6, length.out = 500)
plot(x_compare, dnorm(x_compare), type = "l", lwd = 2.5, col = "black",
     xlab = "Value", ylab = "Density",
     main = "Comparing All Three",
     cex.lab = 1.2, cex.main = 1.3,
     ylim = c(0, 0.45))
lines(x_compare, dt(x_compare, df = 5), lwd = 2.5, col = "#E41A1C", lty = 2)
x_f_plot <- x_compare[x_compare >= 0]
lines(x_f_plot, df(x_f_plot, df1 = 10, df2 = 20) * 0.6, 
      lwd = 2.5, col = "#4DAF4A", lty = 3)
text(-2.5, 0.35, "Symmetric", font = 3, cex = 1.1)
text(2.5, 0.1, "Right-skewed", col = "#4DAF4A", font = 3, cex = 1.1)
legend("topright", legend = c("Normal", "t(5)", "F(scaled)"),
       col = c("black", "#E41A1C", "#4DAF4A"),
       lty = c(1, 2, 3), lwd = 2.5, cex = 0.9)
grid(col = "gray90")

dev.off()

cat("All distribution plots created successfully!\n")
cat("Files saved in /mnt/user-data/outputs/\n")