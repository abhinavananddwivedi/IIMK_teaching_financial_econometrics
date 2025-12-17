# ============================================================================
# Illustrating the "No Serial Correlation" Assumption in Time Series Regression
# ============================================================================

# Load required packages
library(tidyverse)
library(lmtest)  # For Durbin-Watson test
library(gridExtra)  # For arranging plots

set.seed(42)  # For reproducibility

# ============================================================================
# PART 1: Generate Synthetic Time Series Data
# ============================================================================

n <- 200  # Number of time periods

# Generate independent variable (e.g., advertising expenditure over time)
X <- 50 + 0.5 * (1:n) + rnorm(n, 0, 5)

# True parameters
beta_0 <- 100  # Intercept
beta_1 <- 2.5  # Slope (true effect of X on Y)

# ----------------------------------------------------------------------------
# CASE 1: NO SERIAL CORRELATION (Classical Assumption Holds)
# ----------------------------------------------------------------------------

# Generate i.i.d. errors (no serial correlation)
epsilon_iid <- rnorm(n, 0, 10)

# Generate Y with no serial correlation in errors
Y_clean <- beta_0 + beta_1 * X + epsilon_iid

# Create data frame
data_clean <- data.frame(
  time = 1:n,
  X = X,
  Y = Y_clean,
  epsilon = epsilon_iid
)

# ----------------------------------------------------------------------------
# CASE 2: SERIAL CORRELATION PRESENT (Assumption Violated)
# ----------------------------------------------------------------------------

# Generate AR(1) errors: epsilon_t = rho * epsilon_{t-1} + u_t
rho <- 0.7  # Autocorrelation parameter (positive serial correlation)
epsilon_ar1 <- numeric(n)
epsilon_ar1[1] <- rnorm(1, 0, 10)

for (t in 2:n) {
  epsilon_ar1[t] <- rho * epsilon_ar1[t-1] + rnorm(1, 0, 10)
}

# Generate Y with serial correlation in errors
Y_serial <- beta_0 + beta_1 * X + epsilon_ar1

# Create data frame
data_serial <- data.frame(
  time = 1:n,
  X = X,
  Y = Y_serial,
  epsilon = epsilon_ar1
)

# ============================================================================
# PART 2: Run Regressions
# ============================================================================

# Regression with clean data (no serial correlation)
model_clean <- lm(Y ~ X, data = data_clean)

# Regression with serial correlation
model_serial <- lm(Y ~ X, data = data_serial)

# ============================================================================
# PART 3: Compare Results
# ============================================================================

cat("\n========================================\n")
cat("REGRESSION RESULTS COMPARISON\n")
cat("========================================\n\n")

cat("MODEL 1: No Serial Correlation (Assumption Satisfied)\n")
cat("------------------------------------------------------\n")
print(summary(model_clean))

cat("\n\nMODEL 2: With Serial Correlation (Assumption Violated)\n")
cat("--------------------------------------------------------\n")
print(summary(model_serial))

# Extract residuals
residuals_clean <- residuals(model_clean)
residuals_serial <- residuals(model_serial)

# ============================================================================
# PART 4: Visual Diagnostics
# ============================================================================

# Plot 1: Residuals over Time (Clean Data)
p1 <- ggplot(data_clean, aes(x = time, y = residuals_clean)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals: NO Serial Correlation",
    subtitle = "Random scatter around zero (Good!)",
    x = "Time",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Plot 2: Residuals over Time (Serial Correlation)
p2 <- ggplot(data_serial, aes(x = time, y = residuals_serial)) +
  geom_line(color = "darkred", linewidth = 0.8) +
  geom_point(color = "darkred", size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals: WITH Serial Correlation",
    subtitle = "Persistent patterns - residuals cluster together (Bad!)",
    x = "Time",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Plot 3: ACF for Clean Data
acf_clean <- acf(residuals_clean, plot = FALSE)
p3 <- ggplot(data.frame(lag = acf_clean$lag[-1], acf = acf_clean$acf[-1]), 
             aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = c(-1.96/sqrt(n), 1.96/sqrt(n)), 
             linetype = "dashed", color = "blue") +
  labs(
    title = "ACF: No Serial Correlation",
    subtitle = "Spikes stay within confidence bands",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Plot 4: ACF for Serial Correlation
acf_serial <- acf(residuals_serial, plot = FALSE)
p4 <- ggplot(data.frame(lag = acf_serial$lag[-1], acf = acf_serial$acf[-1]), 
             aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(xend = lag, yend = 0), color = "darkred", linewidth = 1) +
  geom_hline(yintercept = c(-1.96/sqrt(n), 1.96/sqrt(n)), 
             linetype = "dashed", color = "blue") +
  labs(
    title = "ACF: With Serial Correlation",
    subtitle = "Spikes exceed confidence bands - clear pattern!",
    x = "Lag",
    y = "Autocorrelation"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# Combine plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# ============================================================================
# PART 5: Formal Tests for Serial Correlation
# ============================================================================

cat("\n\n========================================\n")
cat("DURBIN-WATSON TEST FOR SERIAL CORRELATION\n")
cat("========================================\n\n")

# Durbin-Watson test for clean data
cat("Model 1 (No Serial Correlation):\n")
dw_clean <- dwtest(model_clean)
print(dw_clean)
cat("\nInterpretation: DW ≈ 2 suggests no serial correlation\n")

cat("\n\nModel 2 (With Serial Correlation):\n")
dw_serial <- dwtest(model_serial)
print(dw_serial)
cat("\nInterpretation: DW << 2 suggests POSITIVE serial correlation\n")

# ============================================================================
# PART 6: Demonstrate Impact on Standard Errors
# ============================================================================

cat("\n\n========================================\n")
cat("IMPACT ON INFERENCE\n")
cat("========================================\n\n")

# Extract standard errors
se_clean <- summary(model_clean)$coefficients[2, 2]
se_serial <- summary(model_serial)$coefficients[2, 2]

# Calculate HAC (Heteroskedasticity and Autocorrelation Consistent) standard errors
# These are "correct" standard errors that account for serial correlation
library(sandwich)
library(lmtest)

se_serial_hac <- sqrt(vcovHAC(model_serial)[2, 2])

cat(sprintf("Standard Error for beta_1:\n"))
cat(sprintf("  Clean data (OLS): %.4f\n", se_clean))
cat(sprintf("  Serial corr (OLS - WRONG): %.4f\n", se_serial))
cat(sprintf("  Serial corr (HAC - CORRECT): %.4f\n\n", se_serial_hac))

cat("KEY INSIGHT:\n")
cat("When serial correlation is present, OLS standard errors are BIASED.\n")
cat("This leads to incorrect t-statistics and unreliable hypothesis tests!\n")
cat(sprintf("In this example, OLS underestimates the true SE by %.1f%%\n", 
            100 * (se_serial_hac - se_serial) / se_serial_hac))

# ============================================================================
# PART 7: Create Comparison Table
# ============================================================================

comparison_table <- data.frame(
  Characteristic = c(
    "Coefficient Estimate (β₁)",
    "OLS Standard Error",
    "HAC Standard Error",
    "t-statistic (OLS)",
    "Durbin-Watson Statistic",
    "First-order Autocorrelation"
  ),
  No_Serial_Correlation = c(
    sprintf("%.3f", coef(model_clean)[2]),
    sprintf("%.4f", summary(model_clean)$coefficients[2, 2]),
    "Not needed",
    sprintf("%.2f", summary(model_clean)$coefficients[2, 3]),
    sprintf("%.3f", dw_clean$statistic),
    sprintf("%.3f", acf(residuals_clean, plot = FALSE)$acf[2])
  ),
  With_Serial_Correlation = c(
    sprintf("%.3f", coef(model_serial)[2]),
    sprintf("%.4f", summary(model_serial)$coefficients[2, 2]),
    sprintf("%.4f", se_serial_hac),
    sprintf("%.2f", summary(model_serial)$coefficients[2, 3]),
    sprintf("%.3f", dw_serial$statistic),
    sprintf("%.3f", acf(residuals_serial, plot = FALSE)$acf[2])
  )
)

cat("\n\n========================================\n")
cat("SUMMARY COMPARISON TABLE\n")
cat("========================================\n\n")
print(comparison_table, row.names = FALSE)

# ============================================================================
# PART 8: Illustrate the Problem Visually
# ============================================================================

# Create a plot showing how serial correlation creates false precision
ci_data <- data.frame(
  Model = rep(c("Clean (OLS)", "Serial Corr (OLS - Wrong)", 
                "Serial Corr (HAC - Correct)"), each = 2),
  bound = rep(c("Lower", "Upper"), 3),
  value = c(
    coef(model_clean)[2] - 1.96 * summary(model_clean)$coefficients[2, 2],
    coef(model_clean)[2] + 1.96 * summary(model_clean)$coefficients[2, 2],
    coef(model_serial)[2] - 1.96 * summary(model_serial)$coefficients[2, 2],
    coef(model_serial)[2] + 1.96 * summary(model_serial)$coefficients[2, 2],
    coef(model_serial)[2] - 1.96 * se_serial_hac,
    coef(model_serial)[2] + 1.96 * se_serial_hac
  )
)

ci_plot <- ci_data %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(point_estimate = coef(c(model_clean, model_serial, model_serial))[c(2, 2, 2)]) %>%
  ggplot(aes(y = Model, x = point_estimate)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, linewidth = 1) +
  geom_point(size = 3, color = "blue") +
  geom_vline(xintercept = beta_1, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = beta_1, y = 3.5, label = "True β₁", color = "red", size = 4) +
  labs(
    title = "95% Confidence Intervals for β₁",
    subtitle = "Serial correlation makes OLS confidence intervals artificially narrow!",
    x = "Coefficient Estimate",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11)
  )

print(ci_plot)

# ============================================================================
# FINAL TAKEAWAYS
# ============================================================================

cat("\n\n========================================\n")
cat("KEY TAKEAWAYS FOR MBA STUDENTS\n")
cat("========================================\n\n")

cat("1. WHAT IS SERIAL CORRELATION?\n")
cat("   - Today's error is correlated with yesterday's error\n")
cat("   - Errors show persistent patterns over time\n\n")

cat("2. WHY DOES IT MATTER?\n")
cat("   - OLS coefficient estimates remain unbiased\n")
cat("   - BUT standard errors become WRONG\n")
cat("   - This makes hypothesis tests unreliable\n")
cat("   - You might think results are significant when they're not!\n\n")

cat("3. HOW TO DETECT IT?\n")
cat("   - Plot residuals over time (look for patterns)\n")
cat("   - Check ACF plot (spikes beyond confidence bands)\n")
cat("   - Run Durbin-Watson test (DW ≈ 2 is good, DW << 2 or >> 2 is bad)\n\n")

cat("4. WHAT TO DO ABOUT IT?\n")
cat("   - Use HAC (robust) standard errors\n")
cat("   - Include lagged dependent variable\n")
cat("   - Use time series models (ARIMA, etc.)\n\n")

cat("=========================================\n")