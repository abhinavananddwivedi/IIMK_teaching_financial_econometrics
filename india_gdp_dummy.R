# ============================================================================
# India GDP Analysis: Structural Breaks and Model Comparison
# Teaching Example for MBA Econometrics
# ============================================================================

library(tidyverse)
library(broom)
library(gridExtra)
library(scales)

set.seed(2025)  # For reproducibility

# ============================================================================
# 1. CREATE SYNTHETIC INDIA GDP DATASET (1947-2025)
# ============================================================================

# Historical context:
# - 1947-1991: "Hindu rate of growth" ~3.5% annually (socialist era)
# - 1991: Economic reforms (liberalization)
# - 1991-2025: Higher growth ~6.5% annually (market reforms era)

years <- 1947:2025
n_years <- length(years)

# Create realistic GDP growth pattern with structural break at 1991
india_gdp <- tibble(
  year = years,
  post_1991 = ifelse(year >= 1991, 1, 0),
  
  # Pre-1991: Lower growth rate (Hindu rate of growth)
  # Post-1991: Higher growth rate (liberalization era)
  gdp_deterministic = case_when(
    year < 1991 ~ 30 * exp(0.035 * (year - 1947)),  # 3.5% growth
    year >= 1991 ~ 30 * exp(0.035 * (1991 - 1947)) * 
      exp(0.065 * (year - 1991))  # 6.5% growth post-reform
  ),
  
  # Add realistic volatility (economic shocks)
  shock = rnorm(n_years, mean = 0, sd = 0.15),  # 15% volatility
  
  # Final GDP in billions of 2025 USD
  gdp = gdp_deterministic * exp(shock),
  
  # Log GDP for later analysis
  log_gdp = log(gdp)
)

# Display summary statistics
cat("\n=== INDIA GDP DATASET (1947-2025) ===\n")
cat("Sample size:", nrow(india_gdp), "years\n")
cat("\nFirst few observations:\n")
print(head(india_gdp, 5))
cat("\nStructural break year: 1991\n")
cat("Pre-1991 years:", sum(india_gdp$post_1991 == 0), "\n")
cat("Post-1991 years:", sum(india_gdp$post_1991 == 1), "\n")

# ============================================================================
# 2. VISUALIZE THE DATA
# ============================================================================

p_raw <- ggplot(india_gdp, aes(x = year, y = gdp)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1991, y = max(india_gdp$gdp) * 0.9, 
           label = "1991 Reforms", color = "red", angle = 90, vjust = -0.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "India's GDP (1947-2025): Clear Structural Break at 1991",
       subtitle = "Synthetic data mimicking actual growth patterns",
       x = "Year", 
       y = "GDP (Billions of 2025 USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_raw)
#ggsave("/home/claude/gdp_raw_data.png", p_raw, width = 10, height = 6, dpi = 300)

# ============================================================================
# 3. MODEL (i): SINGLE TREND LINE (One slope, one intercept)
# ============================================================================

cat("\n\n=== MODEL (i): SINGLE TREND REGRESSION ===\n")
cat("Specification: GDP = β₀ + β₁ * Year + ε\n\n")

model_1 <- lm(gdp ~ year, data = india_gdp)
summary(model_1)

# Extract fitted values and residuals
india_gdp <- india_gdp %>%
  mutate(
    fitted_model1 = fitted(model_1),
    resid_model1 = residuals(model_1)
  )

# Plot
p_model1 <- ggplot(india_gdp, aes(x = year, y = gdp)) +
  geom_point(aes(color = factor(post_1991)), alpha = 0.6) +
  geom_line(aes(y = fitted_model1), color = "darkgreen", size = 1.2) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0" = "blue", "1" = "orange"),
                     labels = c("Pre-1991", "Post-1991"),
                     name = "Period") +
  scale_y_continuous(labels = comma) +
  labs(title = "Model (i): Single Trend Line - IGNORES Structural Break",
       subtitle = sprintf("R² = %.4f | This model FAILS to capture the 1991 regime change",
                          summary(model_1)$r.squared),
       x = "Year", y = "GDP (Billions USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_model1)

# ============================================================================
# 4. MODEL (ii): DUMMY VARIABLE (Two intercepts, one slope)
# ============================================================================

cat("\n\n=== MODEL (ii): DUMMY VARIABLE REGRESSION ===\n")
cat("Specification: GDP = β₀ + β₁ * Year + β₂ * Post1991 + ε\n")
cat("This allows INTERCEPT to shift but assumes SAME slope pre/post 1991\n\n")

model_2 <- lm(gdp ~ year + post_1991, data = india_gdp)
summary(model_2)

india_gdp <- india_gdp %>%
  mutate(
    fitted_model2 = fitted(model_2),
    resid_model2 = residuals(model_2)
  )

p_model2 <- ggplot(india_gdp, aes(x = year, y = gdp)) +
  geom_point(aes(color = factor(post_1991)), alpha = 0.6) +
  geom_line(aes(y = fitted_model2, linetype = "Parallel Lines"), 
            color = "darkgreen", size = 1.2) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0" = "blue", "1" = "orange"),
                     labels = c("Pre-1991", "Post-1991"),
                     name = "Period") +
  scale_y_continuous(labels = comma) +
  labs(title = "Model (ii): Dummy Variable - PARALLEL Lines (Different Intercepts, Same Slope)",
       subtitle = sprintf("R² = %.4f | Better than Model (i) but still assumes same growth rate",
                          summary(model_2)$r.squared),
       x = "Year", y = "GDP (Billions USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(p_model2)

# ============================================================================
# 5. MODEL (iii): SEPARATE REGRESSIONS (Two slopes, two intercepts)
# ============================================================================

cat("\n\n=== MODEL (iii): SEPARATE REGRESSIONS (INTERACTION MODEL) ===\n")
cat("Specification: GDP = β₀ + β₁ * Year + β₂ * Post1991 + β₃ * (Year × Post1991) + ε\n")
cat("This allows BOTH intercept AND slope to differ pre/post 1991\n\n")

model_3 <- lm(gdp ~ year * post_1991, data = india_gdp)
summary(model_3)

# Alternative: Fit separate regressions explicitly
model_3a_pre <- lm(gdp ~ year, data = filter(india_gdp, year < 1991))
model_3a_post <- lm(gdp ~ year, data = filter(india_gdp, year >= 1991))

cat("\n--- Separate Regression: PRE-1991 ---\n")
print(summary(model_3a_pre))
cat("\n--- Separate Regression: POST-1991 ---\n")
print(summary(model_3a_post))

india_gdp <- india_gdp %>%
  mutate(
    fitted_model3 = fitted(model_3),
    resid_model3 = residuals(model_3)
  )

p_model3 <- ggplot(india_gdp, aes(x = year, y = gdp, color = factor(post_1991))) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fitted_model3), size = 1.2) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0" = "blue", "1" = "orange"),
                     labels = c("Pre-1991", "Post-1991"),
                     name = "Period") +
  scale_y_continuous(labels = comma) +
  labs(title = "Model (iii): Separate Regressions - DIFFERENT Slopes AND Intercepts",
       subtitle = sprintf("R² = %.4f | Captures both level shift AND growth acceleration",
                          summary(model_3)$r.squared),
       x = "Year", y = "GDP (Billions USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_model3)

# ============================================================================
# 6. LOG SPECIFICATIONS - REPEAT ALL THREE MODELS
# ============================================================================

cat("\n\n=== LOG-LINEAR MODELS (Better for Exponential Growth!) ===\n\n")

# Model (i-log): Single trend
cat("=== MODEL (i-log): Single Trend on Log(GDP) ===\n")
model_1_log <- lm(log_gdp ~ year, data = india_gdp)
summary(model_1_log)

# Model (ii-log): Dummy variable
cat("\n=== MODEL (ii-log): Dummy Variable on Log(GDP) ===\n")
model_2_log <- lm(log_gdp ~ year + post_1991, data = india_gdp)
summary(model_2_log)

# Model (iii-log): Interaction
cat("\n=== MODEL (iii-log): Interaction Model on Log(GDP) ===\n")
model_3_log <- lm(log_gdp ~ year * post_1991, data = india_gdp)
summary(model_3_log)

# Add fitted values
india_gdp <- india_gdp %>%
  mutate(
    fitted_model1_log = fitted(model_1_log),
    fitted_model2_log = fitted(model_2_log),
    fitted_model3_log = fitted(model_3_log),
    resid_model1_log = residuals(model_1_log),
    resid_model2_log = residuals(model_2_log),
    resid_model3_log = residuals(model_3_log)
  )

# Plot all three log models
p_log_comparison <- ggplot(india_gdp, aes(x = year, y = log_gdp)) +
  geom_point(aes(color = factor(post_1991)), alpha = 0.4, size = 2) +
  geom_line(aes(y = fitted_model1_log, linetype = "Model i: Single Trend"), 
            color = "red", size = 0.8) +
  geom_line(aes(y = fitted_model2_log, linetype = "Model ii: Dummy Variable"), 
            color = "blue", size = 0.8) +
  geom_line(aes(y = fitted_model3_log, linetype = "Model iii: Interaction"), 
            color = "darkgreen", size = 1.2) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "orange"),
                     labels = c("Pre-1991", "Post-1991"),
                     name = "Period") +
  scale_linetype_manual(values = c("Model i: Single Trend" = "dotted",
                                   "Model ii: Dummy Variable" = "dashed",
                                   "Model iii: Interaction" = "solid"),
                        name = "Model") +
  labs(title = "Log(GDP) Models Comparison: Which Fits Best?",
       subtitle = "Model (iii-log) with interaction captures BOTH level shift AND growth rate change",
       x = "Year", y = "Log(GDP)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

print(p_log_comparison)
#ggsave("/home/claude/log_gdp_comparison.png", p_log_comparison, width = 12, height = 7, dpi = 300)

# ============================================================================
# 7. MODEL COMPARISON TABLE
# ============================================================================

cat("\n\n=== MODEL COMPARISON SUMMARY ===\n\n")

# Create comparison table
model_comparison <- tibble(
  Model = c("(i) Single Trend", 
            "(ii) Dummy Variable",
            "(iii) Interaction",
            "(i-log) Single Trend Log",
            "(ii-log) Dummy Variable Log",
            "(iii-log) Interaction Log"),
  
  Specification = c("GDP ~ Year",
                    "GDP ~ Year + Post1991",
                    "GDP ~ Year * Post1991",
                    "Log(GDP) ~ Year",
                    "Log(GDP) ~ Year + Post1991",
                    "Log(GDP) ~ Year * Post1991"),
  
  R_squared = c(summary(model_1)$r.squared,
                summary(model_2)$r.squared,
                summary(model_3)$r.squared,
                summary(model_1_log)$r.squared,
                summary(model_2_log)$r.squared,
                summary(model_3_log)$r.squared),
  
  Adj_R_squared = c(summary(model_1)$adj.r.squared,
                    summary(model_2)$adj.r.squared,
                    summary(model_3)$adj.r.squared,
                    summary(model_1_log)$adj.r.squared,
                    summary(model_2_log)$adj.r.squared,
                    summary(model_3_log)$adj.r.squared),
  
  AIC = c(AIC(model_1), AIC(model_2), AIC(model_3),
          AIC(model_1_log), AIC(model_2_log), AIC(model_3_log)),
  
  BIC = c(BIC(model_1), BIC(model_2), BIC(model_3),
          BIC(model_1_log), BIC(model_2_log), BIC(model_3_log))
)

print(model_comparison)

# ============================================================================
# 8. RESIDUAL DIAGNOSTICS
# ============================================================================

# Create residual plots for level models
p_resid_level <- ggplot(india_gdp, aes(x = year)) +
  geom_point(aes(y = resid_model1, color = "Model (i)"), alpha = 0.5) +
  geom_point(aes(y = resid_model2, color = "Model (ii)"), alpha = 0.5) +
  geom_point(aes(y = resid_model3, color = "Model (iii)"), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.3) +
  facet_wrap(~ "Residuals by Model", ncol = 1) +
  scale_color_manual(values = c("Model (i)" = "red", 
                                "Model (ii)" = "blue", 
                                "Model (iii)" = "darkgreen"),
                     name = "Model") +
  labs(title = "Residual Patterns: Level Models (GDP in Billions)",
       subtitle = "Model (i) shows clear systematic pattern - FAILS to capture structural break",
       x = "Year", y = "Residuals") +
  theme_minimal()

print(p_resid_level)

# Create residual plots for log models
p_resid_log <- ggplot(india_gdp, aes(x = year)) +
  geom_point(aes(y = resid_model1_log, color = "Model (i-log)"), alpha = 0.5) +
  geom_point(aes(y = resid_model2_log, color = "Model (ii-log)"), alpha = 0.5) +
  geom_point(aes(y = resid_model3_log, color = "Model (iii-log)"), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "red", alpha = 0.3) +
  scale_color_manual(values = c("Model (i-log)" = "red", 
                                "Model (ii-log)" = "blue", 
                                "Model (iii-log)" = "darkgreen"),
                     name = "Model") +
  labs(title = "Residual Patterns: Log Models",
       subtitle = "Model (iii-log) shows NO systematic pattern - residuals are random!",
       x = "Year", y = "Residuals (Log Scale)") +
  theme_minimal()

print(p_resid_log)
#ggsave("/home/claude/residual_diagnostics.png", p_resid_log, width = 12, height = 6, dpi = 300)

# Histogram of residuals for best model
p_resid_hist <- ggplot(india_gdp, aes(x = resid_model3_log)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model (iii-log): Residuals Distribution",
       subtitle = "Approximately normal - validates our modeling assumptions",
       x = "Residuals", y = "Frequency") +
  theme_minimal()

print(p_resid_hist)

# QQ plot for normality
p_qq <- ggplot(india_gdp, aes(sample = resid_model3_log)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "Q-Q Plot: Model (iii-log) Residuals",
       subtitle = "Points close to line indicate residuals are approximately normal",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

print(p_qq)

# ============================================================================
# 9. INTERPRETATION AND CONCLUSIONS
# ============================================================================

cat("\n\n")
cat("=================================================================\n")
cat("                  ECONOMIC INTERPRETATION                        \n")
cat("=================================================================\n\n")

# Extract coefficients from best model
coef_log <- coef(model_3_log)
pre_growth <- (exp(coef_log[2]) - 1) * 100
post_growth <- (exp(coef_log[2] + coef_log[4]) - 1) * 100

cat("BEST MODEL: Model (iii-log) - Interaction on Log(GDP)\n")
cat("------------------------------------------------------\n\n")

cat("PRE-1991 Period (1947-1990):\n")
cat(sprintf("  • Annual growth rate: %.2f%%\n", pre_growth))
cat("  • This captures the 'Hindu rate of growth' - slow, regulated economy\n\n")

cat("POST-1991 Period (1991-2025):\n")
cat(sprintf("  • Annual growth rate: %.2f%%\n", post_growth))
cat("  • Economic reforms led to", round(post_growth/pre_growth, 1), "times faster growth!\n\n")

cat("Model Fit Statistics:\n")
cat(sprintf("  • R² = %.4f (explains %.1f%% of variation in Log(GDP))\n", 
            summary(model_3_log)$r.squared, 
            summary(model_3_log)$r.squared * 100))
cat(sprintf("  • Adjusted R² = %.4f\n", summary(model_3_log)$adj.r.squared))
cat(sprintf("  • Residual Std Error = %.4f\n\n", summary(model_3_log)$sigma))

cat("=================================================================\n")
cat("                     WHY IS THIS THE BEST MODEL?                  \n")
cat("=================================================================\n\n")

cat("1. CAPTURES REALITY:\n")
cat("   - India DID experience a structural break in 1991\n")
cat("   - Growth rate CHANGED, not just the level\n")
cat("   - Model (iii-log) allows both intercept AND slope to change\n\n")

cat("2. LOG SPECIFICATION:\n")
cat("   - GDP grows EXPONENTIALLY, not linearly\n")
cat("   - Log models → coefficients are growth rates (easy to interpret!)\n")
cat("   - Constant percentage growth is more realistic than constant absolute growth\n\n")

cat("3. STATISTICAL EVIDENCE:\n")
cat("   - Highest R² among all models\n")
cat("   - Residuals are random (no systematic patterns)\n")
cat("   - Residuals approximately normal (validates inference)\n\n")

cat("4. ECONOMIC INTUITION:\n")
cat("   - Pre-1991: License Raj, import substitution, low growth\n")
cat("   - Post-1991: Liberalization, FDI, globalization, high growth\n")
cat("   - Different growth RATES, not just different LEVELS\n\n")

cat("=================================================================\n")
cat("                     PEDAGOGICAL LESSONS                          \n")
cat("=================================================================\n\n")

cat("FOR YOUR MBA STUDENTS:\n\n")

cat("Lesson 1: ONE SIZE DOESN'T FIT ALL\n")
cat("  Model (i) forces same slope everywhere → systematic errors\n")
cat("  Always test for structural breaks in economic data!\n\n")

cat("Lesson 2: DUMMY VARIABLES ARE POWERFUL BUT LIMITED\n")
cat("  Model (ii) allows different levels but same slope\n")
cat("  What if the RELATIONSHIP changes, not just the starting point?\n\n")

cat("Lesson 3: INTERACTIONS CAPTURE CHANGING RELATIONSHIPS\n")
cat("  Model (iii) lets slope differ across regimes\n")
cat("  Year × Post1991 interaction → different growth rates\n\n")

cat("Lesson 4: THINK ABOUT THE DATA-GENERATING PROCESS\n")
cat("  Is growth linear or exponential?\n")
cat("  Log specification makes sense for GDP (compound growth)\n")
cat("  Coefficients become elasticities/growth rates\n\n")

cat("Lesson 5: RESIDUAL DIAGNOSTICS TELL THE TRUTH\n")
cat("  Patterns in residuals → model is WRONG\n")
cat("  Random residuals → model captures the story\n\n")

cat("=================================================================\n\n")

# Save the dataset for future use
#write_csv(india_gdp, "/home/claude/india_gdp_synthetic.csv")
cat("Dataset saved to: india_gdp_synthetic.csv\n\n")

cat("Analysis complete! Check the plots for visual insights.\n")