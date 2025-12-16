# Load libraries
library(tidyverse)
library(lubridate)
library(scales)

# Set seed for reproducibility
set.seed(42)

# Create synthetic ATM withdrawal data
# Jan 1, 2016 to Dec 31, 2017 (731 days)

# Generate date sequence
dates <- seq(as.Date("2016-01-01"), as.Date("2017-12-31"), by = "day")

# Demonetization date
demo_date <- as.Date("2016-11-08")

# Create data
atm_data <- tibble(
  date = dates,
  time = 1:length(dates),  # Time trend
  post_demo = as.numeric(date >= demo_date)  # Dummy: 0 before, 1 after
) %>%
  mutate(
    # True data generating process:
    # Before demo: Higher baseline (80) + small upward trend + noise
    # After demo: Sharp drop (40 crore less) + same trend + noise
    withdrawals = case_when(
      post_demo == 0 ~ 80 + 0.02 * time + rnorm(n(), 0, 3),
      post_demo == 1 ~ 40 + 0.02 * time + rnorm(n(), 0, 3)
    )
  )

# Regression 1: WRONG MODEL (ignores structural break)
model_wrong <- lm(withdrawals ~ time, data = atm_data)

# Regression 2: CORRECT MODEL (includes dummy)
model_correct <- lm(withdrawals ~ time + post_demo, data = atm_data)

# Add fitted values to data
atm_data <- atm_data %>%
  mutate(
    fitted_wrong = predict(model_wrong),
    fitted_correct = predict(model_correct),
    resid_wrong = residuals(model_wrong),
    resid_correct = residuals(model_correct)
  )

# Print regression results
cat("=== WRONG MODEL (No Dummy) ===\n")
summary(model_wrong)

cat("\n=== CORRECT MODEL (With Dummy) ===\n")
summary(model_correct)

# Visualization 1: The Problem - One Line Doesn't Fit
p1 <- ggplot(atm_data, aes(x = date, y = withdrawals)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_line(aes(y = fitted_wrong), color = "red", linewidth = 1.2) +
  geom_vline(xintercept = demo_date, linetype = "dashed", 
             color = "darkred", linewidth = 0.8) +
  annotate("text", x = demo_date, y = max(atm_data$withdrawals), 
           label = "Demonetization\nNov 8, 2016", 
           hjust = -0.1, vjust = 1, color = "darkred", fontface = "bold") +
  labs(
    title = "WRONG MODEL: Single Regression Line",
    subtitle = "Ignoring the structural break leads to systematic errors",
    x = "Date",
    y = "Daily ATM Withdrawals (₹ Crores)",
    caption = "Red line = Fitted values from simple time trend model"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

# Visualization 2: The Solution - Two Lines Fit Better
p2 <- ggplot(atm_data, aes(x = date, y = withdrawals)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_line(aes(y = fitted_correct), color = "darkgreen", linewidth = 1.2) +
  geom_vline(xintercept = demo_date, linetype = "dashed", 
             color = "darkred", linewidth = 0.8) +
  annotate("text", x = demo_date, y = max(atm_data$withdrawals), 
           label = "Demonetization\nNov 8, 2016", 
           hjust = -0.1, vjust = 1, color = "darkred", fontface = "bold") +
  labs(
    title = "CORRECT MODEL: Dummy Variable Allows Intercept Shift",
    subtitle = "The model acknowledges the regime change",
    x = "Date",
    y = "Daily ATM Withdrawals (₹ Crores)",
    caption = "Green line = Fitted values with post-demonetization dummy"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

# Visualization 3: Side-by-Side Comparison
p3 <- ggplot(atm_data, aes(x = date, y = withdrawals)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 0.8) +
  geom_line(aes(y = fitted_wrong, color = "Wrong Model"), linewidth = 1) +
  geom_line(aes(y = fitted_correct, color = "Correct Model"), linewidth = 1) +
  geom_vline(xintercept = demo_date, linetype = "dashed", 
             color = "darkred", linewidth = 0.8) +
  scale_color_manual(
    name = "Model",
    values = c("Wrong Model" = "red", "Correct Model" = "darkgreen")
  ) +
  annotate("text", x = demo_date, y = max(atm_data$withdrawals), 
           label = "Demonetization", 
           hjust = -0.1, vjust = 1, color = "darkred", fontface = "bold") +
  labs(
    title = "Comparison: With vs Without Dummy Variable",
    subtitle = "The dummy captures the known structural break",
    x = "Date",
    y = "Daily ATM Withdrawals (₹ Crores)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# Visualization 4: Residual Plots - The Smoking Gun
p4 <- atm_data %>%
  select(date, resid_wrong, resid_correct) %>%
  pivot_longer(cols = c(resid_wrong, resid_correct), 
               names_to = "model", values_to = "residuals") %>%
  mutate(model = recode(model, 
                        resid_wrong = "Wrong Model (No Dummy)",
                        resid_correct = "Correct Model (With Dummy)")) %>%
  ggplot(aes(x = date, y = residuals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = demo_date, linetype = "dashed", 
             color = "darkred", linewidth = 0.6) +
  facet_wrap(~ model, ncol = 1) +
  labs(
    title = "Residual Patterns Reveal the Problem",
    subtitle = "Wrong model shows systematic pattern; correct model shows random noise",
    x = "Date",
    y = "Residuals",
    caption = "Notice: Wrong model has persistent negative errors after Nov 8"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold")
  )

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

# Calculate and display summary statistics
cat("\n=== Model Comparison ===\n")
cat(sprintf("Wrong Model R-squared: %.4f\n", summary(model_wrong)$r.squared))
cat(sprintf("Correct Model R-squared: %.4f\n", summary(model_correct)$r.squared))
cat(sprintf("\nWrong Model RMSE: %.2f\n", sqrt(mean(atm_data$resid_wrong^2))))
cat(sprintf("Correct Model RMSE: %.2f\n", sqrt(mean(atm_data$resid_correct^2))))

# Show estimated demonetization effect
demo_effect <- coef(model_correct)["post_demo"]
cat(sprintf("\nEstimated demonetization effect: ₹%.2f crores per day\n", demo_effect))
cat(sprintf("(%.1f%% drop in daily withdrawals)\n", 
            100 * demo_effect / mean(atm_data$withdrawals[atm_data$post_demo == 0])))