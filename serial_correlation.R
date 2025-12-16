# Load libraries
library(tidyverse)
library(patchwork)
library(corrplot)

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# SETUP: Create student exam data
# ============================================================================

# Parameters
n_students <- 60
exams <- c("Midterm", "Quiz1", "Quiz2", "Endterm")
n_exams <- length(exams)

# Scenario 1: NO CHEATING (No Serial Correlation)
# ============================================================================

# Generate student knowledge levels (fixed characteristic)
students_no_cheat <- tibble(
  student_id = 1:n_students,
  knowledge = rnorm(n_students, mean = 70, sd = 10)  # Fixed ability
)

# Generate exam scores for each exam (knowledge + independent random error)
exam_data_no_cheat <- expand_grid(
  student_id = 1:n_students,
  exam = factor(exams, levels = exams),
  exam_num = 1:n_exams
) %>%
  left_join(students_no_cheat, by = "student_id") %>%
  mutate(
    # True score = knowledge + independent random shock each time
    error = rnorm(n(), mean = 0, sd = 5),  # Independent errors!
    score = knowledge + error,
    score = pmin(100, pmax(0, score))  # Bound scores between 0-100
  )

# Scenario 2: WITH CHEATING (Serial Correlation)
# ============================================================================

# Identify cheaters (20% of students cheat persistently)
n_cheaters <- 12
cheater_ids <- sample(1:n_students, n_cheaters)

students_with_cheat <- students_no_cheat %>%
  mutate(
    is_cheater = student_id %in% cheater_ids,
    # Cheaters have a persistent "boost" component
    cheat_boost = ifelse(is_cheater, rnorm(1, mean = 15, sd = 3), 0)
  )

# Generate exam scores with serial correlation for cheaters
exam_data_with_cheat <- expand_grid(
  student_id = 1:n_students,
  exam = factor(exams, levels = exams),
  exam_num = 1:n_exams
) %>%
  left_join(students_with_cheat, by = "student_id") %>%
  mutate(
    # Independent random component (still present)
    random_error = rnorm(n(), mean = 0, sd = 5),
    # For cheaters: add persistent boost (creates serial correlation)
    error = ifelse(is_cheater, cheat_boost + random_error, random_error),
    score = knowledge + error,
    score = pmin(100, pmax(0, score))
  )

# ============================================================================
# REGRESSION ANALYSIS
# ============================================================================

# Regression 1: No cheating scenario
model_no_cheat <- lm(score ~ knowledge, data = exam_data_no_cheat)

exam_data_no_cheat <- exam_data_no_cheat %>%
  mutate(
    fitted = predict(model_no_cheat),
    residual = residuals(model_no_cheat)
  )

# Regression 2: With cheating scenario
model_with_cheat <- lm(score ~ knowledge, data = exam_data_with_cheat)

exam_data_with_cheat <- exam_data_with_cheat %>%
  mutate(
    fitted = predict(model_with_cheat),
    residual = residuals(model_with_cheat)
  )

# Print regression results
cat("=== SCENARIO 1: NO CHEATING ===\n")
summary(model_no_cheat)

cat("\n=== SCENARIO 2: WITH CHEATING ===\n")
summary(model_with_cheat)

# ============================================================================
# VISUALIZATION 1: Individual Student Trajectories
# ============================================================================

# Pick a few students to highlight: honest student vs cheater
honest_student <- setdiff(1:n_students, cheater_ids)[1]
cheater_student <- cheater_ids[1]

# Plot residuals over time for specific students
p1 <- exam_data_no_cheat %>%
  filter(student_id %in% c(honest_student, honest_student + 1, honest_student + 2)) %>%
  ggplot(aes(x = exam_num, y = residual, color = factor(student_id), group = student_id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "NO CHEATING: Residuals Are Random 'Coin Flips'",
    subtitle = "Each exam is an independent draw - no pattern over time",
    x = "Exam Number",
    y = "Residual (Actual - Predicted Score)",
    color = "Student ID"
  ) +
  scale_x_continuous(breaks = 1:4, labels = exams) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p2 <- exam_data_with_cheat %>%
  filter(student_id %in% cheater_ids[1:3]) %>%
  ggplot(aes(x = exam_num, y = residual, color = factor(student_id), group = student_id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 10, ymax = 20, 
           alpha = 0.1, fill = "red") +
  annotate("text", x = 2.5, y = 18, 
           label = "Persistently positive\n(suspiciously lucky!)", 
           color = "darkred", fontface = "italic") +
  labs(
    title = "WITH CHEATING: Residuals Are Persistently Positive",
    subtitle = "Same students are 'lucky' across all exams - serial correlation!",
    x = "Exam Number",
    y = "Residual (Actual - Predicted Score)",
    color = "Student ID\n(Cheater)"
  ) +
  scale_x_continuous(breaks = 1:4, labels = exams) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p1 / p2)

# ============================================================================
# VISUALIZATION 2: Residual Correlation Matrices
# ============================================================================

# Create wide format for correlation analysis
create_residual_matrix <- function(data) {
  data %>%
    select(student_id, exam, residual) %>%
    pivot_wider(names_from = exam, values_from = residual) %>%
    select(-student_id) %>%
    as.matrix()
}

resid_matrix_no_cheat <- create_residual_matrix(exam_data_no_cheat)
resid_matrix_with_cheat <- create_residual_matrix(exam_data_with_cheat)

# Calculate correlation matrices
cor_no_cheat <- cor(resid_matrix_no_cheat)
cor_with_cheat <- cor(resid_matrix_with_cheat)

# Plot correlation matrices side by side
par(mfrow = c(1, 2))
corrplot(cor_no_cheat, method = "color", type = "upper", 
         title = "NO CHEATING: Low Correlation Across Exams",
         mar = c(0, 0, 2, 0), tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200))

corrplot(cor_with_cheat, method = "color", type = "upper",
         title = "WITH CHEATING: High Correlation Across Exams", 
         mar = c(0, 0, 2, 0), tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("blue", "white", "red"))(200))

# ============================================================================
# VISUALIZATION 3: Autocorrelation Function (ACF) for Specific Students
# ============================================================================

# Calculate autocorrelation for residuals of individual students
calculate_student_acf <- function(student_data) {
  student_residuals <- student_data %>% 
    arrange(exam_num) %>% 
    pull(residual)
  
  if(length(student_residuals) >= 2) {
    acf_result <- acf(student_residuals, plot = FALSE, lag.max = 2)
    return(tibble(
      lag = 1:2,
      acf = acf_result$acf[2:3]
    ))
  } else {
    return(tibble(lag = numeric(), acf = numeric()))
  }
}

# ACF for honest students
acf_honest <- exam_data_no_cheat %>%
  group_by(student_id) %>%
  group_modify(~ calculate_student_acf(.x)) %>%
  ungroup() %>%
  mutate(type = "No Cheating")

# ACF for cheaters
acf_cheaters <- exam_data_with_cheat %>%
  filter(is_cheater) %>%
  group_by(student_id) %>%
  group_modify(~ calculate_student_acf(.x)) %>%
  ungroup() %>%
  mutate(type = "Cheating")

# Combine and plot
acf_comparison <- bind_rows(acf_honest, acf_cheaters)

p3 <- acf_comparison %>%
  filter(lag == 1) %>%  # Focus on lag-1 autocorrelation
  ggplot(aes(x = acf, fill = type)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  facet_wrap(~ type, ncol = 1) +
  labs(
    title = "Lag-1 Autocorrelation of Residuals",
    subtitle = "No cheating: centered near 0 | Cheating: shifted positive",
    x = "Autocorrelation Coefficient (ρ₁)",
    y = "Number of Students",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

print(p3)

# ============================================================================
# VISUALIZATION 4: Heatmap of Residuals (All Students × All Exams)
# ============================================================================

# Create heatmap data
create_heatmap <- function(data, title) {
  data %>%
    mutate(
      student_id = factor(student_id),
      student_id = fct_reorder(student_id, residual, .fun = mean)
    ) %>%
    ggplot(aes(x = exam, y = student_id, fill = residual)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-20, 20),
      name = "Residual"
    ) +
    labs(
      title = title,
      x = "Exam",
      y = "Student ID (ordered by avg residual)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = 12)
    )
}

p4 <- create_heatmap(exam_data_no_cheat, 
                     "NO CHEATING: Random Pattern Across Students & Exams")
p5 <- create_heatmap(exam_data_with_cheat, 
                     "WITH CHEATING: Persistent Red (Positive Residuals) for Some Students")

print(p4 / p5)

# ============================================================================
# STATISTICAL TESTS FOR SERIAL CORRELATION
# ============================================================================

# Durbin-Watson test requires time series structure
# Calculate DW statistic for a few students

library(lmtest)

# Function to test serial correlation for individual student
test_student_correlation <- function(student_data) {
  if(nrow(student_data) < 3) return(NA)
  
  # Simple regression of residuals on lagged residuals
  student_data <- student_data %>% arrange(exam_num)
  
  # Create lagged residual
  resid_lag <- c(NA, student_data$residual[1:(nrow(student_data)-1)])
  
  # Correlation between residual_t and residual_{t-1}
  cor(student_data$residual, resid_lag, use = "complete.obs")
}

# Test for all students
correlation_results <- tibble(
  scenario = c("No Cheating", "With Cheating"),
  data = list(exam_data_no_cheat, exam_data_with_cheat)
) %>%
  mutate(
    student_cors = map(data, function(df) {
      df %>%
        group_by(student_id) %>%
        summarize(
          lag1_cor = test_student_correlation(cur_data()),
          .groups = "drop"
        )
    })
  ) %>%
  select(-data) %>%
  unnest(student_cors)

# Summary statistics
cat("\n=== SERIAL CORRELATION TEST RESULTS ===\n\n")

correlation_results %>%
  group_by(scenario) %>%
  summarize(
    mean_lag1_cor = mean(lag1_cor, na.rm = TRUE),
    sd_lag1_cor = sd(lag1_cor, na.rm = TRUE),
    prop_positive = mean(lag1_cor > 0.3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ============================================================================
# VISUALIZATION 5: The Money Shot - Side-by-Side Residual Scatter
# ============================================================================

# Create lag-1 scatter plots
create_lag_scatter <- function(data, title) {
  # Create lagged residuals
  data_with_lag <- data %>%
    arrange(student_id, exam_num) %>%
    group_by(student_id) %>%
    mutate(residual_lag = lag(residual)) %>%
    ungroup() %>%
    filter(!is.na(residual_lag))
  
  # Calculate correlation
  cor_val <- cor(data_with_lag$residual, data_with_lag$residual_lag)
  
  ggplot(data_with_lag, aes(x = residual_lag, y = residual)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    annotate("text", x = Inf, y = Inf, 
             label = sprintf("ρ = %.3f", cor_val),
             hjust = 1.1, vjust = 1.5, size = 6, fontface = "bold",
             color = "darkred") +
    labs(
      title = title,
      x = "Residual at time t-1",
      y = "Residual at time t",
      caption = "Each point = one student-exam observation"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

p6 <- create_lag_scatter(exam_data_no_cheat, 
                         "NO CHEATING: No relationship between consecutive errors")
p7 <- create_lag_scatter(exam_data_with_cheat,
                         "WITH CHEATING: Positive correlation - past predicts future!")

print(p6 + p7)

# ============================================================================
# PEDAGOGICAL SUMMARY
# ============================================================================

cat("\n=== PEDAGOGICAL TAKEAWAYS ===\n\n")
cat("1. NO SERIAL CORRELATION means:\n")
cat("   - Error at time t tells you NOTHING about error at time t+1\n")
cat("   - Residuals jump randomly around zero\n")
cat("   - Correlation ρ ≈ 0\n\n")

cat("2. WITH SERIAL CORRELATION (cheating):\n")
cat("   - Error at time t PREDICTS error at time t+1\n")
cat("   - Same students persistently lucky/unlucky\n")
cat("   - Correlation ρ > 0 (positive serial correlation)\n\n")

cat("3. Why it matters:\n")
cat("   - If errors are correlated, standard errors are WRONG\n")
cat("   - Confidence intervals too narrow\n")
cat("   - T-statistics too large → false significance\n\n")

# Calculate impact on standard errors (for illustration)
# Standard error inflation factor for AR(1) process
rho_with_cheat <- cor(correlation_results %>% 
                        filter(scenario == "With Cheating") %>% 
                        pull(lag1_cor), 
                      use = "complete.obs")

inflation_factor <- sqrt((1 + rho_with_cheat) / (1 - rho_with_cheat))

cat(sprintf("4. With ρ = %.3f, true standard errors are %.2fx larger!\n", 
            mean(correlation_results %>% 
                   filter(scenario == "With Cheating") %>% 
                   pull(lag1_cor), na.rm = TRUE),
            inflation_factor))