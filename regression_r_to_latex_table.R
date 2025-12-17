# ============================================================================
# R Function: Convert lm() Output to Publication-Quality LaTeX Table
# ============================================================================

#' Convert Linear Model to LaTeX Regression Table
#'
#' @param model An lm object from a linear regression
#' @param dep_var_name Name of dependent variable for table header (optional)
#' @param model_name Name for this model column (default: "Model 1")
#' @param se_type Type of standard errors: "normal", "robust", or "hac"
#' @param digits Number of decimal places (default: 3)
#' @param caption Table caption
#' @param label LaTeX label for cross-referencing
#' @return LaTeX code as a character string
lm_to_latex <- function(model, 
                        dep_var_name = NULL,
                        model_name = "Model 1",
                        se_type = "normal",
                        digits = 3,
                        caption = "Regression Results",
                        label = "tab:regression") {
  
  # Load required packages
  require(lmtest)
  require(sandwich)
  
  # Extract model information
  coef_summary <- summary(model)$coefficients
  n_obs <- nobs(model)
  r2 <- summary(model)$r.squared
  adj_r2 <- summary(model)$adj.r.squared
  rse <- summary(model)$sigma
  f_stat <- summary(model)$fstatistic[1]
  
  # Get coefficients and standard errors
  coefs <- coef_summary[, "Estimate"]
  
  # Calculate appropriate standard errors
  if (se_type == "robust") {
    vcov_matrix <- vcovHC(model, type = "HC1")
    ses <- sqrt(diag(vcov_matrix))
  } else if (se_type == "hac") {
    vcov_matrix <- vcovHAC(model)
    ses <- sqrt(diag(vcov_matrix))
  } else {
    ses <- coef_summary[, "Std. Error"]
  }
  
  # Calculate t-statistics and p-values
  t_stats <- coefs / ses
  p_values <- 2 * pt(abs(t_stats), df = n_obs - length(coefs), lower.tail = FALSE)
  
  # Add significance stars
  stars <- sapply(p_values, function(p) {
    if (p < 0.01) return("^{***}")
    else if (p < 0.05) return("^{**}")
    else if (p < 0.1) return("^{*}")
    else return("")
  })
  
  # Format coefficients with stars
  coef_formatted <- paste0("$", sprintf(paste0("%.", digits, "f"), coefs), stars, "$")
  se_formatted <- paste0("(", sprintf(paste0("%.", digits, "f"), ses), ")")
  
  # Get variable names
  var_names <- rownames(coef_summary)
  var_names[var_names == "(Intercept)"] <- "Constant"
  
  # Start building LaTeX code
  latex <- "\\begin{table}[htbp]\n"
  latex <- paste0(latex, "\\centering\n")
  latex <- paste0(latex, "\\caption{", caption, "}\n")
  latex <- paste0(latex, "\\label{", label, "}\n")
  latex <- paste0(latex, "\\begin{tabular}{lc}\n")
  latex <- paste0(latex, "\\toprule\n")
  latex <- paste0(latex, "\\toprule\n")
  
  # Add header
  if (!is.null(dep_var_name)) {
    latex <- paste0(latex, " & \\multicolumn{1}{c}{", dep_var_name, "} \\\\\n")
  } else {
    latex <- paste0(latex, " & \\multicolumn{1}{c}{", model_name, "} \\\\\n")
  }
  latex <- paste0(latex, "\\midrule\n")
  
  # Add coefficients
  for (i in 1:length(coefs)) {
    latex <- paste0(latex, var_names[i], " & ", coef_formatted[i], " \\\\\n")
    latex <- paste0(latex, "     & ", se_formatted[i], " \\\\\n")
    if (i < length(coefs)) {
      latex <- paste0(latex, "     & \\\\\n")
    }
  }
  
  # Add model statistics
  latex <- paste0(latex, "\\midrule\n")
  latex <- paste0(latex, "Observations & ", n_obs, " \\\\\n")
  latex <- paste0(latex, "R-squared & ", sprintf("%.3f", r2), " \\\\\n")
  latex <- paste0(latex, "Adjusted R-squared & ", sprintf("%.3f", adj_r2), " \\\\\n")
  latex <- paste0(latex, "Residual Std. Error & ", sprintf("%.3f", rse), " \\\\\n")
  
  # Add F-statistic with stars
  f_p_value <- pf(f_stat, 
                  df1 = summary(model)$fstatistic[2],
                  df2 = summary(model)$fstatistic[3],
                  lower.tail = FALSE)
  f_stars <- if (f_p_value < 0.01) "^{***}" 
  else if (f_p_value < 0.05) "^{**}" 
  else if (f_p_value < 0.1) "^{*}" 
  else ""
  
  latex <- paste0(latex, "F-statistic & $", sprintf("%.3f", f_stat), f_stars, "$ \\\\\n")
  
  # Close table
  latex <- paste0(latex, "\\bottomrule\n")
  latex <- paste0(latex, "\\bottomrule\n")
  latex <- paste0(latex, "\\multicolumn{2}{l}{\\footnotesize \\textit{Notes:} $^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1} \\\\\n")
  
  # Add SE type note
  se_note <- if (se_type == "robust") {
    "Robust standard errors in parentheses."
  } else if (se_type == "hac") {
    "HAC (Newey-West) standard errors in parentheses."
  } else {
    "Standard errors in parentheses."
  }
  latex <- paste0(latex, "\\multicolumn{2}{l}{\\footnotesize ", se_note, "} \\\\\n")
  
  latex <- paste0(latex, "\\end{tabular}\n")
  latex <- paste0(latex, "\\end{table}")
  
  return(latex)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Create example data
set.seed(42)
atm_data <- data.frame(
  time = 1:731,
  withdrawals = 86.5 - 0.06 * (1:731) + rnorm(731, 0, 10.5)
)

# Fit model
model <- lm(withdrawals ~ time, data = atm_data)

# Convert to LaTeX
latex_table <- lm_to_latex(
  model = model,
  dep_var_name = "Withdrawals",
  caption = "OLS Regression: ATM Withdrawals Over Time",
  label = "tab:atm_regression",
  digits = 3
)

# Print the LaTeX code
cat(latex_table)

# Save to file
writeLines(latex_table, "regression_table_from_r.tex")

cat("\n\n✓ LaTeX table saved to: regression_table_from_r.tex\n")

# ============================================================================
# BONUS: Function for multiple models side-by-side
# ============================================================================

#' Convert Multiple Models to LaTeX Table
#'
#' @param models List of lm objects
#' @param model_names Names for each model column
#' @param dep_var_name Name of dependent variable
#' @param ... Additional arguments passed to lm_to_latex
multiple_models_to_latex <- function(models,
                                     model_names = NULL,
                                     dep_var_name = NULL,
                                     digits = 3,
                                     caption = "Regression Results",
                                     label = "tab:regression") {
  
  n_models <- length(models)
  
  if (is.null(model_names)) {
    model_names <- paste("Model", 1:n_models)
  }
  
  # Extract information from all models
  all_vars <- unique(unlist(lapply(models, function(m) names(coef(m)))))
  
  # Start building table
  latex <- "\\begin{table}[htbp]\n"
  latex <- paste0(latex, "\\centering\n")
  latex <- paste0(latex, "\\caption{", caption, "}\n")
  latex <- paste0(latex, "\\label{", label, "}\n")
  latex <- paste0(latex, "\\begin{tabular}{l", paste(rep("c", n_models), collapse = ""), "}\n")
  latex <- paste0(latex, "\\toprule\n")
  
  # Add header
  header <- " & " 
  header <- paste0(header, paste(paste0("\\multicolumn{1}{c}{", model_names, "}"), collapse = " & "))
  latex <- paste0(latex, header, " \\\\\n")
  
  if (!is.null(dep_var_name)) {
    dep_header <- " & "
    dep_header <- paste0(dep_header, paste(rep(dep_var_name, n_models), collapse = " & "))
    latex <- paste0(latex, dep_header, " \\\\\n")
  }
  
  latex <- paste0(latex, "\\midrule\n")
  
  # Add coefficients for each variable
  for (var in all_vars) {
    var_display <- ifelse(var == "(Intercept)", "Constant", var)
    row <- var_display
    se_row <- ""
    
    for (i in 1:n_models) {
      model <- models[[i]]
      if (var %in% names(coef(model))) {
        coef_val <- coef(model)[var]
        se_val <- summary(model)$coefficients[var, "Std. Error"]
        p_val <- summary(model)$coefficients[var, "Pr(>|t|)"]
        
        stars <- if (p_val < 0.01) "^{***}" 
        else if (p_val < 0.05) "^{**}" 
        else if (p_val < 0.1) "^{*}" 
        else ""
        
        row <- paste0(row, " & $", sprintf(paste0("%.", digits, "f"), coef_val), stars, "$")
        se_row <- paste0(se_row, " & (", sprintf(paste0("%.", digits, "f"), se_val), ")")
      } else {
        row <- paste0(row, " & ")
        se_row <- paste0(se_row, " & ")
      }
    }
    
    latex <- paste0(latex, row, " \\\\\n")
    latex <- paste0(latex, se_row, " \\\\\n")
    latex <- paste0(latex, " & ", paste(rep("", n_models), collapse = " & "), " \\\\\n")
  }
  
  # Add model statistics
  latex <- paste0(latex, "\\midrule\n")
  
  # Observations
  n_row <- "Observations"
  for (model in models) {
    n_row <- paste0(n_row, " & ", nobs(model))
  }
  latex <- paste0(latex, n_row, " \\\\\n")
  
  # R-squared
  r2_row <- "$R^2$"
  for (model in models) {
    r2_row <- paste0(r2_row, " & ", sprintf("%.3f", summary(model)$r.squared))
  }
  latex <- paste0(latex, r2_row, " \\\\\n")
  
  # Close table
  latex <- paste0(latex, "\\bottomrule\n")
  latex <- paste0(latex, "\\multicolumn{", n_models + 1, "}{l}{\\footnotesize Standard errors in parentheses.} \\\\\n")
  latex <- paste0(latex, "\\multicolumn{", n_models + 1, "}{l}{\\footnotesize $^{***}$p$<$0.01; $^{**}$p$<$0.05; $^{*}$p$<$0.1} \\\\\n")
  latex <- paste0(latex, "\\end{tabular}\n")
  latex <- paste0(latex, "\\end{table}")
  
  return(latex)
}

# Example usage with multiple models
model1 <- lm(withdrawals ~ time, data = atm_data)
atm_data$time_sq <- atm_data$time^2
model2 <- lm(withdrawals ~ time + time_sq, data = atm_data)

multi_table <- multiple_models_to_latex(
  models = list(model1, model2),
  model_names = c("Linear", "Quadratic"),
  dep_var_name = "Withdrawals",
  caption = "Comparison of Model Specifications",
  label = "tab:model_comparison"
)

cat("\n\n")
cat(multi_table)

writeLines(multi_table, "multiple_models_table.tex")
cat("\n\n✓ Multiple models table saved to: multiple_models_table.tex\n")