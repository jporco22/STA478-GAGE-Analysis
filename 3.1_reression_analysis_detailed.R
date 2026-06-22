# # Resilience
# cyrm_lm
# 
# # Distress
# ghq_lm


# ============================================================
# MULTIPLE LINEAR REGRESSION DIAGNOSTICS
# Models:
#   cyrm_lm = Resilience
#   ghq_lm  = Distress
# ============================================================


# ------------------------------------------------------------
# 1. INSTALL AND LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "broom",
  "car",
  "lmtest",
  "ggplot2",
  "effectsize"
)

packages_to_install <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

library(broom)
library(car)
library(lmtest)
library(ggplot2)
library(effectsize)


# ------------------------------------------------------------
# 2. CREATE OUTPUT FOLDER
# ------------------------------------------------------------

output_folder <- "~/Desktop/multiple_regression_diagnostics"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


# ------------------------------------------------------------
# 3. FUNCTION TO ANALYZE ONE LINEAR MODEL
# ------------------------------------------------------------

analyze_lm <- function(model, model_name, output_folder) {
  
  if (!inherits(model, "lm")) {
    stop(model_name, " is not an lm object.")
  }
  
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", model_name)
  
  model_data <- model.frame(model)
  observed <- model.response(model_data)
  
  n <- nobs(model)
  p <- model$rank
  
  
  # ----------------------------------------------------------
  # Basic model results
  # ----------------------------------------------------------
  
  coefficient_table <- broom::tidy(
    model,
    conf.int = TRUE,
    conf.level = 0.95
  )
  
  model_fit <- broom::glance(model)
  
  standardized_coefficients <- tryCatch(
    {
      effectsize::standardize_parameters(
        model,
        method = "refit"
      )
    },
    error = function(e) {
      data.frame(
        Message = paste(
          "Standardized coefficients could not be calculated:",
          e$message
        )
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Influence diagnostics
  # ----------------------------------------------------------
  
  influence_table <- data.frame(
    Observation = rownames(model_data),
    Observed = as.numeric(observed),
    Fitted = fitted(model),
    Residual = residuals(model),
    Standardized_residual = rstandard(model),
    Studentized_residual = rstudent(model),
    Leverage = hatvalues(model),
    Cooks_distance = cooks.distance(model)
  )
  
  leverage_cutoff <- 2 * p / n
  cooks_cutoff <- 4 / n
  
  influence_table$Large_residual <- (
    abs(influence_table$Studentized_residual) > 3
  )
  
  influence_table$High_leverage <- (
    influence_table$Leverage > leverage_cutoff
  )
  
  influence_table$High_Cooks_distance <- (
    influence_table$Cooks_distance > cooks_cutoff
  )
  
  influence_table$Potentially_influential <- (
    influence_table$Large_residual |
      influence_table$High_leverage |
      influence_table$High_Cooks_distance
  )
  
  flagged_observations <- influence_table[
    influence_table$Potentially_influential,
  ]
  
  
  # ----------------------------------------------------------
  # Save numerical results as CSV files
  # ----------------------------------------------------------
  
  write.csv(
    coefficient_table,
    file.path(
      output_folder,
      paste0(safe_name, "_coefficients.csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    model_fit,
    file.path(
      output_folder,
      paste0(safe_name, "_model_fit.csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    standardized_coefficients,
    file.path(
      output_folder,
      paste0(safe_name, "_standardized_coefficients.csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    influence_table,
    file.path(
      output_folder,
      paste0(safe_name, "_influence_diagnostics.csv")
    ),
    row.names = FALSE
  )
  
  write.csv(
    flagged_observations,
    file.path(
      output_folder,
      paste0(safe_name, "_flagged_observations.csv")
    ),
    row.names = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Save complete numerical report as a text file
  # ----------------------------------------------------------
  
  report_file <- file.path(
    output_folder,
    paste0(safe_name, "_numerical_report.txt")
  )
  
  capture.output(
    {
      cat("============================================================\n")
      cat("MULTIPLE REGRESSION MODEL:", model_name, "\n")
      cat("============================================================\n\n")
      
      
      cat("MODEL FORMULA\n")
      cat("-------------\n")
      print(formula(model))
      
      
      cat("\n\nSAMPLE SIZE AND MODEL DIMENSIONS\n")
      cat("--------------------------------\n")
      cat("Number of observations:", n, "\n")
      cat("Number of estimated parameters:", p, "\n")
      cat("Residual degrees of freedom:", df.residual(model), "\n")
      
      
      cat("\n\nMODEL SUMMARY\n")
      cat("-------------\n")
      print(summary(model))
      
      
      cat("\n\nMODEL-FIT STATISTICS\n")
      cat("--------------------\n")
      print(model_fit)
      
      
      cat("\n\nCOEFFICIENTS WITH 95% CONFIDENCE INTERVALS\n")
      cat("------------------------------------------\n")
      print(coefficient_table)
      
      
      cat("\n\nSTANDARDIZED COEFFICIENTS\n")
      cat("-------------------------\n")
      print(standardized_coefficients)
      
      
      cat("\n\nSEQUENTIAL ANALYSIS OF VARIANCE\n")
      cat("--------------------------------\n")
      print(anova(model))
      
      
      cat("\n\nPARTIAL F TESTS FOR MODEL TERMS\n")
      cat("-------------------------------\n")
      
      drop1_result <- tryCatch(
        drop1(model, test = "F"),
        error = function(e) {
          paste("Could not calculate drop-one tests:", e$message)
        }
      )
      
      print(drop1_result)
      
      
      cat("\n\nTYPE II TESTS FOR MODEL TERMS\n")
      cat("-----------------------------\n")
      
      type_2_result <- tryCatch(
        car::Anova(model, type = 2),
        error = function(e) {
          paste("Could not calculate Type II tests:", e$message)
        }
      )
      
      print(type_2_result)
      
      
      cat("\n\nVARIANCE INFLATION FACTORS\n")
      cat("--------------------------\n")
      
      vif_result <- tryCatch(
        car::vif(model),
        error = function(e) {
          paste("Could not calculate VIF values:", e$message)
        }
      )
      
      print(vif_result)
      
      
      cat("\n\nBREUSCH-PAGAN TEST FOR HETEROSKEDASTICITY\n")
      cat("------------------------------------------\n")
      
      bp_result <- tryCatch(
        lmtest::bptest(model),
        error = function(e) {
          paste("Could not calculate Breusch-Pagan test:", e$message)
        }
      )
      
      print(bp_result)
      
      
      cat("\n\nRAMSEY RESET TEST FOR FUNCTIONAL FORM\n")
      cat("-------------------------------------\n")
      
      reset_result <- tryCatch(
        lmtest::resettest(
          model,
          power = 2:3,
          type = "fitted"
        ),
        error = function(e) {
          paste("Could not calculate RESET test:", e$message)
        }
      )
      
      print(reset_result)
      
      
      cat("\n\nDURBIN-WATSON TEST\n")
      cat("-------------------\n")
      cat(
        "This test is mainly relevant when observations have a",
        "meaningful time or sequence order.\n\n"
      )
      
      dw_result <- tryCatch(
        lmtest::dwtest(model),
        error = function(e) {
          paste("Could not calculate Durbin-Watson test:", e$message)
        }
      )
      
      print(dw_result)
      
      
      cat("\n\nSHAPIRO-WILK TEST OF RESIDUAL NORMALITY\n")
      cat("----------------------------------------\n")
      
      if (n >= 3 && n <= 5000) {
        print(shapiro.test(residuals(model)))
      } else {
        cat(
          "Shapiro-Wilk test omitted because the sample size is",
          "outside the allowable range of 3 to 5000.\n"
        )
      }
      
      
      cat("\n\nINFLUENCE-DIAGNOSTIC THRESHOLDS\n")
      cat("--------------------------------\n")
      cat("Absolute studentized residual greater than 3\n")
      cat("Leverage cutoff:", round(leverage_cutoff, 4), "\n")
      cat("Cook's distance cutoff:", round(cooks_cutoff, 4), "\n")
      
      
      cat("\n\nNUMBER OF FLAGGED OBSERVATIONS\n")
      cat("------------------------------\n")
      cat(
        "Large studentized residual:",
        sum(influence_table$Large_residual, na.rm = TRUE),
        "\n"
      )
      cat(
        "High leverage:",
        sum(influence_table$High_leverage, na.rm = TRUE),
        "\n"
      )
      cat(
        "High Cook's distance:",
        sum(influence_table$High_Cooks_distance, na.rm = TRUE),
        "\n"
      )
      cat(
        "Flagged by at least one criterion:",
        sum(influence_table$Potentially_influential, na.rm = TRUE),
        "\n"
      )
      
      
      cat("\n\nFLAGGED OBSERVATIONS\n")
      cat("--------------------\n")
      
      if (nrow(flagged_observations) == 0) {
        cat("No observations were flagged.\n")
      } else {
        print(flagged_observations)
      }
    },
    file = report_file
  )
  
  
  # ----------------------------------------------------------
  # Standard regression diagnostic plots
  # ----------------------------------------------------------
  
  pdf(
    file.path(
      output_folder,
      paste0(safe_name, "_diagnostic_plots.pdf")
    ),
    width = 10,
    height = 8
  )
  
  old_par <- par(mfrow = c(2, 2))
  
  plot(model, which = 1)
  plot(model, which = 2)
  plot(model, which = 3)
  plot(model, which = 5)
  
  par(old_par)
  dev.off()
  
  
  # ----------------------------------------------------------
  # Coefficient plot
  # ----------------------------------------------------------
  
  coefficient_plot_data <- coefficient_table[
    coefficient_table$term != "(Intercept)",
  ]
  
  coefficient_plot <- ggplot(
    coefficient_plot_data,
    aes(
      x = estimate,
      y = reorder(term, estimate)
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    geom_errorbarh(
      aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      height = 0.15
    ) +
    geom_point(size = 2.5) +
    labs(
      title = paste("Regression coefficients:", model_name),
      subtitle = "Points are estimated coefficients; lines are 95% confidence intervals",
      x = "Estimated coefficient",
      y = NULL
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0(safe_name, "_coefficient_plot.pdf")
    ),
    plot = coefficient_plot,
    width = 9,
    height = max(
      5,
      0.35 * nrow(coefficient_plot_data) + 2
    )
  )
  
  
  # ----------------------------------------------------------
  # Observed-versus-fitted plot
  # ----------------------------------------------------------
  
  prediction_data <- data.frame(
    Observed = as.numeric(observed),
    Fitted = fitted(model),
    Residual = residuals(model)
  )
  
  observed_fitted_plot <- ggplot(
    prediction_data,
    aes(
      x = Fitted,
      y = Observed
    )
  ) +
    geom_point(
      alpha = 0.5
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE
    ) +
    labs(
      title = paste("Observed versus fitted values:", model_name),
      x = "Fitted values",
      y = "Observed values"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0(safe_name, "_observed_vs_fitted.pdf")
    ),
    plot = observed_fitted_plot,
    width = 7,
    height = 6
  )
  
  
  # ----------------------------------------------------------
  # Residual histogram
  # ----------------------------------------------------------
  
  residual_plot <- ggplot(
    prediction_data,
    aes(x = Residual)
  ) +
    geom_histogram(
      bins = 30,
      boundary = 0
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = paste("Distribution of residuals:", model_name),
      x = "Residual",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0(safe_name, "_residual_histogram.pdf")
    ),
    plot = residual_plot,
    width = 7,
    height = 5
  )
  
  
  # ----------------------------------------------------------
  # Return results to R
  # ----------------------------------------------------------
  
  cat("\nCompleted analysis for:", model_name, "\n")
  cat("Numerical report:", report_file, "\n")
  
  invisible(
    list(
      model_fit = model_fit,
      coefficients = coefficient_table,
      standardized_coefficients = standardized_coefficients,
      influence_diagnostics = influence_table,
      flagged_observations = flagged_observations,
      coefficient_plot = coefficient_plot,
      observed_fitted_plot = observed_fitted_plot,
      residual_plot = residual_plot
    )
  )
}


# ------------------------------------------------------------
# 4. RUN THE ANALYSIS FOR BOTH MODELS
# ------------------------------------------------------------

cyrm_results <- analyze_lm(
  model = cyrm_lm,
  model_name = "Resilience_cyrm_lm",
  output_folder = output_folder
)

ghq_results <- analyze_lm(
  model = ghq_lm,
  model_name = "Distress_ghq_lm",
  output_folder = output_folder
)


# ------------------------------------------------------------
# 5. DISPLAY THE MAIN RESULTS IN THE CONSOLE
# ------------------------------------------------------------

cat("\n\n============================================================\n")
cat("RESILIENCE MODEL\n")
cat("============================================================\n")

summary(cyrm_lm)
cyrm_results$model_fit
cyrm_results$coefficients
cyrm_results$standardized_coefficients


cat("\n\n============================================================\n")
cat("DISTRESS MODEL\n")
cat("============================================================\n")

summary(ghq_lm)
ghq_results$model_fit
ghq_results$coefficients
ghq_results$standardized_coefficients
