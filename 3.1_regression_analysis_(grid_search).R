# ============================================================
# SIMPLE PARALLEL GRID SEARCH FOR ORDERED LOGISTIC WEIGHTS
# ============================================================

library(MASS)
library(dplyr)
library(tibble)
library(parallel)
library(parallelly)

# ------------------------------------------------------------
# 1. Keep only complete cases for variables used in the model
# ------------------------------------------------------------

vars_needed <- c(
  "SRH_collapsed",
  "socialself",
  "socialworld",
  "generalthreat",
  "generalsafety",
  "hh_cs_youngcoh",
  "list_crgender",
  "nationality_collapsed",
  "cr_cs_location"
)

model_data <- health_reg_df[, vars_needed]

model_data <- model_data[complete.cases(model_data), ]

# Make outcome ordered
model_data$SRH_collapsed <- ordered(model_data$SRH_collapsed)

# Observed values
observed_score <- model_data$SRH_collapsed

# ------------------------------------------------------------
# 2. Create grid of possible weights
# ------------------------------------------------------------

weight_grid_values <- seq(0.1, 5, by = 0.1)

weight_grid <- expand.grid(
  w1 = weight_grid_values,
  w2 = weight_grid_values,
  w3 = weight_grid_values
)

total_models <- nrow(weight_grid)

cat("Total models to fit:", total_models, "\n")

# ------------------------------------------------------------
# 3. Function to fit ONE model and return the objective value
# ------------------------------------------------------------

fit_one_model <- function(i) {
  
  w1 <- weight_grid$w1[i]
  w2 <- weight_grid$w2[i]
  w3 <- weight_grid$w3[i]
  
  weights_vec <- ifelse(
    model_data$SRH_collapsed == 1, w1,
    ifelse(
      model_data$SRH_collapsed == 2, w2,
      w3
    )
  )
  
  weights_vec <- weights_vec / mean(weights_vec)
  
  health_weighted <- tryCatch(
    {
      polr(
        SRH_collapsed ~ 
          socialself + 
          socialworld + 
          generalthreat + 
          generalsafety + 
          as.factor(hh_cs_youngcoh) + 
          as.factor(list_crgender) + 
          as.factor(nationality_collapsed) + 
          as.factor(cr_cs_location),
        weights = weights_vec,
        Hess = FALSE,
        data = model_data
      )
    },
    error = function(e) NULL
  )
  
  if (is.null(health_weighted)) {
    return(
      data.frame(
        w1 = w1,
        w2 = w2,
        w3 = w3,
        objective = NA_real_
      )
    )
  }
  
  predicted_score <- predict(health_weighted, type = "class")
  
  cm <- table(
    Observed = observed_score,
    Predicted = predicted_score
  )
  
  objective_value <- sum(diag(prop.table(cm, margin = 1)))
  
  data.frame(
    w1 = w1,
    w2 = w2,
    w3 = w3,
    objective = objective_value
  )
}

# ------------------------------------------------------------
# 4. Set up parallel cluster
# ------------------------------------------------------------

n_workers <- max(1, parallelly::availableCores() - 1)

cat("Using", n_workers, "workers\n")

cl <- makeCluster(n_workers, type = "PSOCK")

clusterEvalQ(cl, {
  library(MASS)
})

clusterExport(
  cl,
  varlist = c(
    "model_data",
    "observed_score",
    "weight_grid",
    "fit_one_model"
  )
)

# ------------------------------------------------------------
# 5. Run parallel grid search with simple progress printing
# ------------------------------------------------------------

batch_size <- n_workers * 5

model_indices <- seq_len(total_models)

batches <- split(
  model_indices,
  ceiling(seq_along(model_indices) / batch_size)
)

all_results <- list()

models_done <- 0

for (b in seq_along(batches)) {
  
  current_indices <- batches[[b]]
  
  batch_results <- parLapplyLB(
    cl,
    current_indices,
    fit_one_model
  )
  
  all_results <- c(all_results, batch_results)
  
  models_done <- models_done + length(current_indices)
  
  cat(
    "Completed",
    models_done,
    "of",
    total_models,
    "models |",
    total_models - models_done,
    "left\n"
  )
  
  flush.console()
}

stopCluster(cl)

# ------------------------------------------------------------
# 6. Combine results
# ------------------------------------------------------------

grid_results_df <- bind_rows(all_results)

# Remove failed model fits
grid_results_success <- grid_results_df %>%
  filter(!is.na(objective))

# ------------------------------------------------------------
# 7. Find best weights
# ------------------------------------------------------------

best_result <- grid_results_success %>%
  arrange(desc(objective)) %>%
  slice(1)

best_result
# 4.7 2.4 1.9  1.247157

best_w1 <- best_result$w1
best_w2 <- best_result$w2
best_w3 <- best_result$w3

cat("\nBest weights:\n")
cat("w1 =", best_w1, "\n")
cat("w2 =", best_w2, "\n")
cat("w3 =", best_w3, "\n")
cat("Best objective value =", best_result$objective, "\n")

# ------------------------------------------------------------
# 8. Refit final model using best weights
# ------------------------------------------------------------

best_weights_vec <- ifelse(
  model_data$SRH_collapsed == 1, best_w1,
  ifelse(
    model_data$SRH_collapsed == 2, best_w2,
    best_w3
  )
)

best_weights_vec <- best_weights_vec / mean(best_weights_vec)

health_weighted_best <- polr(
  SRH_collapsed ~ 
    socialself + 
    socialworld + 
    generalthreat + 
    generalsafety + 
    as.factor(hh_cs_youngcoh) + 
    as.factor(list_crgender) + 
    as.factor(nationality_collapsed) + 
    as.factor(cr_cs_location),
  weights = best_weights_vec,
  Hess = TRUE,
  data = model_data
)

# ------------------------------------------------------------
# 9. Final confusion matrix
# ------------------------------------------------------------

predicted_score <- predict(health_weighted_best, type = "class")

cm_best <- table(
  Observed = observed_score,
  Predicted = predicted_score
)

cm_best

round(prop.table(cm_best, margin = 1), 3)

sum(diag(prop.table(cm_best, margin = 1)))

