# ============================================================
# LOAD SAVED RANDOM FOREST MODEL
# ============================================================

rf_saved_object <- readRDS("rf_best_model_saved.rds")

rf_best_model <- rf_saved_object$model
rf_best_threshold <- rf_saved_object$threshold
predictor_vars <- rf_saved_object$predictor_vars

# ============================================================
# PREPARE DATA FOR PREDICTION
# ============================================================

health_model_df$SRH_binary_num <- as.numeric(health_model_df$SRH_binary_num)

prediction_df <- health_model_df %>%
  dplyr::select(dplyr::all_of(predictor_vars))

# ============================================================
# PREDICT PROBABILITY OF CLASS 1
# ============================================================

rf_probs <- predict(
  rf_best_model,
  data = prediction_df
)$predictions[, "one"]

# ============================================================
# CONVERT PROBABILITIES TO 0/1 PREDICTIONS
# ============================================================

rf_predicted_class <- ifelse(
  rf_probs >= rf_best_threshold,
  1,
  0
)

# ============================================================
# CONFUSION MATRIX: COUNTS
# ============================================================

rf_conf_counts <- table(
  Observed = factor(health_model_df$SRH_binary_num, levels = c(0, 1)),
  Predicted = factor(rf_predicted_class, levels = c(0, 1))
)

cat("\nConfusion matrix: counts\n")
print(rf_conf_counts)

# ============================================================
# CONFUSION MATRIX: ROW PERCENTAGES
# ============================================================

rf_conf_row_props <- prop.table(
  rf_conf_counts,
  margin = 1
)

cat("\nConfusion matrix: row percentages\n")
print(round(100 * rf_conf_row_props, 2))

# ============================================================
# CONFUSION MATRIX: PERCENTAGES WITH COUNTS
# ============================================================

rf_conf_percent_count <- matrix(
  paste0(
    round(100 * rf_conf_row_props, 2),
    "% (",
    rf_conf_counts,
    ")"
  ),
  nrow = nrow(rf_conf_counts),
  ncol = ncol(rf_conf_counts)
)

rownames(rf_conf_percent_count) <- rownames(rf_conf_counts)
colnames(rf_conf_percent_count) <- colnames(rf_conf_counts)

cat("\nConfusion matrix: row percentage with counts\n")
print(rf_conf_percent_count)

# ============================================================
# ACCURACY MEASURES
# ============================================================

overall_accuracy <- mean(
  health_model_df$SRH_binary_num == rf_predicted_class
)

class_0_accuracy <- rf_conf_row_props["0", "0"]
class_1_accuracy <- rf_conf_row_props["1", "1"]

balanced_accuracy <- mean(
  c(class_0_accuracy, class_1_accuracy)
)

min_class_accuracy <- min(
  class_0_accuracy,
  class_1_accuracy
)

cat("\nOverall accuracy:", round(100 * overall_accuracy, 2), "%\n")
cat("Correct prediction of observed 0's:", round(100 * class_0_accuracy, 2), "%\n")
cat("Correct prediction of observed 1's:", round(100 * class_1_accuracy, 2), "%\n")
cat("Balanced accuracy:", round(100 * balanced_accuracy, 2), "%\n")
cat("Minimum class accuracy:", round(100 * min_class_accuracy, 2), "%\n")
cat("Threshold used:", rf_best_threshold, "\n")

