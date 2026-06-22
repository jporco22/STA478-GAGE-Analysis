# ============================================================
# LOAD AND EVALUATE SAVED ORDINAL RANDOM FOREST MODEL
# RESPONSE: SRH_collapsed with 3 ordered categories
# 1 = Very Poor/Poor/Fair
# 2 = Good
# 3 = Very Good
# ============================================================

library(ranger)
library(dplyr)
library(tibble)

# ============================================================
# 1. LOAD SAVED ORDINAL RANDOM FOREST MODEL
# ============================================================

ordinal_rf_saved_object <- readRDS(
  "~/Desktop/GAGE-Analysis/ordinal_rf_best_model_saved.rds"
)

ordinal_rf_best_model <- ordinal_rf_saved_object$model
ordinal_rf_best_cut1 <- ordinal_rf_saved_object$cut1
ordinal_rf_best_cut2 <- ordinal_rf_saved_object$cut2
predictor_vars <- ordinal_rf_saved_object$predictor_vars
ordinal_rf_best_parameters <- ordinal_rf_saved_object$best_parameters

cat("\nSaved ordinal random forest model loaded.\n")
cat("Cutpoint 1:", ordinal_rf_best_cut1, "\n")
cat("Cutpoint 2:", ordinal_rf_best_cut2, "\n\n")

cat("Best tuning parameters:\n")
print(ordinal_rf_best_parameters)

# ============================================================
# 2. PREPARE DATA FOR PREDICTION
#    Use the same outcome coding and factor conversion as the
#    script that created the saved model.
# ============================================================

# health_model_df <- health_reg_df %>%
#   mutate(
#     SRH_collapsed_num = case_when(
#       cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 1,
#       cr_hn_gnhlth_REV == 4 ~ 2,
#       cr_hn_gnhlth_REV == 5 ~ 3,
#       TRUE ~ NA_real_
#     ),
#     SRH_collapsed_rf = factor(
#       case_when(
#         SRH_collapsed_num == 1 ~ "PoorFair",
#         SRH_collapsed_num == 2 ~ "Good",
#         SRH_collapsed_num == 3 ~ "VeryGood",
#         TRUE ~ NA_character_
#       ),
#       levels = c("PoorFair", "Good", "VeryGood"),
#       ordered = TRUE
#     ),
#     hh_cs_youngcoh = as.factor(hh_cs_youngcoh),
#     list_crgender = as.factor(list_crgender),
#     nationality_collapsed = as.factor(nationality_collapsed),
#     cr_cs_location = as.factor(cr_cs_location)
#   ) %>%
#   dplyr::select(
#     SRH_collapsed_num,
#     SRH_collapsed_rf,
#     dplyr::all_of(predictor_vars)
#   ) %>%
#   na.omit()
# 
# prediction_df <- health_model_df %>%
#   dplyr::select(dplyr::all_of(predictor_vars))

factor_vars <- c(
  "hh_cs_youngcoh",
  "list_crgender",
  "nationality_collapsed",
  "cr_cs_location"
)

health_model_df <- health_reg_df %>%
  mutate(
    SRH_collapsed_num = case_when(
      cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 1,
      cr_hn_gnhlth_REV == 4 ~ 2,
      cr_hn_gnhlth_REV == 5 ~ 3,
      TRUE ~ NA_real_
    ),
    
    SRH_collapsed_rf = factor(
      case_when(
        SRH_collapsed_num == 1 ~ "PoorFair",
        SRH_collapsed_num == 2 ~ "Good",
        SRH_collapsed_num == 3 ~ "VeryGood",
        TRUE ~ NA_character_
      ),
      levels = c("PoorFair", "Good", "VeryGood"),
      ordered = TRUE
    ),
    
    across(
      all_of(factor_vars),
      ~ haven::as_factor(.x)
    )
  ) %>%
  dplyr::select(
    SRH_collapsed_num,
    SRH_collapsed_rf,
    dplyr::all_of(predictor_vars)
  ) %>%
  na.omit()


# ============================================================
# 3. PREDICT CLASS PROBABILITIES
# ============================================================

ordinal_rf_probs <- predict(
  ordinal_rf_best_model,
  data = prediction_df
)$predictions

# Ensure probability columns are in the required order
ordinal_rf_probs <- ordinal_rf_probs[, c("PoorFair", "Good", "VeryGood")]

# ============================================================
# 4. CONVERT PROBABILITIES TO AN ORDINAL SCORE
# ============================================================

ordinal_rf_score <-
  ordinal_rf_probs[, "PoorFair"] * 1 +
  ordinal_rf_probs[, "Good"] * 2 +
  ordinal_rf_probs[, "VeryGood"] * 3

# ============================================================
# 5. CONVERT ORDINAL SCORES TO PREDICTED CATEGORIES
# ============================================================

ordinal_rf_predicted_num <- ifelse(
  ordinal_rf_score < ordinal_rf_best_cut1,
  1,
  ifelse(
    ordinal_rf_score < ordinal_rf_best_cut2,
    2,
    3
  )
)

ordinal_rf_predicted_class <- factor(
  ordinal_rf_predicted_num,
  levels = c(1, 2, 3),
  labels = c("PoorFair", "Good", "VeryGood")
)

ordinal_rf_observed_class <- factor(
  health_model_df$SRH_collapsed_num,
  levels = c(1, 2, 3),
  labels = c("PoorFair", "Good", "VeryGood")
)

# ============================================================
# 6. CONFUSION MATRIX: COUNTS
# ============================================================

ordinal_rf_conf_counts <- table(
  Observed = ordinal_rf_observed_class,
  Predicted = ordinal_rf_predicted_class
)

cat("\nConfusion matrix: counts\n")
print(ordinal_rf_conf_counts)

# ============================================================
# 7. CONFUSION MATRIX: ROW PERCENTAGES
# ============================================================

ordinal_rf_conf_row_props <- prop.table(
  ordinal_rf_conf_counts,
  margin = 1
)

cat("\nConfusion matrix: row percentages\n")
print(round(100 * ordinal_rf_conf_row_props, 2))

# ============================================================
# 8. CONFUSION MATRIX: PERCENTAGES WITH COUNTS
# ============================================================

ordinal_rf_conf_percent_count <- matrix(
  paste0(
    round(100 * ordinal_rf_conf_row_props, 2),
    "% (",
    ordinal_rf_conf_counts,
    ")"
  ),
  nrow = nrow(ordinal_rf_conf_counts),
  ncol = ncol(ordinal_rf_conf_counts)
)

rownames(ordinal_rf_conf_percent_count) <- rownames(ordinal_rf_conf_counts)
colnames(ordinal_rf_conf_percent_count) <- colnames(ordinal_rf_conf_counts)

cat("\nConfusion matrix: row percentage with counts\n")
print(ordinal_rf_conf_percent_count)

# ============================================================
# 9. ACCURACY MEASURES
# ============================================================

overall_accuracy <- mean(
  health_model_df$SRH_collapsed_num == ordinal_rf_predicted_num
)

class_poor_fair_accuracy <- ordinal_rf_conf_row_props[
  "PoorFair",
  "PoorFair"
]

class_good_accuracy <- ordinal_rf_conf_row_props[
  "Good",
  "Good"
]

class_very_good_accuracy <- ordinal_rf_conf_row_props[
  "VeryGood",
  "VeryGood"
]

balanced_accuracy <- mean(
  c(
    class_poor_fair_accuracy,
    class_good_accuracy,
    class_very_good_accuracy
  )
)

min_class_accuracy <- min(
  class_poor_fair_accuracy,
  class_good_accuracy,
  class_very_good_accuracy
)

cat("\nOverall accuracy:",
    round(100 * overall_accuracy, 2), "%\n")

cat("Correct prediction of observed Very Poor/Poor/Fair:",
    round(100 * class_poor_fair_accuracy, 2), "%\n")

cat("Correct prediction of observed Good:",
    round(100 * class_good_accuracy, 2), "%\n")

cat("Correct prediction of observed Very Good:",
    round(100 * class_very_good_accuracy, 2), "%\n")

cat("Balanced accuracy:",
    round(100 * balanced_accuracy, 2), "%\n")

cat("Minimum class accuracy:",
    round(100 * min_class_accuracy, 2), "%\n")

cat("Cutpoint 1 used:", ordinal_rf_best_cut1, "\n")
cat("Cutpoint 2 used:", ordinal_rf_best_cut2, "\n")

# ============================================================
# 10. MULTICLASS BRIER SCORE
# ============================================================

actual_class_matrix <- model.matrix(
  ~ ordinal_rf_observed_class - 1
)

colnames(actual_class_matrix) <- c(
  "PoorFair",
  "Good",
  "VeryGood"
)

multiclass_brier_score <- mean(
  rowSums(
    (actual_class_matrix - ordinal_rf_probs)^2
  )
)

cat("Multiclass Brier score:",
    round(multiclass_brier_score, 3), "\n")

# ============================================================
# 11. SUMMARY TABLE
# ============================================================

ordinal_rf_accuracy_summary <- tibble(
  model = "Ordinal random forest",
  overall_accuracy = overall_accuracy,
  correct_poor_fair = class_poor_fair_accuracy,
  correct_good = class_good_accuracy,
  correct_very_good = class_very_good_accuracy,
  balanced_accuracy = balanced_accuracy,
  min_class_accuracy = min_class_accuracy,
  cut1 = ordinal_rf_best_cut1,
  cut2 = ordinal_rf_best_cut2,
  multiclass_brier_score = multiclass_brier_score
)

cat("\nOrdinal random forest accuracy summary:\n")
print(ordinal_rf_accuracy_summary)

ordinal_rf_summary_table <- data.frame(
  Measure = c(
    "Overall accuracy",
    "Accuracy for observed Very Poor/Poor/Fair",
    "Accuracy for observed Good health",
    "Accuracy for observed Very Good health",
    "Balanced accuracy",
    "Minimum class accuracy",
    "Cutpoint 1",
    "Cutpoint 2",
    "Multiclass Brier score"
  ),
  Value = c(
    paste0(round(100 * overall_accuracy, 2), "%"),
    paste0(round(100 * class_poor_fair_accuracy, 2), "%"),
    paste0(round(100 * class_good_accuracy, 2), "%"),
    paste0(round(100 * class_very_good_accuracy, 2), "%"),
    paste0(round(100 * balanced_accuracy, 2), "%"),
    paste0(round(100 * min_class_accuracy, 2), "%"),
    round(ordinal_rf_best_cut1, 3),
    round(ordinal_rf_best_cut2, 3),
    round(multiclass_brier_score, 3)
  )
)

cat("\nFormatted summary table:\n")
print(ordinal_rf_summary_table)

# ============================================================
# 12. VARIABLE IMPORTANCE
# ============================================================

ordinal_rf_best_importance <- tibble(
  variable = names(ordinal_rf_best_model$variable.importance),
  importance = as.numeric(ordinal_rf_best_model$variable.importance)
) %>%
  arrange(desc(importance))

cat("\nVariable importance:\n")
print(ordinal_rf_best_importance)

# ============================================================
# 13. OPTIONAL: DATA SET WITH OBSERVED AND PREDICTED RESULTS
# ============================================================

ordinal_rf_prediction_results <- health_model_df %>%
  mutate(
    predicted_ordinal_score = ordinal_rf_score,
    predicted_category_num = ordinal_rf_predicted_num,
    predicted_category = ordinal_rf_predicted_class,
    probability_poor_fair = ordinal_rf_probs[, "PoorFair"],
    probability_good = ordinal_rf_probs[, "Good"],
    probability_very_good = ordinal_rf_probs[, "VeryGood"]
  )

head(ordinal_rf_prediction_results)
