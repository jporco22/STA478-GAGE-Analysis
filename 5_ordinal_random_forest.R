# ============================================================
# PARALLEL ORDINAL RANDOM FOREST GRID SEARCH
# RESPONSE: SRH_collapsed with 3 categories
# 1 = Very Poor/Poor/Fair
# 2 = Good
# 3 = Very Good
# ============================================================

library(parallel)
library(parallelly)
library(ranger)
library(dplyr)
library(tibble)

# ============================================================
# 1. PREDICTORS
# ============================================================

predictor_vars <- c(
  "socialself",
  "socialworld",
  "generalthreat",
  "generalsafety",
  "hh_cs_youngcoh",
  "list_crgender",
  "nationality_collapsed",
  "cr_cs_location"
)

# ============================================================
# 2. CREATE ANALYTIC DATA SET
# ============================================================

health_model_df <- health_reg_df %>%
  mutate(
    SRH_collapsed = case_when(
      cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 1,
      cr_hn_gnhlth_REV == 4 ~ 2,
      cr_hn_gnhlth_REV == 5 ~ 3,
      TRUE ~ NA_real_
    ),
    
    SRH_collapsed_num = as.numeric(SRH_collapsed),
    
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
    
    hh_cs_youngcoh = as.factor(hh_cs_youngcoh),
    list_crgender = as.factor(list_crgender),
    nationality_collapsed = as.factor(nationality_collapsed),
    cr_cs_location = as.factor(cr_cs_location)
  ) %>%
  dplyr::select(
    SRH_collapsed_num,
    SRH_collapsed_rf,
    all_of(predictor_vars)
  ) %>%
  na.omit()

rf_formula <- as.formula(
  paste("SRH_collapsed_rf ~", paste(predictor_vars, collapse = " + "))
)

# ============================================================
# 3. USER SETTINGS
# ============================================================

n_predictors <- length(predictor_vars)

max_seconds_per_model <- 180

# Set this to 0 if you do not want to filter models by minimum class accuracy.
# You can try 0.35, 0.40, or 0.50 depending on how strict you want to be.
min_required_class_accuracy <- 0.00

# Cutpoints for converting predicted ordinal score into 3 categories.
# Predicted score ranges from 1 to 3.
cutpoint_grid <- expand.grid(
  cut1 = seq(1.10, 2.00, by = 0.05),
  cut2 = seq(2.00, 2.90, by = 0.05)
) %>%
  filter(cut1 < cut2) %>%
  as_tibble()

# ============================================================
# 4. RANDOM FOREST GRID
# ============================================================

rf_grid <- expand.grid(
  num.trees = c(500, 1000),
  mtry = unique(pmax(1, pmin(n_predictors, c(2, 3, 4)))),
  min.node.size = c(5, 10, 20),
  sample.fraction = c(0.70, 0.80, 0.90),
  class_weight_1 = c(1, 2, 3, 4),
  class_weight_2 = c(1, 2, 3),
  class_weight_3 = c(1, 2, 3),
  train_prop = c(0.70, 0.80),
  seed = c(123)
) %>%
  as_tibble()

total_models <- nrow(rf_grid)

cat("Total ordinal random forest models to run:", total_models, "\n")
cat("Cutpoint combinations evaluated per model:", nrow(cutpoint_grid), "\n\n")

# ============================================================
# 5. HELPER FUNCTION: CONFUSION OUTPUT
# ============================================================

make_confusion_output_3class <- function(actual, predicted) {
  
  actual_factor <- factor(
    actual,
    levels = c(1, 2, 3),
    labels = c("PoorFair", "Good", "VeryGood")
  )
  
  predicted_factor <- factor(
    predicted,
    levels = c(1, 2, 3),
    labels = c("PoorFair", "Good", "VeryGood")
  )
  
  confusion_counts <- table(
    Observed = actual_factor,
    Predicted = predicted_factor
  )
  
  confusion_row_proportions <- prop.table(
    confusion_counts,
    margin = 1
  )
  
  overall_accuracy <- mean(actual == predicted)
  
  class_accuracy <- diag(confusion_row_proportions)
  
  balanced_accuracy <- mean(class_accuracy)
  
  min_class_accuracy <- min(class_accuracy)
  
  list(
    predicted = predicted,
    confusion_counts = confusion_counts,
    confusion_row_proportions = confusion_row_proportions,
    overall_accuracy = overall_accuracy,
    class_accuracy = class_accuracy,
    balanced_accuracy = balanced_accuracy,
    min_class_accuracy = min_class_accuracy
  )
}

# ============================================================
# 6. HELPER FUNCTION: CONVERT CLASS PROBABILITIES TO ORDINAL SCORE
# ============================================================

make_ordinal_score <- function(prob_matrix) {
  
  prob_matrix[, "PoorFair"] * 1 +
    prob_matrix[, "Good"] * 2 +
    prob_matrix[, "VeryGood"] * 3
}

# ============================================================
# 7. HELPER FUNCTION: EVALUATE CUTPOINTS
# ============================================================

evaluate_cutpoints <- function(actual, prob_matrix, cutpoint_grid) {
  
  ordinal_score <- make_ordinal_score(prob_matrix)
  
  cutpoint_results <- lapply(seq_len(nrow(cutpoint_grid)), function(j) {
    
    cut1 <- cutpoint_grid$cut1[j]
    cut2 <- cutpoint_grid$cut2[j]
    
    predicted <- ifelse(
      ordinal_score < cut1,
      1,
      ifelse(ordinal_score < cut2, 2, 3)
    )
    
    output <- make_confusion_output_3class(
      actual = actual,
      predicted = predicted
    )
    
    tibble(
      cut1 = cut1,
      cut2 = cut2,
      
      actual_1_pred_1 = output$class_accuracy["PoorFair"],
      actual_2_pred_2 = output$class_accuracy["Good"],
      actual_3_pred_3 = output$class_accuracy["VeryGood"],
      
      min_class_accuracy = output$min_class_accuracy,
      balanced_accuracy = output$balanced_accuracy,
      overall_accuracy = output$overall_accuracy
    )
  })
  
  bind_rows(cutpoint_results) %>%
    arrange(
      desc(min_class_accuracy),
      desc(balanced_accuracy),
      desc(overall_accuracy)
    )
}

# ============================================================
# 8. HELPER FUNCTION: FIT ONE ORDINAL RANDOM FOREST MODEL
# ============================================================

fit_one_ordinal_rf_model <- function(i, total_models) {
  
  current_seed <- rf_grid$seed[i]
  current_train_prop <- rf_grid$train_prop[i]
  
  set.seed(current_seed)
  
  idx_1 <- which(health_model_df$SRH_collapsed_num == 1)
  idx_2 <- which(health_model_df$SRH_collapsed_num == 2)
  idx_3 <- which(health_model_df$SRH_collapsed_num == 3)
  
  train_1 <- sample(idx_1, size = floor(current_train_prop * length(idx_1)))
  train_2 <- sample(idx_2, size = floor(current_train_prop * length(idx_2)))
  train_3 <- sample(idx_3, size = floor(current_train_prop * length(idx_3)))
  
  train_idx <- c(train_1, train_2, train_3)
  test_idx <- setdiff(seq_len(nrow(health_model_df)), train_idx)
  
  train_df <- health_model_df[train_idx, ]
  test_df <- health_model_df[test_idx, ]
  
  rf_fit <- ranger(
    formula = rf_formula,
    data = train_df,
    probability = TRUE,
    num.trees = rf_grid$num.trees[i],
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min.node.size[i],
    sample.fraction = rf_grid$sample.fraction[i],
    class.weights = c(
      PoorFair = rf_grid$class_weight_1[i],
      Good = rf_grid$class_weight_2[i],
      VeryGood = rf_grid$class_weight_3[i]
    ),
    importance = "impurity",
    seed = current_seed,
    num.threads = 1
  )
  
  # Full-data probabilities
  full_probs <- predict(
    rf_fit,
    data = health_model_df
  )$predictions
  
  full_cutpoint_results <- evaluate_cutpoints(
    actual = health_model_df$SRH_collapsed_num,
    prob_matrix = full_probs,
    cutpoint_grid = cutpoint_grid
  )
  
  best_full_cutpoint_row <- full_cutpoint_results %>%
    slice(1)
  
  best_cut1 <- best_full_cutpoint_row$cut1
  best_cut2 <- best_full_cutpoint_row$cut2
  
  full_score <- make_ordinal_score(full_probs)
  
  full_predicted <- ifelse(
    full_score < best_cut1,
    1,
    ifelse(full_score < best_cut2, 2, 3)
  )
  
  full_output <- make_confusion_output_3class(
    actual = health_model_df$SRH_collapsed_num,
    predicted = full_predicted
  )
  
  # Test-data probabilities
  if (nrow(test_df) > 0) {
    
    test_probs <- predict(
      rf_fit,
      data = test_df
    )$predictions
    
    test_score <- make_ordinal_score(test_probs)
    
    test_predicted <- ifelse(
      test_score < best_cut1,
      1,
      ifelse(test_score < best_cut2, 2, 3)
    )
    
    test_output <- make_confusion_output_3class(
      actual = test_df$SRH_collapsed_num,
      predicted = test_predicted
    )
    
    test_actual_1_pred_1 <- test_output$class_accuracy["PoorFair"]
    test_actual_2_pred_2 <- test_output$class_accuracy["Good"]
    test_actual_3_pred_3 <- test_output$class_accuracy["VeryGood"]
    test_min_class_accuracy <- test_output$min_class_accuracy
    test_balanced_accuracy <- test_output$balanced_accuracy
    test_overall_accuracy <- test_output$overall_accuracy
    
  } else {
    
    test_output <- NULL
    
    test_actual_1_pred_1 <- NA_real_
    test_actual_2_pred_2 <- NA_real_
    test_actual_3_pred_3 <- NA_real_
    test_min_class_accuracy <- NA_real_
    test_balanced_accuracy <- NA_real_
    test_overall_accuracy <- NA_real_
  }
  
  model_object <- list(
    model_id = i,
    model = NULL,
    tuning_parameters = rf_grid[i, ],
    full_cutpoint_results = full_cutpoint_results,
    best_full_cutpoint_row = best_full_cutpoint_row,
    full_output = full_output,
    test_output = test_output
  )
  
  summary_row <- tibble(
    model_id = i,
    
    num.trees = rf_grid$num.trees[i],
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min.node.size[i],
    sample.fraction = rf_grid$sample.fraction[i],
    class_weight_1 = rf_grid$class_weight_1[i],
    class_weight_2 = rf_grid$class_weight_2[i],
    class_weight_3 = rf_grid$class_weight_3[i],
    train_prop = current_train_prop,
    seed = current_seed,
    
    selected_cut1 = best_cut1,
    selected_cut2 = best_cut2,
    
    full_actual_1_pred_1 = full_output$class_accuracy["PoorFair"],
    full_actual_2_pred_2 = full_output$class_accuracy["Good"],
    full_actual_3_pred_3 = full_output$class_accuracy["VeryGood"],
    full_min_class_accuracy = full_output$min_class_accuracy,
    full_balanced_accuracy = full_output$balanced_accuracy,
    full_overall_accuracy = full_output$overall_accuracy,
    
    test_actual_1_pred_1 = test_actual_1_pred_1,
    test_actual_2_pred_2 = test_actual_2_pred_2,
    test_actual_3_pred_3 = test_actual_3_pred_3,
    test_min_class_accuracy = test_min_class_accuracy,
    test_balanced_accuracy = test_balanced_accuracy,
    test_overall_accuracy = test_overall_accuracy
  )
  
  list(
    model = model_object,
    summary = summary_row
  )
}

# ============================================================
# 9. PARALLEL SETUP WITH LIVE PRINTING
# ============================================================

n_workers <- max(1, parallelly::availableCores() - 1)

cat("Using", n_workers, "parallel workers\n")
cat("Total candidate models:", total_models, "\n")
cat("Maximum time per model:", max_seconds_per_model, "seconds\n\n")

progress_dir <- tempfile("ordinal_rf_progress_")
dir.create(progress_dir)

cl <- parallel::makeCluster(
  n_workers,
  type = "PSOCK",
  outfile = ""
)

ordinal_rf_results <- tryCatch({
  
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(ranger)
      library(dplyr)
      library(tibble)
    })
    NULL
  })
  
  parallel::clusterExport(
    cl = cl,
    varlist = c(
      "health_model_df",
      "rf_grid",
      "rf_formula",
      "cutpoint_grid",
      "total_models",
      "progress_dir",
      "max_seconds_per_model",
      "make_confusion_output_3class",
      "make_ordinal_score",
      "evaluate_cutpoints",
      "fit_one_ordinal_rf_model"
    ),
    envir = .GlobalEnv
  )
  
  parallel::parLapplyLB(
    cl = cl,
    X = seq_len(total_models),
    fun = function(i) {
      
      cat(
        sprintf(
          "[%s] START ordinal random forest model %d of %d\n",
          format(Sys.time(), "%H:%M:%S"),
          i,
          total_models
        )
      )
      flush.console()
      
      result <- tryCatch(
        {
          setTimeLimit(
            elapsed = max_seconds_per_model,
            transient = TRUE
          )
          
          fit_result <- fit_one_ordinal_rf_model(
            i = i,
            total_models = total_models
          )
          
          setTimeLimit(
            elapsed = Inf,
            transient = FALSE
          )
          
          fit_result
        },
        error = function(e) {
          
          setTimeLimit(
            elapsed = Inf,
            transient = FALSE
          )
          
          error_message <- conditionMessage(e)
          
          if (grepl("time limit|elapsed time limit|reached elapsed", error_message)) {
            
            cat(
              sprintf(
                "[%s] SKIPPED ordinal random forest model %d of %d because it took more than %d seconds\n",
                format(Sys.time(), "%H:%M:%S"),
                i,
                total_models,
                max_seconds_per_model
              )
            )
            flush.console()
            
            return(NULL)
          }
          
          cat(
            sprintf(
              "[%s] ERROR ordinal random forest model %d of %d: %s\n",
              format(Sys.time(), "%H:%M:%S"),
              i,
              total_models,
              error_message
            )
          )
          flush.console()
          
          stop(e)
        }
      )
      
      done_file <- file.path(
        progress_dir,
        paste0("model_", i, ".done")
      )
      
      file.create(done_file)
      
      analyzed_so_far <- length(
        list.files(
          progress_dir,
          pattern = "\\.done$",
          full.names = TRUE
        )
      )
      
      if (is.null(result)) {
        
        cat(
          sprintf(
            "[%s] Analyzed %d models out of %d candidate models\n",
            format(Sys.time(), "%H:%M:%S"),
            analyzed_so_far,
            total_models
          )
        )
        flush.console()
        
        return(NULL)
      }
      
      cat(
        sprintf(
          "[%s] END ordinal random forest model %d of %d\n",
          format(Sys.time(), "%H:%M:%S"),
          i,
          total_models
        )
      )
      
      cat(
        sprintf(
          "[%s] Analyzed %d models out of %d candidate models\n",
          format(Sys.time(), "%H:%M:%S"),
          analyzed_so_far,
          total_models
        )
      )
      
      flush.console()
      
      result
    }
  )
  
}, finally = {
  
  parallel::stopCluster(cl)
  
  cat("\nParallel cluster stopped.\n")
  
  if (dir.exists(progress_dir)) {
    unlink(progress_dir, recursive = TRUE)
  }
})

# ============================================================
# 10. COMBINE RESULTS
# ============================================================

ordinal_rf_results_valid <- Filter(
  Negate(is.null),
  ordinal_rf_results
)

cat(
  "\nSkipped or failed",
  total_models - length(ordinal_rf_results_valid),
  "models.\n"
)

if (length(ordinal_rf_results_valid) == 0) {
  stop("All ordinal random forest models were skipped or failed.")
}

ordinal_rf_summary_all <- bind_rows(
  lapply(
    ordinal_rf_results_valid,
    function(x) x$summary
  )
)

ordinal_rf_summary <- ordinal_rf_summary_all %>%
  filter(
    full_actual_1_pred_1 >= min_required_class_accuracy,
    full_actual_2_pred_2 >= min_required_class_accuracy,
    full_actual_3_pred_3 >= min_required_class_accuracy
  ) %>%
  arrange(
    desc(full_min_class_accuracy),
    desc(full_balanced_accuracy),
    desc(full_overall_accuracy)
  )

cat(
  "\nModels satisfying minimum class accuracy of",
  100 * min_required_class_accuracy,
  "% for all 3 observed categories:",
  nrow(ordinal_rf_summary),
  "out of",
  nrow(ordinal_rf_summary_all),
  "\n"
)

ordinal_rf_summary

# ============================================================
# 11. EXTRACT BEST MODEL AND REFIT ON FULL DATA
# ============================================================

best_model_id <- ordinal_rf_summary$model_id[1]

best_result_index <- which(
  sapply(
    ordinal_rf_results_valid,
    function(x) x$model$model_id
  ) == best_model_id
)

ordinal_rf_best_model_object <- ordinal_rf_results_valid[[best_result_index]]$model

ordinal_rf_best_cut1 <- ordinal_rf_best_model_object$best_full_cutpoint_row$cut1
ordinal_rf_best_cut2 <- ordinal_rf_best_model_object$best_full_cutpoint_row$cut2

ordinal_rf_best_parameters <- ordinal_rf_best_model_object$tuning_parameters

cat("\nBest model ID:", best_model_id, "\n")
cat("Best cutpoint 1:", ordinal_rf_best_cut1, "\n")
cat("Best cutpoint 2:", ordinal_rf_best_cut2, "\n\n")

cat("Best tuning parameters:\n")
print(ordinal_rf_best_parameters)

cat("\nRefitting best ordinal random forest model on full data...\n")

ordinal_rf_best_model <- ranger(
  formula = rf_formula,
  data = health_model_df,
  probability = TRUE,
  num.trees = ordinal_rf_best_parameters$num.trees,
  mtry = ordinal_rf_best_parameters$mtry,
  min.node.size = ordinal_rf_best_parameters$min.node.size,
  sample.fraction = ordinal_rf_best_parameters$sample.fraction,
  class.weights = c(
    PoorFair = ordinal_rf_best_parameters$class_weight_1,
    Good = ordinal_rf_best_parameters$class_weight_2,
    VeryGood = ordinal_rf_best_parameters$class_weight_3
  ),
  importance = "impurity",
  seed = ordinal_rf_best_parameters$seed,
  num.threads = max(1, parallelly::availableCores() - 1)
)

ordinal_rf_best_model

# ============================================================
# 12. SAVE BEST MODEL
# ============================================================

ordinal_rf_saved_object <- list(
  model = ordinal_rf_best_model,
  cut1 = ordinal_rf_best_cut1,
  cut2 = ordinal_rf_best_cut2,
  predictor_vars = predictor_vars,
  rf_formula = rf_formula,
  best_parameters = ordinal_rf_best_parameters
)

saveRDS(
  ordinal_rf_saved_object,
  file = "ordinal_rf_best_model_saved.rds"
)

# ============================================================
# 13. CONFUSION MATRICES
# ============================================================

ordinal_rf_full_conf_counts <- ordinal_rf_best_model_object$full_output$confusion_counts

ordinal_rf_full_conf_props <- prop.table(
  ordinal_rf_full_conf_counts,
  margin = 1
)

cat("\nFull-data confusion matrix from selected grid-search model: counts\n")
print(ordinal_rf_full_conf_counts)

cat("\nFull-data confusion matrix from selected grid-search model: row percentages\n")
print(round(100 * ordinal_rf_full_conf_props, 2))

if (!is.null(ordinal_rf_best_model_object$test_output)) {
  
  ordinal_rf_test_conf_counts <- ordinal_rf_best_model_object$test_output$confusion_counts
  
  ordinal_rf_test_conf_props <- prop.table(
    ordinal_rf_test_conf_counts,
    margin = 1
  )
  
  cat("\nTest-data confusion matrix from selected grid-search model: counts\n")
  print(ordinal_rf_test_conf_counts)
  
  cat("\nTest-data confusion matrix from selected grid-search model: row percentages\n")
  print(round(100 * ordinal_rf_test_conf_props, 2))
}

# ============================================================
# 14. CONFUSION MATRIX FOR REFIT BEST MODEL ON FULL DATA
# ============================================================

ordinal_rf_best_full_probs_refit <- predict(
  ordinal_rf_best_model,
  data = health_model_df
)$predictions

ordinal_rf_best_full_score_refit <- make_ordinal_score(
  ordinal_rf_best_full_probs_refit
)

ordinal_rf_best_full_predicted_refit <- ifelse(
  ordinal_rf_best_full_score_refit < ordinal_rf_best_cut1,
  1,
  ifelse(ordinal_rf_best_full_score_refit < ordinal_rf_best_cut2, 2, 3)
)

ordinal_rf_best_full_output_refit <- make_confusion_output_3class(
  actual = health_model_df$SRH_collapsed_num,
  predicted = ordinal_rf_best_full_predicted_refit
)

ordinal_rf_best_refit_conf_counts <- ordinal_rf_best_full_output_refit$confusion_counts

ordinal_rf_best_refit_conf_props <- prop.table(
  ordinal_rf_best_refit_conf_counts,
  margin = 1
)

cat("\nRefit best model full-data confusion matrix: counts\n")
print(ordinal_rf_best_refit_conf_counts)

cat("\nRefit best model full-data confusion matrix: row percentages\n")
print(round(100 * ordinal_rf_best_refit_conf_props, 2))

# ============================================================
# 15. CONFUSION MATRIX WITH PERCENTAGES AND COUNTS
# ============================================================

make_percent_count_matrix <- function(conf_counts) {
  
  conf_props <- prop.table(conf_counts, margin = 1)
  
  output <- matrix(
    paste0(
      round(100 * conf_props, 2),
      "% (",
      conf_counts,
      ")"
    ),
    nrow = nrow(conf_counts),
    ncol = ncol(conf_counts)
  )
  
  rownames(output) <- rownames(conf_counts)
  colnames(output) <- colnames(conf_counts)
  
  output
}

ordinal_rf_full_conf_percent_count <- make_percent_count_matrix(
  ordinal_rf_full_conf_counts
)

cat("\nFull-data confusion matrix from selected grid-search model: percentage with counts\n")
print(ordinal_rf_full_conf_percent_count)

ordinal_rf_best_refit_conf_percent_count <- make_percent_count_matrix(
  ordinal_rf_best_refit_conf_counts
)

cat("\nRefit best model full-data confusion matrix: percentage with counts\n")
print(ordinal_rf_best_refit_conf_percent_count)

# ============================================================
# 16. FINAL ACCURACY SUMMARY FOR TABLES
# ============================================================

ordinal_rf_final_summary <- tibble(
  model = "Ordinal random forest",
  overall_accuracy = ordinal_rf_best_full_output_refit$overall_accuracy,
  correct_poor_fair = ordinal_rf_best_full_output_refit$class_accuracy["PoorFair"],
  correct_good = ordinal_rf_best_full_output_refit$class_accuracy["Good"],
  correct_very_good = ordinal_rf_best_full_output_refit$class_accuracy["VeryGood"],
  balanced_accuracy = ordinal_rf_best_full_output_refit$balanced_accuracy,
  min_class_accuracy = ordinal_rf_best_full_output_refit$min_class_accuracy,
  cut1 = ordinal_rf_best_cut1,
  cut2 = ordinal_rf_best_cut2
)

cat("\nFinal ordinal random forest accuracy summary:\n")
print(ordinal_rf_final_summary)

# ============================================================
# 17. VARIABLE IMPORTANCE
# ============================================================

ordinal_rf_best_importance <- tibble(
  variable = names(ordinal_rf_best_model$variable.importance),
  importance = as.numeric(ordinal_rf_best_model$variable.importance)
) %>%
  arrange(desc(importance))

cat("\nVariable importance for refit best ordinal random forest model:\n")
print(ordinal_rf_best_importance)












# ============================================================
# SIMPLE FINAL SUMMARY TABLE FOR BEST ORDINAL RANDOM FOREST
# ============================================================

# Confusion matrix from refit best model
cm <- ordinal_rf_best_refit_conf_counts

# Row-wise accuracy for each observed category
class_acc <- diag(prop.table(cm, margin = 1))

# Overall accuracy
overall_acc <- sum(diag(cm)) / sum(cm)

# Balanced accuracy
balanced_acc <- mean(class_acc)

# Minimum class accuracy
min_class_acc <- min(class_acc)

# OOB Brier score
actual_class <- factor(
  health_model_df$SRH_collapsed_num,
  levels = c(1, 2, 3),
  labels = c("PoorFair", "Good", "VeryGood")
)

actual_matrix <- model.matrix(~ actual_class - 1)
colnames(actual_matrix) <- c("PoorFair", "Good", "VeryGood")

prob_matrix <- ordinal_rf_best_model$predictions

oob_brier <- mean(rowSums((actual_matrix - prob_matrix)^2))

# Final table
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
    "OOB Brier score"
  ),
  Value = c(
    paste0(round(100 * overall_acc, 2), "%"),
    paste0(round(100 * class_acc["PoorFair"], 2), "%"),
    paste0(round(100 * class_acc["Good"], 2), "%"),
    paste0(round(100 * class_acc["VeryGood"], 2), "%"),
    paste0(round(100 * balanced_acc, 2), "%"),
    paste0(round(100 * min_class_acc, 2), "%"),
    round(ordinal_rf_best_cut1, 3),
    round(ordinal_rf_best_cut2, 3),
    round(oob_brier, 3)
  )
)

ordinal_rf_summary_table
