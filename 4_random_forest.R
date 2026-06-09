setwd("~/Desktop/GAGE-Analysis")


# ============================================================
# FULL PARALLEL RANDOM FOREST GRID SEARCH WITH LIVE PRINTING
# WITH 3-MINUTE MODEL SKIP RULE AND FINAL CONFUSION MATRIX
# ============================================================

library(parallel)
library(parallelly)
library(ranger)
library(dplyr)
library(tibble)

# ============================================================
# 1. USER SETTINGS
# ============================================================

# Analytic data set for Random Forest
# Requires health_reg_df from 3_regression_analysis.R
health_model_df <- health_reg_df %>%
  mutate(
    SRH_binary_num = case_when(
      SRH_binary == 0 ~ 0,
      SRH_binary == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    SRH_binary_fac = factor(
      ifelse(SRH_binary_num == 1, "one", "zero"),
      levels = c("zero", "one")
    ),
    hh_cs_youngcoh = as.factor(hh_cs_youngcoh),
    list_crgender = as.factor(list_crgender),
    nationality_collapsed = as.factor(nationality_collapsed),
    cr_cs_location = as.factor(cr_cs_location)
  ) %>%
  dplyr::select(
    SRH_binary_num,
    SRH_binary_fac,
    all_of(predictor_vars)
  ) %>%
  na.omit()



health_model_df$SRH_binary_num <- as.numeric(health_model_df$SRH_binary_num)

health_model_df$SRH_binary_rf <- factor(
  ifelse(health_model_df$SRH_binary_num == 1, "one", "zero"),
  levels = c("zero", "one")
)

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

health_model_df <- health_model_df %>%
  dplyr::select(
    SRH_binary_num,
    SRH_binary_rf,
    dplyr::all_of(predictor_vars)
  ) %>%
  na.omit()

rf_formula <- as.formula(
  paste("SRH_binary_rf ~", paste(predictor_vars, collapse = " + "))
)

threshold_grid <- seq(0.05, 0.95, by = 0.05)

min_required_class_accuracy <- 0.75

max_seconds_per_model <- 180


# ============================================================
# 2. RANDOM FOREST GRID
# ============================================================

n_predictors <- length(predictor_vars)

rf_grid <- expand.grid(
  num.trees = c(500, 1000),
  mtry = unique(pmax(1, pmin(n_predictors, c(2, 3, 4)))),
  min.node.size = c(5, 10, 20),
  sample.fraction = c(0.70, 0.80, 0.90),
  class_weight_0 = c(1, 2, 3),
  class_weight_1 = c(1, 2, 3),
  train_prop = c(0.70, 0.80),
  seed = c(123)
)

rf_grid <- as_tibble(rf_grid)

total_models <- nrow(rf_grid)

cat("Total random forest models to run:", total_models, "\n\n")


# ============================================================
# 3. HELPER FUNCTION: CONFUSION OUTPUT
# ============================================================

make_confusion_output <- function(actual, prob_one, threshold) {
  
  predicted <- ifelse(prob_one >= threshold, 1, 0)
  
  actual_factor <- factor(actual, levels = c(0, 1))
  predicted_factor <- factor(predicted, levels = c(0, 1))
  
  confusion_counts <- table(
    Observed = actual_factor,
    Predicted = predicted_factor
  )
  
  confusion_row_proportions <- prop.table(
    confusion_counts,
    margin = 1
  )
  
  overall_accuracy <- mean(actual == predicted)
  
  list(
    threshold = threshold,
    predicted = predicted,
    confusion_counts = confusion_counts,
    confusion_row_proportions = confusion_row_proportions,
    overall_accuracy = overall_accuracy
  )
}


# ============================================================
# 4. HELPER FUNCTION: EVALUATE THRESHOLDS
# ============================================================

evaluate_thresholds <- function(actual, prob_one, threshold_grid) {
  
  threshold_results <- lapply(threshold_grid, function(threshold) {
    
    output <- make_confusion_output(
      actual = actual,
      prob_one = prob_one,
      threshold = threshold
    )
    
    row_props <- output$confusion_row_proportions
    
    actual_0_pred_0 <- row_props["0", "0"]
    actual_1_pred_1 <- row_props["1", "1"]
    
    min_class_accuracy <- min(
      actual_0_pred_0,
      actual_1_pred_1
    )
    
    balanced_accuracy <- mean(
      c(actual_0_pred_0, actual_1_pred_1)
    )
    
    tibble(
      threshold = threshold,
      actual_0_pred_0 = actual_0_pred_0,
      actual_1_pred_1 = actual_1_pred_1,
      min_class_accuracy = min_class_accuracy,
      balanced_accuracy = balanced_accuracy,
      overall_accuracy = output$overall_accuracy
    )
  })
  
  bind_rows(threshold_results) %>%
    arrange(
      desc(min_class_accuracy),
      desc(balanced_accuracy),
      desc(overall_accuracy)
    )
}


# ============================================================
# 5. HELPER FUNCTION: FIT ONE RANDOM FOREST MODEL
# ============================================================

fit_one_rf_model <- function(i, total_models) {
  
  current_seed <- rf_grid$seed[i]
  current_train_prop <- rf_grid$train_prop[i]
  
  set.seed(current_seed)
  
  idx_zero <- which(health_model_df$SRH_binary_num == 0)
  idx_one  <- which(health_model_df$SRH_binary_num == 1)
  
  train_zero <- sample(
    idx_zero,
    size = floor(current_train_prop * length(idx_zero))
  )
  
  train_one <- sample(
    idx_one,
    size = floor(current_train_prop * length(idx_one))
  )
  
  train_idx <- c(train_zero, train_one)
  test_idx  <- setdiff(seq_len(nrow(health_model_df)), train_idx)
  
  train_df <- health_model_df[train_idx, ]
  test_df  <- health_model_df[test_idx, ]
  
  rf_fit <- ranger(
    formula = rf_formula,
    data = train_df,
    probability = TRUE,
    num.trees = rf_grid$num.trees[i],
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min.node.size[i],
    sample.fraction = rf_grid$sample.fraction[i],
    class.weights = c(
      zero = rf_grid$class_weight_0[i],
      one  = rf_grid$class_weight_1[i]
    ),
    importance = "impurity",
    seed = current_seed,
    num.threads = 1
  )
  
  full_probs <- predict(
    rf_fit,
    data = health_model_df
  )$predictions[, "one"]
  
  full_threshold_results <- evaluate_thresholds(
    actual = health_model_df$SRH_binary_num,
    prob_one = full_probs,
    threshold_grid = threshold_grid
  )
  
  best_full_threshold_row <- full_threshold_results %>%
    slice(1)
  
  best_full_threshold <- best_full_threshold_row$threshold
  
  full_output <- make_confusion_output(
    actual = health_model_df$SRH_binary_num,
    prob_one = full_probs,
    threshold = best_full_threshold
  )
  
  full_row_props <- full_output$confusion_row_proportions
  
  full_actual_0_pred_0 <- full_row_props["0", "0"]
  full_actual_1_pred_1 <- full_row_props["1", "1"]
  
  full_min_class_accuracy <- min(
    full_actual_0_pred_0,
    full_actual_1_pred_1
  )
  
  full_balanced_accuracy <- mean(
    c(full_actual_0_pred_0, full_actual_1_pred_1)
  )
  
  test_output <- NULL
  
  if (nrow(test_df) > 0) {
    
    test_probs <- predict(
      rf_fit,
      data = test_df
    )$predictions[, "one"]
    
    test_output <- make_confusion_output(
      actual = test_df$SRH_binary_num,
      prob_one = test_probs,
      threshold = best_full_threshold
    )
    
    test_row_props <- test_output$confusion_row_proportions
    
    test_actual_0_pred_0 <- test_row_props["0", "0"]
    test_actual_1_pred_1 <- test_row_props["1", "1"]
    
    test_min_class_accuracy <- min(
      test_actual_0_pred_0,
      test_actual_1_pred_1
    )
    
    test_balanced_accuracy <- mean(
      c(test_actual_0_pred_0, test_actual_1_pred_1)
    )
    
    test_overall_accuracy <- test_output$overall_accuracy
    
  } else {
    
    test_actual_0_pred_0 <- NA_real_
    test_actual_1_pred_1 <- NA_real_
    test_min_class_accuracy <- NA_real_
    test_balanced_accuracy <- NA_real_
    test_overall_accuracy <- NA_real_
  }
  
  model_object <- list(
    model_id = i,
    model = NULL,
    tuning_parameters = rf_grid[i, ],
    full_threshold_results = full_threshold_results,
    best_full_threshold_row = best_full_threshold_row,
    full_output = full_output,
    test_output = test_output
  )
  
  summary_row <- tibble(
    model_id = i,
    
    num.trees = rf_grid$num.trees[i],
    mtry = rf_grid$mtry[i],
    min.node.size = rf_grid$min.node.size[i],
    sample.fraction = rf_grid$sample.fraction[i],
    class_weight_0 = rf_grid$class_weight_0[i],
    class_weight_1 = rf_grid$class_weight_1[i],
    train_prop = current_train_prop,
    seed = current_seed,
    
    selected_threshold = best_full_threshold,
    
    full_actual_0_pred_0 = full_actual_0_pred_0,
    full_actual_1_pred_1 = full_actual_1_pred_1,
    full_min_class_accuracy = full_min_class_accuracy,
    full_balanced_accuracy = full_balanced_accuracy,
    full_overall_accuracy = full_output$overall_accuracy,
    
    test_actual_0_pred_0 = test_actual_0_pred_0,
    test_actual_1_pred_1 = test_actual_1_pred_1,
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
# 6. PARALLEL SETUP WITH LIVE CONSOLE PRINTING
# ============================================================

n_workers <- max(1, parallelly::availableCores() - 1)

cat("Using", n_workers, "parallel workers\n")
cat("Total candidate models:", total_models, "\n")
cat("Maximum time per model:", max_seconds_per_model, "seconds\n\n")

progress_dir <- tempfile("rf_progress_")
dir.create(progress_dir)

cl <- parallel::makeCluster(
  n_workers,
  type = "PSOCK",
  outfile = ""
)

rf_results <- tryCatch({
  
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
      "threshold_grid",
      "total_models",
      "progress_dir",
      "max_seconds_per_model",
      "make_confusion_output",
      "evaluate_thresholds",
      "fit_one_rf_model"
    ),
    envir = .GlobalEnv
  )
  
  parallel::parLapplyLB(
    cl = cl,
    X = seq_len(total_models),
    fun = function(i) {
      
      cat(
        sprintf(
          "[%s] START random forest model %d of %d\n",
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
          
          fit_result <- fit_one_rf_model(
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
                "[%s] SKIPPED random forest model %d of %d because it took more than %d seconds\n",
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
              "[%s] ERROR random forest model %d of %d: %s\n",
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
          "[%s] END random forest model %d of %d\n",
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
# 7. COMBINE RESULTS AND APPLY 75% REQUIREMENT
# ============================================================

rf_results_valid <- Filter(Negate(is.null), rf_results)

cat(
  "\nSkipped or failed",
  total_models - length(rf_results_valid),
  "models.\n"
)

if (length(rf_results_valid) == 0) {
  stop("All random forest models were skipped or failed.")
}

rf_summary_rows <- lapply(
  rf_results_valid,
  function(x) x$summary
)

rf_summary_all <- bind_rows(rf_summary_rows)

rf_summary <- rf_summary_all %>%
  filter(
    full_actual_0_pred_0 >= min_required_class_accuracy,
    full_actual_1_pred_1 >= min_required_class_accuracy
  ) %>%
  arrange(
    desc(full_min_class_accuracy),
    desc(full_balanced_accuracy),
    desc(full_overall_accuracy)
  )

cat(
  "\nModels satisfying at least",
  100 * min_required_class_accuracy,
  "% correct prediction for BOTH 0's and 1's:",
  nrow(rf_summary),
  "out of",
  nrow(rf_summary_all),
  "\n"
)

if (nrow(rf_summary) == 0) {
  
  stop(
    "No random forest model achieved at least 75% correct prediction for BOTH observed 0's and observed 1's. Try expanding the grid, adding predictors, changing class weights, or using a finer threshold grid."
  )
}

rf_summary


# ============================================================
# 8. EXTRACT BEST MODEL AND REFIT ON FULL DATA
# ============================================================

best_model_id <- rf_summary$model_id[1]

best_result_index <- which(
  sapply(
    rf_results_valid,
    function(x) x$model$model_id
  ) == best_model_id
)

rf_best_model_object <- rf_results_valid[[best_result_index]]$model

rf_best_threshold <- rf_best_model_object$best_full_threshold_row$threshold

rf_best_parameters <- rf_best_model_object$tuning_parameters

cat("\nBest model ID:", best_model_id, "\n")
cat("Best threshold:", rf_best_threshold, "\n\n")

cat("Best tuning parameters:\n")
print(rf_best_parameters)

cat("\nRefitting best model on full data...\n")

rf_best_model <- ranger(
  formula = rf_formula,
  data = health_model_df,
  probability = TRUE,
  num.trees = rf_best_parameters$num.trees,
  mtry = rf_best_parameters$mtry,
  min.node.size = rf_best_parameters$min.node.size,
  sample.fraction = rf_best_parameters$sample.fraction,
  class.weights = c(
    zero = rf_best_parameters$class_weight_0,
    one  = rf_best_parameters$class_weight_1
  ),
  importance = "impurity",
  seed = rf_best_parameters$seed,
  num.threads = max(1, parallelly::availableCores() - 1)
)


rf_best_model



# ============================================================
# SAVE BEST RANDOM FOREST MODEL
# ============================================================

rf_saved_object <- list(
  model = rf_best_model,
  threshold = rf_best_threshold,
  predictor_vars = predictor_vars,
  rf_formula = rf_formula,
  best_parameters = rf_best_parameters
)

saveRDS(
  rf_saved_object,
  file = "rf_best_model_saved.rds"
)






# ============================================================
# 9. CONFUSION MATRIX FOR BEST MODEL FROM GRID SEARCH
# ============================================================

rf_full_conf_counts <- rf_best_model_object$full_output$confusion_counts

rf_full_conf_props <- prop.table(
  rf_full_conf_counts,
  margin = 1
)

rf_test_conf_counts <- rf_best_model_object$test_output$confusion_counts

rf_test_conf_props <- prop.table(
  rf_test_conf_counts,
  margin = 1
)

cat("\nFull-data confusion matrix from selected grid-search model: counts\n")
print(rf_full_conf_counts)

cat("\nFull-data confusion matrix from selected grid-search model: row percentages\n")
print(round(100 * rf_full_conf_props, 2))

cat("\nTest-data confusion matrix from selected grid-search model: counts\n")
print(rf_test_conf_counts)

cat("\nTest-data confusion matrix from selected grid-search model: row percentages\n")
print(round(100 * rf_test_conf_props, 2))


# ============================================================
# 10. CONFUSION MATRIX FOR REFIT BEST MODEL ON FULL DATA
# ============================================================

rf_best_full_probs_refit <- predict(
  rf_best_model,
  data = health_model_df
)$predictions[, "one"]

rf_best_full_output_refit <- make_confusion_output(
  actual = health_model_df$SRH_binary_num,
  prob_one = rf_best_full_probs_refit,
  threshold = rf_best_threshold
)

rf_best_refit_conf_counts <- rf_best_full_output_refit$confusion_counts

rf_best_refit_conf_props <- prop.table(
  rf_best_refit_conf_counts,
  margin = 1
)

cat("\nRefit best model full-data confusion matrix: counts\n")
print(rf_best_refit_conf_counts)

cat("\nRefit best model full-data confusion matrix: row percentages\n")
print(round(100 * rf_best_refit_conf_props, 2))


# ============================================================
# 11. CONFUSION MATRIX WITH PERCENTAGES AND COUNTS
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

rf_full_conf_percent_count <- make_percent_count_matrix(rf_full_conf_counts)

rf_test_conf_percent_count <- make_percent_count_matrix(rf_test_conf_counts)

rf_best_refit_conf_percent_count <- make_percent_count_matrix(
  rf_best_refit_conf_counts
)

cat("\nFull-data confusion matrix from selected grid-search model: percentage with counts\n")
print(rf_full_conf_percent_count)

cat("\nTest-data confusion matrix from selected grid-search model: percentage with counts\n")
print(rf_test_conf_percent_count)

cat("\nRefit best model full-data confusion matrix: percentage with counts\n")
print(rf_best_refit_conf_percent_count)


# ============================================================
# 12. VARIABLE IMPORTANCE FOR REFIT BEST MODEL
# ============================================================

rf_best_importance <- tibble(
  variable = names(rf_best_model$variable.importance),
  importance = as.numeric(rf_best_model$variable.importance)
) %>%
  arrange(desc(importance))

rf_best_importance











# rf_best_model

# Prediction with new person


# ============================================================
# PREDICT SRH BINARY OUTCOME FOR A NEW PERSON
# ============================================================

# ------------------------------------------------------------
# Helper: safely create a new-person row using the same
# variable types/levels as the training data
# ------------------------------------------------------------

make_new_person_template <- function(training_df, predictor_vars) {
  
  new_person <- training_df[1, predictor_vars, drop = FALSE]
  
  for (v in predictor_vars) {
    
    if (is.numeric(training_df[[v]])) {
      new_person[[v]] <- median(training_df[[v]], na.rm = TRUE)
    }
    
    if (is.factor(training_df[[v]])) {
      new_person[[v]] <- factor(
        levels(training_df[[v]])[1],
        levels = levels(training_df[[v]])
      )
    }
    
    if (is.character(training_df[[v]])) {
      new_person[[v]] <- unique(training_df[[v]])[1]
    }
  }
  
  new_person
}

# ------------------------------------------------------------
# Create one new person
# ------------------------------------------------------------

new_person <- make_new_person_template(
  training_df = health_model_df,
  predictor_vars = predictor_vars
)

# ------------------------------------------------------------
# Edit values for the new person
# Replace these example values with the real person's values
# ------------------------------------------------------------

# head(reg_df[, c("socialself", "socialworld", "generalthreat", "generalsafety")])

head(health_model_df[, c("socialself", "socialworld", "generalthreat", "generalsafety", "SRH_binary_rf")], n = 10)


new_person$socialself <- 0.296                
new_person$socialworld <- 0.128                       
new_person$generalthreat <- 0.483                      
new_person$generalsafety <- -0.433

# For categorical variables, use values that already exist
# in the original training data.
# To see allowed values, run:
# unique(health_model_df$hh_cs_youngcoh)
# unique(health_model_df$list_crgender)
# unique(health_model_df$nationality_collapsed)
# unique(health_model_df$cr_cs_location)

new_person$hh_cs_youngcoh <- health_model_df$hh_cs_youngcoh[1]
new_person$list_crgender <- health_model_df$list_crgender[1]
new_person$nationality_collapsed <- health_model_df$nationality_collapsed[1]
new_person$cr_cs_location <- health_model_df$cr_cs_location[1]

# ------------------------------------------------------------
# Predict probability of being class 1
# ------------------------------------------------------------

new_person_prob_one <- predict(
  rf_best_model,
  data = new_person
)$predictions[, "one"]

# ------------------------------------------------------------
# Convert probability to predicted class using best threshold
# ------------------------------------------------------------

new_person_predicted_class <- ifelse(
  new_person_prob_one >= rf_best_threshold,
  1,
  0
)




# ------------------------------------------------------------
# Print result
# ------------------------------------------------------------

cat("Predicted probability of class 1:", round(new_person_prob_one, 4), "\n")
cat("Selected threshold:", rf_best_threshold, "\n")
cat("Predicted class:", new_person_predicted_class, "\n")








# ============================================================
# LOAD BEST RANDOM FOREST MODEL
# ============================================================

# rf_saved_object <- readRDS("rf_best_model_saved.rds")
# 
# rf_best_model <- rf_saved_object$model
# rf_best_threshold <- rf_saved_object$threshold
# predictor_vars <- rf_saved_object$predictor_vars
# rf_formula <- rf_saved_object$rf_formula
# rf_best_parameters <- rf_saved_object$best_parameters


# # ============================================================
# # EXAMPLE NEW PERSON AFTER LOAD
# # ============================================================
# 
# new_person <- data.frame(
#   socialself = 3,
#   socialworld = 4,
#   generalthreat = 2,
#   generalsafety = 5,
#   hh_cs_youngcoh = "example_value",
#   list_crgender = "example_value",
#   nationality_collapsed = "example_value",
#   cr_cs_location = "example_value"
# )
# 
# # Replace "example_value" with real values that appeared in your training data.
# 
# prob_one <- predict(
#   rf_best_model,
#   data = new_person
# )$predictions[, "one"]
# 
# predicted_class <- ifelse(
#   prob_one >= rf_best_threshold,
#   1,
#   0
# )
# 
# cat("Predicted probability of 1:", round(prob_one, 4), "\n")
# cat("Threshold:", rf_best_threshold, "\n")
# cat("Predicted class:", predicted_class, "\n")



