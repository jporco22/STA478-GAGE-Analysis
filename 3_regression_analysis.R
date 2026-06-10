### GAGE Project - Regression Models
### By: Julia Porco

library(MASS)
library(dplyr)
library(future)
library(future.apply)
library(parallelly)
library(progressr)
library(knitr)
library(kableExtra)



# source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")
# source("~/Desktop/STA478-GAGE-Analysis/gage_data_cleaning.R")

# Run GAGE
# REQUIRED to get data object reduced_df used in model construction

library(DescTools)
#Run this:
tenth_cfa <- "
  socialself = ~ cr_mva_opinfriend_REV + 
                    cr_mva_se_solve +
                    cr_mva_se_means + 
                    cr_mva_se_goal +
                    cr_mva_se_event + 
                    cr_mva_se_situat + 
                    cr_mva_se_prob +
                    cr_mva_se_calm +
                    cr_mva_se_solut + 
                    cr_mva_se_trouble + 
                    cr_mva_se_handle
                    
                    #### + cr_rc_opportunities + cr_rc_socialsit + cr_rc_famsafe
                    
  socialworld = ~ cr_si_peopletrusted_REV + 
                    cr_si_peoplehelp_REV +
                    cr_si_trust_neighbor_REV + 
                    cr_si_trust_know_REV +  
                    cr_si_friends_REV +
                    cr_si_trust_diffrelig_REV + 
                    cr_si_trust_diffnation_REV
                    
                    ####+ cr_rc_friendsupp + cr_rc_friendtimes 

  generalthreat = ~ cr_vi_peer_times1 + 
                      cr_vi_peer_times2 + 
                      cr_vi_peer_times3 + 
                      cr_vi_peer_times4 + 
                      cr_vi_peer_times5 +
                      cr_vi_peer_times6 + 
                      cr_vio_home_yell + 
                      cr_vio_home_treatpoorly + 
                      cr_vio_home_slapparent + 
                      cr_vio_home_slapbrother + 
                      cr_vio_home_fatherhit + 
                      cr_edu_abuse_REV + 
                      cr_edu_otherabuse_REV + 
                      cr_edu_punish_REV
                      
  generalsafety = ~ cr_vio_safe_friend_REV +
                      cr_vio_safe_neighbor_REV + 
                      cr_vio_safe_relative_REV +
                      cr_vio_safe_work_REV + 
                      cr_vio_safe_home_REV +
                      cr_vio_safe_travelwork_REV + 
                      cr_vio_safe_market_REV +
                      cr_vio_safe_travelmarket_REV + 
                      cr_vio_safe_waterfuel_REV + 
                      cr_vio_safe_religious_REV + 
                      cr_vio_safe_makani_REV + 
                      cr_edu_trvlsafe_REV
              "

# Obtain the factor scores for all individuals in the data set
tenth_cfa_fit <- cfa(tenth_cfa,
                     data = reduced_df, 
                     ordered = T, 
                     estimator = "WLSMV",
                     missing = "pairwise")


### Code from Nishan Mudalige:
### 2-step model for prediciting CYRM


# WARNING: Takes a long time to run
# WARNING: Takes a long time to run
# WARNING: Takes a long time to run
# factor_scores <- lavPredict(tenth_cfa_fit, type = "lv")

# Convert factor scores to a data frame so they are easier to combine
factor_scores_df <- as.data.frame(factor_scores)

# Optional: look at factor score variable names
# names(factor_scores_df)
# names should be:
# "socialself", "socialworld", "generalthreat", "generalsafety"
# The line below removes spascing that may have been introduced

names(factor_scores_df) = c("socialself", "socialworld", "generalthreat", "generalsafety")




################################
################################
################################

## Multivariate model for predicting health outcomes


# Build the regression dataset #
################################
# Keep the continuous outcome (y) and attach the latent factor scores (x_1, ... , x_4 representing each factor).
# drop_na() removes rows with missing values in outcome or predictors.

reg_df <- 
  reduced_df %>%
    dplyr::select(hhid, 
                  cr_rc_cyrm,  
                  hh_cs_youngcoh, 
                  list_crgender, 
                  cr_cs_location,
                  nationality_collapsed) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

names(reg_df)


# Fit the multivariate linear regression 
# Use the FACTOR SCORES calculated above as covariates
# This predicts the continuous outcome cr_rc_cyrm using the
# four latent factor scores as predictors.
# 
# Also include additional variablces of:
# 
#   agecohort:    hh_cs_youngcoh
#   gender:       list_crgender
#   nationality:  nationality_collapsed
#   location:     cr_cs_location

# Regression model
# Response: cr_rc_cyrm
# Covariates:
#   socialself      (from Factor analysis)
#   socialworld     (from Factor analysis)
#   generalthreat   (from Factor analysis)
#   generalsafety   (from Factor analysis)
#   agecohort       (from original survey data)
#   gender          (from original survey data)
#   nationality     (from original survey data)
#   location        (from original survey data)


cyrm_lm <- lm(
  cr_rc_cyrm ~ socialself + 
                socialworld + 
                generalthreat + 
                generalsafety + 
    
                as.factor(hh_cs_youngcoh) + 
                as.factor(list_crgender) + 
                as.factor(nationality_collapsed) + 
                as.factor(cr_cs_location),
  
  data = reg_df
)

# View regression results
summary(cyrm_lm)
confint(cyrm_lm)

# Diagnostics
summary(cyrm_lm)
anova(cyrm_lm)

## Optional diagnostics
# par(mfrow = c(2, 2))
# plot(cyrm_lm)
# par(mfrow = c(1, 1))


# Visual comprison
# Get Fitted values from model
# reg_df$predicted_cr_rc_cyrm = cyrm_lm$fitted.values
# 
# reg_df$predicted_cr_rc_cyrm <- predict(cyrm_lm, newdata = reg_df)
# 
## Look at first few observed vs predicted values
# reg_df %>% dplyr::select(hhid, cr_rc_cyrm, predicted_cr_rc_cyrm)
# 
# min( abs(reg_df$cr_rc_cyrm - reg_df$predicted_cr_rc_cyrm) )
# mean( abs(reg_df$cr_rc_cyrm - reg_df$predicted_cr_rc_cyrm) )
# max( abs(reg_df$cr_rc_cyrm - reg_df$predicted_cr_rc_cyrm) )




################################
################################
################################

## Ordinal model for predicting General Self Rated Health (SRH)

# Must use ordinal logistic regression

# Analytic data set
health_reg_df <- reduced_df %>%
                    dplyr::select(hhid, 
                                  cr_hn_gnhlth_REV, 
                                  hh_cs_youngcoh, 
                                  list_crgender,
                                  nationality_collapsed, 
                                  cr_cs_location) %>%
                  bind_cols(factor_scores_df) %>%
                  drop_na()

# health_reg_df$cr_hn_gnhlth_REV

## SRH collapsing categories 1,2 and 3 due to very low observations and reverse order to be consistent
## collapse categories 1,2,3 into 3. This category represents bad to fair health
# health_reg_df %>%
#   mutate(
#     cr_hn_gnhlth_REV = case_when(
#       cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 1,
#       cr_hn_gnhlth_REV == 4 ~ 2,
#       cr_hn_gnhlth_REV == 5 ~ 3,
#       TRUE ~ NA_real_
#     )
#   )


# table(health_reg_df$SRH_collapsed)

## SRH collapsing categories 1,2 and 3 due to very low observations and reverse order to be consistent
## collapse categories 1,2,3 into 3. This category represents bad to fair health
## Then recode as follows
## 
## New variable stored in SRH_collapsed
health_reg_df <- health_reg_df %>%
  mutate(
    SRH_collapsed = case_when(
      cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 1,
      cr_hn_gnhlth_REV == 4 ~ 2,
      cr_hn_gnhlth_REV == 5 ~ 3,
      TRUE ~ NA_real_
    )
  )


table(health_reg_df$SRH_collapsed)


# For logistic
health_reg_df <- health_reg_df %>%
  mutate(
    SRH_binary = case_when(
      cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 0,
      cr_hn_gnhlth_REV %in% c(4, 5) ~ 1,
      TRUE ~ NA_real_
    )
  )

# save.image("my_workspace.RData")


# playing around with weights (Julia)
# weights_vec <- ifelse(health_reg_df$SRH_collapsed==1, 0.44,
#                     ifelse(health_reg_df$SRH_collapsed==2, 0.28,
#                            0.28 ))


# Weights from grid search algorithm
weights_vec <- ifelse(health_reg_df$SRH_collapsed==1, 4.7,
                    ifelse(health_reg_df$SRH_collapsed==2, 2.4,
                           1.9 ))





rm(health_3)
#NEW SRH model:
health_3 <- polr(as.factor(SRH_collapsed) ~ 
                   socialself + 
                   socialworld + 
                   generalthreat + 
                   generalsafety + 
                   
                   as.factor(hh_cs_youngcoh) + 
                   as.factor(list_crgender) + 
                   as.factor(nationality_collapsed) + 
                   as.factor(cr_cs_location), 
                 
                  weights = weights_vec,
                  
                  Hess = T, 
                  data = health_reg_df)

# any(predict(health_3, type = "class") == 1)
# predict(health_3, type = "class")

# observed_score = health_reg_df$SRH_collapsed
# predicted_score = predict(SRH_collapsed, newdata = health_reg_df)
# predicted_score = predict(health_3, type = "class")

# mean(as.numeric(observed_score) == as.numeric(predicted_score))
# sum(observed_score == predicted_score) / length(observed_score)



observed_score = health_reg_df$SRH_collapsed
predicted_score = predict(health_3, type = "class") # can also use probs instead of class

cm <- table(
  Observed = observed_score,
  Predicted = predicted_score
)


round(prop.table(cm, margin = 1)*100, 2)
cm



summary(health_3)
confint(health_3)






# ============================================================
# ACCURACY MEASURES FOR WEIGHTED ORDINAL LOGISTIC MODEL
# ============================================================

# Predicted class from the polr model
ord_predicted_class <- predict(health_3, type = "class")

# Observed outcome from the model frame
ord_observed_class <- model.response(model.frame(health_3))

# Model weights
ord_weights <- model.weights(model.frame(health_3))

# Weighted confusion matrix
ord_conf_counts <- xtabs(
  ord_weights ~ ord_observed_class + ord_predicted_class
)

# Row proportions: correct prediction within each observed category
ord_conf_row_props <- prop.table(ord_conf_counts, margin = 1)

# Overall weighted accuracy
overall_accuracy <- weighted.mean(
  ord_observed_class == ord_predicted_class,
  ord_weights
)

# Accuracy for each observed category
class_accuracy <- diag(ord_conf_row_props)

# Balanced accuracy across the three categories
balanced_accuracy <- mean(class_accuracy)

# Minimum class accuracy
min_class_accuracy <- min(class_accuracy)

# Ordinal thresholds/cutpoints from polr
thresholds_used <- paste(
  paste0(names(health_3$zeta), " = ", round(as.numeric(health_3$zeta), 3)),
  collapse = "; "
)

cat("\nOverall accuracy:", round(100 * overall_accuracy, 2), "%\n")

for (i in seq_along(class_accuracy)) {
  cat(
    "Correct prediction of observed",
    names(class_accuracy)[i],
    ":",
    round(100 * class_accuracy[i], 2),
    "%\n"
  )
}

cat("Balanced accuracy:", round(100 * balanced_accuracy, 2), "%\n")
cat("Minimum class accuracy:", round(100 * min_class_accuracy, 2), "%\n")
cat("Thresholds used:", thresholds_used, "\n")















#Manually compute CIs for cutoff values (cannot extract)
health_3_output<- coef(summary(health_3))
lower<-numeric(2)
upper<-numeric(2)
cutoff_CI<- data.frame(lower,upper)
for (i in 1:2){
  cutoff_CI$lower[i]<- 
    health_3_output[i+10,1] - 1.96*health_3_output[i+10,2]
  cutoff_CI$upper[i]<- 
    health_3_output[i+10,1] + 1.96*health_3_output[i+10,2]
}
cutoff_CI

#Predicted values
health_reg_df$predicted_gn_health <- predict(health_3, newdata = health_reg_df)
# Look at first few observed vs predicted values
health_reg_df %>% dplyr::select(hhid, SRH_test_col, predicted_gn_health)
#look at density:
ggplot(health_reg_df)+
  geom_bar(aes(x=as.numeric(SRH_test_col)), fill="red", alpha=0.3)+
  geom_bar(aes(x=as.numeric(predicted_gn_health)), fill="blue", alpha=0.3)+
  labs(title = "Actual vs. Predicted SRH Density", x = "SRH", y = "Frequency")
#Pseudo R^2 values for SRH new model #3:
DescTools::PseudoR2(health_3, which = "all")


####SRH model- confusion matrix:

predicted_correct_SRH<- health_reg_df %>% 
  filter(SRH_test_col==predicted_gn_health)
predicted_correct_SRH %>% filter(SRH_test_col %in% c(1)) %>% nrow()
predicted_correct_SRH %>% filter(SRH_test_col %in% c(2,3)) %>% nrow()

predicted_incorrect_SRH<- health_reg_df %>% 
  filter(SRH_test_col!=predicted_gn_health)
predicted_incorrect_SRH %>% filter(SRH_test_col %in% c(1)) %>% nrow()
predicted_incorrect_SRH %>% filter(SRH_test_col %in% c(2,3)) %>% nrow()



###NEW: GHQ Regression Model- Linear Multivariate
ghq_reg_df <- reduced_df %>%
  dplyr::select(hhid, ghq_SUM,  hh_cs_youngcoh, list_crgender, cr_cs_location,
                nationality_collapsed) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

ghq_lm <- lm(
  ghq_SUM ~ socialself + socialworld + generalthreat + generalsafety+
    + as.factor(hh_cs_youngcoh)+ as.factor(list_crgender)
  +as.factor(nationality_collapsed)
  +as.factor(cr_cs_location),
  data = ghq_reg_df
)

summary(ghq_lm)
confint(ghq_lm)

#Predicting GHQ with our model:
ghq_reg_df$predicted_GHQ <- predict(ghq_lm, newdata = ghq_reg_df)




















# Attempted interaction model (not useful)
health_3_interaction <- polr(
  as.factor(SRH_test_col) ~ 
    socialself + socialworld + generalthreat + generalsafety +
    as.factor(hh_cs_youngcoh) +
    as.factor(list_crgender) +
    as.factor(nationality_collapsed) +
    as.factor(cr_cs_location) +
    socialself:as.factor(hh_cs_youngcoh) +
    socialworld:as.factor(hh_cs_youngcoh) +
    generalthreat:as.factor(hh_cs_youngcoh) +
    generalsafety:as.factor(hh_cs_youngcoh) +
    socialself:as.factor(list_crgender) +
    socialworld:as.factor(list_crgender) +
    generalthreat:as.factor(list_crgender) +
    generalsafety:as.factor(list_crgender),
  Hess = TRUE,
  data = health_reg_df
)

pred_interaction <- predict(health_3_interaction, type = "class")

mean(pred_interaction == observed_score)

table(
  Observed = observed_score,
  Predicted = pred_interaction
)











########################################
########################################
########################################

## BINARY Logistic regression model

health_reg_df <- health_reg_df %>%
                    mutate(
                    SRH_binary = case_when(
                      cr_hn_gnhlth_REV %in% c(1, 2, 3) ~ 0,
                      cr_hn_gnhlth_REV %in% c(4, 5) ~ 1,
                      TRUE ~ NA_real_
                    )
                  )




# Unweighted

health_logistic <- glm(as.numeric(SRH_binary) ~ 
                         
                   socialself + 
                   socialworld + 
                   generalthreat + 
                   generalsafety + 
                   
                   as.factor(hh_cs_youngcoh) + 
                   as.factor(list_crgender) + 
                   as.factor(nationality_collapsed) + 
                   as.factor(cr_cs_location), 
                 
                    family = "binomial",
                    data = health_reg_df)




summary(health_logistic)
confint(health_logistic)


health_reg_df$SRH_binary
health_logistic$fitted.values

pred_class <- ifelse(health_logistic$fitted.values >= 0.80, 1, 0)

conf_counts <- table(
  Actual = health_reg_df$SRH_binary,
  Predicted = pred_class
)

# Overall proportions
# round(prop.table(conf_counts), 3)
# Row proportions
conf_counts
round(prop.table(conf_counts, margin = 1)*100, 2)





# # Weighted
# 
# health_reg_df <- health_reg_df %>%
#   mutate(
#     class_weight = case_when(
#       SRH_binary == 0 ~ 0.05,
#       SRH_binary == 1 ~ 0.05,
#       TRUE ~ NA_real_
#     )
#   )
# 
# 
# health_logistic_weighted <- glm(
#   as.numeric(SRH_binary) ~ 
#     
#     socialself + 
#     socialworld + 
#     generalthreat + 
#     generalsafety + 
#     
#     as.factor(hh_cs_youngcoh) + 
#     as.factor(list_crgender) + 
#     as.factor(nationality_collapsed) + 
#     as.factor(cr_cs_location), 
#   
#   data = health_reg_df,
#   family = binomial,
#   weights = class_weight
# )
# 
# 
# pred_class_weighted <- ifelse(
#   fitted(health_logistic_weighted) >= 0.81, 1, 0
# )
# 
# conf_counts_weighted <- table(
#   Actual = health_reg_df$SRH_binary,
#   Predicted = pred_class_weighted
# )
# 
# round(prop.table(conf_counts_weighted, margin = 1), 3)








# ===========================================
# ACCURACY MEASURES FOR BINARY LOGISTIC MODEL
# ===========================================

# Predicted probabilities from the logistic regression model
logit_predicted_prob <- predict(health_logistic, type = "response")

# Threshold for converting predicted probabilities into classes
logit_threshold <- 0.50

# Predicted class
logit_predicted_class <- ifelse(logit_predicted_prob >= logit_threshold, 1, 0)

# Observed class from the model frame
logit_observed_class <- model.response(model.frame(health_logistic))

# If observed classes are coded as 1/2, convert them to 0/1
if (all(sort(unique(logit_observed_class)) == c(1, 2))) {
  logit_observed_class <- logit_observed_class - 1
}

# Confusion matrix
logit_conf_counts <- table(
  Observed = factor(logit_observed_class, levels = c(0, 1)),
  Predicted = factor(logit_predicted_class, levels = c(0, 1))
)

# Row proportions: correct prediction within each observed category
logit_conf_row_props <- prop.table(logit_conf_counts, margin = 1)

# Overall accuracy
overall_accuracy <- mean(
  logit_observed_class == logit_predicted_class
)

# Accuracy for each observed category
class_0_accuracy <- logit_conf_row_props["0", "0"]
class_1_accuracy <- logit_conf_row_props["1", "1"]

# Balanced accuracy
balanced_accuracy <- mean(
  c(class_0_accuracy, class_1_accuracy)
)

# Minimum class accuracy
min_class_accuracy <- min(
  class_0_accuracy,
  class_1_accuracy
)

cat("\nOverall accuracy:", round(100 * overall_accuracy, 2), "%\n")
cat("Correct prediction of observed 0's:", round(100 * class_0_accuracy, 2), "%\n")
cat("Correct prediction of observed 1's:", round(100 * class_1_accuracy, 2), "%\n")
cat("Balanced accuracy:", round(100 * balanced_accuracy, 2), "%\n")
cat("Minimum class accuracy:", round(100 * min_class_accuracy, 2), "%\n")
cat("Threshold used:", logit_threshold, "\n")





