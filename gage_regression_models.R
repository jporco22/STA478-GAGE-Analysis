###GAGE Project- Regression Models
### By: Julia Porco

source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")
library(DescTools)
#Run this:
tenth_cfa<-'
social self=~ cr_mva_opinfriend_REV + cr_mva_se_solve +cr_mva_se_means+ 
cr_mva_se_goal+cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+cr_mva_se_calm+
cr_mva_se_solut+ cr_mva_se_trouble+ cr_mva_se_handle
####+ cr_rc_opportunities+ cr_rc_socialsit+ cr_rc_famsafe
socialworld=~ cr_si_peopletrusted_REV + cr_si_peoplehelp_REV +
cr_si_trust_neighbor_REV + cr_si_trust_know_REV +  cr_si_friends_REV
+cr_si_trust_diffrelig_REV + cr_si_trust_diffnation_REV
####+ cr_rc_friendsupp + cr_rc_friendtimes 
generalthreat=~ cr_vi_peer_times1+ cr_vi_peer_times2+cr_vi_peer_times3+
cr_vi_peer_times4+ cr_vi_peer_times5+  cr_vi_peer_times6+ cr_vio_home_yell+
cr_vio_home_treatpoorly+cr_vio_home_slapparent+cr_vio_home_slapbrother+
cr_vio_home_fatherhit+cr_edu_abuse_REV + cr_edu_otherabuse_REV+ cr_edu_punish_REV
generalsafety=~ cr_vio_safe_friend_REV +cr_vio_safe_neighbor_REV
+ cr_vio_safe_relative_REV +cr_vio_safe_work_REV + cr_vio_safe_home_REV 
+cr_vio_safe_travelwork_REV + cr_vio_safe_market_REV +cr_vio_safe_travelmarket_REV
+ cr_vio_safe_waterfuel_REV+ cr_vio_safe_religious_REV + 
cr_vio_safe_makani_REV + cr_edu_trvlsafe_REV
'
tenth_cfa_fit<-cfa(tenth_cfa,data=reduced_df, ordered=T, missing='pairwise')


###Code from Nishan Mudalige:
###2-step model for prediciting CYRM


# WARNING: Takes a long time to run
# WARNING: Takes a long time to run
# WARNING: Takes a long time to run
factor_scores <- lavPredict(tenth_cfa_fit, type = "lv")

# Convert factor scores to a data frame so they are easier to combine
factor_scores_df <- as.data.frame(factor_scores)

# Optional: look at factor score variable names
names(factor_scores_df)
# names should be:
# "socialself", "socialworld", "generalthreat", "generalsafety"

names(factor_scores_df) = c("socialself", "socialworld", "generalthreat", "generalsafety")


# Build the regression dataset #
################################
# Keep the continuous outcome (y) and attach the latent factor scores (x_1, ... , x_4 representing each factor).
# drop_na() removes rows with missing values in outcome or predictors.

reg_df <- reduced_df %>%
  dplyr::select(hhid, cr_rc_cyrm,  hh_cs_youngcoh, list_crgender, cr_cs_location,
                nationality_collapsed) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

names(reg_df)


# Fit the multivariate linear regression ON FACTOR SCORES #
###########################################################
# This predicts the continuous outcome cr_rc_cyrm using the
# four latent factor scores as predictors.

# Regression model
# Response: cr_rc_cyrm
# Covariates: socialself + socialworld + generalthreat + generalsafety +
     #agecohort + gender
# The covariate values are estimated from the factor analysis

cyrm_lm <- lm(
  cr_rc_cyrm ~ socialself + socialworld + generalthreat + generalsafety+
    + as.factor(hh_cs_youngcoh)+ as.factor(list_crgender)
  +as.factor(nationality_collapsed)
  +as.factor(cr_cs_location),
  data = reg_df
)

# View regression results
summary(cyrm_lm)
confint(cyrm_lm)




# Prediction example:
new_person <- data.frame(
  # social self
  cr_mva_opinfriend_REV = 1,
  cr_mva_se_solve = 1, 
  cr_mva_se_means = 1,
  cr_mva_se_goal = 1,
  cr_mva_se_event = 1,
  cr_mva_se_situat = 1,
  cr_mva_se_prob = 1,
  cr_mva_se_calm = 1,
  cr_mva_se_solut = 1,
  cr_mva_se_trouble = 1,
  cr_mva_se_handle = 1,
  #cr_rc_opportunities = 1,
  #cr_rc_socialsit = 1,
  #cr_rc_famsafe = 1,
  # socialworld = ~ 
  cr_si_peopletrusted_REV = 1,
  cr_si_peoplehelp_REV = 1,
  cr_si_trust_neighbor_REV = 1, 
  cr_si_trust_know_REV = 1,
  cr_si_friends_REV = 1,
  #cr_rc_friendsupp = 1, 
  #cr_rc_friendtimes = 1,
  # generalthreat = ~ 
  cr_vi_peer_times1 = 1,
  cr_vi_peer_times2 = 1,
  cr_vi_peer_times3 = 1,
  cr_vi_peer_times4 = 1,
  cr_vi_peer_times5 = 1,
  cr_vi_peer_times6 = 1,
  cr_vio_home_yell = 1,
  cr_vio_home_treatpoorly = 1,
  cr_vio_home_slapparent = 1, 
  cr_vio_home_slapbrother = 1,
  cr_vio_home_fatherhit = 1, 
  cr_edu_abuse_REV = 1, 
  cr_edu_otherabuse_REV = 1, 
  cr_edu_punish_REV = 1,
  # generalsafety = ~ 
  cr_vio_safe_friend_REV = 1, 
  cr_vio_safe_neighbor_REV = 1,
  cr_vio_safe_relative_REV = 1,
  cr_vio_safe_work_REV = 1,
  cr_vio_safe_home_REV = 1, 
  cr_vio_safe_travelwork_REV = 1, 
  cr_vio_safe_market_REV = 1,
  cr_vio_safe_travelmarket_REV = 1, 
  cr_vio_safe_waterfuel_REV = 1,
  cr_vio_safe_religious_REV = 1,
  cr_vio_safe_makani_REV = 1,
  cr_edu_trvlsafe_REV = 1
)

# STEP 1: Use lavPredict() to estimate latent factor scores the new person

new_person_scores <- lavPredict(tenth_cfa_fit, newdata = new_person)
# new_person_scores <- as.data.frame(new_person_scores)
# Error

# STEP 2: Use the regression model cyrm_lm to predict
# Can not continue since code above isn't working











# USING AI


# --------------------------------------------------
# 1. List the observed indicator variables used in the CFA
# --------------------------------------------------
indicator_vars <- c(
  "cr_mva_opinfriend_REV", "cr_mva_se_solve", "cr_mva_se_means", "cr_mva_se_goal",
  "cr_mva_se_event", "cr_mva_se_situat", "cr_mva_se_prob", "cr_mva_se_calm",
  "cr_mva_se_solut", "cr_mva_se_trouble", "cr_mva_se_handle",
  #"cr_rc_opportunities", "cr_rc_socialsit", "cr_rc_famsafe",
  "cr_si_peopletrusted_REV", "cr_si_peoplehelp_REV", "cr_si_trust_neighbor_REV",
  "cr_si_trust_know_REV", "cr_si_friends_REV", 
  #"cr_rc_friendsupp", "cr_rc_friendtimes",
  "cr_vi_peer_times1", "cr_vi_peer_times2", "cr_vi_peer_times3",
  "cr_vi_peer_times4", "cr_vi_peer_times5", "cr_vi_peer_times6",
  "cr_vio_home_yell", "cr_vio_home_treatpoorly", "cr_vio_home_slapparent",
  "cr_vio_home_slapbrother", "cr_vio_home_fatherhit", "cr_edu_abuse_REV",
  "cr_edu_otherabuse_REV", "cr_edu_punish_REV",
  "cr_vio_safe_friend_REV", "cr_vio_safe_neighbor_REV", "cr_vio_safe_relative_REV",
  "cr_vio_safe_work_REV", "cr_vio_safe_home_REV", "cr_vio_safe_travelwork_REV",
  "cr_vio_safe_market_REV", "cr_vio_safe_travelmarket_REV", 
  "cr_vio_safe_waterfuel_REV",
  "cr_vio_safe_religious_REV", "cr_vio_safe_makani_REV", "cr_edu_trvlsafe_REV"
)

# --------------------------------------------------
# 2. Create one new person with all indicator values = 1
# --------------------------------------------------
# Extract the first row as a data frame to get the structure
new_person <- reduced_df[1, , drop = FALSE]

# Set all columns to NA first
new_person[,] <- NA

# Fill the CFA indicator variables with 1
new_person[indicator_vars] <- 1

# Give an ID column (In case I get errors)
if ("hhid" %in% names(new_person)) {
  new_person$hhid <- -999
}

# --------------------------------------------------
# 3. Workaround for ordered data:
#    APPEND the new person to the original dataset
# --------------------------------------------------
augmented_data <- bind_rows(reduced_df, new_person)



# --------------------------------------------------
# 4. Compute factor scores on the augmented dataset
# --------------------------------------------------
all_scores <- lavPredict(tenth_cfa_fit, newdata = augmented_data[3500:4102, ] 
                         )

# Convert to data frame for convenience
all_scores_df <- as.data.frame(all_scores) 

# --------------------------------------------------
# 5. Extract the scores for the new person (last row)
# --------------------------------------------------
new_scores <- tail(all_scores_df, 1)

new_scores

names(new_scores) = c("socialself", "socialworld",  "generalthreat",    "generalsafety" )


predict(cyrm_lm, new = new_scores)
#now do same for health
predict(health_2, new=new_scores)


# Generate predictions for the existing sample to examine goodness of fit #
###########################################################################
# # These are fitted/predicted values for the rows used in the regression.
# 
reg_df$predicted_cr_rc_cyrm <- predict(cyrm_lm, newdata = reg_df)
# 
# # Look at first few observed vs predicted values
reg_df %>% dplyr::select(hhid, cr_rc_cyrm, predicted_cr_rc_cyrm)
# 
# anova(cyrm_lm)
# 
# # # Optional diagnostics
# # par(mfrow = c(2, 2))
# # plot(cyrm_lm)
# # par(mfrow = c(1, 1))
# 




####Now create the same model for General Self Rated Health (SRH)
#Must use ordinal logistic regression
health_reg_df<- reduced_df %>%
  dplyr::select(hhid, cr_hn_gnhlth_REV, hh_cs_youngcoh, list_crgender,
                nationality_collapsed, cr_cs_location) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

###SRH collapsing categories 1,2,3:
health_reg_df$SRH_test_col<- ifelse(health_reg_df$cr_hn_gnhlth_REV %in% c(1,2,3), 3,
                                    health_reg_df$cr_hn_gnhlth_REV )

health_reg_df$SRH_test_col<- ifelse(health_reg_df$SRH_test_col==3,1,
                                    ifelse(health_reg_df$SRH_test_col==4,2,
                                           3))

#create weights vector based on frequencies of each category
tab<-1/table(health_reg_df$SRH_test_col) #this didn't work
#playing around with weights
weights_vec<-ifelse(health_reg_df$SRH_test_col==1, 0.44,
                    ifelse(health_reg_df$SRH_test_col==2, 0.28,
                           0.28 ))
#NEW SRH model:
health_3<- polr(as.factor(SRH_test_col)~ socialself + socialworld + 
                  generalthreat + generalsafety + as.factor(hh_cs_youngcoh)
                + as.factor(list_crgender) 
                +as.factor(nationality_collapsed)
                +as.factor(cr_cs_location)
                ,weights=weights_vec ,Hess=T
                , data=health_reg_df)
summary(health_3)
confint(health_3)

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

