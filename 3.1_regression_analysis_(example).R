
# Predict factors for a new person based on responses to survey data

model_variables = c(
  
  # socialself = ~ 
  "cr_mva_opinfriend_REV",
  "cr_mva_se_solve",
  "cr_mva_se_means",
  "cr_mva_se_goal",
  "cr_mva_se_event",
  "cr_mva_se_situat", 
  "cr_mva_se_prob",
  "cr_mva_se_calm",
  "cr_mva_se_solut", 
  "cr_mva_se_trouble", 
  "cr_mva_se_handle",
  #### + cr_rc_opportunities + cr_rc_socialsit + cr_rc_famsafe

  # socialworld
  "cr_si_peopletrusted_REV",
  "cr_si_peoplehelp_REV",
  "cr_si_trust_neighbor_REV",
  "cr_si_trust_know_REV",
  "cr_si_friends_REV",
  "cr_si_trust_diffrelig_REV",
  "cr_si_trust_diffnation_REV",
  ####+ cr_rc_friendsupp + cr_rc_friendtimes 

  # generalthreat
  "cr_vi_peer_times1",
  "cr_vi_peer_times2",
  "cr_vi_peer_times3",
  "cr_vi_peer_times4",
  "cr_vi_peer_times5",
  "cr_vi_peer_times6",
  "cr_vio_home_yell",
  "cr_vio_home_treatpoorly",
  "cr_vio_home_slapparent",
  "cr_vio_home_slapbrother",
  "cr_vio_home_fatherhit",
  "cr_edu_abuse_REV",
  "cr_edu_otherabuse_REV",
  "cr_edu_punish_REV",

  # generalsafety
  "cr_vio_safe_friend_REV",
  "cr_vio_safe_neighbor_REV",
  "cr_vio_safe_relative_REV",
  "cr_vio_safe_work_REV",
  "cr_vio_safe_home_REV",
  "cr_vio_safe_travelwork_REV",
  "cr_vio_safe_market_REV",
  "cr_vio_safe_travelmarket_REV",
  "cr_vio_safe_waterfuel_REV",
  "cr_vio_safe_religious_REV",
  "cr_vio_safe_makani_REV",
  "cr_edu_trvlsafe_REV"
  )






# --------------------------------------------------
# 2. Create one new person with all indicator values = 1
# --------------------------------------------------
# Extract the first row as a data frame to get the structure
new_person <- reduced_df[1, , drop = FALSE]

# Set all columns to NA first
new_person[,] <- NA

# Fill the CFA indicator variables with 1
new_person[model_variables] = 1

# Give an ID column (In case I get errors)
if ("hhid" %in% names(new_person)) {
  new_person$hhid <- -999
}



# APPEND the new person to the original dataset (Workaround for ordered data)
augmented_data <- bind_rows(reduced_df, new_person)

nrow_augment = nrow(augmented_data)

# Compute factor scores on the augmented dataset
all_scores <- lavPredict(tenth_cfa_fit, newdata = augmented_data[(nrow_augment-150):nrow_augment, ] )

## Convert to data frame for convenience
all_scores_df <- as.data.frame(all_scores) 

names(all_scores_df) = c("socialself", "socialworld",  "generalthreat",    "generalsafety" )

# Extract the scores for the new person (last row)
new_person_score <- tail(all_scores_df, 1)

# Factor scores of new person
new_person_score





#############################
############################

# Enter data into the model to predict health outcomes:
# Object new_person_score from above

new_scores_health = data.frame(new_person_score, 
                                  hh_cs_youngcoh = 1,
                                  list_crgender = 1,
                                  nationality_collapsed = 1,
                                  cr_cs_location = 1)


predict(cyrm_lm, new = new_scores_health)



