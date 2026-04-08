###GAGE Project- Regression Models
### By: Julia Porco

source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")
####Multivariate regression for predicting Resilience(CYRM)

lavInspect(tenth_cfa_fit, what="std") 
#choose 3 variables with highest loadings from each factor

##social self
#cr_mva_se_situat, cr_mva_se_solut, cr_mva_se_event  
##social world
# cr_si_trust_neighbor + cr_si_peopletrusted + cr_si_peoplehelp
## general threat
#cr_edu_abuse, cr_vi_peer_times4, cr_edu_otherabuse
##general safety
#cr_vio_safe_travelmarket, cr_vio_safe_market, cr_vio_safe_travelwork    
# list_crgender, hh_cs_youngcoh, cr_rc_cyrm

#multivariate model #1
multivar_1 <- lm(cr_rc_cyrm ~ as.factor(cr_mva_se_situat)+ as.factor(cr_mva_se_solut)
                 + as.factor(cr_mva_se_event)+
                   as.factor(cr_si_trust_neighbor_REV) + as.factor(cr_si_peopletrusted_REV)
                 + as.factor(cr_si_peoplehelp_REV)+
                   as.factor(cr_edu_abuse_REV)+ as.factor(cr_vi_peer_times4)+
                   as.factor(cr_edu_otherabuse_REV)+
                   as.factor(cr_vio_safe_travelmarket_REV)+ as.factor(cr_vio_safe_market_REV)+
                   as.factor(cr_vio_safe_travelwork_REV)+ 
                   as.factor(list_crgender) +as.factor(hh_cs_youngcoh), data=reduced_df )

summary(multivar_1)

#model #2, remove variables with no significant correlation
multivar_2<-lm(cr_rc_cyrm ~ 
                 as.factor(cr_mva_se_event)+
                 as.factor(cr_si_trust_neighbor_REV) 
               + as.factor(cr_si_peoplehelp_REV)+
                 as.factor(cr_edu_abuse_REV)+ 
                 as.factor(list_crgender)
               #+as.factor(cr_vio_safe_travelwork_REV) 
               , data=reduced_df )
summary(multivar_2)


#model #3, see if there's any better predictors: top 5 highest loadings
multivar_3 <- lm(cr_rc_cyrm ~  as.factor(cr_mva_se_handle)+
                   as.factor(cr_mva_se_prob)+ as.factor(cr_mva_se_solve)
                 + as.factor(cr_si_peopletrusted)
                 + as.factor(cr_si_peoplehelp)+ as.factor(cr_si_trust_know)
                 + as.factor(cr_vio_home_slapparent)
                 + as.factor(cr_vi_peer_times6) + as.factor(cr_vi_peer_times1)
                 + as.factor(cr_vio_safe_work)+
                   as.factor(cr_vio_safe_friend) + as.factor(cr_vio_safe_neighbor)
                 #as.factor(list_crgender) +as.factor(hh_cs_youngcoh)
                 , data=reduced_df )

summary(multivar_3)

#multivariate model #4
multivar_vector<- numeric(ncol(reduced_df))
reddf_colnames<-colnames(reduced_df) 
# fill a vector with R squared values from the lm of CYRM~ each column 
for (i in 1:ncol(reduced_df)){
  ith_col<-reddf_colnames[i]
  ith_model<- lm(cr_rc_cyrm~ as.factor(reduced_df[[ith_col]]), data=reduced_df)
  multivar_vector[i]<- summary(ith_model)$r.squared
}

rsq_df<-data.frame(reddf_colnames,multivar_vector)
#filter for the higher R squared values, these predict resilience best
rsq_df %>% filter(multivar_vector> 0.02)


#model 4
multivar_4<- lm(cr_rc_cyrm~ 
                  as.factor(cr_mva_se_event) 
                +as.factor(cr_mva_se_handle)
                + as.factor(cr_mva_se_solut)
                + as.factor(cr_mva_se_goal)
                + as.factor(cr_mva_se_situat)
                +as.factor( cr_si_trust_know_REV) 
                + as.factor(cr_si_trust_neighbor_REV)
                + as.factor(cr_si_peoplehelp_REV)
                + as.factor(cr_si_peopletrusted_REV)
                + as.factor(cr_vio_safe_makani_REV)
                + as.factor(cr_vio_safe_relative_REV)
                + as.factor(cr_vi_peer_times1)
                + as.factor(cr_hn_gnhlth_REV) #making another model for this as well
                #+ as.factor(list_crgender)
                , data=reduced_df)
summary(multivar_4)


###test factanal
columns<- c("cr_mva_se_event","cr_si_trust_know_REV", "cr_mva_se_handle",
            "cr_si_trust_neighbor_REV","cr_vio_safe_relative_REV"  )


#regression based on factor scores (attempt, need to adjust for ordinal data)
tenth_load<- lavInspect(tenth_cfa_fit, what="std")
sclslf<- sum(tenth_load$lambda[,1])
sclslf
sclwrl<- sum(tenth_load$lambda[,2])
gnrlth<- sum(tenth_load$lambda[,3])
gnrlsf<- sum(tenth_load$lambda[,4])

ss1<- tenth_load$lambda[1,1]*reduced_df$cr_mva_opinfriend
ss2<- tenth_load$lambda[1,2]*reduced_df$cr_mva_se_solve 
ss3<- tenth_load$lambda[1,3]*reduced_df$cr_mva_se_means 
sw1<- tenth_load$lambda[2,1]*reduced_df$cr_si_peopletrusted
sw2 <- tenth_load$lambda[2,1]*reduced_df$cr_si_peoplehelp
sw3 <- tenth_load$lambda[2,1]*reduced_df$cr_si_trust_neighbor

s1<- ss1+ ss2+ ss3
s2<- sw1 + sw2 +sw3

test<-lm(reduced_df$cr_rc_cyrm~ s1 + s2)
summary(test)





##### ORDINAL LOGISTIC REGRESSION MODEL FOR General Self-Rated Health:

#Attempt 1:


#create models with cr_hn_gnhlth_REV against every variable and compare AICs
#choose variables with lower AICs, try adding these into a multivariate model

#make smaller df bc loop is taking long to run:
reduced_df2<- reduced_df %>% 
  dplyr::select(cr_mva_opinfriend, cr_mva_se_solve,cr_mva_se_means, 
                cr_mva_se_goal,cr_mva_se_event, cr_mva_se_situat, cr_mva_se_prob,
                cr_mva_se_calm, cr_mva_se_solut, cr_mva_se_trouble, cr_mva_se_handle,
                cr_rc_opportunities,cr_rc_socialsit, cr_rc_famsafe,  
                cr_si_peopletrusted, cr_si_peoplehelp, cr_si_trust_neighbor, 
                cr_si_trust_know,   cr_si_friends, cr_rc_friendsupp, cr_rc_friendtimes, 
                cr_vi_peer_times1, cr_vi_peer_times2, cr_vi_peer_times3,
                cr_vi_peer_times4, cr_vi_peer_times5,  cr_vi_peer_times6,
                cr_vio_home_yell,cr_vio_home_treatpoorly, 
                cr_vio_home_slapparent,cr_vio_home_slapbrother,
                cr_vio_home_fatherhit, cr_edu_abuse, cr_edu_otherabuse, cr_edu_punish,
                cr_vio_safe_friend,cr_vio_safe_neighbor, cr_vio_safe_relative,
                cr_vio_safe_work, cr_vio_safe_home, cr_vio_safe_travelwork,
                cr_vio_safe_market,
                cr_vio_safe_travelmarket, cr_vio_safe_waterfuel,
                cr_vio_safe_religious, cr_vio_safe_makani, cr_edu_trvlsafe,
                cr_hn_gnhlth_REV)
aic_vector<-numeric(ncol(reduced_df2))
reddf_colnames<-colnames(reduced_df)



for (i in 1:ncol(reduced_df2)){
  ith_col<-colnames(reduced_df2)[i]
  ith_model<- polr(as.factor(cr_hn_gnhlth_REV)~ 
                     as.factor(reduced_df2[[ith_col]]),
                   data=reduced_df2)
  aic_vector[i]<- AIC(ith_model)
}


#
aic_df<-data.frame(colnames(reduced_df2),aic_vector)
#filter for the LOWER AIC values, these predict health best
aic_df %>% filter(aic_vector < 7000)



#based on the variables with lower AIC:
health_1<- 
  polr(as.factor(cr_hn_gnhlth_REV)~ as.factor(cr_mva_se_goal) +
         as.factor(cr_mva_se_situat)+ as.factor(cr_mva_se_prob)+
         + as.factor(cr_si_peoplehelp)+ as.factor(cr_si_trust_neighbor_REV) 
       + as.factor(cr_si_trust_know_REV) 
       + as.factor(cr_vio_safe_makani_REV)
       + cr_rc_cyrm
       #+ as.factor(list_crgender)
       #+ as.factor(hh_cs_youngcoh)
       #+ as.factor(cr_cs_nationality)
       + as.factor(cr_mva_se_means)
       +as.factor(cr_mva_se_handle)
       +as.factor(cr_edu_trvlsafe_REV)
       +as.factor(cr_edu_abuse_REV)
       , data=reduced_df, Hess=TRUE)

summary(health_1)




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
  dplyr::select(hhid, cr_rc_cyrm) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

names(reg_df)


# Fit the multivariate linear regression ON FACTOR SCORES #
###########################################################
# This predicts the continuous outcome cr_rc_cyrm using the
# four latent factor scores as predictors.

# Regression model
# Response: cr_rc_cyrm
# Covariates: socialself + socialworld + generalthreat + generalsafety
# The covariate values are estimated from the factor analysis

cyrm_lm <- lm(
  cr_rc_cyrm ~ socialself + socialworld + generalthreat + generalsafety,
  data = reg_df
)

# View regression results
summary(cyrm_lm)
confint(cyrm_lm)


####Now create the same model for General Self Rated Health
#Must use ordinal logistic regression

health_reg_df<- reduced_df %>%
  dplyr::select(hhid, cr_hn_gnhlth_REV) %>%
  bind_cols(factor_scores_df) %>%
  drop_na()

health_2<- polr(as.factor(cr_hn_gnhlth_REV)~ socialself + socialworld + 
                  generalthreat + generalsafety, Hess=T
                , data=health_reg_df)
summary(health_2)
confint(health_2)

# Prediction example

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
all_scores <- lavPredict(tenth_cfa_fit, newdata = augmented_data[3500:4102, ])

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


###FOR health
health_reg_df$predicted_gn_health <- predict(health_2, newdata = health_reg_df)
# 
# # Look at first few observed vs predicted values
health_reg_df %>% dplyr::select(hhid, cr_hn_gnhlth_REV, predicted_gn_health)
