###GAGE Project- Stats, Figures ect. for publication
###By: Julia Porco

source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")

#proportion of each gender , n=4101
prop_male<- (reduced_df %>% filter(list_crgender==1) %>%
  nrow())/nrow(reduced_df)
prop_female<- 1- prop_male
prop_male
prop_female

#proportion of each age cohort, n=4101
prop_young<- (reduced_df %>% filter(hh_cs_youngcoh==1) %>%
                nrow())/nrow(reduced_df)
prop_old<- 1-prop_young
prop_young #ages 10-13
prop_old  #ages 14-18

#proportion of each nationality group (including 5 NAs), n=4101
prop_jordan<- (reduced_df %>% filter(cr_cs_nationality==1) %>%
                 nrow())/nrow(reduced_df)
prop_syrian<- (reduced_df %>% filter(cr_cs_nationality==2) %>%
                 nrow())/nrow(reduced_df)
prop_palestine<- (reduced_df %>% filter(cr_cs_nationality==3) %>%
                 nrow())/nrow(reduced_df)
prop_other_NA<- 1 -prop_jordan - prop_syrian -prop_palestine
prop_jordan
prop_syrian
prop_palestine
prop_other_NA

#proportions for living situations/locations, n=4101
prop_camp<- (reduced_df %>% filter(cr_cs_location==1) %>%
               nrow())/nrow(reduced_df)
prop_its<- (reduced_df %>% filter(cr_cs_location==2) %>%
              nrow())/nrow(reduced_df)
prop_host<- (reduced_df %>% filter(cr_cs_location==3) %>%
               nrow())/nrow(reduced_df)
prop_camp
prop_its
prop_host



#Residual plot- resilience
#plot code from AI:
reg_df$resids<- resid(cyrm_lm)
ggplot(reg_df, aes(x =predicted_cr_rc_cyrm, y = resid(cyrm_lm))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "CYRM Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#Predicted vs Actual- Resilience:
ggplot(data = reg_df,
       aes(x = cr_rc_cyrm, y = predicted_cr_rc_cyrm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual CYRM",
       x = "Actual CYRM",
       y = "Predicted CYRM") +
  theme_minimal()
#Density plot: CYRM:
ggplot(data=reg_df)+
  geom_density(aes(x=cr_rc_cyrm, fill="red", alpha=0.3))+
  geom_density(aes(x=predicted_cr_rc_cyrm, fill="blue", alpha=0.3))+
  labs(title = "Actual vs. Predicted CYRM Density Plot", x = "CYRM", y = "Density") 







#residual plots for SRH model:
health_reg_df$resids<- 
  as.numeric(health_reg_df$SRH_test_col)-
  as.numeric(health_reg_df$predicted_gn_health)

ggplot(health_reg_df, aes(x =predicted_gn_health,y =resids)) +
  geom_count() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "SRH Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#since there's only 6 possible residual values, a bar chart may show it better
ggplot(data=health_reg_df, aes(x=resids))+
  geom_bar()+
  labs(title = "SRH Residual Plot", x = "Residuals", y = "Count")

#overlayed density for SRH
ggplot(health_reg_df)+
  geom_bar(aes(x=as.numeric(SRH_test_col)), fill="red", alpha=0.3)+
  geom_bar(aes(x=as.numeric(predicted_gn_health)), fill="blue", alpha=0.3)+
  labs(title = "Actual vs. Predicted SRH Density", x = "SRH", y = "Frequency")



###GHQ plots:
#Predicted vs Actual GHQ Densities:
ggplot(data=ghq_reg_df)+
  geom_density(aes(x=ghq_SUM, fill="red", alpha=0.3))+
  geom_density(aes(x=predicted_GHQ, fill="blue", alpha=0.3))+
  labs(title = "Actual vs. Predicted GHQ Density Plot", x = "GHQ", y = "Density") +
  theme(legend.position= "bottom")

##Residual Plot for GHQ:
ghq_reg_df$resids<- resid(ghq_lm)
ggplot(ghq_reg_df, aes(x =predicted_GHQ, y = resids)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "GHQ Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#actual vs predicted plot for GHQ:
ggplot(data = ghq_reg_df,
       aes(x = ghq_SUM, y = predicted_GHQ)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual GHQ-12",
       x = "Actual GHQ-12",
       y = "Predicted GHQ-12") +
  theme_minimal()
