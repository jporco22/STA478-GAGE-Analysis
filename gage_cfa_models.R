###GAGE Project- CFA Models
### By: Julia Porco


source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")

###CONFIRMATORY FACTOR ANALYSIS
#using lavaan package cfa function

#First test:
first_cfa<- 'socialself=~ cr_mva_se_solve +cr_mva_se_means+ cr_mva_se_goal+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                          cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+
                          cr_mva_se_handle
              socialworld=~ cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_threaten+ cr_si_othersthreaten
              socialsafetythreat=~ cr_si_togetherness+ cr_vi_peer_times1+
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+cr_vi_peer_times6
              nonsocialsafetythreat=~cr_vio_home_yell+cr_vio_home_treatpoorly+
                          cr_vio_home_slapparent+ cr_vio_home_slapbrother+
                          cr_vio_home_fatherhit+ cr_vio_home_motherbeaten'
first_cfa_fit<- cfa(first_cfa, data=reduced_df, ordered=T, 
                    missing = "pairwise")
summary(first_cfa_fit,fit.measures=T)


#2nd CFA trial; summarized variables AND corrected scales:
#need to indicate that "sum" variables are continuous, NOT ordered.
ordered_cols<- reduced_df %>% 
  select(!contains("sum"))
ordered_cols_list<-colnames(ordered_cols)

second_cfa<-'socialself=~cr_mva_opin_sum_REV +cr_mva_se_solve +cr_mva_se_means+ 
                          cr_mva_se_goal+cr_hn_scale+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                          cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+ 
                          cr_mva_se_handle+ cr_rc_opportunities+cr_rc_socialsit
             socialworld=~cr_si_peopletrusted_REV+ cr_si_peoplehelp_REV+
                          cr_si_threaten_REV+ cr_si_othersthreaten_REV+
                          cr_si_trust_REV_sum+ cr_vio_contrbeha+
                          cr_vio_notdisc+ cr_vio_interargue+ 
                          cr_vio_vioprivematt
             socialsafetythreat=~cr_si_togetherness+ cr_si_friends+ 
                          cr_si_partsport+cr_rc_friendsupp+ cr_rc_friendtimes+ 
                          cr_vio_safe_soc_REV_sum + cr_vi_peer_times1+
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6
            nonsocialsafetythreat=~ cr_vio_home_yell+
                          cr_vio_home_treatpoorly+cr_vio_home_slapparent+
                          cr_vio_home_slapbrother+cr_vio_home_fatherhit+
                          cr_vio_home_motherbeaten+ cr_vi_nonsoc_safe_REV_sum+ 
                          cr_edu_abuse+ cr_edu_otherabuse+ 
                          cr_edu_punish+ cr_edu_abusetell'
second_cfa_fit<-cfa(second_cfa,data=reduced_df,ordered=ordered_cols_list, 
                    missing = "pairwise")
summary(second_cfa_fit,fit.measures=T)


####3rd attempt: (remove sum columns+reverses)
third_cfa<-'socialself=~cr_mva_se_solve +cr_mva_se_means+ 
                          cr_mva_se_goal+cr_hn_scale+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                          cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+ 
                          cr_mva_se_handle+ cr_rc_opportunities+cr_rc_socialsit
             socialworld=~cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_threaten+ cr_si_othersthreaten+
                           cr_vio_contrbeha+
                          cr_vio_notdisc+ cr_vio_interargue+ 
                          cr_vio_vioprivematt
             socialsafetythreat=~cr_si_togetherness+ cr_si_friends+ 
                          cr_si_partsport+cr_rc_friendsupp+ cr_rc_friendtimes+ 
                           cr_vi_peer_times1+
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6
            nonsocialsafetythreat=~ cr_vio_home_yell+
                          cr_vio_home_treatpoorly+cr_vio_home_slapparent+
                          cr_vio_home_slapbrother+cr_vio_home_fatherhit+
                          cr_vio_home_motherbeaten+  
                          cr_edu_abuse+ cr_edu_otherabuse+ 
                          cr_edu_punish+ cr_edu_abusetell'
third_cfa_fit<-cfa(third_cfa,data=reduced_df,ordered=T, 
                   missing = "pairwise")
summary(third_cfa_fit,fit.measures=T)


#fourth attempt cfa; use only sum columns
fourth_cfa<-'socialself=~cr_mva_opin_sum_REV + cr_mva_selfeff_sum + 
                          cr_rc_socialself_sum
              socialworld=~cr_si_trust_REV_sum+ cr_si_trustcollective_REV_sum+ 
                          cr_vio_attitude_sum
              socialsafetythreat=~cr_vio_safe_soc_REV_sum+ 
                          cr_vi_peer_times_REV_sum + cr_rc_friend_sum+ 
                          cr_si_ever_REV_sum
              nonsocialsafetythreat=~cr_vi_nonsoc_safe_REV_sum+ 
                          cr_vio_home_REV_sum+ cr_edu_vio_sum'
fourth_cfa_fit=cfa(fourth_cfa,data=reduced_df,ordered=T,missing='pairwise')
summary(fourth_cfa_fit, fit.measures=T)

#fifth attempt cfa: start from attempt one and add only 1 indicators per factor:
fifth_cfa<- 'socialself=~ cr_mva_se_solve +cr_mva_se_means+ cr_mva_se_goal+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                          cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+ 
                          cr_mva_se_handle+cr_hn_scale
              socialworld=~ cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_threaten+ cr_si_othersthreaten+ 
                          cr_vio_contrbeha
              socialsafetythreat=~ cr_si_togetherness+ cr_vi_peer_times1+
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6+cr_rc_friendsupp
              nonsocialsafetythreat=~cr_vio_home_yell+cr_vio_home_treatpoorly+
                          cr_vio_home_slapparent+ cr_vio_home_slapbrother+
                          cr_vio_home_fatherhit+cr_vio_home_motherbeaten+
                          cr_edu_abuse'
fifth_cfa_fit<-cfa(fifth_cfa,data=reduced_df,ordered = T,missing='pairwise')
summary(fifth_cfa_fit,fit.measures=T)

#6th attempt cfa: averages; 
#above, for all of the "sum" columns, the sum is divided by n
sixth_cfa<-'socialself=~cr_mva_opin_sum_REV + cr_mva_selfeff_sum + 
                          cr_rc_socialself_sum
              socialworld=~cr_si_trust_REV_sum+ cr_si_trustcollective_REV_sum+ 
                          cr_vio_attitude_sum
              socialsafetythreat=~cr_vio_safe_soc_REV_sum+ 
                          cr_vi_peer_times_REV_sum + cr_rc_friend_sum+ 
                          cr_si_ever_REV_sum
              nonsocialsafetythreat=~cr_vi_nonsoc_safe_REV_sum+ 
                          cr_vio_home_REV_sum+ cr_edu_vio_sum'
sixth_cfa_fit=cfa(sixth_cfa,data=reduced_df,ordered=F,missing='pairwise')
summary(sixth_cfa_fit, fit.measures=T)

#7th attempt cfa: log(sum)
seventh_cfa<-'socialself=~cr_mva_opin_sum_REV + cr_mva_selfeff_sum + 
                          cr_rc_socialself_sum
              socialworld=~cr_si_trust_REV_sum+ cr_si_trustcollective_REV_sum+ 
                          cr_vio_attitude_sum
              socialsafetythreat=~cr_vio_safe_soc_REV_sum+ 
                          cr_vi_peer_times_REV_sum + cr_rc_friend_sum+ 
                          cr_si_ever_REV_sum
              nonsocialsafetythreat=~cr_vi_nonsoc_safe_REV_sum+ 
                          cr_vio_home_REV_sum+ cr_edu_vio_sum'
seventh_cfa_fit=cfa(seventh_cfa,data=reduced_df,ordered=T,missing='pairwise')
summary(seventh_cfa_fit, fit.measures=T)

#8th attempt CFA:
eighth_cfa<- 'socialself=~ cr_mva_se_means+ cr_mva_se_goal+
                          cr_mva_se_event+ cr_mva_se_prob+
                          cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+
                          cr_mva_se_handle
              socialworld=~  cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_threaten+ cr_si_othersthreaten
              socialsafetythreat=~  cr_vi_peer_times1+
                          cr_vi_peer_times2+
                          cr_vi_peer_times4+ cr_vi_peer_times5+cr_vi_peer_times6
              nonsocialsafetythreat=~cr_vio_home_treatpoorly+
                           cr_vio_home_slapbrother+
                          cr_vio_home_fatherhit+ cr_vio_home_motherbeaten'
eighth_cfa_fit<- cfa(eighth_cfa, data=reduced_df, ordered=T, 
                     missing = "pairwise")
summary(eighth_cfa_fit,fit.measures=T)


#cfa 9th attempt (adjusting factors after looking at which ones have <0.3 loadings)
ninth_cfa<-'socialself=~cr_mva_se_solve +
                          cr_mva_se_goal+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                           cr_mva_se_solut+ cr_mva_se_trouble+ 
                          cr_mva_se_handle 
             socialworld=~ cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_trust_neighbor+ cr_si_trust_know
             socialsafetythreat=~
                          cr_rc_friendsupp+ cr_rc_friendtimes+ 
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6+ cr_vio_safe_friend+
                          cr_vio_safe_neighbor+ cr_vio_safe_relative+
                          cr_vio_safe_work+cr_rc_famsafe 
            nonsocialsafetythreat=~ 
                          cr_vio_home_slapparent+cr_vio_home_slapbrother+
                          cr_edu_abuse+ cr_edu_otherabuse+ cr_vio_safe_home+
                          cr_vio_safe_travelwork+ cr_vio_safe_market+
                          cr_vio_safe_travelmarket+ cr_vio_safe_waterfuel
                           '
ninth_cfa_fit<-cfa(ninth_cfa,data=reduced_df,ordered=T, 
                   missing = "pairwise")
summary(ninth_cfa_fit,fit.measures=T)
inspect(ninth_cfa_fit, what="std")

#10th CFA, changed factor composition based on EFA results 
#Note; had to remove "cr_rc" variables, as these cannot be used for the regression model
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
summary(tenth_cfa_fit, fit.measures=T)
lavInspect(tenth_cfa_fit, what="std")

#For paper: "Model A"- show what happens with everything
model_a_cfa<-'socialself=~cr_hn_scale+ cr_mva_opinfriend+ cr_mva_opinionelder+
                cr_mva_opinionbroth+cr_mva_opinionsist+ cr_mva_se_solve +
                          cr_mva_se_means+ cr_mva_se_goal+
                          cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+
                           cr_mva_se_calm+ cr_mva_se_solut+ cr_mva_se_trouble+ 
                          cr_mva_se_handle 
             socialworld=~ cr_si_diversity+ cr_si_peopletrusted+ cr_si_peoplehelp+
                          cr_si_threaten+ cr_si_othersthreaten+ 
                          cr_si_trust_family+ cr_si_trust_neighbor +
                          cr_si_trust_know+ cr_si_trust_first+ 
                          cr_si_trust_diffrelig + cr_si_trust_diffnation + 
                          cr_vio_contrbeha+ cr_vio_notdisc+ cr_vio_interargue+
                          cr_vio_vioprivematt
             socialsafetythreat=~ cr_si_togetherness+
                           cr_vi_peer_times1+
                          cr_vi_peer_times2+cr_vi_peer_times3+
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6+ cr_vio_safe_friend+
                          cr_vio_safe_neighbor+ cr_vio_safe_relative+
                          cr_vio_safe_work+ cr_si_friends+
                          cr_si_partsport 
            nonsocialsafetythreat=~ 
                          cr_vio_home_yell+ cr_vio_home_treatpoorly +
                          cr_vio_home_slapparent+ cr_vio_home_slapbrother+
                          cr_vio_home_fatherhit+ cr_vio_home_motherbeaten+
                          cr_edu_abuse+ cr_edu_otherabuse+cr_edu_punish+
                          cr_edu_abusetell+
                          cr_vio_safe_home+
                          cr_vio_safe_travelwork+ cr_vio_safe_market+
                          cr_vio_safe_travelmarket+ cr_vio_safe_waterfuel+
                          cr_vio_safe_religious+ cr_vio_safe_makani+ 
                          cr_edu_trvlsafe+ cr_edu_schsafe
                          '
model_a_cfa_fit<-cfa(model_a_cfa,data=reduced_df,ordered=T, 
                     missing = "pairwise")
summary(model_a_cfa_fit,fit.measures=T)
