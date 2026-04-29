###GAGE Project- CFA Models
### By: Julia Porco


source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")

###CONFIRMATORY FACTOR ANALYSIS
#using lavaan package cfa function

#Previous attempts can be found in previous pushes.


####FINAL CFA MODEL:
#10th CFA, changed factor composition based on EFA results 
#Note; had to remove "cr_rc" variables, as these cannot be used for the regression model
tenth_cfa<-'
social self=~ cr_mva_opinfriend_REV + cr_mva_se_solve +cr_mva_se_means+ 
cr_mva_se_goal+cr_mva_se_event+ cr_mva_se_situat+ cr_mva_se_prob+cr_mva_se_calm+
cr_mva_se_solut+ cr_mva_se_trouble+ cr_mva_se_handle

socialworld=~ cr_si_peopletrusted_REV + cr_si_peoplehelp_REV +
cr_si_trust_neighbor_REV + cr_si_trust_know_REV +  cr_si_friends_REV
+cr_si_trust_diffrelig_REV + cr_si_trust_diffnation_REV

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
lavInspect(tenth_cfa_fit, what="std")$lambda

#For paper: "Model A/ Initial Model"- all variables included. 
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
