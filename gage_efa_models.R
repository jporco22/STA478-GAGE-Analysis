###GAGE Project- EFA Models
### By: Julia Porco

source("C:/Users/jporc/STA478-GAGE-Analysis/gage_data_cleaning.R")

#######EFA First attempt
# contains most indicator variables from the 4 main factors.
# exclude the participation in groups questions and other ones that didn't seem ordinal
efa_test<- efa(data=reduced_df, nfactors=4,
               ov.names=
                 c('cr_mva_opinfriend','cr_mva_opinionelder','cr_mva_opinionbroth',
                   'cr_mva_opinionsist',
                   "cr_mva_se_solve","cr_mva_se_means",'cr_mva_se_goal','cr_hn_scale',
                   'cr_mva_se_event', 'cr_mva_se_situat', 'cr_mva_se_prob',
                   'cr_mva_se_calm', 'cr_mva_se_solut', 'cr_mva_se_trouble',
                   'cr_mva_se_handle', 
                   'cr_si_peopletrusted', 'cr_si_peoplehelp',
                   'cr_si_threaten', 'cr_si_othersthreaten','cr_si_trust_family',
                   'cr_si_trust_neighbor','cr_si_trust_know', 'cr_si_trust_first',
                   'cr_si_trust_diffrelig', 'cr_si_trust_diffnation',
                   'cr_vio_contrbeha',
                   'cr_vio_notdisc', 'cr_vio_interargue',
                   'cr_vio_vioprivematt', 'cr_si_togetherness','cr_si_friends', 
                   'cr_si_partsport','cr_vio_safe_friend','cr_vio_safe_neighbor',
                   'cr_vio_safe_relative','cr_vio_safe_work',  
                   'cr_vi_peer_times1',
                   'cr_vi_peer_times2','cr_vi_peer_times3',
                   'cr_vi_peer_times4', 'cr_vi_peer_times5',
                   'cr_vi_peer_times6', 'cr_vio_home_yell',
                   'cr_vio_home_treatpoorly', 'cr_vio_home_slapparent',
                   'cr_vio_home_slapbrother','cr_vio_home_fatherhit',
                   'cr_vio_home_motherbeaten', 
                   'cr_edu_abuse', 'cr_edu_otherabuse',
                   'cr_edu_punish', 'cr_edu_abusetell', 'cr_vio_safe_home',
                   'cr_vio_safe_travelwork', 'cr_vio_safe_market',
                   'cr_vio_safe_travelmarket','cr_vio_safe_waterfuel',
                   'cr_vio_safe_religious','cr_vio_safe_makani','cr_edu_trvlsafe',
                   'cr_edu_schsafe'),
               missing="pairwise", ordered=T)
summary(efa_test, fit.measures=T)

#create df without resilience, sociogeo, edueco and outcomes tables.
reduced_df_4factors<-table_1_1_ss %>%
  inner_join(table_1_2_sw,by='hhid') %>% 
  inner_join(table_1_3_sst,by='hhid') %>%
  inner_join(table_1_4_nsst,by='hhid')
reduced_df_4factors[reduced_df_4factors<0]<-NA
#EFA attempt with all indicators: 
first_efa<- efa(data=reduced_df_4factors, nfactors=4,missing="pairwise", 
                ordered=T)
summary(first_efa, fit.measures=T)
# UPDATE THIS ONE WON'T RUN AFTER 10 MINUTES 



#efa 2nd attempt: same as first attempt just replace nfactor=4 with 5
second_efa<- efa(data=reduced_df, nfactors=5,
                 ov.names=
                   c('cr_mva_opinfriend','cr_mva_opinionelder','cr_mva_opinionbroth',
                     'cr_mva_opinionsist',
                     "cr_mva_se_solve","cr_mva_se_means",'cr_mva_se_goal','cr_hn_scale',
                     'cr_mva_se_event', 'cr_mva_se_situat', 'cr_mva_se_prob',
                     'cr_mva_se_calm', 'cr_mva_se_solut', 'cr_mva_se_trouble',
                     'cr_mva_se_handle', 'cr_rc_opportunities','cr_rc_socialsit',
                     'cr_si_peopletrusted', 'cr_si_peoplehelp',
                     'cr_si_threaten', 'cr_si_othersthreaten','cr_si_trust_family',
                     'cr_si_trust_neighbor','cr_si_trust_know', 'cr_si_trust_first',
                     'cr_si_trust_diffrelig', 'cr_si_trust_diffnation',
                     'cr_vio_contrbeha',
                     'cr_vio_notdisc', 'cr_vio_interargue',
                     'cr_vio_vioprivematt', 'cr_si_togetherness','cr_si_friends', 
                     'cr_si_partsport','cr_vio_safe_friend','cr_vio_safe_neighbor',
                     'cr_vio_safe_relative','cr_vio_safe_work','cr_rc_famsafe',
                     'cr_rc_friendsupp', 'cr_rc_friendtimes', 
                     'cr_vi_peer_times1',
                     'cr_vi_peer_times2','cr_vi_peer_times3',
                     'cr_vi_peer_times4', 'cr_vi_peer_times5',
                     'cr_vi_peer_times6', 'cr_vio_home_yell',
                     'cr_vio_home_treatpoorly', 'cr_vio_home_slapparent',
                     'cr_vio_home_slapbrother','cr_vio_home_fatherhit',
                     'cr_vio_home_motherbeaten', 
                     'cr_edu_abuse', 'cr_edu_otherabuse',
                     'cr_edu_punish', 'cr_edu_abusetell', 'cr_vio_safe_home',
                     'cr_vio_safe_travelwork', 'cr_vio_safe_market',
                     'cr_vio_safe_travelmarket','cr_vio_safe_waterfuel',
                     'cr_vio_safe_religious','cr_vio_safe_makani','cr_edu_trvlsafe',
                     'cr_edu_schsafe'),
                 missing="pairwise", ordered=T)
summary(second_efa, fit.measures=T)


#EFA 3rd attempt
#remove variables not loaded into any factors 
third_efa<- efa(data=reduced_df, nfactors=4,
                ov.names=
                  c('cr_mva_opinfriend',
                    "cr_mva_se_solve","cr_mva_se_means",'cr_mva_se_goal',
                    'cr_mva_se_event', 'cr_mva_se_situat', 'cr_mva_se_prob',
                    'cr_mva_se_calm', 'cr_mva_se_solut', 'cr_mva_se_trouble',
                    'cr_mva_se_handle', 'cr_rc_opportunities','cr_rc_socialsit',
                    'cr_si_peopletrusted', 'cr_si_peoplehelp',
                    
                    'cr_si_trust_neighbor','cr_si_trust_know', 
                    'cr_si_trust_diffrelig', 'cr_si_trust_diffnation',
                    'cr_vio_interargue',
                    'cr_vio_vioprivematt','cr_si_friends', 
                    'cr_vio_safe_friend','cr_vio_safe_neighbor',
                    'cr_vio_safe_relative','cr_vio_safe_work','cr_rc_famsafe',
                    'cr_rc_friendsupp', 'cr_rc_friendtimes', 
                    'cr_vi_peer_times1',
                    'cr_vi_peer_times2','cr_vi_peer_times3',
                    'cr_vi_peer_times4', 'cr_vi_peer_times5',
                    'cr_vi_peer_times6', 'cr_vio_home_yell',
                    'cr_vio_home_treatpoorly', 'cr_vio_home_slapparent',
                    'cr_vio_home_slapbrother','cr_vio_home_fatherhit',
                    
                    'cr_edu_abuse', 'cr_edu_otherabuse',
                    'cr_edu_punish', 'cr_vio_safe_home',
                    'cr_vio_safe_travelwork', 'cr_vio_safe_market',
                    'cr_vio_safe_travelmarket','cr_vio_safe_waterfuel',
                    'cr_vio_safe_religious','cr_vio_safe_makani','cr_edu_trvlsafe'
                  ),
                missing="pairwise", ordered=T)
summary(third_efa, fit.measures=T)


#EFA fourth attempt: remove indicators with 0 loadings and with cross loadings
fourth_efa<- efa(data=reduced_df, nfactors=4,
                 ov.names=
                   c('cr_mva_opinfriend',
                     "cr_mva_se_solve","cr_mva_se_means",'cr_mva_se_goal',
                     'cr_mva_se_event', 'cr_mva_se_situat', 'cr_mva_se_prob',
                     'cr_mva_se_calm', 'cr_mva_se_solut', 'cr_mva_se_trouble',
                     'cr_mva_se_handle', 'cr_rc_opportunities','cr_rc_socialsit',
                     'cr_si_peopletrusted', 'cr_si_peoplehelp',
                     
                     'cr_si_trust_neighbor','cr_si_trust_know', 
                     'cr_si_trust_diffrelig', 'cr_si_trust_diffnation',
                     'cr_si_friends', 
                     'cr_vio_safe_friend',
                     'cr_vio_safe_relative','cr_vio_safe_work','cr_rc_famsafe',
                     
                     'cr_vi_peer_times1',
                     'cr_vi_peer_times2','cr_vi_peer_times3',
                     'cr_vi_peer_times4', 'cr_vi_peer_times5',
                     'cr_vi_peer_times6', 'cr_vio_home_yell',
                     'cr_vio_home_treatpoorly', 'cr_vio_home_slapparent',
                     'cr_vio_home_slapbrother','cr_vio_home_fatherhit',
                     
                     
                     'cr_edu_punish', 
                     'cr_vio_safe_travelwork', 'cr_vio_safe_market',
                     'cr_vio_safe_travelmarket','cr_vio_safe_waterfuel',
                     'cr_vio_safe_religious','cr_vio_safe_makani','cr_edu_trvlsafe'
                   ),
                 missing="pairwise", ordered=T)
summary(fourth_efa, fit.measures=T)


#EFA fifth attempt, put nfactors=6 in model #1
fifth_efa<- efa(data=reduced_df, nfactors=6,
                ov.names=
                  c('cr_mva_opinfriend','cr_mva_opinionelder','cr_mva_opinionbroth',
                    'cr_mva_opinionsist',
                    "cr_mva_se_solve","cr_mva_se_means",'cr_mva_se_goal','cr_hn_scale',
                    'cr_mva_se_event', 'cr_mva_se_situat', 'cr_mva_se_prob',
                    'cr_mva_se_calm', 'cr_mva_se_solut', 'cr_mva_se_trouble',
                    'cr_mva_se_handle', 'cr_rc_opportunities','cr_rc_socialsit',
                    'cr_si_peopletrusted', 'cr_si_peoplehelp',
                    'cr_si_threaten', 'cr_si_othersthreaten','cr_si_trust_family',
                    'cr_si_trust_neighbor','cr_si_trust_know', 'cr_si_trust_first',
                    'cr_si_trust_diffrelig', 'cr_si_trust_diffnation',
                    'cr_vio_contrbeha',
                    'cr_vio_notdisc', 'cr_vio_interargue',
                    'cr_vio_vioprivematt', 'cr_si_togetherness','cr_si_friends', 
                    'cr_si_partsport','cr_vio_safe_friend','cr_vio_safe_neighbor',
                    'cr_vio_safe_relative','cr_vio_safe_work','cr_rc_famsafe',
                    'cr_rc_friendsupp', 'cr_rc_friendtimes', 
                    'cr_vi_peer_times1',
                    'cr_vi_peer_times2','cr_vi_peer_times3',
                    'cr_vi_peer_times4', 'cr_vi_peer_times5',
                    'cr_vi_peer_times6', 'cr_vio_home_yell',
                    'cr_vio_home_treatpoorly', 'cr_vio_home_slapparent',
                    'cr_vio_home_slapbrother','cr_vio_home_fatherhit',
                    'cr_vio_home_motherbeaten', 
                    'cr_edu_abuse', 'cr_edu_otherabuse',
                    'cr_edu_punish', 'cr_edu_abusetell', 'cr_vio_safe_home',
                    'cr_vio_safe_travelwork', 'cr_vio_safe_market',
                    'cr_vio_safe_travelmarket','cr_vio_safe_waterfuel',
                    'cr_vio_safe_religious','cr_vio_safe_makani','cr_edu_trvlsafe',
                    'cr_edu_schsafe'),
                missing="pairwise", ordered=T)
summary(fifth_efa, fit.measures=T)
