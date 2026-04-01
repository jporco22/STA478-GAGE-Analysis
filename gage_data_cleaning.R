###GAGE Project- Data cleaning
### By: Julia Porco


library(tidyverse)
library(haven)
library(lavaan)
library(MASS)
#read core respondent data
gage_baseline18 <- read_dta("C:/Users/jporc/OneDrive/Desktop/sta478/gage_jordan_baseline_cr_public_v1.dta")
#read violence data 
cr_violence_base<- read_dta("C:/Users/jporc/OneDrive/Desktop/sta478/GAGE_Jordan_Baseline_Data_CR_Violenceonly.dta")

#Create a vector containing column labels for CR data:
cr_labels <- sapply(gage_baseline18, function(x) attr(x, "label"))
cat(attr(gage_baseline18$cr_crh_control, "label"))
#Find labels containing a specific string:
cr_labels[grep("injur", cr_labels, ignore.case = TRUE)]

#Create vector of column labels for violence data:
vio_labels<-sapply(cr_violence_base, function(x) attr(x, "label"))
#Find labels containing a string (violence data):
vio_labels[grep("string", vio_labels, ignore.case = TRUE)]

#Now make tables with selected variables
#table 1.1 df social self:
table_1_1_ss<-gage_baseline18 %>% 
  dplyr::select(hhid,cr_hn_scale,
                cr_mva_opinfriend, cr_mva_opinionelder,
                cr_mva_opinionbroth,cr_mva_opinionsist,
                cr_mva_se_solve,cr_mva_se_means,
                cr_mva_se_goal,cr_mva_se_event,
                cr_mva_se_situat,cr_mva_se_prob,
                cr_mva_se_calm, cr_mva_se_solut,
                cr_mva_se_trouble,cr_mva_se_handle, 
                cr_rc_opportunities, cr_rc_socialsit)
#reversing scale for opinion questions:
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_opinfriend_REV=ifelse(cr_mva_opinfriend==1,2,
                                      ifelse(cr_mva_opinfriend==2,1,
                                             NA)))
#test to see if categories are flipped:    
table(table_1_1_ss$cr_mva_opinfriend_REV, useNA="ifany")
table(table_1_1_ss$cr_mva_opinfriend, useNA="ifany")

table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_opinionelder_REV=ifelse(cr_mva_opinionelder==1,2,
                                        ifelse(cr_mva_opinionelder==2,1,
                                               NA)))
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_opinionsist_REV=ifelse(cr_mva_opinionsist==1,2,
                                       ifelse(cr_mva_opinionsist==2,1,
                                              NA)))
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_opinionbroth_REV=ifelse(cr_mva_opinionbroth==1,2,
                                        ifelse(cr_mva_opinionbroth==2,1,
                                               NA)))

#table 1.2 df, social world:
table_1_2_sw<- gage_baseline18 %>%
  dplyr::select(hhid, cr_si_diversity,
                cr_si_peopletrusted, cr_si_peoplehelp,
                cr_si_threaten,cr_si_othersthreaten,
                cr_si_trust_family,cr_si_trust_neighbor,
                cr_si_trust_know,cr_si_trust_first,
                cr_si_trust_diffrelig,cr_si_trust_diffnation,
                cr_vio_contrbeha,cr_vio_notdisc,
                cr_vio_interargue,cr_vio_vioprivematt)
#add in columns with corrected/reversed scales for certain variables. 
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_peopletrusted_REV= 
           ifelse(cr_si_peopletrusted==1,3,ifelse(cr_si_peopletrusted==2,2,
                                                  ifelse(cr_si_peopletrusted==3,1, NA))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_peoplehelp_REV= 
           ifelse(cr_si_peoplehelp==1,3,ifelse(cr_si_peoplehelp==2,2,
                                               ifelse(cr_si_peoplehelp==3,1, NA)))) 
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_threaten_REV= 
           ifelse(cr_si_threaten==1,3,ifelse(cr_si_threaten==2,2,
                                             ifelse(cr_si_threaten==3,1, NA))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_othersthreaten_REV= 
           ifelse(cr_si_othersthreaten==1,3,ifelse(cr_si_othersthreaten==2,2,
                                                   ifelse(cr_si_othersthreaten==3,1, NA))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_family_REV= 
           ifelse(cr_si_trust_family==1,4,
                  ifelse(cr_si_trust_family==2,3,
                         ifelse(cr_si_trust_family==3,2,
                                ifelse(cr_si_trust_family==4,1,NA)))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_neighbor_REV= 
           ifelse(cr_si_trust_neighbor==1,4,
                  ifelse(cr_si_trust_neighbor==2,3,
                         ifelse(cr_si_trust_neighbor==3,2,
                                ifelse(cr_si_trust_neighbor==4,1,NA)))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_know_REV= 
           ifelse(cr_si_trust_know==1,4,
                  ifelse(cr_si_trust_know==2,3,
                         ifelse(cr_si_trust_know==3,2,
                                ifelse(cr_si_trust_know==4,1,NA)))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_first_REV= 
           ifelse(cr_si_trust_first==1,4,
                  ifelse(cr_si_trust_first==2,3,
                         ifelse(cr_si_trust_first==3,2,
                                ifelse(cr_si_trust_first==4,1,NA)))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_diffrelig_REV= 
           ifelse(cr_si_trust_diffrelig==1,4,
                  ifelse(cr_si_trust_diffrelig==2,3,
                         ifelse(cr_si_trust_diffrelig==3,2,
                                ifelse(cr_si_trust_diffrelig==4,1,NA)))))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_diffnation_REV= 
           ifelse(cr_si_trust_diffnation==1,4,
                  ifelse(cr_si_trust_diffnation==2,3,
                         ifelse(cr_si_trust_diffnation==3,2,
                                ifelse(cr_si_trust_diffnation==4,1,NA)))))

#table 1.3, social safety/threat
table_1_3_violenceportion<- cr_violence_base %>%
  dplyr::select(hhid, cr_vi_peer_times1,cr_vi_peer_times2,
                cr_vi_peer_times3, cr_vi_peer_times4,
                cr_vi_peer_times5,cr_vi_peer_times6)
table_1_3_crportion<- gage_baseline18 %>%
  dplyr::select(hhid, cr_si_togetherness,
                cr_vio_safe_friend,cr_vio_safe_neighbor,
                cr_vio_safe_relative,cr_vio_safe_work,
                cr_rc_famsafe,cr_si_friends,cr_si_partsport,
                cr_si_ever_1,cr_si_ever_2, cr_si_ever_3,
                cr_si_ever_4, cr_si_ever_5,
                cr_si_member1,cr_si_member2, cr_si_member3,
                cr_si_member4,cr_si_member5,
                cr_si_oftpart1, cr_si_oftpart2, cr_si_oftpart3,
                cr_si_oftpart4,cr_si_oftpart5,
                cr_rc_friendsupp,
                cr_rc_friendtimes)
table_1_3_sst<- inner_join(table_1_3_crportion,table_1_3_violenceportion, by="hhid")

#add in reverse-scale versions of certain columns:
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vio_safe_friend_REV=
           ifelse(cr_vio_safe_friend==1,2,
                  ifelse(cr_vio_safe_friend==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vio_safe_neighbor_REV=
           ifelse(cr_vio_safe_neighbor==1,2,
                  ifelse(cr_vio_safe_neighbor==2,1,NA))) 
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vio_safe_relative_REV=
           ifelse(cr_vio_safe_relative==1,2,
                  ifelse(cr_vio_safe_relative==2,1,NA))) 
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vio_safe_work_REV=
           ifelse(cr_vio_safe_work==1,2,
                  ifelse(cr_vio_safe_work==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_friends_REV=
           ifelse(cr_si_friends==1,2,
                  ifelse(cr_si_friends==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_partsport_REV=
           ifelse(cr_si_partsport==1,2,
                  ifelse(cr_si_partsport==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_ever_1_REV=ifelse(cr_si_ever_1==1,2,
                                 ifelse(cr_si_ever_1==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_ever_2_REV=ifelse(cr_si_ever_2==1,2,
                                 ifelse(cr_si_ever_2==2,1,NA))) 
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_ever_3_REV=ifelse(cr_si_ever_3==1,2,
                                 ifelse(cr_si_ever_3==2,1,NA)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_ever_5_REV=ifelse(cr_si_ever_5==1,2,
                                 ifelse(cr_si_ever_5==2,1,NA)))

#table 1.4, non-social safety/threat: (pulling from both data frames)
table_1_4_cr_portion<- gage_baseline18 %>% 
  dplyr::select(hhid, cr_edu_abusetell,
                cr_vio_safe_home, cr_vio_safe_travelwork,
                cr_vio_safe_market, cr_vio_safe_travelmarket,
                cr_vio_safe_waterfuel,cr_vio_safe_religious,
                cr_vio_safe_makani,cr_edu_trvlsafe,
                cr_edu_schsafe,cr_vio_discipline)
table_1_4_vio_portion<- cr_violence_base %>%
  dplyr::select(hhid, cr_vio_home_yell,
                cr_vio_home_treatpoorly,cr_vio_home_slapparent,
                cr_vio_home_slapbrother,cr_vio_home_fatherhit,
                cr_vio_home_motherbeaten, cr_edu_abuse,
                cr_edu_otherabuse,cr_edu_punish)
table_1_4_nsst<- inner_join(table_1_4_cr_portion,
                            table_1_4_vio_portion, by="hhid")
#add in reverse-scaled version of certain columns:
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_home_REV=
           ifelse(cr_vio_safe_home==1,2,
                  ifelse(cr_vio_safe_home==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_travelwork_REV=
           ifelse(cr_vio_safe_travelwork==1,2,
                  ifelse(cr_vio_safe_travelwork==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_market_REV=
           ifelse(cr_vio_safe_market==1,2,
                  ifelse(cr_vio_safe_market==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_travelmarket_REV=
           ifelse(cr_vio_safe_travelmarket==1,2,
                  ifelse(cr_vio_safe_travelmarket==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_waterfuel_REV=
           ifelse(cr_vio_safe_waterfuel==1,2,
                  ifelse(cr_vio_safe_waterfuel==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_religious_REV=
           ifelse(cr_vio_safe_religious==1,2,
                  ifelse(cr_vio_safe_religious==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_safe_makani_REV=
           ifelse(cr_vio_safe_makani==1,2,
                  ifelse(cr_vio_safe_makani==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_edu_trvlsafe_REV=
           ifelse(cr_edu_trvlsafe==1,2,
                  ifelse(cr_edu_trvlsafe==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_edu_schsafe_REV=
           ifelse(cr_edu_schsafe==1,2,
                  ifelse(cr_edu_schsafe==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_edu_abuse_REV=
           ifelse(cr_edu_abuse==1,2,
                  ifelse(cr_edu_abuse==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_edu_otherabuse_REV=
           ifelse(cr_edu_otherabuse==1,2,
                  ifelse(cr_edu_otherabuse==2,1,NA)))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_edu_punish_REV=
           ifelse(cr_edu_punish==1,2,
                  ifelse(cr_edu_punish==2,1,NA)))
#table 2, social and geopolitical positioning
table_2_socgeo<- gage_baseline18 %>%
  dplyr::select(hhid,
                list_crgender,crmodule_gender,
                list_crage, hh_cs_youngcoh,
                cr_cs_nationality,cr_rc_enoughfood)

#table 3, education and economic empowerment 
table_3_edueco<- gage_baseline18 %>%
  dplyr::select(hhid, cr_edu_attndever,
                cr_edu_neverr,cr_edu_lastattndage,
                cr_edu_highatt,
                cr_edu_stopr,cr_cs_location)

#table 4: Moderators (child and youth resilience measure)
#contains 28 cr_rc columns 
table_4_resi <- gage_baseline18 %>% dplyr::select(hhid, contains("cr_rc"))
#add in cyrm column:(note, negative values mean at least one question was not answered)
table_4_resi$cr_rc_cyrm<-rowSums(table_4_resi)- table_4_resi$hhid


# table 5: outcomes and treatment
table_5_outcomes<- gage_baseline18 %>%
  dplyr::select(hhid,cr_hn_gnhlth,
                cr_crh_worry, cr_crh_control,
                cr_crh_focus,cr_crh_accept,
                cr_crh_friends, cr_hn_injuryyn)
#reverse scaled column for general self-rated health:
table_5_outcomes<- table_5_outcomes %>% rowwise() %>%
  mutate(cr_hn_gnhlth_REV=
           ifelse(cr_hn_gnhlth==1,5,
                  ifelse(cr_hn_gnhlth==2,4, 
                         ifelse(cr_hn_gnhlth==3,3,
                                ifelse(cr_hn_gnhlth==4,2,
                                       ifelse(cr_hn_gnhlth==5,1,NA))))))


#All tables merged: (total reduced dataset)
reduced_df<-table_1_1_ss %>%
  inner_join(table_1_2_sw,by='hhid') %>% 
  inner_join(table_1_3_sst,by='hhid') %>%
  inner_join(table_1_4_nsst,by='hhid') %>% 
  inner_join(table_2_socgeo,by='hhid') %>%
  inner_join(table_3_edueco,by='hhid') %>%
  inner_join(table_4_resi,by='hhid',suffix=c("", "_duplic")) %>%
  inner_join(table_5_outcomes,by='hhid')
#Table 4 has overlap with other tables, label them as duplicates
#turn negative values to NAs so lavaan can understand:
reduced_df[reduced_df<0]<-NA
