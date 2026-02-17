
##Jordan 2018 Baseline Data 
## By: Julia Porco

#github TEST change
#change on github browser

library(tidyverse)
library(dplyr)
library(haven)
#install.packages("lavaan", dependencies = TRUE)
library(lavaan)
#read core respondent data
gage_baseline18 <- read_dta("C:/Users/jporc/OneDrive/Desktop/sta478/gage_jordan_baseline_cr_public_v1.dta")
#read violence data 
cr_violence_base<- read_dta("C:/Users/jporc/OneDrive/Desktop/sta478/GAGE_Jordan_Baseline_Data_CR_Violenceonly.dta")

#Create a vector containing column labels for CR data:
cr_labels <- sapply(gage_baseline18, function(x) attr(x, "label"))
cat(attr(gage_baseline18$cr_crh_control, "label"))
#Find labels containing a specific string:
cr_labels[grep("attended", cr_labels, ignore.case = TRUE)]

#Create vector of column labels for violence data:
vio_labels<-sapply(cr_violence_base, function(x) attr(x, "label"))
#Find labels containing a string (violence data):
vio_labels[grep("string", vio_labels, ignore.case = TRUE)]

#Now make tables with selected variables
#table 1.1 df social self:
table_1_1_ss<-gage_baseline18 %>% 
  select(hhid,cr_hn_scale,
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
#now create summarizing column for opinion questions and more:
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_opin_sum_REV=log(sum(cr_mva_opinionelder_REV,cr_mva_opinfriend_REV,
                                 cr_mva_opinionbroth_REV,cr_mva_opinionsist_REV)))
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_mva_selfeff_sum=log(sum(cr_mva_se_solve, cr_mva_se_means,
                                cr_mva_se_goal,cr_mva_se_event,
                                cr_mva_se_situat, cr_mva_se_prob, 
                                cr_mva_se_calm, cr_mva_se_solut,
                                cr_mva_se_trouble, cr_mva_se_handle)))
table_1_1_ss<-table_1_1_ss %>% rowwise() %>%
  mutate(cr_rc_socialself_sum=log(sum(cr_rc_opportunities, cr_rc_socialsit)))

#table 1.2 df, social world:
table_1_2_sw<- gage_baseline18 %>%
  select(hhid, cr_si_diversity,
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
#create summarizing columns:
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trust_REV_sum=
           log(sum(cr_si_trust_family_REV,cr_si_trust_neighbor_REV,
               cr_si_trust_know_REV,cr_si_trust_first_REV,
               cr_si_trust_diffrelig_REV,cr_si_trust_diffnation_REV)))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_si_trustcollective_REV_sum=
           log(sum(cr_si_peopletrusted_REV,cr_si_peoplehelp_REV,cr_si_threaten_REV,
               cr_si_othersthreaten_REV)))
table_1_2_sw<- table_1_2_sw %>% rowwise() %>% 
  mutate(cr_vio_attitude_sum=log(sum(cr_vio_contrbeha, cr_vio_notdisc, 
                                 cr_vio_interargue, cr_vio_vioprivematt)))

#table 1.3, social safety/threat
table_1_3_violenceportion<- cr_violence_base %>%
  select(hhid, cr_vi_peer_times1,cr_vi_peer_times2,
         cr_vi_peer_times3, cr_vi_peer_times4,
         cr_vi_peer_times5,cr_vi_peer_times6)
table_1_3_crportion<- gage_baseline18 %>%
  select(hhid, cr_si_togetherness,
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
#add in columns to summarize group/activity participation data:
#exclude group #4 (JHOUD Amira Bassma centre) due to NA values
table_1_3_sst<- table_1_3_sst %>% 
  mutate(cr_si_ever_sum=ifelse((cr_si_ever_1+ cr_si_ever_2 
         + cr_si_ever_3 + cr_si_ever_5)>4, 1,0))
table_1_3_sst<- table_1_3_sst %>%
  mutate(cr_si_member_sum=
           ifelse((cr_si_member1+ cr_si_member2 
                    + cr_si_member3 +  
                     cr_si_member5)>4, 1,0))
table_1_3_sst<- table_1_3_sst %>%
  mutate(cr_si_oftpart_sum=
           ifelse((cr_si_oftpart1+ cr_si_oftpart2 
                   + cr_si_oftpart3 + cr_si_oftpart5)<6, 1,0))

#add in reverse-scale versions of certain columns:
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times1_REV=
           ifelse(cr_vi_peer_times1==1,3,
                  ifelse(cr_vi_peer_times1==2,2,
                         ifelse(cr_vi_peer_times1==3,1,NA))))
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times2_REV=
           ifelse(cr_vi_peer_times2==1,3,
                  ifelse(cr_vi_peer_times2==2,2,
                         ifelse(cr_vi_peer_times2==3,1,NA))))
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times3_REV=
           ifelse(cr_vi_peer_times3==1,3,
                  ifelse(cr_vi_peer_times3==2,2,
                         ifelse(cr_vi_peer_times3==3,1,NA))))
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times4_REV=
           ifelse(cr_vi_peer_times4==1,3,
                  ifelse(cr_vi_peer_times4==2,2,
                         ifelse(cr_vi_peer_times4==3,1,NA))))
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times5_REV=
           ifelse(cr_vi_peer_times5==1,3,
                  ifelse(cr_vi_peer_times5==2,2,
                         ifelse(cr_vi_peer_times5==3,1,NA))))
table_1_3_sst<- table_1_3_sst %>% rowwise() %>%
  mutate(cr_vi_peer_times6_REV=
           ifelse(cr_vi_peer_times6==1,3,
                  ifelse(cr_vi_peer_times6==2,2,
                         ifelse(cr_vi_peer_times6==3,1,NA))))
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

#create summarizing columns:
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vio_safe_soc_REV_sum=
           log(sum(cr_vio_safe_friend_REV,cr_vio_safe_neighbor_REV,
               cr_vio_safe_relative_REV)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_vi_peer_times_REV_sum=log(sum(
    cr_vi_peer_times1_REV, cr_vi_peer_times2_REV, cr_vi_peer_times3_REV, 
    cr_vi_peer_times4_REV, cr_vi_peer_times5_REV, cr_vi_peer_times6_REV)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_rc_friend_sum=log(sum(cr_rc_friendsupp,cr_rc_friendtimes)))
table_1_3_sst<-table_1_3_sst%>% rowwise() %>%
  mutate(cr_si_ever_REV_sum=log(sum(
    cr_si_ever_1_REV, cr_si_ever_2_REV, cr_si_ever_3_REV, cr_si_ever_5_REV, 
    cr_si_partsport_REV)))

#table 1.4, non-social safety/threat: (pulling from both data frames)
table_1_4_cr_portion<- gage_baseline18 %>% 
  select(hhid, cr_edu_abusetell,
    cr_vio_safe_home, cr_vio_safe_travelwork,
    cr_vio_safe_market, cr_vio_safe_travelmarket,
    cr_vio_safe_waterfuel,cr_vio_safe_religious,
    cr_vio_safe_makani,cr_edu_trvlsafe,
    cr_edu_schsafe,cr_vio_discipline)
table_1_4_vio_portion<- cr_violence_base %>%
  select(hhid, cr_vio_home_yell,
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
  mutate(cr_vio_home_yell_REV=ifelse(
    cr_vio_home_yell==1,3,ifelse(cr_vio_home_yell==2,2,ifelse(
      cr_vio_home_yell==3,1,NA))))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_home_treatpoorly_REV=ifelse(
    cr_vio_home_treatpoorly==1,3,ifelse(cr_vio_home_treatpoorly==2,2,ifelse(
      cr_vio_home_treatpoorly==3,1,NA))))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_home_slapparent_REV=ifelse(
    cr_vio_home_slapparent==1,3,ifelse(cr_vio_home_slapparent==2,2,ifelse(
      cr_vio_home_slapparent==3,1,NA))))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_home_slapbrother_REV=ifelse(
    cr_vio_home_slapbrother==1,3,ifelse(cr_vio_home_slapbrother==2,2,ifelse(
      cr_vio_home_slapbrother==3,1,NA))))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_home_fatherhit_REV=ifelse(
    cr_vio_home_fatherhit==1,3,ifelse(cr_vio_home_fatherhit==2,2,ifelse(
      cr_vio_home_fatherhit==3,1,NA))))
table_1_4_nsst<- table_1_4_nsst %>% rowwise()%>%
  mutate(cr_vio_home_motherbeaten_REV=ifelse(
    cr_vio_home_motherbeaten==1,3,ifelse(cr_vio_home_motherbeaten==2,2,ifelse(
      cr_vio_home_motherbeaten==3,1,NA))))

# create summarizing columns:
table_1_4_nsst<- table_1_4_nsst%>% rowwise() %>%
  mutate(cr_vi_nonsoc_safe_REV_sum=
           log(sum(cr_vio_safe_home_REV,
               cr_vio_safe_market_REV,cr_vio_safe_travelmarket_REV,
               cr_vio_safe_religious_REV,
               cr_vio_safe_makani_REV,cr_edu_trvlsafe_REV,cr_edu_schsafe_REV)))
table_1_4_nsst<- table_1_4_nsst%>% rowwise() %>%
  mutate(cr_vio_home_REV_sum=log(sum(
    cr_vio_home_treatpoorly_REV, cr_vio_home_slapparent_REV, 
    cr_vio_home_slapbrother_REV, cr_vio_home_fatherhit_REV, 
    cr_vio_home_motherbeaten_REV, cr_vio_home_yell_REV)))
table_1_4_nsst<- table_1_4_nsst%>% rowwise() %>%
  mutate(cr_edu_vio_sum=log(sum(
    cr_edu_abuse, cr_edu_otherabuse, cr_edu_punish)))

#table 2, social and geopolitical positioning
table_2_socgeo<- gage_baseline18 %>%
  select(hhid,
         list_crgender,crmodule_gender,
         list_crage, hh_cs_youngcoh,
         cr_cs_nationality,cr_rc_enoughfood)

#table 3, education and economic empowerment 
table_3_edueco<- gage_baseline18 %>%
  select(hhid, cr_edu_attndever,
         cr_edu_neverr,cr_edu_lastattndage,
         cr_edu_highatt,
         cr_edu_stopr,cr_cs_location)

#table 4: Moderators (child and youth resilience measure)
#contains 28 cr_rc columns 
table_4_resi <- gage_baseline18 %>% select(hhid, contains("cr_rc"))
#add in cyrm column:(note, negative values mean at least one question was not answered)
table_4_resi$cr_rc_cyrm<-rowSums(table_4_resi)- table_4_resi$hhid


# table 5: outcomes and treatment
table_5_outcomes<- gage_baseline18 %>%
  select(hhid,cr_hn_gnhlth,
         cr_crh_worry, cr_crh_control,
         cr_crh_focus,cr_crh_accept,
         cr_crh_friends, cr_hn_injuryyn)
  
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
                          cr_vi_peer_times4+ cr_vi_peer_times5+
                          cr_vi_peer_times6
              nonsocialsafetythreat=~cr_vio_home_yell+cr_vio_home_treatpoorly+
                          cr_vio_home_slapparent+ cr_vio_home_slapbrother+
                          cr_vio_home_fatherhit+cr_vio_home_motherbeaten'
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
seventh_cfa_fit=cfa(seventh_cfa,data=reduced_df,ordered=F,missing='pairwise')
summary(seventh_cfa_fit, fit.measures=T)


