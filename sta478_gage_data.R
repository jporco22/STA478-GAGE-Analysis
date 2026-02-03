
##Jordan 2018 Baseline Data 
## By: Julia Porco

#github TEST change

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

#table 1.3, social safety/threat,  unsure about certain columns
table_1_3_violence<- cr_violence_base %>%
  select(hhid, cr_vi_peer_times1,cr_vi_peer_times2,
         cr_vi_peer_times3, cr_vi_peer_times4,
         cr_vi_peer_times5,cr_vi_peer_times6)

table_1_3_cr<- gage_baseline18 %>%
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

table_1_3_sst<- inner_join(table_1_3_cr,table_1_3_violence, by="hhid")

#add in columns to summarize group/activity data:
#exclude group #4 (JHOUD Amira Bassma centre) due to NA values
table_1_3_sst<- table_1_3_sst %>% 
  mutate(cr_si_ever_sum=ifelse((cr_si_ever_1+ cr_si_ever_2 
         + cr_si_ever_3 + cr_si_ever_5)>4, 1,0))
table(table_1_3_sst$cr_si_ever_sum, useNA="ifany")

table_1_3_sst<- table_1_3_sst %>%
  mutate(cr_si_member_sum=
           ifelse((cr_si_member1+ cr_si_member2 
                    + cr_si_member3 +  
                     cr_si_member5)>4, 1,0))
table(table_1_3_sst$cr_si_member_sum, useNA="ifany")

table_1_3_sst<- table_1_3_sst %>%
  mutate(cr_si_oftpart_sum=
           ifelse((cr_si_oftpart1+ cr_si_oftpart2 
                   + cr_si_oftpart3 + cr_si_oftpart5)<6, 1,0))
table(table_1_3_sst$cr_si_oftpart_sum, useNA="ifany")



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
table(table_4_resi$cr_rc_cyrm,useNA='ifany')

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


###CONFIRMATORY FACTOR ANALYSIS
#using lavaan package cfa function
#Random test trial:
example_model<- 'socialself=~ cr_rc_opportunities + cr_mva_se_means
                  socialworld=~ cr_si_peopletrusted 
                  socialsafetythreat=~ cr_si_togetherness+ cr_vio_safe_neighbor
                  nonsocialsafetythreat=~cr_edu_trvlsafe'
example_fit<- cfa(example_model, data=reduced_df)
summary(example_fit, fit.measures=T)

#test_efa<-efa(data=reduced_df)
