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
prop_young #ages 10-12
prop_old  #ages 15-18

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
