setwd("~/Desktop/Data/")
list.files()

# gage_jordan_baseline_af_public_v1
# gage_jordan_baseline_cr_public_v1
# GAGE_Jordan_Baseline_Data_CR_Violenceonly
# gage_jordan_baseline_hh_information_public_v1


af <- read_dta("~/Desktop/Data/gage_jordan_baseline_cr_public_v1.dta")
cr <- read_dta("~/Desktop/Data/gage_jordan_baseline_cr_public_v1.dta")
cr_violence <- read_dta("~/Desktop/Data/gage_jordan_baseline_hh_information_public_v1.dta")
# hh <- read_dta("~/Desktop/Data/gage_jordan_baseline_hh_information_public_v1.dta")


