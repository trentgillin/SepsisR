## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(SepsisR)
library(dplyr)
library(lubridate)
library(DT)

## ----sirs_example-------------------------------------------------------------
# practice data
test_sirs <- SepsisR::sirs_data

# implementing the function
test_sirs <- SepsisR::assess_organ(.data = test_sirs,
                          method = "SIRS",
                          patientid = Encounter,
                          time = `Service_Timestamp`,
                          vitals = c("Temperature" = "Temperature",
                                     "RR" = "RR",
                                     "HR" = "HR",
                                     "WBC" = "Wbc"))
# looking at the results
datatable(test_sirs)

## ----sofa_example-------------------------------------------------------------

# get the test dataset
test_sofa <- sofa_data

# implementing the function
test_sofa <- SepsisR::assess_organ(.data = test_sofa,
                                   method = "SOFA",
                                   patientid = Encounter,
                                   time = Service_Timestamp,
                                   vitals = c("PaO2" = "Pa02",
                                              "FiO2" = "Fi02",
                                              "Platelets" = "Platelet",
                                              "Bilirubin" = "Bili",
                                              "GCS" = "GCS",
                                              "Creatinine" = "Creatinine",
                                              "SBP" = "SBP",
                                              "DBP" = "DBP",
                                              "Vasopressor" = "Vasopressor",
                                              "Vasopressor_dose" = "Vasopressor Dosage"))

# view results
test_sofa %>%
  datatable(options = list(scrollX = T, pageLength = 5))

## ----blood_culture_window-----------------------------------------------------

# use test sirs data
test_sirs <- sirs_data

# add on a fake blood culture time
test_sirs <- test_sirs %>%
  mutate(blood_cult_time = lubridate::ymd_hms("2018-12-9 09:00:00")) %>%
  find_bx_window(timestamp_variable = Service_Timestamp,blood_culture_time = blood_cult_time)

## ----blood_culture_window_cont.-----------------------------------------------
test_sirs %>%
  select(Encounter, Service_Timestamp, blood_cult_time, within_window)

