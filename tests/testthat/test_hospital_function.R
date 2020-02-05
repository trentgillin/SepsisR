library(testthat)
library(dplyr)
library(lubridate)
library(tidyr)


test_sirs <- SepsisR::sirs_data

test_that("Hospital day gives correct day", {
  test_sirs <- dplyr::rename(test_sirs, "Vital Time" = Service_Timestamp)
  test_sirs <- dplyr::select(test_sirs, Encounter, `Vital Time`, HR)
  test_sirs <- dplyr::mutate(test_sirs, `Admission Day` = lubridate::ymd("2018-12-01"),
                             `Vital Time` = as.Date(`Vital Time`))
  test_sirs <- find_hospital_day(test_sirs, event = `Vital Time`, patientid = Encounter,
                                 admission_day = `Admission Day`)
  expect_equal(test_sirs[1,]$hospital_day, 11)
  
})
