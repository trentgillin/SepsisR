library(testthat)
library(dplyr)
library(tidyr)

test_sirs <- SepsisR::sirs_data

test_that("You get an error with missing vital", {
  expect_error(score <- find_sirs(test_sirs,
                     patientid = Encounter,
                     time = Service_Timestamp,
                     vitals = c("HR" = "HR", "Temperature" = "Temperature")))

})


test_that("SIRs is correct", {
  test_sirs <- test_sirs[1,]
  test_sirs <- find_sirs(test_sirs,
                         patientid = Encounter,
                         time = Service_Timestamp,
                         vitals = c("HR" = "HR",
                                     "Temperature" = "Temperature",
                                     "RR" = "RR",
                                     "WBC" = "Wbc"))
  expect_equal(test_sirs$sirs_total, 2)
    
})

test_that("Assess organ function gives error with missing vital", {
  expect_error(score <- assess_organ(test_sirs,
                                     method = "SOFA",
                                  patientid = Encounter,
                                  time = Service_Timestamp,
                                  vitals = c("HR" = "HR", "Temperature" = "Temperature")))
})
