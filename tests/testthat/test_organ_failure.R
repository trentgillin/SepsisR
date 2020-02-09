library(testthat)
library(dplyr)
library(tidyr)

test_sirs <- SepsisR::sirs_data
test_qsofa <- SepsisR::qsofa_data
test_sofa <-SepsisR::sofa_data

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


test_that("qSOFA is crorrect", {
  test_qsofa_1 <- test_qsofa[1,]
  result <- find_qsofa(.data = test_qsofa_1,
                       time = Service_Timestamp,
                       vitals = c("RR" = "RR",
                                  "SBP" = "SBP",
                                  "GCS" = "GCS"))
  expect_equal(result$qsofa_total, 1)
})

test_that("qSOFA period functionaliy is working and vitals good for more than 1 hour", {
  test_qsofa_2 <- test_qsofa
  test_qsofa_2 <- test_qsofa_2 %>%
    dplyr::mutate(SBP = dplyr::if_else(Service_Timestamp == lubridate::ymd_hms("2019-10-12 10:00:00"), 99, SBP),
                  SBP = dplyr::if_else(Service_Timestamp == lubridate::ymd_hms("2019-10-12 12:00:00"), as.double(NA), SBP))
  result <- find_qsofa(.data = test_qsofa_2,
                       time = Service_Timestamp,
                       vitals = c("RR" = "RR",
                                  "SBP" = "SBP",
                                  "GCS" = "GCS"),
                       period = 2)
  
  expect_equal(result$qsofa_total[3], 2)
})

test_that("Cardiac sofa score portion gives correct score with no vasopressor present", {
  test_sofa_1 <- sofa_data[1,]
  test_sofa_1 <- test_sofa_1 %>%
    calc_card_sofa(SBP = SBP, DBP = DBP, Vasopressor = Vasopressor, Vasopressor_dose = `Vasopressor Dosage`)
  expect_equal(test_sofa_1$cardiovascular_flag, 0)
  })

test_that("Cardiac sofa score portion gives correct score with vasopressor present", {
  test_sofa_1 <- sofa_data[4,]
  test_sofa_1 <- test_sofa_1 %>%
    calc_card_sofa(SBP = SBP, DBP = DBP, Vasopressor = Vasopressor, Vasopressor_dose = `Vasopressor Dosage`)
  expect_equal(test_sofa_1$cardiovascular_flag, 1)
})

test_that("Cardiac sofa score gives warning when other vasopressors besides dopamine and dobutamine present", {
  test_sofa_2 <- sofa_data[4,]
  test_sofa_2 <- test_sofa_2 %>%
    dplyr::mutate(Vasopressor = "Levophed")
  expect_warning(calc_card_sofa(.data = test_sofa_2, SBP = SBP, DBP = DBP, Vasopressor = Vasopressor, Vasopressor_dose = `Vasopressor Dosage`))
})
