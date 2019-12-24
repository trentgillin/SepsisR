library(testthat)
test_sirs <- SepsisR::sirs_data

test_that("You get an error with missing vital", {
  expect_error(score <- find_sirs(test_sirs,
                     patientid = Encounter,
                     time = Service_Timestamp,
                     vitals = c("HR" = "HR", "Temperature" = "Temperature")))

})
