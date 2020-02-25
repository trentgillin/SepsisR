library(testthat)
library(dplyr)
library(lubridate)
library(tidyr)


test_data <- SepsisR::sirs_data
test_data_window <- test_data[1,]
test_data_window <- dplyr::mutate(test_data_window, blood_culture_time = lubridate::ymd_hms("2018-12-10 10:32:00"))

test_that("That blood culture window function throws an error when timestamp_variable is not POSIXct", {
  test_data_window <- dplyr::mutate(test_data_window, Service_Timestamp = as.Date(Service_Timestamp))
  expect_error(find_bx_window(test_data_window, blood_culture_time = blood_culture_time, timestamp_variable = Service_Timestamp))
})

test_that("Function will throw an error when misspell timestamp_variable name", {
  expect_error(find_bx_window(test_data_window, blood_culture_time = blood_culture_time, timestamp_variable = service_Timestamp))
})

test_that("That blood culture window is corrent", {
  test_data_window <- find_bx_window(test_data_window, blood_culture_time = blood_culture_time, timestamp_variable = Service_Timestamp)
  expect_equal(test_data_window$within_window, TRUE)
})

test_that("Onset day gives me community", {
  qad <- c(lubridate::ymd("2019-10-01"))
  blood <- c(lubridate::ymd("2019-10-11"))
  encounter <- c(1)
  admission <- c(lubridate::ymd("2019-10-01"))
  test_onset <- data.frame(qad, blood, encounter, admission)
  test_onset <- find_onset(test_onset, patientid = encounter, first_qad = qad, blood_day = blood, admission_day = admission)
  expect_equal(test_onset$onset_type, "Community")
})

test_that("Onset day gives me hospital", {
  qad <- c(lubridate::ymd("2019-10-15"))
  blood <- c(lubridate::ymd("2019-10-11"))
  encounter <- c(1)
  admission <- c(lubridate::ymd("2019-10-01"))
  test_onset <- data.frame(qad, blood, encounter, admission)
  test_onset <- find_onset(test_onset, patientid = encounter, first_qad = qad, blood_day = blood, admission_day = admission)
  expect_equal(test_onset$onset_type, "Hospital")
})

test_that("The baseline function is correct", {
  result <- c(lubridate::ymd_hms("2019-10-15 02:00:00"), lubridate::ymd_hms("2019-10-20 03:00:00"))
  encounter <- c(1, 1)
  creat <-c(1.4, 2.4)
  onset <- c("Community", "Community")
  blood_time <- c(lubridate::ymd_hms("2019-10-15 01:00:00"))
  test_baseline <- data.frame(result, encounter, onset, creat, blood_time)
  rm(result)
  test_baseline <- get_baseline(test_baseline, labvalue = creat, patientid = encounter, result_day = result, onset_type = onset,
                                blood = blood_time)
  expect_equal(unique(test_baseline$baseline), 1.4)
})
