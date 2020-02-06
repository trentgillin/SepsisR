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
