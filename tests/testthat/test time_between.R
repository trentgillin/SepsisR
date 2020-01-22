library(testthat)
test_sirs <- SepsisR::sirs_data

test_that("Calc time between works for hours", {
  set.seed(300)
  test_times <- dplyr::sample_n(test_sirs, 2)
  test_times <- dplyr::select(test_times, Service_Timestamp)
  test_times <- dplyr::mutate(test_times, time_cat = seq_along(Service_Timestamp),
                              time_cat = paste0("time_", time_cat))
  test_times <- tidyr::pivot_wider(test_times, names_from = time_cat, values_from = Service_Timestamp)
  test_times <- calc_time_between(test_times, time_2, time_1,"time_diff", unitx = "hours")
  expect_equal(test_times$time_diff, 1)
})

test_that("Calc time between works for minutes", {
  set.seed(300)
  test_times <- dplyr::sample_n(test_sirs, 2)
  test_times <- dplyr::select(test_times, Service_Timestamp)
  test_times <- dplyr::mutate(test_times, time_cat = seq_along(Service_Timestamp),
                              time_cat = paste0("time_", time_cat))
  test_times <- tidyr::pivot_wider(test_times, names_from = time_cat, values_from = Service_Timestamp)
  test_times <- calc_time_between(test_times, time_2, time_1,"time_diff", unitx = "mins")
  expect_equal(test_times$time_diff, 60)
})

test_that("Calc time gives error when not enter time unit correctly", {
  set.seed(300)
  test_times <- dplyr::sample_n(test_sirs, 2)
  test_times <- dplyr::select(test_times, Service_Timestamp)
  test_times <- dplyr::mutate(test_times, time_cat = seq_along(Service_Timestamp),
                              time_cat = paste0("time_", time_cat))
  test_times <- tidyr::pivot_wider(test_times, names_from = time_cat, values_from = Service_Timestamp)
  expect_error(calc_time_between(test_times, time_2, time_1,"time_diff", unitx = "minutes"))
})

test_that("Calc time gives error if give a character value for timestamp", {
  set.seed(300)
  test_times <- dplyr::sample_n(test_sirs, 2)
  test_times <- dplyr::select(test_times, Service_Timestamp)
  test_times <- dplyr::mutate(test_times, time_cat = seq_along(Service_Timestamp),
                              time_cat = paste0("time_", time_cat))
  test_times <- tidyr::pivot_wider(test_times, names_from = time_cat, values_from = Service_Timestamp)
  test_times <- dplyr::mutate(test_times, time_1 = as.character(time_1))
  expect_error(calc_time_between(test_times, time_2, time_1,"time_diff", unitx = "mins"))
})
