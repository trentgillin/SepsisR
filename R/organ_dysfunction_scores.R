#' SIRS calculation function
#'
#' Finds the current organ failure score using SIRS (Systematic Inflammatory Response Syndrome)
#'
#' @param .data The dataset you are working with. Must include heart rate (hr), respiratory rate (rr),
#' white blood cell count (wbc), and temperature.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long to carry over a vital when next is 
#' missing, default is one hour.
#' @param vitals A character vector denoting what columns represent the vitals needed 
#' to calculate SIRS, it is important to have temperature in Celsius and WBC in mmm^3
#' @return Retuns a dataset with a new column denoting the SIRS total
#' @examples
#' dataset <- find_sirs(sirs_data, Encounter, Service_Timestamp, 
#'    vitals = c("RR" = "RR", "Temperature" = "Temperature", "HR" = "HR", "WBC" = "Wbc"))
#' @export

find_sirs <- function(.data, patientid, time, period = 1,
                      vitals = c("Temperature" = NA,
                                 "RR" = NA,
                                 "HR" = NA,
                                 "WBC" = NA)) {

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)
  
  # save orignial
  .orig <- .data

  # make sure have all required vitals
  if (length(vitals) != 4) {
    stop("You need Temperature, RR, HR, and WBC to calculate sirs")
  }

  # Need to make sure that columns translate from user input
  .data <- dplyr::select(.data, !!patientid, !!time, tidyselect::all_of(vitals))

  # group by patient
  .data <- dplyr::group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- dplyr::arrange(.data, !!time)
  .data <- dplyr::mutate(.data, lag_time = dplyr::lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- dplyr::mutate(.data, time_diff = dplyr::if_else(is.na(time_diff), 0, time_diff))

  # fill in missing wbc, set to 24 hours, one wbc value good for 24 hours
  .data <- tidyr::fill(.data, WBC)
  .data <- dplyr::mutate(.data, WBC = dplyr::if_else(time_diff > 24, as.double(NA), WBC))

  # fill in other missing vitals
  .data <- tidyr::fill(.data, HR, RR, Temperature)
  .data <- dplyr::mutate_at(.data, dplyr::vars(HR, RR, Temperature), 
                            list(~dplyr::if_else(time_diff > period, as.double(NA), .)))

  # ungroup
  .data <- dplyr::ungroup(.data)

  # indicate when desired levels are hit
  .data <- dplyr::mutate(.data, hr_flag = dplyr::if_else(HR > 90, 1, 0),
                  rr_flag = dplyr::if_else(RR > 20, 1, 0),
                  temp_flag = dplyr::if_else(Temperature > 38 | Temperature < 36, 1, 0),
                  wbc_flag = dplyr::if_else(WBC < 4000 | WBC > 12000, 1, 0),
                  sirs_total = hr_flag + rr_flag + temp_flag + wbc_flag)

  # remove flag variables
  .data <- dplyr::select(.data, -tidyselect::contains("_flag"), -time_diff, -lag_time)
  
  # get important columns
  .data <- dplyr::select(.data, !!patientid, !!time, sirs_total)
  
  # add back to orignial
  .data <- dplyr::left_join(.orig, .data, by = c(rlang::quo_name(patientid), rlang::quo_name(time)))

  return(.data)
}

#' SOFA calculation function
#'
#' Finds the current organ failure score using SOFA.
#'
#' @param .data The dataset you are working with. Must contain 
#' PaO2, FiO2, bilirubin, vasopressor, vasopressor dosage, creatinine, glasgow coma scale (gcs), 
#' systolic blood pressure (sbc), diastolic blood pressure (dbc), and platelet count columns.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long to carry over a vital when next is 
#' missing, default is one hour.
#' @param vitals A character vector denoting what columns represent the vitals. It is important to 
#' remember that right now only dopamine and dobutamine are considered in vasopressors used.Platelets should 
#' be in 10^3/microliter and FiO2 should be listed as a percent, not a decimal.Furthermore bilirubin
#' should be in micrograms/deciliter. 
#' needed to calculate SOFA
#' @return Returns a dataset with a new column denoting the SOFA total
#' @examples
#' result <- find_sofa(.data = sofa_data, patientid = Encounter, time = Service_Timestamp, 
#'  vitals = c("PaO2" = "Pa02", "FiO2" = "Fi02", 
#'  "Platelets" = "Platelet", "Bilirubin" = "Bili", "GCS" = "GCS", 
#'  "Creatinine" = "Creatinine", "SBP" = "SBP", "DBP" = "DBP", "Vasopressor" = "Vasopressor", 
#'  "Vasopressor_dose" = "Vasopressor Dosage"))
#' @export

find_sofa <- function(.data, patientid, time, period = 1,
                      vitals = c("PaO2" = NA,
                                 "FiO2" = NA,
                                 "Platelets" = NA,
                                 "Bilirubin" = NA,
                                 "GCS" = NA,
                                 "Creatinine" = NA,
                                 "SBP" = NA,
                                 "DBP" = NA,
                                 "Vasopressor" = NA,
                                 "Vasopressor_dose" = NA)) {
  if (length(vitals) != 10) {
    stop("You need PaO2, FiO2, Platelets, Bilirubin, 
         GCS, Creatinie, SBP, DBP, Vasopressor, and Vasopressor Doses to calculate SOFA")
  }

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)
  
  # save orignial
  .orig <- .data

  # Need to make sure that columns translate from user input
  .data <- dplyr::select(.data, !!patientid, !!time, tidyselect::all_of(vitals))

  # calculate Pao2/FiO2
  .data <- dplyr::mutate(.data, FiO2 = FiO2 * 100,
                         PaO2_FiO2 = PaO2/FiO2)

  # group by patient
  .data <- dplyr::group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- dplyr::arrange(.data, !!time)
  .data <- dplyr::mutate(.data, lag_time = dplyr::lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- dplyr::mutate(.data, time_diff = dplyr::if_else(is.na(time_diff), 0, time_diff))

  # fill in vitals
  .data <- tidyr::fill(.data, PaO2_FiO2, Platelets, Bilirubin, GCS, Creatinine, 
                       Vasopressor, Vasopressor_dose, SBP, DBP)
  .data <- dplyr::mutate_at(.data, dplyr::vars(PaO2_FiO2, Platelets, Bilirubin, GCS, 
                                        Creatinine, Vasopressor_dose, SBP, DBP), 
                            list(~dplyr::if_else(time_diff > period, as.double(NA), .)))
  .data <- dplyr::mutate_at(.data, dplyr::vars(Vasopressor), 
                            list(~dplyr::if_else(time_diff > period, NA_character_, .)))

  # ungroup
  .data <- dplyr::ungroup(.data)

  # indicate when desired levels are hit
  .data <- dplyr::mutate(.data, PaO2_FiO2_flag = dplyr::case_when(
    dplyr::between(PaO2_FiO2, 400, 300) ~ 1,
    dplyr::between(PaO2_FiO2, 299, 200) ~ 2,
    dplyr::between(PaO2_FiO2, 199, 100) ~ 3,
    PaO2_FiO2 < 100 ~ 4),
    platelets_flag = dplyr::case_when(dplyr::between(Platelets, 150, 100) ~ 1,
                                      dplyr::between(Platelets, 99, 50) ~ 2,
                                      dplyr::between(Platelets, 49, 20) ~ 3,
                                      Platelets < 20 ~ 4),
    bilirubin_flag = dplyr::case_when(dplyr::between(Bilirubin, 1.2, 1.9) ~ 1,
                               dplyr::between(Bilirubin, 2.0, 5.9) ~ 2,
                               dplyr::between(Bilirubin, 6.0, 11.9) ~ 3,
                               Bilirubin > 12.0 ~ 4),
    gcs_flag = dplyr::case_when(dplyr::between(GCS, 13, 14) ~ 1,
                         dplyr::between(GCS, 10, 12) ~ 2,
                         dplyr::between(GCS, 6, 9) ~ 3,
                         GCS < 6 ~ 4),
    creatinine_flag = dplyr::case_when(dplyr::between(Creatinine, 1.2, 1.9) ~ 1,
                                dplyr::between(Creatinine, 2.0, 3.4) ~ 2,
                                dplyr::between(Creatinine, 3.5, 4.9) ~ 3,
                                Creatinine > 5.0 ~ 4))
  # cardiac flag
  .data <-  calc_card_sofa(.data, SBP, DBP, Vasopressor, Vasopressor_dose)

  # fill in NAs with zeros
  .data <- dplyr::mutate_at(.data, dplyr::vars(tidyselect::contains("_flag")), 
                            list(~dplyr::if_else(is.na(.), 0, .)))
  # create sofa total
  .data <-dplyr::mutate(.data,
                 sofa_total = PaO2_FiO2_flag + platelets_flag + bilirubin_flag +
                   gcs_flag + cardiovascular_flag + creatinine_flag)

  # get important columns
  .data <- dplyr::select(.data, !!patientid, !!time, sofa_total)
  
  # add back to orignial
  .data <- dplyr::left_join(.orig, .data, by = c(rlang::quo_name(patientid), rlang::quo_name(time)))

  return(.data)
}


#' qSOFA calculation function
#'
#' Finds the current qSOFA score
#'
#' @param .data Your dataset. Must have respiratory rate (rr), systolic blood pressure (sbp). 
#' and glasgow coma scale (gcs).
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long to carry over a vital when next is 
#' missing, default is one hour.
#' @param vitals A character vector denoting what columns represent the vitals 
#' needed to calculate qSOFA
#' @return Returns a dataset with a new column denoting the qSOFA total
#' @examples
#' result <- find_qsofa(qsofa_data, patientid = Encounter, time = Service_Timestamp, 
#' vitals = c("RR" = "RR", "SBP" = "SBP", "GCS" = "GCS"))
#' @export

find_qsofa <- function(.data, patientid, time, period = 1,
                       vitals = c("RR" = NA,
                       "SBP" = NA,
                        "GCS" = NA)) {

   if (length(vitals) != 3) {
     stop("You need RR, SBP, and GCS to calculate qSOFA")
   }

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)
  
  # save orignial
  .orig <- .data

  # Need to make sure that columns translate from user input
  .data <- dplyr::select(.data, !!patientid, !!time, tidyselect::all_of(vitals))

  # group data
  .data <- dplyr::group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- dplyr::arrange(.data, !!time)
  .data <- dplyr::mutate(.data, lag_time = dplyr::lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- dplyr::mutate(.data, time_diff = dplyr::if_else(is.na(time_diff), 0, time_diff))

  # fill in other missing vitals
  .data <- tidyr::fill(.data, RR, SBP, GCS)
  .data <- dplyr::mutate_at(.data, dplyr::vars(RR, SBP, GCS), 
                            list(~dplyr::if_else(time_diff > period, as.double(NA), .)))

  # ungroup
  .data <- dplyr::ungroup(.data)
  # indicate when levels are hit
  .data <- dplyr::mutate(.data,
                  rr_flag = dplyr::if_else(RR >= 22, 1, 0),
                  sbp_flag = dplyr::if_else(SBP <= 100, 1, 0),
                  gcs_flag = dplyr::if_else(GCS < 15, 1, 0))

  # fill in NAs
  .data <- dplyr::mutate_at(.data, dplyr::vars(tidyselect::contains("_flag")), 
                            list(~dplyr::if_else(is.na(.), 0, .)))

  # sum up
  .data <- dplyr::mutate(.data, qsofa_total = rr_flag + sbp_flag + gcs_flag)

  # get important columns
  .data <- dplyr::select(.data, !!patientid, !!time, qsofa_total)
  
  # add back to orignial
  .data <- dplyr::left_join(.orig, .data, by = c(rlang::quo_name(patientid), rlang::quo_name(time)))

  return(.data)
}

# so that check will ignore global variables
utils::globalVariables(c("time_diff", "lag_time", "RR", "SBP", "DBP", "GCS", "Creatinine",
                         "WBC", "Temperature", "PaO2", "FiO2", "PaO2_FiO2", "Platelets", 
                         "Bilirubin", "Vasopressor", "Vasopressor_dose", "MAP",
                         "HR", "wb_flag", "rr_flag", "sbp_flag", "gcs_flag",
                         "hr_flag", "temp_flag", "wbc_flag", "PaO2_FiO2_flag",
                         "cardiovascular_flag", "creatinine_flag", "platelets_flag",
                       "bilirubin_flag", "qsofa_total", "sirs_total", "sofa_total"))
