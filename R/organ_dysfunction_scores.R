#' SIRS calculation function
#'
#' Finds the current organ failure score using SIRS.
#'
#' @param .data The dataset you are working with.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @param vitals A character vector denoting what columns represent the vitals needed to calculate SIRS, it is important to have temperature in Celsius and WBC in mmm^3
#' @examples
#' dataset <- find_sirs(dataset, Encounter, `Vital Timestamp`, vitals = c("rr" = "RR", "temperature" = "Temperature", "hr" = "HR", "wbc" = "Wbc))
#' @export

find_sirs <- function(.data, patientid, time, period = 1,
                      vitals = c("temperature" = NA,
                                 "rr" = NA,
                                 "hr" = NA,
                                 "wbc" = NA)) {

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)

  # make sure have all required vitals
  if (sum(is.na(vitals)) > 0) {
    stop("You need temperature, rr, hr, and wbc to calculate sirs")
  }

  # Need to make sure that columns translate from user input
  .data <- rename_at(.data, vars(vitals[["temperature"]], vitals[["rr"]], vitals[["hr"]], vitals[["wbc"]]), list(~c("temp", "rr", "hr", "wbc")))

  # group by patient
  .data <- group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- arrange(.data, !!time)
  .data <- mutate(.data, lag_time = lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- mutate(.data, time_diff = if_else(is.na(time_diff), 0, time_diff))

  # fill in missing wbc, set to 24 hours, one wbc value good for 24 hours
  .data <- fill(.data, wbc)
  .data <- mutate(.data, wbc = if_else(time_diff > 24, as.double(NA), wbc))

  # fill in other missing vitals
  .data <- fill(.data, hr, rr, temp)
  .data <- mutate_at(.data, vars(hr, rr, temp), list(~if_else(time_diff > period, as.double(NA), .)))

  # indicate when desired levels are hit
  .data <- mutate(.data, hr_flag = if_else(hr > 90, 1, 0),
                  rr_flag = if_else(rr > 20, 1, 0),
                  temp_flag = if_else(temp > 38 | temp < 36, 1, 0),
                  wbc_flag = if_else(wbc < 4000 | wbc > 12000, 1, 0),
                  sirs_total = hr_flag + rr_flag + temp_flag + wbc_flag)

  # remove flag variables
  .data <- select(.data, -contains("_flag"), -time_diff, -lag_time)

  # rename columns back to what they were
  .data <- rename_at(.data, vars(temp, rr, hr, wbc), list(~c(vitals[["temperature"]], vitals[["rr"]], vitals[["hr"]], vitals[["wbc"]])))

  return(.data)
}

#' SOFA calculation function
#'
#' Finds the current organ failure score using SOFA.
#'
#' @param .data The dataset you are working with. Must contain PaO2, FiO2, bilirubin, MAP, vasopressor, creatinine, gcs,  and platelet columns.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @param vitals A character vector denoting what columns represent the vitals needed to calculate SOFA
#' @examples
#' dataset <- find_sofa(dataset, Encounter, `Vital Timestamp`)
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
                                 "Vasopressor_Dose" = NA)) {
  if (sum(is.na(vitals) > 0)) {
    stop("You need PaO2, FiO2, Platelets, Bilirubin, GCS, Creatinie, SBP, DBP, Vasopressor, and Vasopressor Doses to calculate SOFA")
  }

  parentid <- enquo(parentid)
  time <- enquo(time)

  # rename columns
  .data <- rename_at(vars(vital), list(~c("PaO2", "FiO2", "Platelets", "Bilirubin", "GCS", "Creatinine", "SBP", "DBP", "Vasopressor", "Vasopressor_Dose")))

  # calculate Pao2/FiO2
  .data <- mutate(.data, PaO2_FiO2 = PaO2/FiO2)

  # indicate when desired levels are hit
  .data <- mutate(.data, PaO2_FiO2_flag = case_when(
    PaO2_FiO2 < 400 ~ 1,
    PaO2_FiO2 < 300 ~ 2,
    PaO2_FiO2 < 200 ~ 3,
    PaO2_FiO2 < 100 ~ 4),
    platelets_flag = case_when(platelets < 150 ~ 1,
                               platelets < 100 ~ 2,
                               platelets < 50 ~ 3,
                               platelets < 20 ~ 4),
    bilirubin_flag = case_when(between(bilirubin, 1.2, 1.9) ~ 1,
                               between(bilirubin, 2.0, 5.9) ~ 2,
                               between(bilirubin, 6.0, 11.9) ~ 3,
                               bilirubin > 12.0 ~ 4),
    gcs_flag = case_when(between(gcs, 13, 14) ~ 1,
                         between(gcs, 10, 12) ~ 2,
                         between(gcs, 6, 9) ~ 3,
                         gcs <6 ~ 4),
    cardiovascular_flag = calc_card_sofa(.data, sbp, dbp, vasopressor, vasopressor_dose),
    creatinine_flag = case_when(between(creatinine, 1.2, 1.9) ~ 1,
                                between(creatinine, 2.0, 3.4) ~ 2,
                                between(creatinine, 3.5, 4.9) ~ 3,
                                creatinine > 5.0, ~ 4))
  # create sofa total
  .data <-mutate(.data,
                 sofa_total = PaO2_FiO2_flag + platelets_flag + bilirubin_flag +
                   gcs_flag + cardiovascular_flag + creatinine_flag)

  # rename columns back
  .data <- rename_at(.data (vars()PaO2, FiO2, Platelets, Bilirubin, GCS, Creatinine, SBP, DBP, Vasopressor, Vasopressor_Dose), list(~vitals))

  return(.data)
}


#' qSOFA calculation function
#'
#' Finds the current qSOFA score
#'
#'@param .data Your dataset
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @examples
#' @export

find_qsofa <- function(.data, patientid, time, period = 1) {
  if (!"rr" %in% colnames(.data)) {
    stop("You need respiratory rate to calcuate qsofa")
  } else if (!"sbp" %in% colnames(.data)) {
    stop("You need systolic blood pressure to calcuate qsofa")
  } else if (!"gcs" %in% colnames(.data)) {
    stop("You need glasgow coma scale to calcuate qsofa")
  }

}
