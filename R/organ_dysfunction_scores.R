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
#' dataset <- find_sirs(dataset, Encounter, `Vital Timestamp`, vitals = c("rr" = "RR", "temperature" = "Temperature", "hr" = "HR", "wbc" = "Wbc"))
#' @export

find_sirs <- function(.data, patientid, time, period = 1,
                      vitals = c("Temperature" = NA,
                                 "RR" = NA,
                                 "HR" = NA,
                                 "WBC" = NA)) {

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)

  # make sure have all required vitals
  if (length(vitals) != 4) {
    stop("You need Temperature, RR, HR, and WBC to calculate sirs")
  }

  # Need to make sure that columns translate from user input
  .data <- rename_at(.data, vars(vitals), list(~c("Temperature", "RR", "HR", "WBC")))

  # group by patient
  .data <- group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- arrange(.data, !!time)
  .data <- mutate(.data, lag_time = lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- mutate(.data, time_diff = if_else(is.na(time_diff), 0, time_diff))

  # fill in missing wbc, set to 24 hours, one wbc value good for 24 hours
  .data <- fill(.data, WBC)
  .data <- mutate(.data, WBC = if_else(time_diff > 24, as.double(NA), WBC))

  # fill in other missing vitals
  .data <- fill(.data, HR, RR, Temperature)
  .data <- mutate_at(.data, vars(HR, RR, Temperature), list(~if_else(time_diff > period, as.double(NA), .)))

  # ungroup
  .data <- ungroup(.data)

  # indicate when desired levels are hit
  .data <- mutate(.data, hr_flag = if_else(HR > 90, 1, 0),
                  rr_flag = if_else(RR > 20, 1, 0),
                  temp_flag = if_else(Temperature > 38 | Temperature < 36, 1, 0),
                  wbc_flag = if_else(WBC < 4000 | WBC > 12000, 1, 0),
                  sirs_total = hr_flag + rr_flag + temp_flag + wbc_flag)

  # remove flag variables
  .data <- select(.data, -contains("_flag"), -time_diff, -lag_time)

  # rename columns back to what they were
  .data <- rename_at(.data, vars(Temperature, RR, HR, WBC), list(~c(vitals)))

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
  if (length(vitals) != 10) {
    stop("You need PaO2, FiO2, Platelets, Bilirubin, GCS, Creatinie, SBP, DBP, Vasopressor, and Vasopressor Doses to calculate SOFA")
  }

  patientid <- enquo(patientid)
  time <- enquo(time)

  # rename columns
  .data <- rename_at(.data, vars(vitals), list(~c("PaO2", "FiO2", "Platelets", "Bilirubin", "GCS", "Creatinine", "SBP", "DBP", "Vasopressor", "Vasopressor_dose")))

  # calculate Pao2/FiO2
  .data <- mutate(.data, PaO2_FiO2 = PaO2/FiO2)

  # group by patient
  .data <- group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- arrange(.data, !!time)
  .data <- mutate(.data, lag_time = lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- mutate(.data, time_diff = if_else(is.na(time_diff), 0, time_diff))

  # fill in vitals
  .data <- fill(.data, PaO2_FiO2, Platelets, Bilirubin, GCS, Creatinine, Vasopressor, Vasopressor_dose, SBP, DBP)
  .data <- mutate_at(.data, vars(PaO2_FiO2, Platelets, Bilirubin, GCS, Creatinine, Vasopressor_dose, SBP, DBP), list(~if_else(time_diff > period, as.double(NA), .)))
  .data <- mutate_at(.data, vars(Vasopressor), list(~if_else(time_diff > period, NA_character_, .)))

  # ungroup
  .data <- ungroup(.data)

  # indicate when desired levels are hit
  .data <- mutate(.data, PaO2_FiO2_flag = case_when(
    PaO2_FiO2 < 400 ~ 1,
    PaO2_FiO2 < 300 ~ 2,
    PaO2_FiO2 < 200 ~ 3,
    PaO2_FiO2 < 100 ~ 4),
    platelets_flag = case_when(Platelets < 150 ~ 1,
                               Platelets < 100 ~ 2,
                               Platelets < 50 ~ 3,
                               Platelets < 20 ~ 4),
    bilirubin_flag = case_when(between(Bilirubin, 1.2, 1.9) ~ 1,
                               between(Bilirubin, 2.0, 5.9) ~ 2,
                               between(Bilirubin, 6.0, 11.9) ~ 3,
                               Bilirubin > 12.0 ~ 4),
    gcs_flag = case_when(between(GCS, 13, 14) ~ 1,
                         between(GCS, 10, 12) ~ 2,
                         between(GCS, 6, 9) ~ 3,
                         GCS < 6 ~ 4),
    creatinine_flag = case_when(between(Creatinine, 1.2, 1.9) ~ 1,
                                between(Creatinine, 2.0, 3.4) ~ 2,
                                between(Creatinine, 3.5, 4.9) ~ 3,
                                Creatinine > 5.0 ~ 4))
  # cardiac flag
  .data <-  calc_card_sofa(.data, SBP, DBP, Vasopressor, Vasopressor_dose)

  # fill in NAs with zeros
  .data <- mutate_at(.data, vars(contains("_flag")), list(~if_else(is.na(.), 0, .)))
  # create sofa total
  .data <-mutate(.data,
                 sofa_total = PaO2_FiO2_flag + platelets_flag + bilirubin_flag +
                   gcs_flag + cardiovascular_flag + creatinine_flag)

  # select important columns
  .data <- select(.data,
                  -time_diff,
                  -lag_time,
                  -contains("_flag"),
                  -MAP)

  # rename columns back
  .data <- rename_at(.data, vars(PaO2, FiO2, Platelets, Bilirubin, GCS, Creatinine, SBP, DBP, Vasopressor, Vasopressor_dose), list(~vitals))

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
#' result <- find_sirs(data, patientid = Encounter, time = Servie_Time, vitals = c("RR = "rr, "SBP" = "sbp", "GCS" = "gcs"))
#' @export

find_qsofa <- function(.data, patientid, time, period = 1,
                       vitals = c("RR" = NA,
                       "SBP" = NA,
                        "GCS" = NA)) {

   if (length(vitals) != 3) {
     stop("You need RR, SBP, and GCS to calculate qSOFA")
   }

  patientid <- enquo(patientid)
  time <- enquo(time)

  # rename columns
  .data <- rename_at(.data, vars(vitals), list(~c("RR", "SBP", "GCS")))

  # group data
  .data <- group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- arrange(.data, !!time)
  .data <- mutate(.data, lag_time = lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- mutate(.data, time_diff = if_else(is.na(time_diff), 0, time_diff))

  # fill in other missing vitals
  .data <- fill(.data, RR, SBP, GCS)
  .data <- mutate_at(.data, vars(RR, SBP, GCS), list(~if_else(time_diff > period, as.double(NA), .)))

  # ungroup
  .data <- ungroup(.data)
  # indicate when levels are hit
  .data <- mutate(.data,
                  rr_flag = if_else(RR >= 22, 1, 0),
                  sbp_flag = if_else(SBP <= 100, 1, 0),
                  gcs_flag = if_else(GCS < 15, 1, 0))

  # fill in NAs
  .data <- mutate_at(.data, vars(contains("_flag")), list(~if_else(is.na(.), 0, .)))

  # sum up
  .data <- mutate(.data, qsofa_total = rr_flag + sbp_flag + gcs_flag)

  # remove time_diff, and flags
  .data <- select(.data,
                  -time_diff,
                  -lag_time,
                  -contains("_flag"))

  # rename columns back
  .data <- rename_at(.data, vars(RR, SBP, GCS), list(~vitals))

  return(.data)
}
