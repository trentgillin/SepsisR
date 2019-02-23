#' SIRS calculation function
#'
#' Finds the current organ failure score using SIRS.
#'
#' @param .data The dataset you are working with.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @examples
#' dataset <- find_sirs(dataset, Encounter, `Vital Timestamp`)
#' @export

find_sirs <- function(.data, patientid, time, period = 1) {
  if(!"temperature" %in% colnames(.data)) {
    stop("You need temperature to calculate sirs, please make sure your temperatures are in Celsius")
  }  else if (!"rr" %in% colnames(.data)) {
    stop("You need rr to calculate sirs")
  } else if (!"hr" %in% colnames(.data)) {
    stop("You need hr to calculate sirs")
  } else if (!"wbc" %in% colnames(.data)) {
    stop("You need wbc to calculate sirs, please make sure your wbcs are per mm ^3 ")
  }

  patientid <- enquo(patientid)
  time <- enquo(time)

  # group by patient
  .data <- group_by(.data, !!patientid)

  # calculate time between vital signs
  .data <- arrange(.data, !!time)
  .data <- mutate(.data, lag_time = lag(!!time))
  .data <- calc_time_between(.data, !!time, lag_time, "time_diff", unitx = "hours")
  .data <- mutate(.data, time_diff = if_else(is.na(time_diff), 0, time_diff))

  # fill in missing wbc, set to 24 hours
  .data <- fill(.data, wbc)
  .data <- mutate(.data, wbc = if_else(time_diff > 24, NA_integer_, wbc))

  # fill in other missing vitals
  .data <- fill(.data, hr, rr, temperature)
  .data <- mutate_at(.data, vars(hr, rr, temperature), funs(if_else(time_diff > period, NA_integer_, .)))

  # indicate when desired levels are hit
  .data <- mutate(.data, hr_flag = if_else(hr > 90, 1, 0),
                  rr_flag = if_else(rr >20, 1, 0),
                  temp_flag = if_else(temperature > 38 | temperature < 36, 1, 0),
                  wbc_flag = if_else(wbc < 4000 | wbc > 12000, 1, 0),
                  sirs_total = hr_flag + rr_flag + temp_flag + wbc_flag)

  # remove flag variables
  .data <- select(.data, -contains("_flag"), -time_diff, -lag_time)

  return(.data)
}
