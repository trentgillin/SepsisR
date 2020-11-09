#' Function to calculate what hospital day event occurred
#' @param .data Your data
#' @param event Date object to be used against admit time
#' @param patientid A column denoting a unique patient identifier
#' @param admission_day First day that a patient arrived at the hospital
#' @examples 
#' \dontrun{
#' result <- find_hospital_day(blood_abx, Service_Day)
#' }
#' @rdname find_hospital_day
#' @export

find_hospital_day <- function(.data, event, patientid, admission_day) {
  UseMethod("find_hospital_day")
}

#' default method
#' @rdname find_hospital_day
#' @export

find_hospital_day.default <- function(.data, event, patientid, admission_day) {
  event <- rlang::enquo(event)
  patientid <- rlang::enquo(patientid)
  admission_day <- rlang::enquo(admission_day)
  
  .data <- dplyr::group_by(.data, !!patientid)
  .data <- tidyr::fill(.data, !!admission_day)
  .data <- dplyr::ungroup(.data)
  .data <- calc_time_between(.data, !!admission_day, !!event, "hospital_day", unitx = "days")
  .data <- dplyr::mutate(.data, hospital_day = hospital_day + 1,
                  hospital_day = dplyr::if_else(hospital_day < 0, 0, hospital_day))
}

# so that check will ignore begining and ending time
utils::globalVariables(c("hospital_day"))