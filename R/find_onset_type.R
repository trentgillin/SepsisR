# Need to id those that are community versus hospital
# for those that had blood cultures pre-admit, marked them as getting blood cultures on day 0
# Also if a patient had community, this overrode hospital onset


#' Onset Type Function
#' A function used to determine if a patient has community sepsis or hospital onset sepsis
#' @param .data Your dataset
#' @param blood_day This is a Date column that represents when your blood cultures were taken
#' @param first_qad This is a Date column that represents your first qualifying antibiotic day (see CDC definition) time
#' @param patientid A unique identifier for each patient
#' @param admission_day A Date object for the day a patient was admitted to the hospital
#' @return Returns a dataset with a new column that flags a patient has having 'Community' or 'Hosptial' onset
#' @examples 
#' \donttest{
#' final_data <- data %>% find_onset(blood_day = blood_service_day, 
#'                                   first_qad = first_qad, 
#'                                   patientid = encounter, 
#'                                  admission_day = admission_date)
#' }
#' @export

find_onset <- function(.data, blood_day, first_qad, patientid, admission_day) {
  blood_day <- rlang::enquo(blood_day)
  firt_quad <- rlang::enquo(first_qad)
  patientid <- rlang::enquo(patientid)
  admission_day <- rlang::enquo(admission_day)
  
  .data <- find_hospital_day(.data, event = !!blood_day, patientid = !!patientid, admission_day = !!admission_day)
  .data <- dplyr::rename(.data, "blood_hospital_day" = hospital_day)
  .data <- find_hospital_day(.data, event = !!first_qad, patientid = !!patientid, admission_day = !!admission_day)
  .data <- dplyr::rename(.data, "qad_hospital_day" = hospital_day)
  .data <- dplyr::group_by(.data, !!patientid)
  .data <- tidyr::fill(.data, tidyselect::contains("hospital_day"), .direction = "updown")
  .data <- dplyr:: mutate(.data, onset_type = dplyr::if_else(blood_hospital_day >= 3 & qad_hospital_day >= 3, 0, 1),
                          onset_type = max(onset_type),
                          onset_type = dplyr::if_else(onset_type == 1, "Community", "Hospital"),
                          onset_type = dplyr::if_else(is.na(onset_type), "Community", onset_type))
  .data <- dplyr::ungroup(.data)
  .data <- dplyr::select(.data, -tidyselect::contains("hospital_day"))
  .data <- dplyr::distinct(.data)
}

# so that check will ignore begining and ending time
utils::globalVariables(c("hospital_day", "blood_hospital_day", "qad_hospital_day", "onset_type"))