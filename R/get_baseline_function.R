#' Function to get the baseline lab value
#' @param .data Your dataset
#' @param labvalue Your lab value, can be bilirubin, creatinine, GFR, or platelets
#' @param  patientid A column denoting a unique patient identifier
#' @param result_day A Date column denoting when a lab resulted
#' @param onset_type A character value denoting if an encounter is community or hospital onset
#' @param blood A POSIXct indicating the blood culture time
#' @return Returns a dataset with a new column for the baseline of the specified lab value
#' @rdname get_baseline
#' @examples 
#' \donttest{
#' data <- get_baseline(creat, result_day = lab_results, .blood = blood_data)
#' }
#' @export

get_baseline <- function(.data, labvalue, patientid, result_day, onset_type, blood) {
  
  labvalue <- rlang::enquo(labvalue)
  patientid <- rlang::enquo(patientid)
  result_day <- rlang::enquo(result_day)
  onset_type <- rlang::enquo(onset_type)
  blood <- rlang::enquo(blood)
  
  class(.data) <- dplyr::case_when(stringr::str_detect(rlang::as_label(labvalue), "bili|creat")~ append(class(.data), "bili"),
                            stringr::str_detect(rlang::as_label(labvalue), "GFR|Platelet")~ append(class(.data), "plate"))
  
  
  UseMethod("get_baseline", .data)
}

#' baseline function for bilirubin and creatinine
#' @rdname get_baseline
#' @export
get_baseline.bili <- function(.data, labvalue, patientid, result_day, onset_type, blood) {
  
  baseline_community <- dplyr::filter(.data, !!onset_type == "Community")
  baseline_hospital <- dplyr::filter(.data, !!onset_type == "Hospital")
  
  baseline_community <- dplyr::group_by(baseline_community, !!patientid)
  baseline_community <- dplyr::mutate(baseline_community, baseline = min(!!labvalue, na.rm = TRUE))
  baseline_community <- dplyr::ungroup(baseline_community)
  
  if (nrow(baseline_hospital) > 0) {
  baseline_hospital <- find_bx_window(baseline_hospital, timestamp_variable = !!result_day, blood_culture_time = !!blood)
  baseline_hospital <- dplyr::filter(baseline_hospital, within_window == TRUE)
  baseline_hospital <- dplyr::group_by(baseline_hospital, !!patientid)
  baseline_hospital <- dplyr::mutate(baseline_hospital, baseline = min(!!labvalue, na.rm = TRUE))
  baseline_hospital <- dplyr::ungroup(baseline_hospital)
  baseline_together <- dplyr::bind_rows(baseline_hospital, baseline_community)
  } else {
    baseline_together <- baseline_community
  }
  
}

#' baseline function for GFR and Platelets
#' @rdname get_baseline
#' @export
get_baseline.plate <- function(.data, labvalue, patientid, result_day, onset_type, blood) {
  
  baseline_community <- dplyr::filter(.data, !!onset_type == "Community")
  baseline_hospital <- dplyr::filter(.data, !!onset_type == "Hospital")
  
  baseline_community <- dplyr::group_by(baseline_community, !!patientid)
  baseline_community <- dplyr::mutate(baseline_community, baseline = max(!!labvalue, na.rm = TRUE),
                               baseline = dplyr::if_else(stringr::str_detect(rlang::as_label(labvalue), "plate") & baseline < 100, as.double(NA), baseline),
                               baseline = dplyr::if_else(stringr::str_detect(rlang::as_label(labvalue), "gfr") & baseline >= 60, 60, baseline))
  baseline_community <- dplyr::ungroup(baseline_community)
  
  if (nrow(baseline_hospital) > 0) {
  baseline_hospital <- find_bx_window(baseline_hospital, timestamp_variable = !!result_day, blood_culture_time = !!blood)
  baseline_hospital <- dplyr::filter(baseline_hospital, within_window == TRUE)
  baseline_hospital <- dplyr::group_by(baseline_hospital, !!patientid)
  baseline_hospital <- dplyr::mutate(baseline_hospital, baseline = max(!!labvalue, na.rm = TRUE))
  baseline_hospital <- dplyr::ungroup(baseline_hospital)
  baseline_together <- dplyr::bind_rows(baseline_hospital, baseline_community)
  } else {
    baseline_together <- baseline_community
  }
}

# so that check will ignore begining and ending time
utils::globalVariables(c("onset_type", "baseline", "within_window"))
